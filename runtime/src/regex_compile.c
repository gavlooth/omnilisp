/*
 * regex_compile.c - Pika-style Regex Engine Implementation
 *
 * Self-contained regex engine using Pika-style PEG matching.
 * Compiles regex to AST and matches directly against the AST.
 *
 * Pipeline:
 *   1. Tokenizer: regex string -> token stream
 *   2. Parser: token stream -> AST (respecting precedence)
 *   3. Matcher: AST + input -> match result
 *
 * Supported Features:
 *   - Literals: abc
 *   - Character classes: [abc], [a-z], [^abc]
 *   - Any character: .
 *   - Quantifiers: *, +, ?, {n}, {n,}, {n,m}
 *   - Possessive quantifiers: *+, ++, ?+ (no backtracking)
 *   - Alternation: a|b
 *   - Grouping: (ab)+
 *   - Non-capturing groups: (?:...)
 *   - Lookahead: (?=...) positive, (?!...) negative
 *   - Anchors: ^ (start), $ (end), \b (word boundary)
 *   - Escapes: \d, \D, \w, \W, \s, \S, \n, \t, \r, \\
 */

#include "regex_compile.h"
#include "internal_types.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

/* ============== AST Node Types ============== */

typedef enum {
    AST_LITERAL,     /* Single character */
    AST_CHAR_CLASS,  /* Character class */
    AST_DOT,         /* Any character */
    AST_SEQ,         /* Sequence of nodes */
    AST_ALT,         /* Alternation */
    AST_REP,         /* Repetition (*, +, ?) */
    AST_GROUP,       /* Grouping (just for structure) */
    AST_ANCHOR,      /* Anchor: ^, $, \b */
    AST_LOOKAHEAD    /* Lookahead: (?=...) or (?!...) */
} ASTNodeType;

/* Anchor types */
typedef enum {
    ANCHOR_START,    /* ^ - start of string */
    ANCHOR_END,      /* $ - end of string */
    ANCHOR_WORD_BOUNDARY  /* \b - word boundary */
} AnchorType;

/* Character class: stores ranges and individual chars */
typedef struct {
    char ranges[64][2];  /* Start-end pairs for ranges */
    int num_ranges;
    char chars[64];      /* Individual characters */
    int num_chars;
    bool negated;        /* [^...] negation */
} CharClass;

typedef struct ASTNode {
    ASTNodeType type;
    union {
        char ch;              /* AST_LITERAL */
        CharClass* cclass;    /* AST_CHAR_CLASS */
        struct {              /* AST_SEQ, AST_ALT */
            struct ASTNode** children;
            int num_children;
        } list;
        struct {              /* AST_REP */
            struct ASTNode* child;
            int min;          /* 0 for *, 1 for +, 0 for ? */
            int max;          /* -1 for unlimited, 1 for ? */
            bool possessive;  /* true for *+, ++, ?+ */
        } rep;
        struct ASTNode* group; /* AST_GROUP */
        AnchorType anchor;    /* AST_ANCHOR */
        struct {              /* AST_LOOKAHEAD */
            struct ASTNode* child;
            bool positive;    /* true for (?=...), false for (?!...) */
        } lookahead;
    } data;
} ASTNode;

/* Compiled regex structure */
struct PikaRegexCompiled {
    ASTNode* ast;      /* Root of AST */
    char* error;       /* Error message if compilation failed */
};

/* ============== Token Types ============== */

typedef enum {
    TOK_LITERAL,     /* Single character literal */
    TOK_CHAR_CLASS,  /* Character class [abc] or [a-z] */
    TOK_DOT,         /* Any character */
    TOK_STAR,        /* Zero or more */
    TOK_PLUS,        /* One or more */
    TOK_QUESTION,    /* Optional */
    TOK_STAR_POSS,   /* Possessive zero or more *+ */
    TOK_PLUS_POSS,   /* Possessive one or more ++ */
    TOK_QUEST_POSS,  /* Possessive optional ?+ */
    TOK_PIPE,        /* Alternation */
    TOK_LPAREN,      /* Left paren ( */
    TOK_RPAREN,      /* Right paren ) */
    TOK_NONCAP,      /* Non-capturing group (?: */
    TOK_LOOKAHEAD_POS, /* Positive lookahead (?= */
    TOK_LOOKAHEAD_NEG, /* Negative lookahead (?! */
    TOK_LBRACE,      /* Left brace { for quantifiers */
    TOK_ANCHOR_START,/* ^ anchor */
    TOK_ANCHOR_END,  /* $ anchor */
    TOK_WORD_BOUND,  /* \b word boundary */
    TOK_END          /* End of pattern */
} TokenType;

typedef struct {
    TokenType type;
    union {
        char ch;           /* For TOK_LITERAL */
        CharClass* cclass; /* For TOK_CHAR_CLASS */
        struct {           /* For TOK_LBRACE (bounded quantifier) */
            int min;
            int max;       /* -1 for unbounded {n,} */
            bool possessive;
        } bounds;
    } data;
} Token;

/* ============== Tokenizer ============== */

typedef struct {
    const char* pattern;
    int pos;
    int len;
    char* error;
} Tokenizer;

static Tokenizer* tokenizer_new(const char* pattern) {
    Tokenizer* t = malloc(sizeof(Tokenizer));
    t->pattern = pattern;
    t->pos = 0;
    t->len = strlen(pattern);
    t->error = NULL;
    return t;
}

static void tokenizer_free(Tokenizer* t) {
    if (t->error) free(t->error);
    free(t);
}

static void tokenizer_error(Tokenizer* t, const char* msg) {
    if (t->error) free(t->error);
    t->error = malloc(256);
    snprintf(t->error, 256, "Regex error at position %d: %s", t->pos, msg);
}

static char tokenizer_peek(Tokenizer* t) {
    if (t->pos >= t->len) return '\0';
    return t->pattern[t->pos];
}

static char tokenizer_advance(Tokenizer* t) {
    if (t->pos >= t->len) return '\0';
    return t->pattern[t->pos++];
}

static CharClass* parse_char_class(Tokenizer* t) {
    CharClass* cc = malloc(sizeof(CharClass));
    cc->num_ranges = 0;
    cc->num_chars = 0;
    cc->negated = false;

    /* Skip opening [ */
    tokenizer_advance(t);

    /* Check for negation */
    if (tokenizer_peek(t) == '^') {
        cc->negated = true;
        tokenizer_advance(t);
    }

    while (tokenizer_peek(t) != ']' && tokenizer_peek(t) != '\0') {
        char c = tokenizer_advance(t);

        /* Handle escape in char class */
        if (c == '\\') {
            c = tokenizer_advance(t);
            switch (c) {
                case 'n': c = '\n'; break;
                case 't': c = '\t'; break;
                case 'r': c = '\r'; break;
                case 'd': /* \d = 0-9 */
                    if (cc->num_ranges < 64) {
                        cc->ranges[cc->num_ranges][0] = '0';
                        cc->ranges[cc->num_ranges][1] = '9';
                        cc->num_ranges++;
                    }
                    continue;
                case 'w': /* \w = a-zA-Z0-9_ */
                    if (cc->num_ranges < 61) {
                        cc->ranges[cc->num_ranges][0] = 'a';
                        cc->ranges[cc->num_ranges][1] = 'z';
                        cc->num_ranges++;
                        cc->ranges[cc->num_ranges][0] = 'A';
                        cc->ranges[cc->num_ranges][1] = 'Z';
                        cc->num_ranges++;
                        cc->ranges[cc->num_ranges][0] = '0';
                        cc->ranges[cc->num_ranges][1] = '9';
                        cc->num_ranges++;
                    }
                    if (cc->num_chars < 64) cc->chars[cc->num_chars++] = '_';
                    continue;
                case 's': /* \s = space, tab, newline, return */
                    if (cc->num_chars < 60) {
                        cc->chars[cc->num_chars++] = ' ';
                        cc->chars[cc->num_chars++] = '\t';
                        cc->chars[cc->num_chars++] = '\n';
                        cc->chars[cc->num_chars++] = '\r';
                    }
                    continue;
                default:
                    /* Literal escaped char */
                    break;
            }
        }

        /* Check for range */
        if (tokenizer_peek(t) == '-' && t->pos + 1 < t->len && t->pattern[t->pos + 1] != ']') {
            tokenizer_advance(t); /* Skip - */
            char end = tokenizer_advance(t);
            if (end == '\\') {
                end = tokenizer_advance(t);
                switch (end) {
                    case 'n': end = '\n'; break;
                    case 't': end = '\t'; break;
                    case 'r': end = '\r'; break;
                    default: break;
                }
            }
            if (cc->num_ranges < 64) {
                cc->ranges[cc->num_ranges][0] = c;
                cc->ranges[cc->num_ranges][1] = end;
                cc->num_ranges++;
            }
        } else {
            if (cc->num_chars < 64) {
                cc->chars[cc->num_chars++] = c;
            }
        }
    }

    if (tokenizer_peek(t) == ']') {
        tokenizer_advance(t);
    } else {
        tokenizer_error(t, "Unclosed character class");
    }

    return cc;
}

/* Parse bounded quantifier {n}, {n,}, {n,m} */
static Token parse_bounded_quantifier(Tokenizer* t) {
    Token tok;
    tok.type = TOK_LBRACE;
    tok.data.bounds.min = 0;
    tok.data.bounds.max = 0;
    tok.data.bounds.possessive = false;

    tokenizer_advance(t); /* Skip { */

    /* Parse min */
    int min = 0;
    while (tokenizer_peek(t) >= '0' && tokenizer_peek(t) <= '9') {
        min = min * 10 + (tokenizer_advance(t) - '0');
    }
    tok.data.bounds.min = min;

    if (tokenizer_peek(t) == '}') {
        /* {n} - exact count */
        tokenizer_advance(t);
        tok.data.bounds.max = min;
    } else if (tokenizer_peek(t) == ',') {
        tokenizer_advance(t); /* Skip , */
        if (tokenizer_peek(t) == '}') {
            /* {n,} - at least n */
            tokenizer_advance(t);
            tok.data.bounds.max = -1; /* Unbounded */
        } else {
            /* {n,m} - between n and m */
            int max = 0;
            while (tokenizer_peek(t) >= '0' && tokenizer_peek(t) <= '9') {
                max = max * 10 + (tokenizer_advance(t) - '0');
            }
            tok.data.bounds.max = max;
            if (tokenizer_peek(t) == '}') {
                tokenizer_advance(t);
            } else {
                tokenizer_error(t, "Expected } in bounded quantifier");
            }
        }
    } else {
        tokenizer_error(t, "Invalid bounded quantifier syntax");
        tok.type = TOK_LITERAL;
        tok.data.ch = '{';
        return tok;
    }

    /* Check for possessive + */
    if (tokenizer_peek(t) == '+') {
        tokenizer_advance(t);
        tok.data.bounds.possessive = true;
    }

    return tok;
}

static Token next_token(Tokenizer* t) {
    Token tok;

    if (t->pos >= t->len) {
        tok.type = TOK_END;
        return tok;
    }

    char c = tokenizer_peek(t);

    switch (c) {
        case '[':
            tok.type = TOK_CHAR_CLASS;
            tok.data.cclass = parse_char_class(t);
            return tok;

        case '.':
            tokenizer_advance(t);
            tok.type = TOK_DOT;
            return tok;

        case '*':
            tokenizer_advance(t);
            /* Check for possessive *+ */
            if (tokenizer_peek(t) == '+') {
                tokenizer_advance(t);
                tok.type = TOK_STAR_POSS;
            } else {
                tok.type = TOK_STAR;
            }
            return tok;

        case '+':
            tokenizer_advance(t);
            /* Check for possessive ++ */
            if (tokenizer_peek(t) == '+') {
                tokenizer_advance(t);
                tok.type = TOK_PLUS_POSS;
            } else {
                tok.type = TOK_PLUS;
            }
            return tok;

        case '?':
            tokenizer_advance(t);
            /* Check for possessive ?+ */
            if (tokenizer_peek(t) == '+') {
                tokenizer_advance(t);
                tok.type = TOK_QUEST_POSS;
            } else {
                tok.type = TOK_QUESTION;
            }
            return tok;

        case '|':
            tokenizer_advance(t);
            tok.type = TOK_PIPE;
            return tok;

        case '(':
            tokenizer_advance(t);
            /* Check for special groups (?:, (?=, (?! */
            if (tokenizer_peek(t) == '?') {
                tokenizer_advance(t);
                char next = tokenizer_peek(t);
                if (next == ':') {
                    tokenizer_advance(t);
                    tok.type = TOK_NONCAP;
                } else if (next == '=') {
                    tokenizer_advance(t);
                    tok.type = TOK_LOOKAHEAD_POS;
                } else if (next == '!') {
                    tokenizer_advance(t);
                    tok.type = TOK_LOOKAHEAD_NEG;
                } else {
                    /* Just (? - treat as error or literal */
                    tokenizer_error(t, "Unknown group type (?");
                    tok.type = TOK_LPAREN;
                }
            } else {
                tok.type = TOK_LPAREN;
            }
            return tok;

        case ')':
            tokenizer_advance(t);
            tok.type = TOK_RPAREN;
            return tok;

        case '^':
            tokenizer_advance(t);
            tok.type = TOK_ANCHOR_START;
            return tok;

        case '$':
            tokenizer_advance(t);
            tok.type = TOK_ANCHOR_END;
            return tok;

        case '{':
            return parse_bounded_quantifier(t);

        case '\\':
            tokenizer_advance(t);
            c = tokenizer_advance(t);
            switch (c) {
                case 'n': tok.type = TOK_LITERAL; tok.data.ch = '\n'; break;
                case 't': tok.type = TOK_LITERAL; tok.data.ch = '\t'; break;
                case 'r': tok.type = TOK_LITERAL; tok.data.ch = '\r'; break;
                case 'b': tok.type = TOK_WORD_BOUND; break;
                case 'd': {
                    /* \d = [0-9] */
                    tok.type = TOK_CHAR_CLASS;
                    tok.data.cclass = malloc(sizeof(CharClass));
                    tok.data.cclass->num_ranges = 1;
                    tok.data.cclass->ranges[0][0] = '0';
                    tok.data.cclass->ranges[0][1] = '9';
                    tok.data.cclass->num_chars = 0;
                    tok.data.cclass->negated = false;
                    break;
                }
                case 'D': {
                    /* \D = [^0-9] (negated digits) */
                    tok.type = TOK_CHAR_CLASS;
                    tok.data.cclass = malloc(sizeof(CharClass));
                    tok.data.cclass->num_ranges = 1;
                    tok.data.cclass->ranges[0][0] = '0';
                    tok.data.cclass->ranges[0][1] = '9';
                    tok.data.cclass->num_chars = 0;
                    tok.data.cclass->negated = true;
                    break;
                }
                case 'w': {
                    /* \w = [a-zA-Z0-9_] */
                    tok.type = TOK_CHAR_CLASS;
                    tok.data.cclass = malloc(sizeof(CharClass));
                    tok.data.cclass->num_ranges = 3;
                    tok.data.cclass->ranges[0][0] = 'a';
                    tok.data.cclass->ranges[0][1] = 'z';
                    tok.data.cclass->ranges[1][0] = 'A';
                    tok.data.cclass->ranges[1][1] = 'Z';
                    tok.data.cclass->ranges[2][0] = '0';
                    tok.data.cclass->ranges[2][1] = '9';
                    tok.data.cclass->num_chars = 1;
                    tok.data.cclass->chars[0] = '_';
                    tok.data.cclass->negated = false;
                    break;
                }
                case 'W': {
                    /* \W = [^a-zA-Z0-9_] (negated word) */
                    tok.type = TOK_CHAR_CLASS;
                    tok.data.cclass = malloc(sizeof(CharClass));
                    tok.data.cclass->num_ranges = 3;
                    tok.data.cclass->ranges[0][0] = 'a';
                    tok.data.cclass->ranges[0][1] = 'z';
                    tok.data.cclass->ranges[1][0] = 'A';
                    tok.data.cclass->ranges[1][1] = 'Z';
                    tok.data.cclass->ranges[2][0] = '0';
                    tok.data.cclass->ranges[2][1] = '9';
                    tok.data.cclass->num_chars = 1;
                    tok.data.cclass->chars[0] = '_';
                    tok.data.cclass->negated = true;
                    break;
                }
                case 's': {
                    /* \s = [ \t\n\r] */
                    tok.type = TOK_CHAR_CLASS;
                    tok.data.cclass = malloc(sizeof(CharClass));
                    tok.data.cclass->num_ranges = 0;
                    tok.data.cclass->num_chars = 4;
                    tok.data.cclass->chars[0] = ' ';
                    tok.data.cclass->chars[1] = '\t';
                    tok.data.cclass->chars[2] = '\n';
                    tok.data.cclass->chars[3] = '\r';
                    tok.data.cclass->negated = false;
                    break;
                }
                case 'S': {
                    /* \S = [^ \t\n\r] (negated space) */
                    tok.type = TOK_CHAR_CLASS;
                    tok.data.cclass = malloc(sizeof(CharClass));
                    tok.data.cclass->num_ranges = 0;
                    tok.data.cclass->num_chars = 4;
                    tok.data.cclass->chars[0] = ' ';
                    tok.data.cclass->chars[1] = '\t';
                    tok.data.cclass->chars[2] = '\n';
                    tok.data.cclass->chars[3] = '\r';
                    tok.data.cclass->negated = true;
                    break;
                }
                default:
                    /* Literal escaped character */
                    tok.type = TOK_LITERAL;
                    tok.data.ch = c;
                    break;
            }
            return tok;

        default:
            tokenizer_advance(t);
            tok.type = TOK_LITERAL;
            tok.data.ch = c;
            return tok;
    }
}

/* ============== Parser ============== */

typedef struct {
    Tokenizer* tokenizer;
    Token current;
    char* error;
} Parser;

static Parser* parser_new(const char* pattern) {
    Parser* p = malloc(sizeof(Parser));
    p->tokenizer = tokenizer_new(pattern);
    p->current = next_token(p->tokenizer);
    p->error = NULL;
    return p;
}

static void parser_free(Parser* p) {
    tokenizer_free(p->tokenizer);
    if (p->error) free(p->error);
    free(p);
}

static void parser_advance(Parser* p) {
    p->current = next_token(p->tokenizer);
}

static ASTNode* ast_new(ASTNodeType type) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = type;
    return node;
}

static void ast_free(ASTNode* node) {
    if (!node) return;
    switch (node->type) {
        case AST_CHAR_CLASS:
            if (node->data.cclass) free(node->data.cclass);
            break;
        case AST_SEQ:
        case AST_ALT:
            for (int i = 0; i < node->data.list.num_children; i++) {
                ast_free(node->data.list.children[i]);
            }
            free(node->data.list.children);
            break;
        case AST_REP:
            ast_free(node->data.rep.child);
            break;
        case AST_GROUP:
            ast_free(node->data.group);
            break;
        case AST_LOOKAHEAD:
            ast_free(node->data.lookahead.child);
            break;
        case AST_ANCHOR:
        case AST_LITERAL:
        case AST_DOT:
            break;
    }
    free(node);
}

/* Forward declarations for recursive descent parser */
static ASTNode* parse_alternation(Parser* p);
static ASTNode* parse_sequence(Parser* p);
static ASTNode* parse_quantified(Parser* p);
static ASTNode* parse_atom(Parser* p);

static ASTNode* parse_atom(Parser* p) {
    Token tok = p->current;

    switch (tok.type) {
        case TOK_LITERAL: {
            ASTNode* node = ast_new(AST_LITERAL);
            node->data.ch = tok.data.ch;
            parser_advance(p);
            return node;
        }

        case TOK_CHAR_CLASS: {
            ASTNode* node = ast_new(AST_CHAR_CLASS);
            node->data.cclass = tok.data.cclass;
            parser_advance(p);
            return node;
        }

        case TOK_DOT: {
            ASTNode* node = ast_new(AST_DOT);
            parser_advance(p);
            return node;
        }

        case TOK_ANCHOR_START: {
            ASTNode* node = ast_new(AST_ANCHOR);
            node->data.anchor = ANCHOR_START;
            parser_advance(p);
            return node;
        }

        case TOK_ANCHOR_END: {
            ASTNode* node = ast_new(AST_ANCHOR);
            node->data.anchor = ANCHOR_END;
            parser_advance(p);
            return node;
        }

        case TOK_WORD_BOUND: {
            ASTNode* node = ast_new(AST_ANCHOR);
            node->data.anchor = ANCHOR_WORD_BOUNDARY;
            parser_advance(p);
            return node;
        }

        case TOK_LPAREN: {
            parser_advance(p); /* Skip ( */
            ASTNode* inner = parse_alternation(p);
            if (p->current.type != TOK_RPAREN) {
                p->error = strdup("Expected closing parenthesis");
                ast_free(inner);
                return NULL;
            }
            parser_advance(p); /* Skip ) */
            ASTNode* node = ast_new(AST_GROUP);
            node->data.group = inner;
            return node;
        }

        case TOK_NONCAP: {
            /* Non-capturing group (?:...) - same as regular group in our engine */
            parser_advance(p); /* Skip (?: */
            ASTNode* inner = parse_alternation(p);
            if (p->current.type != TOK_RPAREN) {
                p->error = strdup("Expected closing parenthesis for non-capturing group");
                ast_free(inner);
                return NULL;
            }
            parser_advance(p); /* Skip ) */
            ASTNode* node = ast_new(AST_GROUP);
            node->data.group = inner;
            return node;
        }

        case TOK_LOOKAHEAD_POS: {
            parser_advance(p); /* Skip (?= */
            ASTNode* inner = parse_alternation(p);
            if (p->current.type != TOK_RPAREN) {
                p->error = strdup("Expected closing parenthesis for lookahead");
                ast_free(inner);
                return NULL;
            }
            parser_advance(p); /* Skip ) */
            ASTNode* node = ast_new(AST_LOOKAHEAD);
            node->data.lookahead.child = inner;
            node->data.lookahead.positive = true;
            return node;
        }

        case TOK_LOOKAHEAD_NEG: {
            parser_advance(p); /* Skip (?! */
            ASTNode* inner = parse_alternation(p);
            if (p->current.type != TOK_RPAREN) {
                p->error = strdup("Expected closing parenthesis for negative lookahead");
                ast_free(inner);
                return NULL;
            }
            parser_advance(p); /* Skip ) */
            ASTNode* node = ast_new(AST_LOOKAHEAD);
            node->data.lookahead.child = inner;
            node->data.lookahead.positive = false;
            return node;
        }

        default:
            return NULL;
    }
}

static ASTNode* parse_quantified(Parser* p) {
    ASTNode* atom = parse_atom(p);
    if (!atom) return NULL;

    /* Check for quantifier */
    Token tok = p->current;

    /* Standard quantifiers */
    if (tok.type == TOK_STAR || tok.type == TOK_PLUS || tok.type == TOK_QUESTION) {
        ASTNode* rep = ast_new(AST_REP);
        rep->data.rep.child = atom;
        rep->data.rep.possessive = false;
        switch (tok.type) {
            case TOK_STAR:
                rep->data.rep.min = 0;
                rep->data.rep.max = -1;
                break;
            case TOK_PLUS:
                rep->data.rep.min = 1;
                rep->data.rep.max = -1;
                break;
            case TOK_QUESTION:
                rep->data.rep.min = 0;
                rep->data.rep.max = 1;
                break;
            default:
                break;
        }
        parser_advance(p);
        return rep;
    }

    /* Possessive quantifiers */
    if (tok.type == TOK_STAR_POSS || tok.type == TOK_PLUS_POSS || tok.type == TOK_QUEST_POSS) {
        ASTNode* rep = ast_new(AST_REP);
        rep->data.rep.child = atom;
        rep->data.rep.possessive = true;
        switch (tok.type) {
            case TOK_STAR_POSS:
                rep->data.rep.min = 0;
                rep->data.rep.max = -1;
                break;
            case TOK_PLUS_POSS:
                rep->data.rep.min = 1;
                rep->data.rep.max = -1;
                break;
            case TOK_QUEST_POSS:
                rep->data.rep.min = 0;
                rep->data.rep.max = 1;
                break;
            default:
                break;
        }
        parser_advance(p);
        return rep;
    }

    /* Bounded quantifier {n}, {n,}, {n,m} */
    if (tok.type == TOK_LBRACE) {
        ASTNode* rep = ast_new(AST_REP);
        rep->data.rep.child = atom;
        rep->data.rep.min = tok.data.bounds.min;
        rep->data.rep.max = tok.data.bounds.max;
        rep->data.rep.possessive = tok.data.bounds.possessive;
        parser_advance(p);
        return rep;
    }

    return atom;
}

static ASTNode* parse_sequence(Parser* p) {
    ASTNode* children[128];
    int num_children = 0;

    while (p->current.type != TOK_END &&
           p->current.type != TOK_PIPE &&
           p->current.type != TOK_RPAREN) {
        ASTNode* child = parse_quantified(p);
        if (!child) break;
        if (num_children < 128) {
            children[num_children++] = child;
        }
    }

    if (num_children == 0) {
        return NULL;
    }
    if (num_children == 1) {
        return children[0];
    }

    ASTNode* seq = ast_new(AST_SEQ);
    seq->data.list.children = malloc(num_children * sizeof(ASTNode*));
    seq->data.list.num_children = num_children;
    memcpy(seq->data.list.children, children, num_children * sizeof(ASTNode*));
    return seq;
}

static ASTNode* parse_alternation(Parser* p) {
    ASTNode* left = parse_sequence(p);

    if (p->current.type != TOK_PIPE) {
        return left;
    }

    /* Collect all alternatives */
    ASTNode* alternatives[64];
    int num_alts = 0;
    alternatives[num_alts++] = left;

    while (p->current.type == TOK_PIPE) {
        parser_advance(p); /* Skip | */
        ASTNode* alt = parse_sequence(p);
        if (alt && num_alts < 64) {
            alternatives[num_alts++] = alt;
        }
    }

    if (num_alts == 1) {
        return alternatives[0];
    }

    ASTNode* node = ast_new(AST_ALT);
    node->data.list.children = malloc(num_alts * sizeof(ASTNode*));
    node->data.list.num_children = num_alts;
    memcpy(node->data.list.children, alternatives, num_alts * sizeof(ASTNode*));
    return node;
}

/* ============== Matcher ============== */

/* Match result: -1 means no match, >= 0 is length of match */
static int match_ast(ASTNode* node, const char* input, int pos, int len);

static bool char_class_contains(CharClass* cc, char c) {
    /* Check ranges */
    for (int i = 0; i < cc->num_ranges; i++) {
        if (c >= cc->ranges[i][0] && c <= cc->ranges[i][1]) {
            return !cc->negated;
        }
    }

    /* Check individual chars */
    for (int i = 0; i < cc->num_chars; i++) {
        if (c == cc->chars[i]) {
            return !cc->negated;
        }
    }

    return cc->negated;
}

/* Check if character is a word character (for \b) */
static bool is_word_char(char c) {
    return (c >= 'a' && c <= 'z') ||
           (c >= 'A' && c <= 'Z') ||
           (c >= '0' && c <= '9') ||
           c == '_';
}

static int match_ast(ASTNode* node, const char* input, int pos, int len) {
    if (!node) return -1;

    switch (node->type) {
        case AST_LITERAL: {
            if (pos >= len) return -1;
            if (input[pos] == node->data.ch) return 1;
            return -1;
        }

        case AST_CHAR_CLASS: {
            if (pos >= len) return -1;
            if (char_class_contains(node->data.cclass, input[pos])) return 1;
            return -1;
        }

        case AST_DOT: {
            if (pos >= len) return -1;
            return 1;  /* Match any character */
        }

        case AST_ANCHOR: {
            switch (node->data.anchor) {
                case ANCHOR_START:
                    /* ^ matches only at position 0 */
                    return (pos == 0) ? 0 : -1;

                case ANCHOR_END:
                    /* $ matches only at end of string */
                    return (pos == len) ? 0 : -1;

                case ANCHOR_WORD_BOUNDARY: {
                    /* \b matches at word boundary */
                    bool prev_is_word = (pos > 0) && is_word_char(input[pos - 1]);
                    bool curr_is_word = (pos < len) && is_word_char(input[pos]);
                    /* Word boundary: transition between word and non-word */
                    return (prev_is_word != curr_is_word) ? 0 : -1;
                }
            }
            return -1;
        }

        case AST_LOOKAHEAD: {
            /* Lookahead: check if pattern matches but don't consume */
            int child_len = match_ast(node->data.lookahead.child, input, pos, len);
            if (node->data.lookahead.positive) {
                /* Positive lookahead: succeed if child matches */
                return (child_len >= 0) ? 0 : -1;
            } else {
                /* Negative lookahead: succeed if child doesn't match */
                return (child_len < 0) ? 0 : -1;
            }
        }

        case AST_SEQ: {
            int total_len = 0;
            int current_pos = pos;
            for (int i = 0; i < node->data.list.num_children; i++) {
                int child_len = match_ast(node->data.list.children[i], input, current_pos, len);
                if (child_len < 0) return -1;
                total_len += child_len;
                current_pos += child_len;
            }
            return total_len;
        }

        case AST_ALT: {
            /* Try each alternative in order (PEG ordered choice) */
            for (int i = 0; i < node->data.list.num_children; i++) {
                int child_len = match_ast(node->data.list.children[i], input, pos, len);
                if (child_len >= 0) return child_len;
            }
            return -1;
        }

        case AST_REP: {
            int total_len = 0;
            int matches = 0;
            int current_pos = pos;

            /* Match as many as possible (greedy/possessive - PEG doesn't backtrack) */
            while (node->data.rep.max < 0 || matches < node->data.rep.max) {
                int child_len = match_ast(node->data.rep.child, input, current_pos, len);
                if (child_len <= 0) break;  /* No more matches */
                total_len += child_len;
                current_pos += child_len;
                matches++;
            }

            /* Check minimum requirement */
            if (matches < node->data.rep.min) return -1;
            return total_len;
        }

        case AST_GROUP: {
            return match_ast(node->data.group, input, pos, len);
        }
    }

    return -1;
}

/* ============== Public API ============== */

PikaRegexCompiled* pika_regex_compile(const char* pattern) {
    PikaRegexCompiled* compiled = malloc(sizeof(PikaRegexCompiled));
    compiled->ast = NULL;
    compiled->error = NULL;

    if (!pattern) {
        compiled->error = strdup("Pattern is NULL");
        return compiled;
    }

    /* Parse pattern to AST */
    Parser* p = parser_new(pattern);
    ASTNode* ast = parse_alternation(p);

    if (p->error) {
        compiled->error = strdup(p->error);
        parser_free(p);
        return compiled;
    }

    if (p->tokenizer->error) {
        compiled->error = strdup(p->tokenizer->error);
        parser_free(p);
        ast_free(ast);
        return compiled;
    }

    if (!ast) {
        compiled->error = strdup("Failed to parse pattern");
        parser_free(p);
        return compiled;
    }

    compiled->ast = ast;
    parser_free(p);
    return compiled;
}

const char* pika_regex_error(PikaRegexCompiled* compiled) {
    if (!compiled) return "NULL compiled regex";
    return compiled->error;
}

void pika_regex_free(PikaRegexCompiled* compiled) {
    if (!compiled) return;
    ast_free(compiled->ast);
    if (compiled->error) free(compiled->error);
    free(compiled);
}

/* Helper: Create string Obj from C string */
static Obj* cstr_to_obj(const char* s) {
    if (!s) return NULL;
    omni_ensure_global_region();
    return mk_sym_region(omni_get_global_region(), s);
}

Obj* pika_regex_search(PikaRegexCompiled* compiled, const char* input) {
    if (!compiled || compiled->error || !compiled->ast || !input) {
        return NULL;
    }

    int len = strlen(input);

    /* Try matching at each position */
    for (int pos = 0; pos < len; pos++) {
        int match_len = match_ast(compiled->ast, input, pos, len);
        if (match_len > 0) {
            char* matched_str = malloc(match_len + 1);
            strncpy(matched_str, input + pos, match_len);
            matched_str[match_len] = '\0';
            Obj* ret = cstr_to_obj(matched_str);
            free(matched_str);
            return ret;
        }
    }

    return NULL;
}

Obj* pika_regex_find_all(PikaRegexCompiled* compiled, const char* input) {
    if (!compiled || compiled->error || !compiled->ast || !input) {
        return NULL;
    }

    Obj* result_list = NULL;
    Obj* list_tail = NULL;
    int len = strlen(input);
    int pos = 0;

    while (pos < len) {
        int match_len = match_ast(compiled->ast, input, pos, len);
        if (match_len > 0) {
            char* matched_str = malloc(match_len + 1);
            strncpy(matched_str, input + pos, match_len);
            matched_str[match_len] = '\0';

            Obj* matched_obj = cstr_to_obj(matched_str);
            Obj* new_pair = mk_pair(matched_obj, NULL);

            if (!result_list) {
                result_list = new_pair;
            } else {
                list_tail->b = new_pair;
            }
            list_tail = new_pair;

            free(matched_str);
            pos += match_len;
        } else {
            pos++;
        }
    }

    return result_list;
}

Obj* pika_regex_split(PikaRegexCompiled* compiled, const char* input) {
    if (!compiled || compiled->error || !compiled->ast || !input) {
        return NULL;
    }

    Obj* result_list = NULL;
    Obj* list_tail = NULL;
    int len = strlen(input);
    int pos = 0;
    int last_end = 0;

    while (pos < len) {
        int match_len = match_ast(compiled->ast, input, pos, len);
        if (match_len > 0) {
            /* Add segment before match */
            if (pos > last_end) {
                int seg_len = pos - last_end;
                char* segment = malloc(seg_len + 1);
                strncpy(segment, input + last_end, seg_len);
                segment[seg_len] = '\0';

                Obj* segment_obj = cstr_to_obj(segment);
                Obj* new_pair = mk_pair(segment_obj, NULL);

                if (!result_list) {
                    result_list = new_pair;
                } else {
                    list_tail->b = new_pair;
                }
                list_tail = new_pair;
                free(segment);
            }

            last_end = pos + match_len;
            pos = last_end;
        } else {
            pos++;
        }
    }

    /* Add remaining segment */
    if (last_end < len) {
        char* segment = strdup(input + last_end);
        Obj* segment_obj = cstr_to_obj(segment);
        Obj* new_pair = mk_pair(segment_obj, NULL);

        if (!result_list) {
            result_list = new_pair;
        } else {
            list_tail->b = new_pair;
        }
        free(segment);
    }

    return result_list;
}

Obj* pika_regex_replace(PikaRegexCompiled* compiled, const char* replacement,
                        const char* input, bool global) {
    if (!compiled || compiled->error || !compiled->ast || !input) {
        return cstr_to_obj(input);
    }
    if (!replacement) replacement = "";

    int len = strlen(input);
    size_t repl_len = strlen(replacement);
    size_t result_size = len * 2 + 1;
    char* result = malloc(result_size);
    result[0] = '\0';
    size_t result_len = 0;

    int pos = 0;
    bool first_match_done = false;

    while (pos < len) {
        if (!global && first_match_done) {
            /* Append rest of string */
            strcat(result, input + pos);
            break;
        }

        int match_len = match_ast(compiled->ast, input, pos, len);
        if (match_len > 0) {
            /* Append replacement */
            if (result_len + repl_len >= result_size) {
                result_size = result_size * 2 + repl_len;
                result = realloc(result, result_size);
            }
            strcat(result, replacement);
            result_len += repl_len;

            pos += match_len;
            first_match_done = true;
        } else {
            /* No match at this position - append char */
            if (result_len + 1 >= result_size) {
                result_size *= 2;
                result = realloc(result, result_size);
            }
            result[result_len++] = input[pos++];
            result[result_len] = '\0';
        }
    }

    Obj* ret = cstr_to_obj(result);
    free(result);
    return ret;
}

Obj* pika_regex_fullmatch(PikaRegexCompiled* compiled, const char* input) {
    if (!compiled || compiled->error || !compiled->ast || !input) {
        return mk_bool(0);
    }

    int len = strlen(input);
    int match_len = match_ast(compiled->ast, input, 0, len);

    /* Full match requires matching entire string from start */
    return mk_bool(match_len == len);
}
