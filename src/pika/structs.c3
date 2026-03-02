module pika;

// ============================================================
// Clause Tag - identifies which type of grammar clause
// ============================================================
enum ClauseTag : char {
    SATISFY,
    SCAN,
    TOKEN,
    TOKENS,
    EPSILON,
    FAIL,
    END_OF_INPUT,
    SEQ,
    FIRST,
    NOT_FOLLOWED_BY,
    FOLLOWED_BY,
    SOME_CLAUSE,
    MANY_CLAUSE,
    TIE,
}

// Function pointer types for Satisfy and Scan clauses
alias SatisfyFn = fn bool(char token);
alias ScanFn = fn int(char[] view); // returns 0 if no match, else last index+1 (length matched)

// ============================================================
// Clause - tagged union for grammar rules
// Uses integer (int) children references after grammar normalization.
// ============================================================
struct Clause {
    ClauseTag tag;
    union {
        // SATISFY
        SatisfyFn satisfy_fn;
        // SCAN
        ScanFn scan_fn;
        // TOKEN
        char token_val;
        // TOKENS
        struct {
            char[] tokens_val;
        }
        // SEQ, FIRST
        struct {
            int[] children;
        }
        // NOT_FOLLOWED_BY, FOLLOWED_BY, SOME_CLAUSE, MANY_CLAUSE, TIE
        struct {
            int child;
        }
        // EPSILON, FAIL, END_OF_INPUT have no data
    }
}

// ============================================================
// Match - internal match representation stored in splay tree
// ============================================================
struct Match {
    int clause;      // which clause matched
    int first;       // starting position in input
    int last;        // last position (empty match: first - 1)
    int option_idx;  // for First: which alternative (1-based), else 0
    int submatches;  // index into ParserState.submatches
    int left;        // left child in splay tree
    int right;       // right child in splay tree
    int parent;      // parent in splay tree
}

fn Match make_match(int clause, int first, int last, int option_idx, int submatches) {
    Match m;
    m.clause = clause;
    m.first = first;
    m.last = last;
    m.option_idx = option_idx;
    m.submatches = submatches;
    m.left = 0;
    m.right = 0;
    m.parent = 0;
    return m;
}

// ============================================================
// UserMatch - user-facing match representation
// ============================================================
struct UserMatch {
    int rule;           // rule index in grammar.names
    int first;          // start position
    int last;           // end position
    char[] view;        // slice of matched input
    int[] submatches;   // match indices of direct children
}

// ============================================================
// PikaQueue - min-heap priority queue with presence tracking
// ============================================================
struct PikaQueue {
    int n;              // current queue size
    int[] q;            // heap storage (1-indexed logically, 0-indexed in array)
    bool[] p;           // presence flags (indexed by clause id, 0-based)
}

// ============================================================
// Grammar - prepared grammar for parsing
// ============================================================
struct Grammar {
    String[] names;             // topologically sorted rule labels
    int[] name_to_idx;          // rule name hash -> index (we use a separate lookup)
    Clause[] clauses;           // clauses indexed by integer
    bool[] can_match_epsilon;   // flags per clause
    int[][] seed_clauses;       // which clauses get seeded on match
    int[] terminals;            // indices of terminal clauses

    // For name lookup: we store a simple linear mapping
    // names[i] corresponds to clause index i (1-based conceptually, 0-based in C3)
    int num_clauses;
}

// ============================================================
// ParserState - mutable parsing state and result
// ============================================================
struct ParserState {
    Grammar* grammar;
    PikaQueue q;
    Match[] matches;          // all recorded matches (dynamic)
    int matches_len;          // actual number of matches
    int matches_cap;          // capacity
    int memo_root;            // root of splay tree (0 = empty, 1-based indexing)
    int[] submatches;         // children pointers
    int submatches_len;
    int submatches_cap;
    char[] input;             // parser input
}

// ============================================================
// TraverseNode - for stack-based tree traversal
// ============================================================
struct TraverseNode {
    int parent_idx;
    int parent_sub_idx;
    UserMatch match;
    bool open;
}

// ============================================================
// Rule - frontend rule before grammar normalization
// A rule has a name and a FrontendClause
// ============================================================
enum FClauseTag : char {
    F_SATISFY,
    F_SCAN,
    F_TOKEN,
    F_TOKENS,
    F_EPSILON,
    F_FAIL,
    F_END_OF_INPUT,
    F_SEQ,
    F_FIRST,
    F_NOT_FOLLOWED_BY,
    F_FOLLOWED_BY,
    F_SOME,
    F_MANY,
    F_TIE,
}

// Frontend clause child can be either a name reference or an inline clause
enum FChildTag : char {
    FC_NAME,    // reference by name string
    FC_CLAUSE,  // inline anonymous clause
    FC_NAMED,   // named inline clause (pair of name + clause)
}

struct FChild {
    FChildTag tag;
    String name;            // for FC_NAME and FC_NAMED
    FrontendClause* clause; // for FC_CLAUSE and FC_NAMED
}

struct FrontendClause {
    FClauseTag tag;
    union {
        SatisfyFn satisfy_fn;
        ScanFn scan_fn;
        char token_val;
        struct { char[] tokens_val; }
        struct { FChild[] fchildren; }   // SEQ, FIRST
        struct { FChild fchild; }        // NOT_FOLLOWED_BY, FOLLOWED_BY, SOME, MANY, TIE
    }
}

// A named rule: name => clause
struct NamedRule {
    String name;
    FrontendClause* clause;
}

// ============================================================
// Allocator helpers - we use the standard heap allocator
// ============================================================
