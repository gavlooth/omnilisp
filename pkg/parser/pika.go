package parser

import (
	"fmt"
	"purple_go/pkg/ast"
	"unicode"
)

// PikaResult represents a parse result
type PikaResult struct {
	Success bool       // Whether parsing succeeded
	Value   *ast.Value // The parsed AST (as first-class data)
	Pos     int        // Position after match
	Err     string     // Error message if failed
}

// Failed creates a failed result
func Failed(pos int, msg string) PikaResult {
	return PikaResult{Success: false, Pos: pos, Err: msg}
}

// Succeeded creates a successful result
func Succeeded(value *ast.Value, pos int) PikaResult {
	return PikaResult{Success: true, Value: value, Pos: pos}
}

// MemoKey for memoization
type MemoKey struct {
	Rule string
	Pos  int
}

// PikaParser implements a Pika-style parser with homoiconic AST
// Key properties:
// - Right-to-left parsing (handles left recursion)
// - O(n) with memoization
// - AST nodes are first-class Lisp data
type PikaParser struct {
	Input    []rune
	Memo     map[MemoKey]PikaResult
	RuleMap  map[string]func(int) PikaResult
}

// NewPikaParser creates a new Pika parser
func NewPikaParser(input string) *PikaParser {
	p := &PikaParser{
		Input: []rune(input),
		Memo:  make(map[MemoKey]PikaResult),
	}
	p.initRules()
	return p
}

// initRules initializes the grammar rules
func (p *PikaParser) initRules() {
	p.RuleMap = map[string]func(int) PikaResult{
		"expr":       p.parseExpr,
		"atom":       p.parseAtom,
		"list":       p.parseList,
		"quote":      p.parseQuote,
		"quasiquote": p.parseQuasiquote,
		"unquote":    p.parseUnquote,
		"number":     p.parseNumber,
		"symbol":     p.parseSymbol,
		"string":     p.parseString,
	}
}

// Parse parses the input from position 0
func (p *PikaParser) Parse() (*ast.Value, error) {
	p.skipWhitespace(0)
	result := p.parseProgram(0)
	if !result.Success {
		return nil, fmt.Errorf("parse error at position %d: %s", result.Pos, result.Err)
	}
	return result.Value, nil
}

// parseProgram parses multiple expressions
func (p *PikaParser) parseProgram(pos int) PikaResult {
	var exprs []*ast.Value
	pos = p.skipWhitespace(pos)

	for pos < len(p.Input) {
		result := p.memoized("expr", pos)
		if !result.Success {
			if len(exprs) == 0 {
				return result
			}
			break
		}
		exprs = append(exprs, result.Value)
		pos = p.skipWhitespace(result.Pos)
	}

	if len(exprs) == 0 {
		return Failed(pos, "expected expression")
	}
	if len(exprs) == 1 {
		return Succeeded(exprs[0], pos)
	}

	// Multiple expressions become (begin ...)
	result := ast.NewSym("begin")
	for _, e := range exprs {
		result = ast.NewCell(result, ast.NewCell(e, ast.Nil))
	}
	return Succeeded(result, pos)
}

// memoized applies memoization to a rule
func (p *PikaParser) memoized(rule string, pos int) PikaResult {
	key := MemoKey{Rule: rule, Pos: pos}
	if result, ok := p.Memo[key]; ok {
		return result
	}

	fn, ok := p.RuleMap[rule]
	if !ok {
		return Failed(pos, fmt.Sprintf("unknown rule: %s", rule))
	}

	result := fn(pos)
	p.Memo[key] = result
	return result
}

// parseExpr parses an expression (main entry point per expression)
func (p *PikaParser) parseExpr(pos int) PikaResult {
	pos = p.skipWhitespace(pos)

	if pos >= len(p.Input) {
		return Failed(pos, "unexpected end of input")
	}

	ch := p.Input[pos]

	switch ch {
	case '(':
		return p.memoized("list", pos)
	case '\'':
		return p.memoized("quote", pos)
	case '`':
		return p.memoized("quasiquote", pos)
	case ',':
		return p.memoized("unquote", pos)
	case '"':
		return p.memoized("string", pos)
	case '#':
		return p.parseSpecial(pos)
	default:
		return p.memoized("atom", pos)
	}
}

// parseAtom parses an atom (number or symbol)
func (p *PikaParser) parseAtom(pos int) PikaResult {
	pos = p.skipWhitespace(pos)

	if pos >= len(p.Input) {
		return Failed(pos, "unexpected end of input")
	}

	// Try number first
	if p.isDigitStart(pos) {
		return p.memoized("number", pos)
	}

	// Otherwise symbol
	return p.memoized("symbol", pos)
}

// parseNumber parses an integer or float
func (p *PikaParser) parseNumber(pos int) PikaResult {
	pos = p.skipWhitespace(pos)
	start := pos

	// Optional sign
	if pos < len(p.Input) && (p.Input[pos] == '-' || p.Input[pos] == '+') {
		pos++
	}

	if pos >= len(p.Input) || !unicode.IsDigit(p.Input[pos]) {
		return Failed(start, "expected number")
	}

	// Integer part
	for pos < len(p.Input) && unicode.IsDigit(p.Input[pos]) {
		pos++
	}

	// Check for float
	isFloat := false
	if pos < len(p.Input) && p.Input[pos] == '.' {
		isFloat = true
		pos++
		for pos < len(p.Input) && unicode.IsDigit(p.Input[pos]) {
			pos++
		}
	}

	// Exponent
	if pos < len(p.Input) && (p.Input[pos] == 'e' || p.Input[pos] == 'E') {
		isFloat = true
		pos++
		if pos < len(p.Input) && (p.Input[pos] == '+' || p.Input[pos] == '-') {
			pos++
		}
		for pos < len(p.Input) && unicode.IsDigit(p.Input[pos]) {
			pos++
		}
	}

	numStr := string(p.Input[start:pos])

	if isFloat {
		var val float64
		fmt.Sscanf(numStr, "%f", &val)
		return Succeeded(ast.NewFloat(val), pos)
	}

	var val int64
	fmt.Sscanf(numStr, "%d", &val)
	return Succeeded(ast.NewInt(val), pos)
}

// parseSymbol parses a symbol
func (p *PikaParser) parseSymbol(pos int) PikaResult {
	pos = p.skipWhitespace(pos)
	start := pos

	if pos >= len(p.Input) {
		return Failed(pos, "expected symbol")
	}

	ch := p.Input[pos]
	if !p.isSymbolStart(ch) {
		return Failed(pos, "expected symbol")
	}

	pos++
	for pos < len(p.Input) && p.isSymbolChar(p.Input[pos]) {
		pos++
	}

	name := string(p.Input[start:pos])

	// Special symbols
	switch name {
	case "nil":
		return Succeeded(ast.Nil, pos)
	case "#t", "true":
		return Succeeded(ast.NewSym("#t"), pos)
	case "#f", "false":
		return Succeeded(ast.NewSym("#f"), pos)
	}

	return Succeeded(ast.NewSym(name), pos)
}

// parseString parses a string literal
func (p *PikaParser) parseString(pos int) PikaResult {
	if pos >= len(p.Input) || p.Input[pos] != '"' {
		return Failed(pos, "expected string")
	}
	pos++

	var chars []rune
	for pos < len(p.Input) && p.Input[pos] != '"' {
		if p.Input[pos] == '\\' && pos+1 < len(p.Input) {
			pos++
			switch p.Input[pos] {
			case 'n':
				chars = append(chars, '\n')
			case 't':
				chars = append(chars, '\t')
			case 'r':
				chars = append(chars, '\r')
			case '\\':
				chars = append(chars, '\\')
			case '"':
				chars = append(chars, '"')
			default:
				chars = append(chars, p.Input[pos])
			}
		} else {
			chars = append(chars, p.Input[pos])
		}
		pos++
	}

	if pos >= len(p.Input) {
		return Failed(pos, "unterminated string")
	}
	pos++ // Skip closing quote

	// String as a list of characters (homoiconic)
	// (string 'h' 'e' 'l' 'l' 'o')
	// Build the list from the end
	result := ast.Nil
	for i := len(chars) - 1; i >= 0; i-- {
		result = ast.NewCell(ast.NewChar(chars[i]), result)
	}
	result = ast.NewCell(ast.NewSym("string"), result)

	return Succeeded(result, pos)
}

// parseList parses a list (s-expression)
func (p *PikaParser) parseList(pos int) PikaResult {
	if pos >= len(p.Input) || p.Input[pos] != '(' {
		return Failed(pos, "expected '('")
	}
	pos++
	pos = p.skipWhitespace(pos)

	var elements []*ast.Value

	for pos < len(p.Input) && p.Input[pos] != ')' {
		result := p.memoized("expr", pos)
		if !result.Success {
			return result
		}
		elements = append(elements, result.Value)
		pos = p.skipWhitespace(result.Pos)

		// Check for dotted pair
		if pos+1 < len(p.Input) && p.Input[pos] == '.' &&
			(p.Input[pos+1] == ' ' || p.Input[pos+1] == '\t' || p.Input[pos+1] == '\n') {
			pos++
			pos = p.skipWhitespace(pos)
			cdrResult := p.memoized("expr", pos)
			if !cdrResult.Success {
				return cdrResult
			}
			pos = p.skipWhitespace(cdrResult.Pos)

			if pos >= len(p.Input) || p.Input[pos] != ')' {
				return Failed(pos, "expected ')' after dotted pair")
			}
			pos++

			// Build improper list
			result := cdrResult.Value
			for i := len(elements) - 1; i >= 0; i-- {
				result = ast.NewCell(elements[i], result)
			}
			return Succeeded(result, pos)
		}
	}

	if pos >= len(p.Input) {
		return Failed(pos, "expected ')'")
	}
	pos++ // Skip ')'

	// Build proper list
	result := ast.Nil
	for i := len(elements) - 1; i >= 0; i-- {
		result = ast.NewCell(elements[i], result)
	}

	return Succeeded(result, pos)
}

// parseQuote parses a quoted expression
func (p *PikaParser) parseQuote(pos int) PikaResult {
	if pos >= len(p.Input) || p.Input[pos] != '\'' {
		return Failed(pos, "expected quote")
	}
	pos++

	result := p.memoized("expr", pos)
	if !result.Success {
		return result
	}

	// (quote <expr>)
	quoted := ast.NewCell(ast.NewSym("quote"), ast.NewCell(result.Value, ast.Nil))
	return Succeeded(quoted, result.Pos)
}

// parseQuasiquote parses a quasiquoted expression
func (p *PikaParser) parseQuasiquote(pos int) PikaResult {
	if pos >= len(p.Input) || p.Input[pos] != '`' {
		return Failed(pos, "expected quasiquote")
	}
	pos++

	result := p.memoized("expr", pos)
	if !result.Success {
		return result
	}

	// (quasiquote <expr>)
	quoted := ast.NewCell(ast.NewSym("quasiquote"), ast.NewCell(result.Value, ast.Nil))
	return Succeeded(quoted, result.Pos)
}

// parseUnquote parses an unquote or unquote-splicing
func (p *PikaParser) parseUnquote(pos int) PikaResult {
	if pos >= len(p.Input) || p.Input[pos] != ',' {
		return Failed(pos, "expected unquote")
	}
	pos++

	splice := false
	if pos < len(p.Input) && p.Input[pos] == '@' {
		splice = true
		pos++
	}

	result := p.memoized("expr", pos)
	if !result.Success {
		return result
	}

	sym := "unquote"
	if splice {
		sym = "unquote-splicing"
	}

	// (unquote <expr>) or (unquote-splicing <expr>)
	quoted := ast.NewCell(ast.NewSym(sym), ast.NewCell(result.Value, ast.Nil))
	return Succeeded(quoted, result.Pos)
}

// parseSpecial parses special syntax like #t, #f, #\char
func (p *PikaParser) parseSpecial(pos int) PikaResult {
	if pos >= len(p.Input) || p.Input[pos] != '#' {
		return Failed(pos, "expected #")
	}
	pos++

	if pos >= len(p.Input) {
		return Failed(pos, "unexpected end after #")
	}

	switch p.Input[pos] {
	case 't':
		pos++
		return Succeeded(ast.NewSym("#t"), pos)
	case 'f':
		pos++
		return Succeeded(ast.NewSym("#f"), pos)
	case '\\':
		// Character literal
		pos++
		if pos >= len(p.Input) {
			return Failed(pos, "expected character after #\\")
		}
		ch := p.Input[pos]
		pos++

		// Check for named characters
		if p.isSymbolChar(ch) {
			start := pos - 1
			for pos < len(p.Input) && p.isSymbolChar(p.Input[pos]) {
				pos++
			}
			name := string(p.Input[start:pos])
			switch name {
			case "space":
				ch = ' '
			case "newline":
				ch = '\n'
			case "tab":
				ch = '\t'
			case "return":
				ch = '\r'
			default:
				if len(name) == 1 {
					ch = rune(name[0])
				}
			}
		}

		return Succeeded(ast.NewChar(ch), pos)

	case '(':
		// Vector literal (as list with vec tag)
		listResult := p.memoized("list", pos)
		if !listResult.Success {
			return listResult
		}
		// (vec ...)
		vec := ast.NewCell(ast.NewSym("vec"), listResult.Value)
		return Succeeded(vec, listResult.Pos)

	default:
		return Failed(pos, fmt.Sprintf("unknown special syntax #%c", p.Input[pos]))
	}
}

// Helper methods

func (p *PikaParser) skipWhitespace(pos int) int {
	for pos < len(p.Input) {
		ch := p.Input[pos]
		if ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' {
			pos++
		} else if ch == ';' {
			// Skip comment
			for pos < len(p.Input) && p.Input[pos] != '\n' {
				pos++
			}
		} else {
			break
		}
	}
	return pos
}

func (p *PikaParser) isDigitStart(pos int) bool {
	if pos >= len(p.Input) {
		return false
	}
	ch := p.Input[pos]
	if unicode.IsDigit(ch) {
		return true
	}
	if (ch == '-' || ch == '+') && pos+1 < len(p.Input) && unicode.IsDigit(p.Input[pos+1]) {
		return true
	}
	return false
}

func (p *PikaParser) isSymbolStart(ch rune) bool {
	if unicode.IsLetter(ch) {
		return true
	}
	// Extended symbol characters
	switch ch {
	case '!', '$', '%', '&', '*', '+', '-', '.', '/', ':', '<', '=', '>', '?', '@', '^', '_', '~':
		return true
	}
	return false
}

func (p *PikaParser) isSymbolChar(ch rune) bool {
	return p.isSymbolStart(ch) || unicode.IsDigit(ch)
}

// Left Recursion Support
// Pika handles left recursion by parsing right-to-left

// PikaLeftRecursive extends PikaParser with left recursion support
type PikaLeftRecursive struct {
	*PikaParser
	LeftRecursive map[string]bool
	Growing       map[MemoKey]bool
}

// NewPikaLeftRecursive creates a parser with left recursion support
func NewPikaLeftRecursive(input string) *PikaLeftRecursive {
	return &PikaLeftRecursive{
		PikaParser:    NewPikaParser(input),
		LeftRecursive: make(map[string]bool),
		Growing:       make(map[MemoKey]bool),
	}
}

// RegisterLeftRecursive marks a rule as left-recursive
func (p *PikaLeftRecursive) RegisterLeftRecursive(rule string) {
	p.LeftRecursive[rule] = true
}

// memoizedLR applies memoization with left-recursion handling
func (p *PikaLeftRecursive) memoizedLR(rule string, pos int) PikaResult {
	key := MemoKey{Rule: rule, Pos: pos}

	// Check memo
	if result, ok := p.Memo[key]; ok {
		return result
	}

	// For left-recursive rules, use growing approach
	if p.LeftRecursive[rule] {
		return p.growLR(rule, pos, key)
	}

	// Non-left-recursive: normal memoization
	fn, ok := p.RuleMap[rule]
	if !ok {
		return Failed(pos, fmt.Sprintf("unknown rule: %s", rule))
	}

	result := fn(pos)
	p.Memo[key] = result
	return result
}

// growLR implements the growing approach for left recursion
func (p *PikaLeftRecursive) growLR(rule string, pos int, key MemoKey) PikaResult {
	if p.Growing[key] {
		// In the middle of growing, return failure to break recursion
		return Failed(pos, "left recursion base case")
	}

	// Seed with failure
	p.Memo[key] = Failed(pos, "left recursion seed")
	p.Growing[key] = true

	fn := p.RuleMap[rule]
	for {
		result := fn(pos)
		prev := p.Memo[key]

		// Stop growing if we didn't make progress
		if !result.Success || result.Pos <= prev.Pos {
			break
		}

		// Update memo with better result
		p.Memo[key] = result
	}

	p.Growing[key] = false
	return p.Memo[key]
}

// AST as First-Class Data
// The AST nodes produced by Pika ARE Lisp values that can be:
// - Quoted and manipulated
// - Pattern matched
// - Transformed by macros
// - Evaluated at multiple meta-levels (tower of interpreters)

// ASTNode represents an AST node as a Lisp value
// This is the homoiconic representation
type ASTNode = *ast.Value

// CreateASTNode creates an AST node from components
// (node-type children...)
func CreateASTNode(nodeType string, children ...*ast.Value) *ast.Value {
	// Build children list from the end
	result := ast.Nil
	for i := len(children) - 1; i >= 0; i-- {
		result = ast.NewCell(children[i], result)
	}
	// Prepend the node type
	return ast.NewCell(ast.NewSym(nodeType), result)
}

// GetNodeType extracts the type from an AST node
func GetNodeType(node *ast.Value) string {
	if ast.IsCell(node) && ast.IsSym(node.Car) {
		return node.Car.Str
	}
	return ""
}

// GetNodeChildren extracts children from an AST node
func GetNodeChildren(node *ast.Value) []*ast.Value {
	if !ast.IsCell(node) {
		return nil
	}

	var children []*ast.Value
	for n := node.Cdr; !ast.IsNil(n) && ast.IsCell(n); n = n.Cdr {
		children = append(children, n.Car)
	}
	return children
}
