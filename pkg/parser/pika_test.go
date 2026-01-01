package parser

import (
	"testing"

	"purple_go/pkg/ast"
)

func TestPikaParserBasics(t *testing.T) {
	t.Run("ParseInteger", func(t *testing.T) {
		p := NewPikaParser("42")
		result, err := p.Parse()
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if !ast.IsInt(result) || result.Int != 42 {
			t.Errorf("expected 42, got %v", result)
		}
	})

	t.Run("ParseNegativeInteger", func(t *testing.T) {
		p := NewPikaParser("-17")
		result, err := p.Parse()
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if !ast.IsInt(result) || result.Int != -17 {
			t.Errorf("expected -17, got %v", result)
		}
	})

	t.Run("ParseFloat", func(t *testing.T) {
		p := NewPikaParser("3.14")
		result, err := p.Parse()
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if !ast.IsFloat(result) {
			t.Errorf("expected float, got %v", result)
		}
	})

	t.Run("ParseSymbol", func(t *testing.T) {
		p := NewPikaParser("hello-world")
		result, err := p.Parse()
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if !ast.IsSym(result) || result.Str != "hello-world" {
			t.Errorf("expected hello-world, got %v", result)
		}
	})

	t.Run("ParseNil", func(t *testing.T) {
		p := NewPikaParser("nil")
		result, err := p.Parse()
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if !ast.IsNil(result) {
			t.Errorf("expected nil, got %v", result)
		}
	})
}

func TestPikaParserLists(t *testing.T) {
	t.Run("EmptyList", func(t *testing.T) {
		p := NewPikaParser("()")
		result, err := p.Parse()
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if !ast.IsNil(result) {
			t.Errorf("expected nil for empty list, got %v", result)
		}
	})

	t.Run("SimpleList", func(t *testing.T) {
		p := NewPikaParser("(1 2 3)")
		result, err := p.Parse()
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if !ast.IsCell(result) {
			t.Errorf("expected list, got %v", result)
		}
		// Check first element
		if !ast.IsInt(result.Car) || result.Car.Int != 1 {
			t.Errorf("expected first element 1, got %v", result.Car)
		}
	})

	t.Run("NestedList", func(t *testing.T) {
		p := NewPikaParser("((a b) (c d))")
		result, err := p.Parse()
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if !ast.IsCell(result) {
			t.Errorf("expected nested list")
		}
		// First element should be a list
		if !ast.IsCell(result.Car) {
			t.Errorf("expected first element to be list")
		}
	})

	t.Run("DottedPair", func(t *testing.T) {
		p := NewPikaParser("(a . b)")
		result, err := p.Parse()
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if !ast.IsCell(result) {
			t.Errorf("expected pair")
		}
		if !ast.IsSym(result.Car) || result.Car.Str != "a" {
			t.Errorf("expected car to be 'a'")
		}
		if !ast.IsSym(result.Cdr) || result.Cdr.Str != "b" {
			t.Errorf("expected cdr to be 'b'")
		}
	})
}

func TestPikaParserQuotes(t *testing.T) {
	t.Run("Quote", func(t *testing.T) {
		p := NewPikaParser("'x")
		result, err := p.Parse()
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		// Should be (quote x)
		if !ast.IsCell(result) {
			t.Errorf("expected list for quote")
		}
		if !ast.IsSym(result.Car) || result.Car.Str != "quote" {
			t.Errorf("expected quote symbol")
		}
	})

	t.Run("Quasiquote", func(t *testing.T) {
		p := NewPikaParser("`(a ,b)")
		result, err := p.Parse()
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		// Should be (quasiquote (a (unquote b)))
		if !ast.IsCell(result) || !ast.IsSym(result.Car) || result.Car.Str != "quasiquote" {
			t.Errorf("expected quasiquote")
		}
	})

	t.Run("UnquoteSplicing", func(t *testing.T) {
		p := NewPikaParser(",@xs")
		result, err := p.Parse()
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		// Should be (unquote-splicing xs)
		if !ast.IsCell(result) || !ast.IsSym(result.Car) || result.Car.Str != "unquote-splicing" {
			t.Errorf("expected unquote-splicing")
		}
	})
}

func TestPikaParserSpecials(t *testing.T) {
	t.Run("True", func(t *testing.T) {
		p := NewPikaParser("#t")
		result, err := p.Parse()
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if !ast.IsSym(result) || result.Str != "#t" {
			t.Errorf("expected #t symbol")
		}
	})

	t.Run("False", func(t *testing.T) {
		p := NewPikaParser("#f")
		result, err := p.Parse()
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if !ast.IsSym(result) || result.Str != "#f" {
			t.Errorf("expected #f symbol")
		}
	})

	t.Run("Character", func(t *testing.T) {
		p := NewPikaParser("#\\a")
		result, err := p.Parse()
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if !ast.IsChar(result) || result.Int != int64('a') {
			t.Errorf("expected #\\a, got %v", result)
		}
	})

	t.Run("CharacterSpace", func(t *testing.T) {
		p := NewPikaParser("#\\space")
		result, err := p.Parse()
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if !ast.IsChar(result) || result.Int != int64(' ') {
			t.Errorf("expected space character")
		}
	})
}

func TestPikaParserComments(t *testing.T) {
	p := NewPikaParser("; this is a comment\n42")
	result, err := p.Parse()
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if !ast.IsInt(result) || result.Int != 42 {
		t.Errorf("expected 42 after comment")
	}
}

func TestPikaParserComplexExpressions(t *testing.T) {
	t.Run("Define", func(t *testing.T) {
		p := NewPikaParser("(define (square x) (* x x))")
		result, err := p.Parse()
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if !ast.IsCell(result) || !ast.IsSym(result.Car) || result.Car.Str != "define" {
			t.Errorf("expected define form")
		}
	})

	t.Run("Lambda", func(t *testing.T) {
		p := NewPikaParser("(lambda (x y) (+ x y))")
		result, err := p.Parse()
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if !ast.IsCell(result) || !ast.IsSym(result.Car) || result.Car.Str != "lambda" {
			t.Errorf("expected lambda form")
		}
	})

	t.Run("Let", func(t *testing.T) {
		p := NewPikaParser("(let ((x 1) (y 2)) (+ x y))")
		result, err := p.Parse()
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if !ast.IsCell(result) || !ast.IsSym(result.Car) || result.Car.Str != "let" {
			t.Errorf("expected let form")
		}
	})
}

func TestPikaParserMemoization(t *testing.T) {
	p := NewPikaParser("(a (b (c d)))")

	// Parse should populate memo
	result, err := p.Parse()
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result == nil {
		t.Error("expected result")
	}

	// Check that memo is populated
	if len(p.Memo) == 0 {
		t.Error("memo should be populated")
	}
}

func TestPikaLeftRecursive(t *testing.T) {
	p := NewPikaLeftRecursive("(a b c)")
	p.RegisterLeftRecursive("expr") // Mark as left-recursive for testing

	// This should still parse correctly
	result, err := p.Parse()
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if !ast.IsCell(result) {
		t.Errorf("expected list")
	}
}

func TestASTNodeHelpers(t *testing.T) {
	t.Run("CreateASTNode", func(t *testing.T) {
		node := CreateASTNode("if",
			ast.NewSym("cond"),
			ast.NewInt(1),
			ast.NewInt(2))

		if GetNodeType(node) != "if" {
			t.Errorf("expected node type 'if'")
		}

		children := GetNodeChildren(node)
		if len(children) != 3 {
			t.Errorf("expected 3 children, got %d", len(children))
		}
	})

	t.Run("GetNodeType", func(t *testing.T) {
		p := NewPikaParser("(lambda (x) x)")
		result, _ := p.Parse()

		nodeType := GetNodeType(result)
		if nodeType != "lambda" {
			t.Errorf("expected lambda, got %s", nodeType)
		}
	})
}

func TestPikaParserWhitespace(t *testing.T) {
	inputs := []string{
		"  42  ",
		"\t42\t",
		"\n42\n",
		"  ( a  b  c )  ",
		"(a\n  b\n  c)",
	}

	for _, input := range inputs {
		p := NewPikaParser(input)
		result, err := p.Parse()
		if err != nil {
			t.Errorf("failed to parse '%s': %v", input, err)
		}
		if result == nil {
			t.Errorf("nil result for '%s'", input)
		}
	}
}

func TestPikaParserErrors(t *testing.T) {
	errorCases := []string{
		"(",       // unclosed paren
		"(a b",    // unclosed paren
		")",       // unexpected close
	}

	for _, input := range errorCases {
		p := NewPikaParser(input)
		_, err := p.Parse()
		if err == nil {
			t.Errorf("expected error for '%s'", input)
		}
	}
}

func TestPikaParserString(t *testing.T) {
	p := NewPikaParser(`"hello world"`)
	result, err := p.Parse()
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// String is represented as (string 'h' 'e' ...)
	if !ast.IsCell(result) {
		t.Error("expected string as list")
	}
	if !ast.IsSym(result.Car) || result.Car.Str != "string" {
		t.Error("expected string tag")
	}
}

func TestPikaParserEscapeSequences(t *testing.T) {
	p := NewPikaParser(`"hello\nworld"`)
	result, err := p.Parse()
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// Should contain newline character
	if !ast.IsCell(result) {
		t.Error("expected string as list")
	}
}

func TestPikaVector(t *testing.T) {
	p := NewPikaParser("#(1 2 3)")
	result, err := p.Parse()
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// Vector is (vec (1 2 3))
	if !ast.IsCell(result) || !ast.IsSym(result.Car) || result.Car.Str != "vec" {
		t.Error("expected vec tag")
	}
}
