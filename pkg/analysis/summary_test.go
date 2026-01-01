package analysis

import (
	"testing"

	"purple_go/pkg/ast"
)

func TestSummaryRegistry(t *testing.T) {
	sr := NewSummaryRegistry()

	// Test primitive summaries exist
	primitives := []string{"cons", "car", "cdr", "list", "map", "filter", "fold", "box", "unbox"}
	for _, name := range primitives {
		if sr.Lookup(name) == nil {
			t.Errorf("Missing primitive summary for: %s", name)
		}
	}
}

func TestConsSummary(t *testing.T) {
	sr := NewSummaryRegistry()
	cons := sr.Lookup("cons")

	if cons == nil {
		t.Fatal("cons summary not found")
	}

	if !cons.IsPrimitive {
		t.Error("cons should be primitive")
	}

	if len(cons.Params) != 2 {
		t.Errorf("cons should have 2 params, got %d", len(cons.Params))
	}

	if cons.Params[0].Ownership != OwnerBorrowed {
		t.Error("cons first param should be borrowed")
	}

	if cons.Return.Ownership != OwnerFresh {
		t.Error("cons should return fresh")
	}

	if cons.Effects&EffectAllocates == 0 {
		t.Error("cons should allocate")
	}
}

func TestChanSendConsumesOwnership(t *testing.T) {
	sr := NewSummaryRegistry()
	chanSend := sr.Lookup("chan-send!")

	if chanSend == nil {
		t.Fatal("chan-send! summary not found")
	}

	// Second param (value) should be consumed
	if len(chanSend.Params) < 2 {
		t.Fatal("chan-send! should have 2 params")
	}

	if chanSend.Params[1].Ownership != OwnerConsumed {
		t.Errorf("chan-send! second param should be consumed, got %v", chanSend.Params[1].Ownership)
	}
}

func TestSummaryAnalyzer(t *testing.T) {
	sa := NewSummaryAnalyzer()

	// Analyze a simple function: (define (f x) (cons x nil))
	params := ast.List1(ast.NewSym("x"))
	body := ast.List3(ast.NewSym("cons"), ast.NewSym("x"), ast.Nil)

	summary := sa.AnalyzeFunction("f", params, body)

	if summary == nil {
		t.Fatal("AnalyzeFunction returned nil")
	}

	if len(summary.Params) != 1 {
		t.Errorf("f should have 1 param, got %d", len(summary.Params))
	}

	if summary.Effects&EffectAllocates == 0 {
		t.Error("f should allocate (calls cons)")
	}

	// cons should be in call graph
	if summary.CallGraph["cons"] == nil {
		t.Error("cons should be in call graph")
	}
}

func TestRecursiveFunctionDetection(t *testing.T) {
	sa := NewSummaryAnalyzer()

	// Analyze: (define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
	params := ast.List1(ast.NewSym("n"))
	body := ast.NewCell(
		ast.NewSym("if"),
		ast.NewCell(
			ast.List3(ast.NewSym("="), ast.NewSym("n"), ast.NewInt(0)),
			ast.NewCell(
				ast.NewInt(1),
				ast.List1(
					ast.List3(ast.NewSym("*"), ast.NewSym("n"),
						ast.List2(ast.NewSym("fact"),
							ast.List3(ast.NewSym("-"), ast.NewSym("n"), ast.NewInt(1)))),
				),
			),
		),
	)

	summary := sa.AnalyzeFunction("fact", params, body)

	if !summary.IsRecursive {
		t.Error("fact should be detected as recursive")
	}
}

func TestSideEffectsDetection(t *testing.T) {
	sa := NewSummaryAnalyzer()

	// Function with I/O
	ioBody := ast.List2(ast.NewSym("display"), ast.NewSym("x"))
	ioSummary := sa.AnalyzeFunction("print-x", ast.List1(ast.NewSym("x")), ioBody)
	if ioSummary.Effects&EffectIO == 0 {
		t.Error("print-x should have IO effect")
	}

	// Function with mutation
	mutBody := ast.List3(ast.NewSym("set-box!"), ast.NewSym("b"), ast.NewInt(1))
	mutSummary := sa.AnalyzeFunction("mutate-b", ast.List1(ast.NewSym("b")), mutBody)
	if mutSummary.Effects&EffectMutates == 0 {
		t.Error("mutate-b should have mutation effect")
	}

	// Function that throws
	throwBody := ast.List2(ast.NewSym("error"), ast.NewSym("msg"))
	throwSummary := sa.AnalyzeFunction("fail", ast.List1(ast.NewSym("msg")), throwBody)
	if throwSummary.Effects&EffectThrows == 0 {
		t.Error("fail should have throws effect")
	}
}

func TestGetParamOwnership(t *testing.T) {
	sr := NewSummaryRegistry()

	// chan-send! second param is consumed
	ownership := sr.GetParamOwnership("chan-send!", 1)
	if ownership != OwnerConsumed {
		t.Errorf("chan-send! param 1 should be consumed, got %v", ownership)
	}

	// cons first param is borrowed
	ownership = sr.GetParamOwnership("cons", 0)
	if ownership != OwnerBorrowed {
		t.Errorf("cons param 0 should be borrowed, got %v", ownership)
	}

	// Unknown function defaults to borrowed
	ownership = sr.GetParamOwnership("unknown-fn", 0)
	if ownership != OwnerBorrowed {
		t.Error("unknown function param should default to borrowed")
	}
}

func TestGetReturnOwnership(t *testing.T) {
	sr := NewSummaryRegistry()

	// cons returns fresh
	ownership := sr.GetReturnOwnership("cons")
	if ownership != OwnerFresh {
		t.Errorf("cons should return fresh, got %v", ownership)
	}

	// car returns borrowed
	ownership = sr.GetReturnOwnership("car")
	if ownership != OwnerBorrowed {
		t.Errorf("car should return borrowed, got %v", ownership)
	}
}

func TestIsPure(t *testing.T) {
	sr := NewSummaryRegistry()

	// Add a pure function summary
	pure := NewFunctionSummary("identity")
	pure.AddParam("x", OwnerBorrowed)
	pure.Return = &ReturnSummary{Ownership: OwnerBorrowed, FromParam: "x"}
	sr.Register(pure)

	if !sr.IsPure("identity") {
		t.Error("identity should be pure")
	}

	if sr.IsPure("cons") {
		t.Error("cons should not be pure (allocates)")
	}

	if sr.IsPure("display") {
		t.Error("display should not be pure (IO)")
	}
}
