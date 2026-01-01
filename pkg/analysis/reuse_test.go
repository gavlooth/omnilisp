package analysis

import (
	"strings"
	"testing"

	"purple_go/pkg/ast"
)

func TestTypeSize(t *testing.T) {
	ts := NewTypeSize()

	t.Run("DefaultSizes", func(t *testing.T) {
		tests := []struct {
			typeName string
			expected int
		}{
			{"int", 2},
			{"float", 2},
			{"pair", 4},
			{"closure", 4},
			{"box", 2},
			{"unknown", 2}, // default
		}

		for _, tc := range tests {
			size := ts.GetSize(tc.typeName)
			if size != tc.expected {
				t.Errorf("GetSize(%s) = %d, want %d", tc.typeName, size, tc.expected)
			}
		}
	})

	t.Run("CanReuse", func(t *testing.T) {
		tests := []struct {
			freeType  string
			allocType string
			expected  ReusePattern
		}{
			{"int", "int", ReuseExact},
			{"int", "box", ReuseExact},       // same size
			{"pair", "int", ReusePadded},     // pair > int
			{"pair", "pair", ReuseExact},
			{"int", "pair", ReuseNone},       // int < pair
			{"closure", "pair", ReuseExact},  // same size
		}

		for _, tc := range tests {
			result := ts.CanReuse(tc.freeType, tc.allocType)
			if result != tc.expected {
				t.Errorf("CanReuse(%s, %s) = %s, want %s",
					tc.freeType, tc.allocType, result.String(), tc.expected.String())
			}
		}
	})
}

func TestReuseContext(t *testing.T) {
	ctx := NewReuseContext()

	t.Run("PendingFrees", func(t *testing.T) {
		ctx.AddPendingFree("x", "int")
		ctx.AddPendingFree("y", "pair")

		if len(ctx.PendingFrees) != 2 {
			t.Errorf("expected 2 pending frees, got %d", len(ctx.PendingFrees))
		}
	})

	t.Run("TryReuse", func(t *testing.T) {
		// y (pair) can be reused for z (pair)
		candidate := ctx.TryReuse("z", "pair", 10)

		if candidate == nil {
			t.Fatal("expected reuse candidate")
		}
		if candidate.FreeVar != "y" {
			t.Errorf("expected to reuse y, got %s", candidate.FreeVar)
		}
		if candidate.AllocVar != "z" {
			t.Errorf("expected alloc var z, got %s", candidate.AllocVar)
		}

		// y should be removed from pending
		if len(ctx.PendingFrees) != 1 {
			t.Errorf("expected 1 pending free, got %d", len(ctx.PendingFrees))
		}
	})

	t.Run("GetReuse", func(t *testing.T) {
		freeVar, ok := ctx.GetReuse("z")
		if !ok {
			t.Error("z should have a reuse mapping")
		}
		if freeVar != "y" {
			t.Errorf("z should reuse y, got %s", freeVar)
		}
	})
}

func TestReuseAnalyzer(t *testing.T) {
	t.Run("AnalyzeLet", func(t *testing.T) {
		ra := NewReuseAnalyzer()

		// (let ((x 1) (y 2))
		//   (let ((z (cons x y)))
		//     z))
		expr := ast.NewCell(
			ast.NewSym("let"),
			ast.NewCell(
				ast.NewCell(
					ast.NewCell(ast.NewSym("x"), ast.NewCell(ast.NewInt(1), ast.Nil)),
					ast.NewCell(
						ast.NewCell(ast.NewSym("y"), ast.NewCell(ast.NewInt(2), ast.Nil)),
						ast.Nil)),
				ast.NewCell(
					ast.NewCell(ast.NewSym("let"),
						ast.NewCell(
							ast.NewCell(
								ast.NewCell(ast.NewSym("z"),
									ast.NewCell(
										ast.NewCell(ast.NewSym("cons"),
											ast.NewCell(ast.NewSym("x"),
												ast.NewCell(ast.NewSym("y"), ast.Nil))),
										ast.Nil)),
								ast.Nil),
							ast.NewCell(ast.NewSym("z"), ast.Nil))),
					ast.Nil),
			),
		)

		ra.Analyze(expr)

		// Should track variables through scopes
		stats := ra.GenerateReuseStats()
		if !strings.Contains(stats, "Reuse Analysis") {
			t.Error("expected reuse stats in output")
		}
	})
}

func TestShapeRouter(t *testing.T) {
	shapeCtx := NewShapeContext()
	sr := NewShapeRouter(shapeCtx)

	t.Run("RouteByShape", func(t *testing.T) {
		shapeCtx.AddShape("tree_var", ShapeTree)
		shapeCtx.AddShape("dag_var", ShapeDAG)
		shapeCtx.AddShape("cyclic_var", ShapeCyclic)

		tests := []struct {
			varName  string
			expected string
		}{
			{"tree_var", "free_tree"},
			{"dag_var", "dec_ref"},
			{"cyclic_var", "arena_release"},
			{"unknown_var", "dec_ref"},  // default
		}

		for _, tc := range tests {
			strategy := sr.RouteStrategy(tc.varName)
			if strategy != tc.expected {
				t.Errorf("RouteStrategy(%s) = %s, want %s",
					tc.varName, strategy, tc.expected)
			}
		}
	})

	t.Run("GenerateShapeRoutedFree", func(t *testing.T) {
		shapeCtx.AddShape("my_tree", ShapeTree)
		code := sr.GenerateShapeRoutedFree("my_tree")

		if !strings.Contains(code, "free_tree") {
			t.Error("expected free_tree in generated code")
		}
		if !strings.Contains(code, "my_tree") {
			t.Error("expected variable name in generated code")
		}
	})
}

func TestPerceusOptimizer(t *testing.T) {
	po := NewPerceusOptimizer()

	t.Run("GenerateReuseExact", func(t *testing.T) {
		code := po.GenerateReuse("new_int", "int", "old_int", "int")

		if !strings.Contains(code, "reuse_as_int") {
			t.Error("expected reuse_as_int in output")
		}
		if !strings.Contains(code, "exact match") {
			t.Error("expected exact match comment")
		}
	})

	t.Run("GenerateReusePadded", func(t *testing.T) {
		code := po.GenerateReuse("new_int", "int", "old_pair", "pair")

		if !strings.Contains(code, "padded") {
			t.Error("expected padded reuse")
		}
		if !strings.Contains(code, "words unused") {
			t.Error("expected unused words comment")
		}
	})

	t.Run("GenerateNoReuse", func(t *testing.T) {
		code := po.GenerateReuse("new_pair", "pair", "old_int", "int")

		if !strings.Contains(code, "No reuse possible") {
			t.Error("expected no reuse comment")
		}
		if !strings.Contains(code, "mk_pair") {
			t.Error("expected fallback to fresh allocation")
		}
	})

	t.Run("GenerateReuseRuntime", func(t *testing.T) {
		runtime := po.GenerateReuseRuntime()

		checks := []string{
			"reuse_as_int",
			"reuse_as_pair",
			"reuse_as_box",
			"can_reuse",
			"consume_for_reuse",
			"FBIP",
			"In-place update",
		}

		for _, check := range checks {
			if !strings.Contains(runtime, check) {
				t.Errorf("runtime missing: %s", check)
			}
		}
	})
}

func TestDPSOptimizer(t *testing.T) {
	dps := NewDPSOptimizer()

	runtime := dps.GenerateDPSRuntime()

	checks := []string{
		"Destination-Passing",
		"map_into",
		"filter_into",
		"append_into",
		"dest",
	}

	for _, check := range checks {
		if !strings.Contains(runtime, check) {
			t.Errorf("DPS runtime missing: %s", check)
		}
	}
}

func TestReusePatternString(t *testing.T) {
	tests := []struct {
		pattern  ReusePattern
		expected string
	}{
		{ReuseNone, "none"},
		{ReuseExact, "exact"},
		{ReusePadded, "padded"},
		{ReusePartial, "partial"},
	}

	for _, tc := range tests {
		if tc.pattern.String() != tc.expected {
			t.Errorf("%d.String() = %s, want %s", tc.pattern, tc.pattern.String(), tc.expected)
		}
	}
}

func TestScopedReuseAnalysis(t *testing.T) {
	ra := NewReuseAnalyzer()

	// Test scope-based reuse
	ra.PushScope()
	ra.AddVar("x", "int")
	ra.AddVar("y", "pair")
	ra.PopScope()

	// Variables from popped scope should be pending for reuse
	candidate := ra.Ctx.TryReuse("z", "pair", 1)
	if candidate == nil {
		t.Fatal("should find reuse candidate from popped scope")
	}
	if candidate.FreeVar != "y" {
		t.Errorf("expected y to be reused, got %s", candidate.FreeVar)
	}
}
