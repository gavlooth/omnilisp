package codegen

import (
	"strings"
	"testing"

	"purple_go/pkg/ast"
)

func TestReuseCodeGeneration(t *testing.T) {
	gen := NewCodeGenerator(&strings.Builder{})

	// Pair reuse
	gen.reuseCtx.Ctx.AddPendingFree("x", "pair")
	bindings := []struct {
		sym *ast.Value
		val *ast.Value
	}{
		{
			ast.NewSym("y"),
			ast.NewCell(
				ast.NewSym("cons"),
				ast.NewCell(ast.NewInt(3), ast.NewCell(ast.NewInt(4), ast.Nil)),
			),
		},
	}
	code := gen.GenerateLet(bindings, ast.NewSym("y"))
	if !strings.Contains(code, "reuse_as_pair") {
		t.Errorf("expected reuse_as_pair in generated code:\n%s", code)
	}

	// Int reuse
	gen2 := NewCodeGenerator(&strings.Builder{})
	gen2.reuseCtx.Ctx.AddPendingFree("a", "int")
	bindings2 := []struct {
		sym *ast.Value
		val *ast.Value
	}{
		{ast.NewSym("b"), ast.NewInt(7)},
	}
	code2 := gen2.GenerateLet(bindings2, ast.NewSym("b"))
	if !strings.Contains(code2, "reuse_as_int") {
		t.Errorf("expected reuse_as_int in generated code:\n%s", code2)
	}
}
