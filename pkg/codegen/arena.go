package codegen

import (
	"fmt"
	"strings"

	"purple_go/pkg/analysis"
	"purple_go/pkg/ast"
)

// ArenaScope represents an arena-scoped code block
type ArenaScope struct {
	Name       string
	Vars       []string
	Shape      analysis.Shape
	NeedsArena bool
}

// ArenaCodeGenerator generates arena-aware C code
type ArenaCodeGenerator struct {
	scopes      []*ArenaScope
	scopeID     int
	shapeCtx    *analysis.ShapeContext
	escapeCtx   *analysis.AnalysisContext
}

// NewArenaCodeGenerator creates a new arena-aware code generator
func NewArenaCodeGenerator() *ArenaCodeGenerator {
	return &ArenaCodeGenerator{
		shapeCtx:  analysis.NewShapeContext(),
		escapeCtx: analysis.NewAnalysisContext(),
	}
}

// ShouldUseArena determines if an expression should use arena allocation
func (g *ArenaCodeGenerator) ShouldUseArena(expr *ast.Value) bool {
	g.shapeCtx.AnalyzeShapes(expr)
	shape := g.shapeCtx.ResultShape

	// Use arena for CYCLIC or UNKNOWN shapes
	return shape == analysis.ShapeCyclic || shape == analysis.ShapeUnknown
}

// GenerateArenaLet generates a let binding with arena scope if needed
func (g *ArenaCodeGenerator) GenerateArenaLet(bindings []struct {
	sym *ast.Value
	val string
}, bodyCode string, forceArena bool) string {
	// Check if any binding needs arena
	needsArena := forceArena
	for _, bi := range bindings {
		if ast.IsSym(bi.sym) {
			info := g.shapeCtx.FindShape(bi.sym.Str)
			if info != nil && (info.Shape == analysis.ShapeCyclic || info.Shape == analysis.ShapeUnknown) {
				needsArena = true
				break
			}
		}
	}

	if needsArena {
		return g.generateArenaBlock(bindings, bodyCode)
	}
	return g.generateStandardBlock(bindings, bodyCode)
}

func (g *ArenaCodeGenerator) generateArenaBlock(bindings []struct {
	sym *ast.Value
	val string
}, bodyCode string) string {
	var sb strings.Builder
	arenaName := fmt.Sprintf("_arena%d", g.scopeID)
	g.scopeID++

	sb.WriteString("({\n")
	sb.WriteString(fmt.Sprintf("    Arena* %s = arena_create();\n", arenaName))

	// Generate declarations using arena allocation
	for _, bi := range bindings {
		// Replace mk_int/mk_pair with arena versions
		val := replaceWithArenaAlloc(bi.val, arenaName)
		sb.WriteString(fmt.Sprintf("    Obj* %s = %s;\n", bi.sym.Str, val))
	}

	sb.WriteString(fmt.Sprintf("    Obj* _res = %s;\n", bodyCode))

	// Copy result out of arena if needed
	sb.WriteString("    Obj* _out = NULL;\n")
	sb.WriteString("    if (_res) {\n")
	sb.WriteString("        if (_res->is_pair) {\n")
	sb.WriteString("            _out = mk_pair(_res->a, _res->b);\n")
	sb.WriteString("        } else {\n")
	sb.WriteString("            _out = mk_int(_res->i);\n")
	sb.WriteString("        }\n")
	sb.WriteString("    }\n")

	// Destroy arena - bulk deallocation O(1)
	sb.WriteString(fmt.Sprintf("    arena_destroy(%s); /* Bulk free - O(1) */\n", arenaName))
	sb.WriteString("    _out;\n})")

	return sb.String()
}

func (g *ArenaCodeGenerator) generateStandardBlock(bindings []struct {
	sym *ast.Value
	val string
}, bodyCode string) string {
	var sb strings.Builder

	sb.WriteString("({\n")

	// Generate declarations
	for _, bi := range bindings {
		sb.WriteString(fmt.Sprintf("    Obj* %s = %s;\n", bi.sym.Str, bi.val))
	}

	sb.WriteString(fmt.Sprintf("    Obj* _res = %s;\n", bodyCode))

	// Generate frees based on shape analysis
	for i := len(bindings) - 1; i >= 0; i-- {
		bi := bindings[i]
		info := g.shapeCtx.FindShape(bi.sym.Str)
		shape := analysis.ShapeUnknown
		if info != nil {
			shape = info.Shape
		}
		freeFn := analysis.ShapeFreeStrategy(shape)
		sb.WriteString(fmt.Sprintf("    %s(%s); /* ASAP (shape: %s) */\n",
			freeFn, bi.sym.Str, analysis.ShapeString(shape)))
	}

	sb.WriteString("    _res;\n})")
	return sb.String()
}

// replaceWithArenaAlloc replaces standard allocators with arena versions
func replaceWithArenaAlloc(code, arenaName string) string {
	// Replace mk_int( with arena_mk_int(arena,
	code = strings.ReplaceAll(code, "mk_int(", fmt.Sprintf("arena_mk_int(%s, ", arenaName))
	// Replace mk_pair( with arena_mk_pair(arena,
	code = strings.ReplaceAll(code, "mk_pair(", fmt.Sprintf("arena_mk_pair(%s, ", arenaName))
	return code
}

// WeakEdgeInfo tracks weak edge information for a type
type WeakEdgeInfo struct {
	TypeName  string
	FieldName string
	Reason    string
}

// DetectWeakEdges analyzes types and returns fields that should be weak
func DetectWeakEdges(registry *TypeRegistry) []WeakEdgeInfo {
	var weakEdges []WeakEdgeInfo

	// Build and analyze ownership graph
	registry.BuildOwnershipGraph()
	registry.AnalyzeBackEdges()

	// Collect detected weak edges
	for _, t := range registry.Types {
		for _, f := range t.Fields {
			if f.Strength == FieldWeak {
				weakEdges = append(weakEdges, WeakEdgeInfo{
					TypeName:  t.Name,
					FieldName: f.Name,
					Reason:    "back-edge detected via DFS",
				})
			}
		}
	}

	return weakEdges
}

// GenerateWeakEdgeComment generates a comment documenting weak edges
func GenerateWeakEdgeComment(weakEdges []WeakEdgeInfo) string {
	if len(weakEdges) == 0 {
		return "/* No weak edges detected */\n"
	}

	var sb strings.Builder
	sb.WriteString("/* Automatically detected weak edges (break ownership cycles):\n")
	for _, we := range weakEdges {
		sb.WriteString(fmt.Sprintf(" *   %s.%s - %s\n", we.TypeName, we.FieldName, we.Reason))
	}
	sb.WriteString(" */\n")
	return sb.String()
}
