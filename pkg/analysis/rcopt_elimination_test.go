package analysis

import (
	"testing"

	"purple_go/pkg/parser"
)

func TestRCElimination(t *testing.T) {
	tests := []struct {
		name    string
		code    string
		minElim float64 // Minimum elimination percentage
	}{
		{"unique_int", "(let ((x (+ 1 2))) x)", 100.0},
		{"unique_pair", "(let ((x (cons 1 2))) (car x))", 50.0},
		{"borrowed_car", "(let ((x (cons 1 2))) (let ((y (car x))) y))", 50.0},
		{"chain", "(let ((a 1)) (let ((b a)) (let ((c b)) c)))", 66.0},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			ctx := NewRCOptContext()
			expr, err := parser.Parse(tc.code)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			ctx.Analyze(expr)

			stats := ctx.GetStats()
			if stats.TotalOps == 0 {
				return
			}

			pct := float64(stats.EliminatedOps) / float64(stats.TotalOps) * 100
			if pct < tc.minElim {
				t.Errorf("elimination %.1f%% < expected %.1f%%", pct, tc.minElim)
			}
			t.Logf("%s: %s", tc.name, ctx.ReportStats())
		})
	}
}
