package analysis

import (
	"strings"
	"testing"

	"purple_go/pkg/ast"
)

func TestConcurrencyContext(t *testing.T) {
	ctx := NewConcurrencyContext()

	// Test thread locality tracking
	t.Run("ThreadLocality", func(t *testing.T) {
		ctx.MarkThreadLocal("x")
		if ctx.GetLocality("x") != LocalityThreadLocal {
			t.Error("x should be thread-local")
		}

		ctx.MarkShared("y")
		if ctx.GetLocality("y") != LocalityShared {
			t.Error("y should be shared")
		}

		// Shared overrides thread-local
		ctx.MarkThreadLocal("y")
		if ctx.GetLocality("y") != LocalityShared {
			t.Error("y should remain shared")
		}
	})

	t.Run("ChannelRegistration", func(t *testing.T) {
		ctx.RegisterChannel("ch1")
		if !ctx.Channels["ch1"] {
			t.Error("ch1 should be registered as channel")
		}
		if ctx.GetLocality("ch1") != LocalityShared {
			t.Error("channels should be shared")
		}
	})

	t.Run("TransferPoints", func(t *testing.T) {
		ctx.MarkThreadLocal("msg")
		tp := ctx.AddTransferPoint("ch1", "msg", ChanOpSend, 10)

		if tp == nil {
			t.Fatal("transfer point should not be nil")
		}
		if tp.Op != ChanOpSend {
			t.Error("operation should be send")
		}
		if ctx.GetLocality("msg") != LocalityTransferred {
			t.Error("msg should be transferred after send")
		}
	})

	t.Run("GoroutineCapture", func(t *testing.T) {
		ctx.MarkThreadLocal("z")
		ctx.CapturedByGoroutine("z")

		if !ctx.IsCapturedByGoroutine("z") {
			t.Error("z should be captured by goroutine")
		}
		if ctx.GetLocality("z") != LocalityShared {
			t.Error("goroutine-captured vars should become shared")
		}
		if !ctx.NeedsAtomicRC("z") {
			t.Error("shared objects should need atomic RC")
		}
	})
}

func TestConcurrencyAnalyzer(t *testing.T) {
	t.Run("AnalyzeChannelOperations", func(t *testing.T) {
		ca := NewConcurrencyAnalyzer()

		// (let ((ch (make-chan 1))
		//       (msg (mk_int 42)))
		//   (chan-send! ch msg))
		expr := ast.NewCell(
			ast.NewSym("let"),
			ast.NewCell(
				ast.NewCell(
					ast.NewCell(ast.NewSym("ch"),
						ast.NewCell(
							ast.NewCell(ast.NewSym("make-chan"), ast.NewCell(ast.NewInt(1), ast.Nil)),
							ast.Nil)),
					ast.NewCell(
						ast.NewCell(ast.NewSym("msg"),
							ast.NewCell(ast.NewInt(42), ast.Nil)),
						ast.Nil)),
				ast.NewCell(
					ast.NewCell(ast.NewSym("chan-send!"),
						ast.NewCell(ast.NewSym("ch"),
							ast.NewCell(ast.NewSym("msg"), ast.Nil))),
					ast.Nil),
			),
		)

		ca.Analyze(expr)

		if !ca.Ctx.Channels["ch"] {
			t.Error("ch should be registered as channel")
		}

		if len(ca.Ctx.TransferPoints) != 1 {
			t.Errorf("expected 1 transfer point, got %d", len(ca.Ctx.TransferPoints))
		}
	})

	t.Run("AnalyzeGoroutine", func(t *testing.T) {
		ca := NewConcurrencyAnalyzer()

		// (let ((x 1))
		//   (go (lambda () x)))
		expr := ast.NewCell(
			ast.NewSym("let"),
			ast.NewCell(
				ast.NewCell(
					ast.NewCell(ast.NewSym("x"), ast.NewCell(ast.NewInt(1), ast.Nil)),
					ast.Nil),
				ast.NewCell(
					ast.NewCell(ast.NewSym("go"),
						ast.NewCell(
							ast.NewCell(ast.NewSym("lambda"),
								ast.NewCell(ast.Nil,
									ast.NewCell(ast.NewSym("x"), ast.Nil))),
							ast.Nil)),
					ast.Nil),
			),
		)

		ca.Analyze(expr)

		if !ca.Ctx.IsCapturedByGoroutine("x") {
			t.Error("x should be captured by goroutine")
		}
		if !ca.Ctx.NeedsAtomicRC("x") {
			t.Error("x should need atomic RC")
		}
	})
}

func TestThreadLocalityString(t *testing.T) {
	tests := []struct {
		locality ThreadLocality
		expected string
	}{
		{LocalityThreadLocal, "thread-local"},
		{LocalityShared, "shared"},
		{LocalityTransferred, "transferred"},
		{LocalityUnknown, "unknown"},
	}

	for _, tc := range tests {
		if tc.locality.String() != tc.expected {
			t.Errorf("%d.String() = %s, want %s", tc.locality, tc.locality.String(), tc.expected)
		}
	}
}

func TestConcurrencyCodeGenerator(t *testing.T) {
	ctx := NewConcurrencyContext()
	gen := NewConcurrencyCodeGenerator(ctx)

	t.Run("GenerateAtomicRC", func(t *testing.T) {
		code := gen.GenerateAtomicRC()
		checks := []string{
			"atomic_inc_ref",
			"atomic_dec_ref",
			"__atomic_add_fetch",
			"__atomic_sub_fetch",
			"try_acquire_unique",
		}
		for _, check := range checks {
			if !strings.Contains(code, check) {
				t.Errorf("atomic RC code missing: %s", check)
			}
		}
	})

	t.Run("GenerateChannelRuntime", func(t *testing.T) {
		code := gen.GenerateChannelRuntime()
		checks := []string{
			"struct Channel",
			"make_channel",
			"channel_send",
			"channel_recv",
			"channel_close",
			"TRANSFERS OWNERSHIP",
			"RECEIVES OWNERSHIP",
			"pthread_mutex",
		}
		for _, check := range checks {
			if !strings.Contains(code, check) {
				t.Errorf("channel runtime missing: %s", check)
			}
		}
	})

	t.Run("GenerateGoroutineRuntime", func(t *testing.T) {
		code := gen.GenerateGoroutineRuntime()
		checks := []string{
			"GoroutineArg",
			"goroutine_entry",
			"spawn_goroutine",
			"pthread_create",
			"atomic_inc_ref",
			"atomic_dec_ref",
		}
		for _, check := range checks {
			if !strings.Contains(code, check) {
				t.Errorf("goroutine runtime missing: %s", check)
			}
		}
	})
}

func TestConcurrencyAnalysisInfo(t *testing.T) {
	ca := NewConcurrencyAnalyzer()

	// Add some data
	ca.Ctx.RegisterChannel("ch")
	ca.Ctx.AddTransferPoint("ch", "msg", ChanOpSend, 10)
	ca.Ctx.CapturedByGoroutine("x")

	info := ca.GenerateConcurrencyInfo()

	checks := []string{
		"Concurrency Analysis Results",
		"Transfer points: 1",
		"Shared objects:",
		"send msg via channel ch",
	}

	for _, check := range checks {
		if !strings.Contains(info, check) {
			t.Errorf("info missing: %s", check)
		}
	}
}

func TestOwnershipTransferSemantics(t *testing.T) {
	ctx := NewConcurrencyContext()

	// Before send: value is thread-local
	ctx.MarkThreadLocal("value")
	if ctx.GetLocality("value") != LocalityThreadLocal {
		t.Fatal("value should start as thread-local")
	}

	// After send: ownership transferred
	ctx.AddTransferPoint("ch", "value", ChanOpSend, 1)
	if ctx.GetLocality("value") != LocalityTransferred {
		t.Error("value should be transferred after send")
	}

	// Sender should NOT use value after transfer
	// This would be a use-after-transfer error
	// (enforced by analysis, not runtime)
}

func TestScopeTracking(t *testing.T) {
	ca := NewConcurrencyAnalyzer()

	ca.AddVar("outer")
	if !ca.IsInScope("outer") {
		t.Error("outer should be in scope")
	}

	ca.PushScope()
	ca.AddVar("inner")
	if !ca.IsInScope("inner") {
		t.Error("inner should be in scope")
	}
	if !ca.IsInScope("outer") {
		t.Error("outer should still be in scope")
	}

	ca.PopScope()
	if ca.IsInScope("inner") {
		t.Error("inner should be out of scope")
	}
	if !ca.IsInScope("outer") {
		t.Error("outer should still be in scope")
	}
}
