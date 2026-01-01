package eval

import (
	"sync"

	"purple_go/pkg/ast"
)

// Global environment for top-level definitions
var (
	globalEnv   *ast.Value
	globalMutex sync.RWMutex
)

// InitGlobalEnv initializes the global environment with default bindings
func InitGlobalEnv() {
	globalMutex.Lock()
	defer globalMutex.Unlock()
	globalEnv = DefaultEnv()
}

// GetGlobalEnv returns the current global environment
func GetGlobalEnv() *ast.Value {
	globalMutex.RLock()
	if globalEnv != nil {
		defer globalMutex.RUnlock()
		return globalEnv
	}
	globalMutex.RUnlock()
	// InitGlobalEnv grabs its own write lock
	InitGlobalEnv()
	globalMutex.RLock()
	defer globalMutex.RUnlock()
	return globalEnv
}

// GlobalDefine adds a definition to the global environment
func GlobalDefine(sym, val *ast.Value) {
	globalMutex.Lock()
	defer globalMutex.Unlock()
	if globalEnv == nil {
		globalEnv = ast.Nil
	}
	// Check if already defined, update if so
	updated := false
	env := globalEnv
	for !ast.IsNil(env) && ast.IsCell(env) {
		pair := env.Car
		if ast.IsCell(pair) && ast.SymEq(pair.Car, sym) {
			pair.Cdr = val
			updated = true
			break
		}
		env = env.Cdr
	}
	if !updated {
		globalEnv = ast.NewCell(ast.NewCell(sym, val), globalEnv)
	}
}

// GlobalLookup looks up a symbol in the global environment
func GlobalLookup(sym *ast.Value) *ast.Value {
	globalMutex.RLock()
	defer globalMutex.RUnlock()
	if globalEnv == nil {
		return nil
	}
	return EnvLookup(globalEnv, sym)
}

// ResetGlobalEnv resets the global environment (for testing)
func ResetGlobalEnv() {
	globalMutex.Lock()
	defer globalMutex.Unlock()
	globalEnv = nil
}

// EnvLookup looks up a symbol in an environment (association list)
func EnvLookup(env, sym *ast.Value) *ast.Value {
	for !ast.IsNil(env) && ast.IsCell(env) {
		pair := env.Car
		if ast.IsCell(pair) && ast.SymEq(pair.Car, sym) {
			return pair.Cdr
		}
		env = env.Cdr
	}
	return nil
}

// EnvExtend extends an environment with a new binding
func EnvExtend(env, sym, val *ast.Value) *ast.Value {
	return ast.NewCell(ast.NewCell(sym, val), env)
}

// EnvExtendMulti extends an environment with multiple bindings
func EnvExtendMulti(env *ast.Value, syms, vals []*ast.Value) *ast.Value {
	for i := range syms {
		if i < len(vals) {
			env = EnvExtend(env, syms[i], vals[i])
		}
	}
	return env
}

// BuildEnv creates an environment from a list of (name, value) pairs
func BuildEnv(pairs ...*ast.Value) *ast.Value {
	env := ast.Nil
	for i := 0; i < len(pairs)-1; i += 2 {
		env = EnvExtend(env, pairs[i], pairs[i+1])
	}
	return env
}

// EnvSet mutates an existing binding in the environment
// Returns true if binding was found and mutated, false otherwise
func EnvSet(env, sym, val *ast.Value) bool {
	for !ast.IsNil(env) && ast.IsCell(env) {
		pair := env.Car
		if ast.IsCell(pair) && ast.SymEq(pair.Car, sym) {
			// Mutate the binding in place
			pair.Cdr = val
			return true
		}
		env = env.Cdr
	}
	return false
}
