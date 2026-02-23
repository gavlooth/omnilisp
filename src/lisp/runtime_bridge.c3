module lisp;

import std::io;
import main;
import lisp::runtime;

// =============================================================================
// RUNTIME BRIDGE — Interpreter ↔ Runtime conversion
// =============================================================================
//
// This module bridges the interpreter (lisp::*) and compiled runtime
// (lisp::runtime::*). Only included in the full interpreter build,
// NOT in standalone AOT binaries.

/**
 * Thread-local interpreter instance for bridge operations.
 * Lazily initialized on first use.
 */
tlocal Interp* g_bridge_interp = null;

/**
 * Ensure the bridge interpreter is initialized.
 */
fn void bridge_ensure_interp() {
    if (g_bridge_interp != null) return;

    g_bridge_interp = (Interp*)mem::malloc(Interp.sizeof);
    g_bridge_interp.init();
    register_primitives(g_bridge_interp);
    register_stdlib(g_bridge_interp);
    g_bridge_interp.flags.jit_enabled = true;
}

/**
 * Convert an interpreter Value* to a runtime Value.
 * HASHMAP/ARRAY convert to native V_DICT/V_ARRAY.
 */
fn runtime::Value bridge_interp_to_runtime(Value* v) {
    if (v == null) {
        return runtime::make_nil();
    }

    ValueTag itag = v.tag;

    if (itag == ValueTag.NIL) {
        return runtime::make_nil();
    } else if (itag == ValueTag.INT) {
        return runtime::make_int(v.int_val);
    } else if (itag == ValueTag.STRING) {
        return runtime::make_string(v.str_val.chars[:v.str_val.len]);
    } else if (itag == ValueTag.SYMBOL) {
        if (g_bridge_interp != null) {
            if ((uint)v.sym_val == (uint)g_bridge_interp.sym_true) {
                return runtime::make_true();
            }
            if ((uint)v.sym_val == (uint)g_bridge_interp.sym_false) {
                return runtime::make_false();
            }
        }
        char[] sym_name = g_bridge_interp.symbols.get_name(v.sym_val);
        return runtime::make_symbol(sym_name);
    } else if (itag == ValueTag.CONS) {
        runtime::Value car = bridge_interp_to_runtime(v.cons_val.car);
        runtime::Value cdr = bridge_interp_to_runtime(v.cons_val.cdr);
        return runtime::rt_cons(car, cdr);
    } else if (itag == ValueTag.CLOSURE || itag == ValueTag.PRIMITIVE || itag == ValueTag.PARTIAL_PRIM) {
        return bridge_make_interp_closure_wrapper(v);
    } else if (itag == ValueTag.CONTINUATION) {
        return bridge_make_interp_continuation_wrapper(v);
    } else if (itag == ValueTag.DOUBLE) {
        return runtime::rt_make_double(v.double_val);
    } else if (itag == ValueTag.HASHMAP) {
        HashMap* map = v.hashmap_val;
        runtime::Value d = runtime::make_dict(map.capacity > 16 ? map.capacity : 16);
        for (uint i = 0; i < map.capacity; i++) {
            if (map.entries[i].key != null) {
                runtime::Value rk = bridge_interp_to_runtime(map.entries[i].key);
                runtime::Value rv = bridge_interp_to_runtime(map.entries[i].value);
                runtime::rtmap_set(d.dict_val, rk, rv);
            }
        }
        return d;
    } else if (itag == ValueTag.ARRAY) {
        runtime::Value a = runtime::make_array(v.array_val.length > 0 ? v.array_val.length : 4);
        for (usz i = 0; i < v.array_val.length; i++) {
            runtime::Value elem = bridge_interp_to_runtime(v.array_val.items[i]);
            runtime::rtarray_push(a.array_val, elem);
        }
        return a;
    } else if (itag == ValueTag.INSTANCE) {
        // Instance: keep as opaque interp ref
        runtime::Value r;
        r.tag = runtime::ValueTag.V_INTERP_REF;
        r.interp_ref = v;
        return r;
    }
    return runtime::make_nil();
}

/**
 * Convert a runtime Value to an interpreter Value*.
 * V_DICT converts to interpreter HASHMAP, V_ARRAY to interpreter ARRAY.
 */
fn Value* bridge_runtime_to_interp(runtime::Value v) {
    bridge_ensure_interp();

    runtime::ValueTag rtag = v.tag;

    if (rtag == runtime::ValueTag.V_NIL) {
        return make_nil(g_bridge_interp);
    } else if (rtag == runtime::ValueTag.V_TRUE) {
        return make_symbol(g_bridge_interp, g_bridge_interp.sym_true);
    } else if (rtag == runtime::ValueTag.V_FALSE) {
        return make_symbol(g_bridge_interp, g_bridge_interp.sym_false);
    } else if (rtag == runtime::ValueTag.V_INT) {
        return make_int(g_bridge_interp, v.int_val);
    } else if (rtag == runtime::ValueTag.V_STRING) {
        runtime::StringData* s = main::dereference_as(runtime::StringData, v.str_handle);
        return make_string(g_bridge_interp, s.chars[:s.len]);
    } else if (rtag == runtime::ValueTag.V_SYMBOL) {
        runtime::SymbolData* sym = main::dereference_as(runtime::SymbolData, v.sym_handle);
        SymbolId sid = g_bridge_interp.symbols.intern(sym.chars[:sym.len]);
        return make_symbol(g_bridge_interp, sid);
    } else if (rtag == runtime::ValueTag.V_CONS) {
        runtime::Value* car_rt = main::dereference_as(runtime::Value, v.cons_val.car);
        runtime::Value* cdr_rt = main::dereference_as(runtime::Value, v.cons_val.cdr);
        Value* car_interp = bridge_runtime_to_interp(*car_rt);
        Value* cdr_interp = bridge_runtime_to_interp(*cdr_rt);
        return make_cons(g_bridge_interp, car_interp, cdr_interp);
    } else if (rtag == runtime::ValueTag.V_DOUBLE) {
        return make_double(g_bridge_interp, v.double_val);
    } else if (rtag == runtime::ValueTag.V_INTERP_REF) {
        return (Value*)v.interp_ref;
    } else if (rtag == runtime::ValueTag.V_DICT) {
        runtime::RtHashMap* dm = v.dict_val;
        Value* map_val = make_hashmap(g_bridge_interp, dm.capacity > 16 ? dm.capacity : 16);
        HashMap* map = map_val.hashmap_val;
        for (uint i = 0; i < dm.capacity; i++) {
            if (!runtime::rtmap_slot_empty(&dm.entries[i])) {
                Value* ik = bridge_runtime_to_interp(dm.entries[i].key);
                Value* iv = bridge_runtime_to_interp(dm.entries[i].value);
                hashmap_set(map, ik, iv, g_bridge_interp);
            }
        }
        return map_val;
    } else if (rtag == runtime::ValueTag.V_ARRAY) {
        runtime::RtArray* ra = v.array_val;
        Value* arr_val = g_bridge_interp.alloc_value();
        arr_val.tag = ARRAY;
        arr_val.array_val.length = ra.length;
        arr_val.array_val.capacity = ra.length > 0 ? ra.length : 4;
        arr_val.array_val.items = (Value**)mem::malloc(Value*.sizeof * arr_val.array_val.capacity);
        for (usz i = 0; i < ra.length; i++) {
            arr_val.array_val.items[i] = bridge_runtime_to_interp(ra.items[i]);
        }
        return arr_val;
    } else if (rtag == runtime::ValueTag.V_CLOSURE) {
        BridgeClosureWrapper* ccw = (BridgeClosureWrapper*)mem::malloc(BridgeClosureWrapper.sizeof);
        ccw.data = v.closure_val.data;
        ccw.func = v.closure_val.invoke;
        Value* prim = make_primitive(g_bridge_interp, "compiled-closure", &bridge_invoke_compiled_closure, 1);
        prim.prim_val.user_data = (void*)ccw;
        return prim;
    }
    return make_nil(g_bridge_interp);
}

/**
 * Wrapper state for compiled closures passed back to interpreter.
 */
struct BridgeClosureWrapper {
    void* data;
    runtime::ClosureFn func;
}

/**
 * PrimitiveFn adapter: invoke a compiled closure from interpreter context.
 */
fn Value* bridge_invoke_compiled_closure(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return make_nil(interp);

    BridgeClosureWrapper* ccw = (BridgeClosureWrapper*)interp.prim_user_data;
    if (ccw == null) return make_nil(interp);

    runtime::Value rt_arg = bridge_interp_to_runtime(args[0]);
    runtime::Value result = ccw.func(ccw.data, rt_arg);
    return bridge_runtime_to_interp(result);
}

/**
 * Wrapper state for interpreter closures used in compiled code.
 */
struct BridgeInterpWrapper {
    Value* interp_value;
}

/**
 * Invoke function for interpreter closure wrappers.
 */
fn runtime::Value bridge_invoke_interp_closure(void* data, runtime::Value arg) {
    BridgeInterpWrapper* wrapper = (BridgeInterpWrapper*)data;
    bridge_ensure_interp();

    Value* interp_arg = bridge_runtime_to_interp(arg);
    Value* result = jit_apply_value(wrapper.interp_value, interp_arg, g_bridge_interp);

    if (result != null && is_error(result)) {
        io::printfn("Error in interpreter closure: %s", (ZString)result.str_val.chars[:result.str_val.len]);
        return runtime::make_nil();
    }

    return bridge_interp_to_runtime(result);
}

/**
 * Create a runtime closure wrapping an interpreter closure.
 */
fn runtime::Value bridge_make_interp_closure_wrapper(Value* interp_val) {
    BridgeInterpWrapper* wrapper = (BridgeInterpWrapper*)mem::malloc(BridgeInterpWrapper.sizeof);
    wrapper.interp_value = interp_val;
    return runtime::make_closure((void*)wrapper, &bridge_invoke_interp_closure);
}

/**
 * Create a runtime closure wrapping an interpreter continuation.
 */
fn runtime::Value bridge_make_interp_continuation_wrapper(Value* interp_val) {
    BridgeInterpWrapper* wrapper = (BridgeInterpWrapper*)mem::malloc(BridgeInterpWrapper.sizeof);
    wrapper.interp_value = interp_val;
    return runtime::make_closure((void*)wrapper, &bridge_invoke_interp_closure);
}

/**
 * Evaluate Omni source code using the interpreter.
 * Registered as hook via rt_set_eval_source_hook.
 */
fn runtime::Value bridge_eval_source(char[] source) {
    bridge_ensure_interp();

    EvalResult result = run(source, &*g_bridge_interp);

    if (result.error.has_error) {
        io::printfn("Error in eval_source: %s", (ZString)&result.error.message);
        return runtime::make_nil();
    }

    return bridge_interp_to_runtime(result.value);
}

/**
 * Initialize the runtime bridge.
 * Registers hooks so that runtime.c3's rt_eval_source dispatches to
 * the interpreter-backed implementation.
 * Must be called during interpreter startup.
 */
fn void bridge_init() {
    runtime::rt_set_eval_source_hook(&bridge_eval_source);
}
