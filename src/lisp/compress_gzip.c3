module lisp;

import std::core::mem;

// ============================================================
// (gzip s) -> compressed string
// ============================================================

fn Value* prim_gzip(Value*[] args, Env* env, Interp* interp) {
    int level = LIBDEFLATE_DEFAULT_LEVEL;
    if (Value* err = parse_optional_compression_level(args, &level, interp)) return err;

    char[] src = args[0].str_chars[:args[0].str_len];

    DeflateCompressor* c = libdeflate_alloc_compressor(level);
    // fault: lisp::WRITE_FAILED
    if (c == null) return compression_runtime_raise(interp, "runtime/out-of-memory", "gzip: failed to allocate compressor");
    defer libdeflate_free_compressor(c);

    usz bound = libdeflate_gzip_compress_bound(c, src.len);
    char* out_buf = (char*)mem::malloc(bound);
    // fault: lisp::WRITE_FAILED
    if (out_buf == null) return compression_runtime_raise(interp, "runtime/out-of-memory", "gzip: out of memory");
    defer mem::free(out_buf);

    usz actual = libdeflate_gzip_compress(c, src.ptr, src.len, out_buf, bound);

    if (actual == 0) {
        // fault: lisp::WRITE_FAILED
        return compression_runtime_raise(interp, "runtime/gzip-compress-failed", "gzip: compression failed");
    }

    return make_string(interp, out_buf[:actual]);
}

// ============================================================
// (gunzip s) -> decompressed string
// ============================================================

fn Value* prim_gunzip(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 1) return compression_type_raise(interp, "type/arity", "gunzip: expected 1 argument");
    // fault: lisp::EXPECTED_STRING
    if (!is_string(args[0])) return compression_type_raise(interp, "type/arg-mismatch", "gunzip: expected string argument");

    char[] src = args[0].str_chars[:args[0].str_len];
    if (src.len == 0) return make_string(interp, "");

    DeflateDecompressor* d = libdeflate_alloc_decompressor();
    // fault: lisp::READ_FAILED
    if (d == null) return compression_runtime_raise(interp, "runtime/out-of-memory", "gunzip: failed to allocate decompressor");
    defer libdeflate_free_decompressor(d);

    // Try progressively larger buffers
    usz try_size = src.len * 4;
    if (try_size < 256) try_size = 256;

    for (int attempt = 0; attempt < 8; attempt++) {
        char* out_buf = (char*)mem::malloc(try_size);
        if (out_buf == null) {
            // fault: lisp::READ_FAILED
            return compression_runtime_raise(interp, "runtime/out-of-memory", "gunzip: out of memory");
        }

        usz actual = 0;
        int status = libdeflate_gzip_decompress(d, src.ptr, src.len, out_buf, try_size, &actual);

        if (status == LIBDEFLATE_SUCCESS) {
            Value* result = make_string(interp, out_buf[:actual]);
            mem::free(out_buf);
            return result;
        }

        mem::free(out_buf);

        // Status 3 = INSUFFICIENT_SPACE, try bigger buffer
        if (status == 3) {
            try_size *= 4;
            continue;
        }

        // Bad data or other error
        // fault: lisp::READ_FAILED
        return compression_runtime_raise(interp, "runtime/gunzip-failed", "gunzip: decompression failed (bad data)");
    }

    // fault: lisp::READ_FAILED
    return compression_runtime_raise(interp, "runtime/gunzip-size-limit", "gunzip: decompressed data too large");
}
