module lisp;

import std::core::mem;

// ============================================================
// (deflate s) -> raw deflate compressed string
// ============================================================

fn Value* prim_deflate(Value*[] args, Env* env, Interp* interp) {
    int level = LIBDEFLATE_DEFAULT_LEVEL;
    if (Value* err = parse_optional_compression_level(args, &level, interp)) return err;

    char[] src = args[0].str_chars[:args[0].str_len];

    DeflateCompressor* c = libdeflate_alloc_compressor(level);
    // fault: lisp::WRITE_FAILED
    if (c == null) return compression_runtime_raise(interp, "runtime/out-of-memory", "deflate: failed to allocate compressor");
    defer libdeflate_free_compressor(c);

    usz bound = libdeflate_deflate_compress_bound(c, src.len);
    char* out_buf = (char*)mem::malloc(bound);
    // fault: lisp::WRITE_FAILED
    if (out_buf == null) return compression_runtime_raise(interp, "runtime/out-of-memory", "deflate: out of memory");
    defer mem::free(out_buf);

    usz actual = libdeflate_deflate_compress(c, src.ptr, src.len, out_buf, bound);

    if (actual == 0) {
        // fault: lisp::WRITE_FAILED
        return compression_runtime_raise(interp, "runtime/deflate-compress-failed", "deflate: compression failed");
    }

    return make_string(interp, out_buf[:actual]);
}

// ============================================================
// (inflate s [original-size]) -> decompressed string
// ============================================================

fn Value* prim_inflate(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 1 || args.len > 2) return compression_type_raise(interp, "type/arity", "inflate: expected 1-2 arguments");
    // fault: lisp::EXPECTED_STRING
    if (!is_string(args[0])) return compression_type_raise(interp, "type/arg-mismatch", "inflate: expected string argument");

    char[] src = args[0].str_chars[:args[0].str_len];
    if (src.len == 0) return make_string(interp, "");

    DeflateDecompressor* d = libdeflate_alloc_decompressor();
    // fault: lisp::READ_FAILED
    if (d == null) return compression_runtime_raise(interp, "runtime/out-of-memory", "inflate: failed to allocate decompressor");
    defer libdeflate_free_decompressor(d);

    // If original size provided, use it directly
    usz try_size;
    if (args.len == 2) {
        if (!is_int(args[1])) {
            return compression_type_raise(interp, "type/arg-mismatch", "inflate: original-size must be an integer");
        }
        long requested = args[1].int_val;
        if (requested <= 0) {
            return compression_type_raise(interp, "type/arg-mismatch", "inflate: original-size must be > 0");
        }
        try_size = (usz)requested;
    } else {
        try_size = src.len * 4;
        if (try_size < 256) try_size = 256;
    }

    for (int attempt = 0; attempt < 8; attempt++) {
        char* out_buf = (char*)mem::malloc(try_size);
        if (out_buf == null) {
            // fault: lisp::READ_FAILED
            return compression_runtime_raise(interp, "runtime/out-of-memory", "inflate: out of memory");
        }

        usz actual = 0;
        int status = libdeflate_deflate_decompress(d, src.ptr, src.len, out_buf, try_size, &actual);

        if (status == LIBDEFLATE_SUCCESS) {
            Value* result = make_string(interp, out_buf[:actual]);
            mem::free(out_buf);
            return result;
        }

        mem::free(out_buf);

        if (status == 3) {  // INSUFFICIENT_SPACE
            try_size *= 4;
            continue;
        }

        // fault: lisp::READ_FAILED
        return compression_runtime_raise(interp, "runtime/inflate-failed", "inflate: decompression failed (bad data)");
    }

    // fault: lisp::READ_FAILED
    return compression_runtime_raise(interp, "runtime/inflate-size-limit", "inflate: decompressed data too large");
}

// ============================================================
// (zlib-compress s [level]) -> zlib compressed string
// ============================================================

fn Value* prim_zlib_compress(Value*[] args, Env* env, Interp* interp) {
    int level = LIBDEFLATE_DEFAULT_LEVEL;
    if (Value* err = parse_optional_compression_level(args, &level, interp)) return err;

    char[] src = args[0].str_chars[:args[0].str_len];

    DeflateCompressor* c = libdeflate_alloc_compressor(level);
    // fault: lisp::WRITE_FAILED
    if (c == null) return compression_runtime_raise(interp, "runtime/out-of-memory", "zlib-compress: failed to allocate compressor");
    defer libdeflate_free_compressor(c);

    usz bound = libdeflate_zlib_compress_bound(c, src.len);
    char* out_buf = (char*)mem::malloc(bound);
    // fault: lisp::WRITE_FAILED
    if (out_buf == null) return compression_runtime_raise(interp, "runtime/out-of-memory", "zlib-compress: out of memory");
    defer mem::free(out_buf);

    usz actual = libdeflate_zlib_compress(c, src.ptr, src.len, out_buf, bound);
    if (actual == 0) {
        // fault: lisp::WRITE_FAILED
        return compression_runtime_raise(interp, "runtime/zlib-compress-failed", "zlib-compress: compression failed");
    }

    return make_string(interp, out_buf[:actual]);
}

// ============================================================
// (zlib-decompress s [original-size]) -> decompressed string
// ============================================================

fn Value* prim_zlib_decompress(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 1 || args.len > 2) return compression_type_raise(interp, "type/arity", "zlib-decompress: expected 1-2 arguments");
    // fault: lisp::EXPECTED_STRING
    if (!is_string(args[0])) return compression_type_raise(interp, "type/arg-mismatch", "zlib-decompress: expected string argument");

    char[] src = args[0].str_chars[:args[0].str_len];
    if (src.len == 0) return make_string(interp, "");

    DeflateDecompressor* d = libdeflate_alloc_decompressor();
    // fault: lisp::READ_FAILED
    if (d == null) return compression_runtime_raise(interp, "runtime/out-of-memory", "zlib-decompress: failed to allocate decompressor");
    defer libdeflate_free_decompressor(d);

    usz try_size;
    if (args.len == 2) {
        // fault: lisp::EXPECTED_INT
        if (!is_int(args[1])) return compression_type_raise(interp, "type/arg-mismatch", "zlib-decompress: expected integer original-size");
        if (args[1].int_val <= 0) return compression_type_raise(interp, "type/arg-mismatch", "zlib-decompress: original-size must be > 0");
        try_size = (usz)args[1].int_val;
    } else {
        try_size = src.len * 4;
        if (try_size < 256) try_size = 256;
    }

    for (int attempt = 0; attempt < 8; attempt++) {
        char* out_buf = (char*)mem::malloc(try_size);
        if (out_buf == null) {
            // fault: lisp::READ_FAILED
            return compression_runtime_raise(interp, "runtime/out-of-memory", "zlib-decompress: out of memory");
        }

        usz actual = 0;
        int status = libdeflate_zlib_decompress(d, src.ptr, src.len, out_buf, try_size, &actual);

        if (status == LIBDEFLATE_SUCCESS) {
            Value* result = make_string(interp, out_buf[:actual]);
            mem::free(out_buf);
            return result;
        }

        mem::free(out_buf);

        if (status == 3) {  // INSUFFICIENT_SPACE
            try_size *= 4;
            continue;
        }

        // fault: lisp::READ_FAILED
        return compression_runtime_raise(interp, "runtime/zlib-decompress-failed", "zlib-decompress: decompression failed (bad data)");
    }

    // fault: lisp::READ_FAILED
    return compression_runtime_raise(interp, "runtime/zlib-size-limit", "zlib-decompress: decompressed data too large");
}
