module lisp;

import std::core::mem;
import std::io;

// ============================================================
// libdeflate extern declarations
// ============================================================

alias DeflateCompressor = void;
alias DeflateDecompressor = void;

extern fn DeflateCompressor* libdeflate_alloc_compressor(int compression_level) @extern("libdeflate_alloc_compressor");
extern fn void libdeflate_free_compressor(DeflateCompressor* c) @extern("libdeflate_free_compressor");
extern fn usz libdeflate_deflate_compress(DeflateCompressor* c, void* in_data, usz in_nbytes, void* out_buf, usz out_nbytes_avail) @extern("libdeflate_deflate_compress");
extern fn usz libdeflate_deflate_compress_bound(DeflateCompressor* c, usz in_nbytes) @extern("libdeflate_deflate_compress_bound");
extern fn usz libdeflate_gzip_compress(DeflateCompressor* c, void* in_data, usz in_nbytes, void* out_buf, usz out_nbytes_avail) @extern("libdeflate_gzip_compress");
extern fn usz libdeflate_gzip_compress_bound(DeflateCompressor* c, usz in_nbytes) @extern("libdeflate_gzip_compress_bound");
extern fn usz libdeflate_zlib_compress(DeflateCompressor* c, void* in_data, usz in_nbytes, void* out_buf, usz out_nbytes_avail) @extern("libdeflate_zlib_compress");
extern fn usz libdeflate_zlib_compress_bound(DeflateCompressor* c, usz in_nbytes) @extern("libdeflate_zlib_compress_bound");
extern fn uint libdeflate_adler32(uint adler, void* buffer, usz len) @extern("libdeflate_adler32");
extern fn uint libdeflate_crc32(uint crc, void* buffer, usz len) @extern("libdeflate_crc32");

extern fn DeflateDecompressor* libdeflate_alloc_decompressor() @extern("libdeflate_alloc_decompressor");
extern fn void libdeflate_free_decompressor(DeflateDecompressor* d) @extern("libdeflate_free_decompressor");
extern fn int libdeflate_deflate_decompress(DeflateDecompressor* d, void* in_data, usz in_nbytes, void* out_buf, usz out_nbytes_avail, usz* actual_out_nbytes) @extern("libdeflate_deflate_decompress");
extern fn int libdeflate_gzip_decompress(DeflateDecompressor* d, void* in_data, usz in_nbytes, void* out_buf, usz out_nbytes_avail, usz* actual_out_nbytes) @extern("libdeflate_gzip_decompress");
extern fn int libdeflate_zlib_decompress(DeflateDecompressor* d, void* in_data, usz in_nbytes, void* out_buf, usz out_nbytes_avail, usz* actual_out_nbytes) @extern("libdeflate_zlib_decompress");

const int LIBDEFLATE_SUCCESS = 0;
const int LIBDEFLATE_DEFAULT_LEVEL = 6;
const int LIBDEFLATE_MIN_LEVEL = 0;
const int LIBDEFLATE_MAX_LEVEL = 12;

fn Value* compression_type_raise(Interp* interp, char[] code, char[] message) @inline {
    return raise_error_with_payload_names(interp, "type", code, message, null);
}

fn Value* compression_runtime_raise(Interp* interp, char[] code, char[] message) @inline {
    return raise_error_with_payload_names(interp, "runtime", code, message, null);
}

fn Value* parse_optional_compression_level(Value*[] args, int* level_out, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 1 || args.len > 2) return compression_type_raise(interp, "type/arity", "compression: expected 1-2 arguments");
    // fault: lisp::EXPECTED_STRING
    if (!is_string(args[0])) return compression_type_raise(interp, "type/arg-mismatch", "compression: expected string argument");

    int level = LIBDEFLATE_DEFAULT_LEVEL;
    if (args.len == 2) {
        // fault: lisp::EXPECTED_INT
        if (!is_int(args[1])) return compression_type_raise(interp, "type/arg-mismatch", "compression: level must be an integer");
        long candidate = args[1].int_val;
        // fault: lisp::INVALID_ARGUMENT
        if (candidate < LIBDEFLATE_MIN_LEVEL || candidate > LIBDEFLATE_MAX_LEVEL) {
            return compression_type_raise(interp, "type/arg-mismatch", "compression: level must be in range 0..12");
        }
        level = (int)candidate;
    }

    *level_out = level;
    return null;
}

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

// ============================================================
// (adler32 s) -> checksum int
// ============================================================

fn Value* prim_adler32(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len != 1) return compression_type_raise(interp, "type/arity", "adler32: expected 1 argument");
    // fault: lisp::EXPECTED_STRING
    if (!is_string(args[0])) return compression_type_raise(interp, "type/arg-mismatch", "adler32: expected string argument");

    char[] src = args[0].str_chars[:args[0].str_len];
    uint checksum = libdeflate_adler32(1u, src.ptr, src.len);
    return make_int(interp, (long)checksum);
}

// ============================================================
// (crc32 s) -> checksum int
// ============================================================

fn Value* prim_crc32(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len != 1) return compression_type_raise(interp, "type/arity", "crc32: expected 1 argument");
    // fault: lisp::EXPECTED_STRING
    if (!is_string(args[0])) return compression_type_raise(interp, "type/arg-mismatch", "crc32: expected string argument");

    char[] src = args[0].str_chars[:args[0].str_len];
    uint checksum = libdeflate_crc32(0u, src.ptr, src.len);
    return make_int(interp, (long)checksum);
}
