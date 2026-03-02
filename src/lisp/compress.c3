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

extern fn DeflateDecompressor* libdeflate_alloc_decompressor() @extern("libdeflate_alloc_decompressor");
extern fn void libdeflate_free_decompressor(DeflateDecompressor* d) @extern("libdeflate_free_decompressor");
extern fn int libdeflate_deflate_decompress(DeflateDecompressor* d, void* in_data, usz in_nbytes, void* out_buf, usz out_nbytes_avail, usz* actual_out_nbytes) @extern("libdeflate_deflate_decompress");
extern fn int libdeflate_gzip_decompress(DeflateDecompressor* d, void* in_data, usz in_nbytes, void* out_buf, usz out_nbytes_avail, usz* actual_out_nbytes) @extern("libdeflate_gzip_decompress");

const int LIBDEFLATE_SUCCESS = 0;

// ============================================================
// (gzip s) -> compressed string
// ============================================================

fn Value* prim_gzip(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "gzip: expected 1 argument");
    if (!is_string(args[0])) return raise_error(interp, "gzip: expected string argument");

    char[] src = args[0].str_val.chars[:args[0].str_val.len];

    DeflateCompressor* c = libdeflate_alloc_compressor(6);  // default compression level
    if (c == null) return raise_error(interp, "gzip: failed to allocate compressor");

    usz bound = libdeflate_gzip_compress_bound(c, src.len);
    char* out_buf = (char*)mem::malloc(bound);
    if (out_buf == null) {
        libdeflate_free_compressor(c);
        return raise_error(interp, "gzip: out of memory");
    }

    usz actual = libdeflate_gzip_compress(c, src.ptr, src.len, out_buf, bound);
    libdeflate_free_compressor(c);

    if (actual == 0) {
        mem::free(out_buf);
        return raise_error(interp, "gzip: compression failed");
    }

    Value* result = make_string(interp, out_buf[:actual]);
    mem::free(out_buf);
    return result;
}

// ============================================================
// (gunzip s) -> decompressed string
// ============================================================

fn Value* prim_gunzip(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "gunzip: expected 1 argument");
    if (!is_string(args[0])) return raise_error(interp, "gunzip: expected string argument");

    char[] src = args[0].str_val.chars[:args[0].str_val.len];
    if (src.len == 0) return make_string(interp, "");

    DeflateDecompressor* d = libdeflate_alloc_decompressor();
    if (d == null) return raise_error(interp, "gunzip: failed to allocate decompressor");

    // Try progressively larger buffers
    usz try_size = src.len * 4;
    if (try_size < 256) try_size = 256;

    for (int attempt = 0; attempt < 8; attempt++) {
        char* out_buf = (char*)mem::malloc(try_size);
        if (out_buf == null) {
            libdeflate_free_decompressor(d);
            return raise_error(interp, "gunzip: out of memory");
        }

        usz actual = 0;
        int status = libdeflate_gzip_decompress(d, src.ptr, src.len, out_buf, try_size, &actual);

        if (status == LIBDEFLATE_SUCCESS) {
            Value* result = make_string(interp, out_buf[:actual]);
            mem::free(out_buf);
            libdeflate_free_decompressor(d);
            return result;
        }

        mem::free(out_buf);

        // Status 3 = INSUFFICIENT_SPACE, try bigger buffer
        if (status == 3) {
            try_size *= 4;
            continue;
        }

        // Bad data or other error
        libdeflate_free_decompressor(d);
        return raise_error(interp, "gunzip: decompression failed (bad data)");
    }

    libdeflate_free_decompressor(d);
    return raise_error(interp, "gunzip: decompressed data too large");
}

// ============================================================
// (deflate s) -> raw deflate compressed string
// ============================================================

fn Value* prim_deflate(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "deflate: expected 1 argument");
    if (!is_string(args[0])) return raise_error(interp, "deflate: expected string argument");

    char[] src = args[0].str_val.chars[:args[0].str_val.len];

    DeflateCompressor* c = libdeflate_alloc_compressor(6);
    if (c == null) return raise_error(interp, "deflate: failed to allocate compressor");

    usz bound = libdeflate_deflate_compress_bound(c, src.len);
    char* out_buf = (char*)mem::malloc(bound);
    if (out_buf == null) {
        libdeflate_free_compressor(c);
        return raise_error(interp, "deflate: out of memory");
    }

    usz actual = libdeflate_deflate_compress(c, src.ptr, src.len, out_buf, bound);
    libdeflate_free_compressor(c);

    if (actual == 0) {
        mem::free(out_buf);
        return raise_error(interp, "deflate: compression failed");
    }

    Value* result = make_string(interp, out_buf[:actual]);
    mem::free(out_buf);
    return result;
}

// ============================================================
// (inflate s [original-size]) -> decompressed string
// ============================================================

fn Value* prim_inflate(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "inflate: expected 1-2 arguments");
    if (!is_string(args[0])) return raise_error(interp, "inflate: expected string argument");

    char[] src = args[0].str_val.chars[:args[0].str_val.len];
    if (src.len == 0) return make_string(interp, "");

    DeflateDecompressor* d = libdeflate_alloc_decompressor();
    if (d == null) return raise_error(interp, "inflate: failed to allocate decompressor");

    // If original size provided, use it directly
    usz try_size;
    if (args.len >= 2 && is_int(args[1])) {
        try_size = (usz)args[1].int_val;
    } else {
        try_size = src.len * 4;
        if (try_size < 256) try_size = 256;
    }

    for (int attempt = 0; attempt < 8; attempt++) {
        char* out_buf = (char*)mem::malloc(try_size);
        if (out_buf == null) {
            libdeflate_free_decompressor(d);
            return raise_error(interp, "inflate: out of memory");
        }

        usz actual = 0;
        int status = libdeflate_deflate_decompress(d, src.ptr, src.len, out_buf, try_size, &actual);

        if (status == LIBDEFLATE_SUCCESS) {
            Value* result = make_string(interp, out_buf[:actual]);
            mem::free(out_buf);
            libdeflate_free_decompressor(d);
            return result;
        }

        mem::free(out_buf);

        if (status == 3) {  // INSUFFICIENT_SPACE
            try_size *= 4;
            continue;
        }

        libdeflate_free_decompressor(d);
        return raise_error(interp, "inflate: decompression failed (bad data)");
    }

    libdeflate_free_decompressor(d);
    return raise_error(interp, "inflate: decompressed data too large");
}
