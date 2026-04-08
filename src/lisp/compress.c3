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
