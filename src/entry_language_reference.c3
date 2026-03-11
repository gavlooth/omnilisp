module main;

import std::io;
import std::core::mem;

alias DeflateDecompressor = void;

extern fn DeflateDecompressor* libdeflate_alloc_decompressor() @extern("libdeflate_alloc_decompressor");
extern fn void libdeflate_free_decompressor(DeflateDecompressor* d) @extern("libdeflate_free_decompressor");
extern fn int libdeflate_gzip_decompress(DeflateDecompressor* d, void* in_data, usz in_nbytes, void* out_buf, usz out_nbytes_avail, usz* actual_out_nbytes) @extern("libdeflate_gzip_decompress");

const int LIBDEFLATE_SUCCESS = 0;

fn int print_language_reference() {
    DeflateDecompressor* d = libdeflate_alloc_decompressor();
    if (d == null) {
        io::printn("Error: failed to allocate decompressor for embedded language reference");
        return 1;
    }
    defer libdeflate_free_decompressor(d);

    char* out_buf = (char*)mem::malloc(OMNI_LANGUAGE_REF_TEXT_LEN + 1);
    if (out_buf == null) {
        io::printn("Error: out of memory while decoding embedded language reference");
        return 1;
    }
    defer mem::free(out_buf);

    usz actual = 0;
    int status = libdeflate_gzip_decompress(
        d,
        OMNI_LANGUAGE_REF_GZIP.ptr,
        OMNI_LANGUAGE_REF_GZIP.len,
        out_buf,
        OMNI_LANGUAGE_REF_TEXT_LEN,
        &actual
    );
    if (status != LIBDEFLATE_SUCCESS) {
        io::printn("Error: failed to decode embedded language reference");
        return 1;
    }

    out_buf[actual] = 0;
    io::print((String)out_buf[:actual]);
    return 0;
}
