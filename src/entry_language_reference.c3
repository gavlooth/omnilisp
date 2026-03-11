module main;

import std::io;
import std::core::mem;

alias DeflateDecompressor = void;

extern fn DeflateDecompressor* libdeflate_alloc_decompressor() @extern("libdeflate_alloc_decompressor");
extern fn void libdeflate_free_decompressor(DeflateDecompressor* d) @extern("libdeflate_free_decompressor");
extern fn int libdeflate_gzip_decompress(DeflateDecompressor* d, void* in_data, usz in_nbytes, void* out_buf, usz out_nbytes_avail, usz* actual_out_nbytes) @extern("libdeflate_gzip_decompress");

const int LIBDEFLATE_SUCCESS = 0;
const usz OMNI_LANGUAGE_REF_TEXT_LEN = 907;
const char[] OMNI_LANGUAGE_REF_GZIP = x"1f8b0808f5afb16902036f6d6e695f6c616e67756167655f7265662e747874007d924b4fdb401485f7fe1547de60ab4978f4b1885aa4280444150805ba42088dedeb70cb3cac99314a54f5bff74e1ccaaed93873eeebdc6f66652c63a9ecba576bc28f9eeb17dc524b9e6c4dd9b7fffdb26cee3ce1ca35a4b331169bce5308ececd879261ba9c192433791d85df45c47b4bdada32440798e5b14d6814da7b9e6884ef9c84a4375495029ad4ca5e74a872d5e95ee294c61598fd08a442936775ad3aea38434873892ce5e6d476864dc646ff0dc791324bba1962d8da049f2b81da1d2ae7e91b33255a346302ad6cf92f7ac6ca3098708bcb6e2e810b297d3af8482da56e68532cb961cc98b0fc99725e31407c5314ef0b11461963c4cf1b0131e4538133753fc3eb0ca107227c8731c6c49799c1c9d7cf993658b8d120e14b26230890d8e8fca7fa7826d0d5ba2f8008be35202b2030a858fa8f069a72b5449e716c5a9547f2e9157bcce9107a3b4cec5f1f91e7e80ec279642b7dff7aad79165ba401b347040e8bbce79b9c3c47966a457effa808a421c4091f411b89e7e099194962d063892bf074786639a8601dbe49d2ddb10c556d89f7d1005cd56f8708d50bb8ee07c433e95bcc1976f6fd2d488f84c6f3382dc83cc9647d8277e635c9ae41bd21db4497fc33483101ce4dcec1227fb3b902c2173e35d5a02f7ce69b66b6992c2188fd9cac3fc9a6eedf45dac58f03d7443cdb861ff98c60b8a33578b411b776f37bd375787c3e5ecfae2e7ec62f17477b3984f4cf3a6dfdcaebe2fe6f74ff7abd5f2f2fa2245fe02f37d1d6a8b030000";

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
