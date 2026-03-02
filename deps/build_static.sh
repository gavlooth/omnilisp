#!/usr/bin/env bash
# Build static libraries for Omni Lisp
# Creates .a files in deps/lib/
set -eu

DEPS_DIR="$(cd "$(dirname "$0")" && pwd)"
SRC_DIR="$DEPS_DIR/src"
LIB_DIR="$DEPS_DIR/lib"
BUILD_DIR="$DEPS_DIR/build"

mkdir -p "$SRC_DIR" "$LIB_DIR" "$BUILD_DIR"

NPROC=$(nproc 2>/dev/null || echo 4)

# ============================================================
# utf8proc
# ============================================================
build_utf8proc() {
    echo "=== Building utf8proc ==="
    cd "$SRC_DIR"
    if [ ! -d utf8proc ]; then
        git clone --depth 1 https://github.com/JuliaStrings/utf8proc.git
    fi
    cd utf8proc
    make clean 2>/dev/null || true
    make -j"$NPROC" libutf8proc.a
    cp libutf8proc.a "$LIB_DIR/"
    echo "  -> $LIB_DIR/libutf8proc.a"
}

# ============================================================
# libdeflate
# ============================================================
build_libdeflate() {
    echo "=== Building libdeflate ==="
    cd "$SRC_DIR"
    if [ ! -d libdeflate ]; then
        git clone --depth 1 https://github.com/ebiggers/libdeflate.git
    fi
    cd libdeflate
    rm -rf build_static
    cmake -B build_static -DCMAKE_BUILD_TYPE=Release \
        -DLIBDEFLATE_BUILD_SHARED_LIB=OFF \
        -DLIBDEFLATE_BUILD_GZIP=OFF
    cmake --build build_static -j"$NPROC"
    cp build_static/libdeflate.a "$LIB_DIR/"
    echo "  -> $LIB_DIR/libdeflate.a"
}

# ============================================================
# yyjson
# ============================================================
build_yyjson() {
    echo "=== Building yyjson ==="
    cd "$SRC_DIR"
    if [ ! -d yyjson ]; then
        git clone --depth 1 https://github.com/ibireme/yyjson.git
    fi
    cd yyjson
    rm -rf build_static
    cmake -B build_static -DCMAKE_BUILD_TYPE=Release \
        -DBUILD_SHARED_LIBS=OFF \
        -DYYJSON_BUILD_TESTS=OFF
    cmake --build build_static -j"$NPROC"
    cp build_static/libyyjson.a "$LIB_DIR/"
    echo "  -> $LIB_DIR/libyyjson.a"
}

# ============================================================
# libuv
# ============================================================
build_libuv() {
    echo "=== Building libuv ==="
    cd "$SRC_DIR"
    if [ ! -d libuv ]; then
        git clone --depth 1 https://github.com/libuv/libuv.git
    fi
    cd libuv
    rm -rf build_static
    cmake -B build_static -DCMAKE_BUILD_TYPE=Release \
        -DBUILD_TESTING=OFF \
        -DLIBUV_BUILD_SHARED=OFF
    cmake --build build_static -j"$NPROC"
    cp build_static/libuv.a "$LIB_DIR/libuv.a"
    echo "  -> $LIB_DIR/libuv.a"
}

# ============================================================
# BearSSL
# ============================================================
build_bearssl() {
    echo "=== Building BearSSL ==="
    cd "$SRC_DIR"
    if [ ! -d BearSSL ]; then
        git clone --depth 1 https://www.bearssl.org/git/BearSSL
    fi
    cd BearSSL
    make clean 2>/dev/null || true
    make -j"$NPROC" lib
    cp build/libbearssl.a "$LIB_DIR/"
    echo "  -> $LIB_DIR/libbearssl.a"
}

# ============================================================
# Build all
# ============================================================
echo "Building static libraries in $LIB_DIR"
echo ""

build_utf8proc
build_libdeflate
build_yyjson
build_libuv
build_bearssl

echo ""
echo "=== All static libraries built ==="
ls -lh "$LIB_DIR"/*.a
echo ""
echo "Add to project.json linker-search-paths: \"$LIB_DIR\""
