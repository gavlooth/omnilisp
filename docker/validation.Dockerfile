FROM ubuntu:24.04

ARG DEBIAN_FRONTEND=noninteractive
ARG C3_VERSION=v0.7.10
ARG C3_TARBALL_URL=https://github.com/c3lang/c3c/releases/download/v0.7.10/c3-linux.tar.gz
ARG C3_TARBALL_SHA256=9f5ca5d13c74040d1dcd21c1749d2a27e06715f02ed24b019c3b0f836c90bcd9
ARG LIGHTNING_VERSION=2.2.2
ARG REPLXX_REF=release-0.0.4

RUN apt-get update && apt-get install -y --no-install-recommends \
    bash \
    ca-certificates \
    curl \
    git \
    make \
    cmake \
    ninja-build \
    pkg-config \
    gcc \
    g++ \
    libc6-dev \
    libffi-dev \
    autoconf \
    automake \
    libtool \
    xz-utils \
  && rm -rf /var/lib/apt/lists/*

WORKDIR /tmp

RUN curl -L --fail --silent --show-error "${C3_TARBALL_URL}" -o c3-linux.tar.gz \
  && echo "${C3_TARBALL_SHA256}  c3-linux.tar.gz" | sha256sum -c - \
  && tar -xzf c3-linux.tar.gz \
  && mv c3 /opt/c3 \
  && ln -sf /opt/c3/c3c /usr/local/bin/c3c \
  && rm -f c3-linux.tar.gz

RUN curl -L --fail --silent --show-error \
    "https://ftp.gnu.org/gnu/lightning/lightning-${LIGHTNING_VERSION}.tar.gz" \
    -o lightning.tar.gz \
  && tar -xzf lightning.tar.gz \
  && cd "lightning-${LIGHTNING_VERSION}" \
  && ./configure --prefix=/usr/local \
  && make -j"$(nproc)" \
  && make install \
  && ldconfig \
  && cd /tmp \
  && rm -rf "lightning-${LIGHTNING_VERSION}" lightning.tar.gz

RUN git clone --depth 1 --branch "${REPLXX_REF}" https://github.com/AmokHuginnsson/replxx.git \
  && cmake -S replxx -B replxx/build \
       -DCMAKE_BUILD_TYPE=Release \
       -DBUILD_SHARED_LIBS=ON \
       -DREPLXX_BUILD_EXAMPLES=OFF \
       -DREPLXX_BUILD_TESTS=OFF \
  && cmake --build replxx/build -j"$(nproc)" \
  && cmake --install replxx/build \
  && ldconfig \
  && rm -rf replxx

WORKDIR /workspace

ENTRYPOINT ["/workspace/scripts/container_exec.sh"]
