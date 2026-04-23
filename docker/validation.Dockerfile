FROM ubuntu:24.04

ARG DEBIAN_FRONTEND=noninteractive
ARG C3_VERSION=v0.7.11
ARG C3_TARBALL_URL=https://github.com/c3lang/c3c/releases/download/v0.7.11/c3-linux.tar.gz
ARG C3_TARBALL_SHA256=e41e0295fa55b3ad0a55bfc3bf9e9882af05a01bf7117a3c31015b3308e38bbd
ARG C3_SOURCE_TARBALL_URL=https://github.com/c3lang/c3c/archive/refs/tags/v0.7.11.tar.gz
ARG C3_SOURCE_TARBALL_SHA256=2d5fd6b0757549062af5162516b4715bf9af693de683cc9d8b1e81306432278b
ARG LIGHTNING_VERSION=2.2.2
ARG REPLXX_REF=release-0.0.4
ARG TARGETARCH

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
    libboost-dev \
    autoconf \
    automake \
    libtool \
    xz-utils \
  && rm -rf /var/lib/apt/lists/*

WORKDIR /tmp

RUN set -eux; \
  arch="${TARGETARCH:-$(uname -m)}"; \
  case "$arch" in \
    amd64|x86_64) \
      curl -L --fail --silent --show-error "${C3_TARBALL_URL}" -o c3-linux.tar.gz; \
      echo "${C3_TARBALL_SHA256}  c3-linux.tar.gz" | sha256sum -c -; \
      tar -xzf c3-linux.tar.gz; \
      mv c3 /opt/c3; \
      ln -sf /opt/c3/c3c /usr/local/bin/c3c; \
      rm -f c3-linux.tar.gz; \
      ;; \
    arm64|aarch64) \
      apt-get update; \
      apt-get install -y --no-install-recommends \
        clang-19 \
        zlib1g-dev \
        libllvm19 \
        llvm-19 \
        llvm-19-dev \
        llvm-19-runtime \
        liblld-19 \
        liblld-19-dev \
        libpolly-19-dev; \
      rm -rf /var/lib/apt/lists/*; \
      curl -L --fail --silent --show-error "${C3_SOURCE_TARBALL_URL}" -o c3-source.tar.gz; \
      echo "${C3_SOURCE_TARBALL_SHA256}  c3-source.tar.gz" | sha256sum -c -; \
      mkdir c3-source; \
      tar -xzf c3-source.tar.gz -C c3-source --strip-components=1; \
      cmake -S c3-source -B c3-source/build -DCMAKE_BUILD_TYPE=Release; \
      cmake --build c3-source/build -j"$(nproc)"; \
      cmake --install c3-source/build --prefix /opt/c3; \
      ln -sf /opt/c3/bin/c3c /opt/c3/c3c; \
      ln -sf /opt/c3/bin/c3c /usr/local/bin/c3c; \
      rm -rf c3-source c3-source.tar.gz; \
      ;; \
    *) \
      echo "unsupported validation C3 architecture: ${arch}" >&2; \
      exit 2; \
      ;; \
  esac; \
  c3c -V

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
