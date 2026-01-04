# Makefile for OmniLisp FFI C wrappers
#
# Builds:
#   - libomnitorch.so : LibTorch C wrapper
#
# Usage:
#   make                  # Build all (requires LIBTORCH_PATH)
#   make CUDA=1           # Build with CUDA support
#   make install          # Install to /usr/local/lib
#   make clean            # Remove build artifacts
#
# Environment variables:
#   LIBTORCH_PATH - Path to LibTorch installation
#                   Default: /usr/local/lib/libtorch
#
# Example:
#   LIBTORCH_PATH=/opt/libtorch make

# Compiler settings
CXX := g++
CXXFLAGS := -std=c++17 -fPIC -O2 -Wall -Wextra

# LibTorch configuration
LIBTORCH_PATH ?= /usr/local/lib/libtorch

LIBTORCH_INCLUDES := \
    -I$(LIBTORCH_PATH)/include \
    -I$(LIBTORCH_PATH)/include/torch/csrc/api/include

LIBTORCH_LIBS := \
    -L$(LIBTORCH_PATH)/lib \
    -ltorch \
    -ltorch_cpu \
    -lc10 \
    -Wl,-rpath,$(LIBTORCH_PATH)/lib

# Add CUDA support if requested
ifdef CUDA
    CXXFLAGS += -DWITH_CUDA
    LIBTORCH_LIBS += -ltorch_cuda -lc10_cuda
endif

# Output
TARGET := libomnitorch.so
SOURCES := libtorch_c_api.cpp
OBJECTS := $(SOURCES:.cpp=.o)
HEADERS := libtorch_c_api.h

# Installation paths
PREFIX ?= /usr/local
LIBDIR := $(PREFIX)/lib
INCLUDEDIR := $(PREFIX)/include/omni

# Targets
.PHONY: all clean install uninstall check-libtorch

all: check-libtorch $(TARGET)

$(TARGET): $(OBJECTS)
	$(CXX) -shared -o $@ $^ $(LIBTORCH_LIBS)

%.o: %.cpp $(HEADERS)
	$(CXX) $(CXXFLAGS) $(LIBTORCH_INCLUDES) -c $< -o $@

check-libtorch:
	@if [ ! -d "$(LIBTORCH_PATH)" ]; then \
		echo "Error: LibTorch not found at $(LIBTORCH_PATH)"; \
		echo "Set LIBTORCH_PATH to your LibTorch installation directory."; \
		echo ""; \
		echo "Download LibTorch:"; \
		echo "  https://pytorch.org/get-started/locally/"; \
		echo ""; \
		echo "Example:"; \
		echo "  wget https://download.pytorch.org/libtorch/cpu/libtorch-cxx11-abi-shared-with-deps-2.1.0%2Bcpu.zip"; \
		echo "  unzip libtorch-*.zip -d /opt"; \
		echo "  LIBTORCH_PATH=/opt/libtorch make"; \
		exit 1; \
	fi
	@if [ ! -f "$(LIBTORCH_PATH)/lib/libtorch.so" ] && [ ! -f "$(LIBTORCH_PATH)/lib/libtorch.dylib" ]; then \
		echo "Error: libtorch library not found in $(LIBTORCH_PATH)/lib"; \
		exit 1; \
	fi
	@echo "Using LibTorch at $(LIBTORCH_PATH)"

install: $(TARGET)
	install -d $(LIBDIR)
	install -d $(INCLUDEDIR)
	install -m 644 $(TARGET) $(LIBDIR)/
	install -m 644 $(HEADERS) $(INCLUDEDIR)/
	ldconfig 2>/dev/null || true
	@echo "Installed $(TARGET) to $(LIBDIR)"
	@echo "Installed headers to $(INCLUDEDIR)"

uninstall:
	rm -f $(LIBDIR)/$(TARGET)
	rm -f $(INCLUDEDIR)/libtorch_c_api.h
	@echo "Uninstalled libomnitorch"

clean:
	rm -f $(OBJECTS) $(TARGET)

# Test target (requires a test file)
test: $(TARGET)
	@echo "TODO: Add test runner"

# Debug build
debug: CXXFLAGS += -g -DDEBUG
debug: all

# Show configuration
info:
	@echo "Configuration:"
	@echo "  CXX          = $(CXX)"
	@echo "  CXXFLAGS     = $(CXXFLAGS)"
	@echo "  LIBTORCH_PATH = $(LIBTORCH_PATH)"
	@echo "  TARGET       = $(TARGET)"
	@echo "  CUDA         = $(if $(CUDA),enabled,disabled)"
