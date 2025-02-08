ROOT_DIR := $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
UNAME_M := $(shell uname -m)
UNAME_S := $(shell uname -s)

# Autodetect architecture
ifeq ($(ARCH),)
ifeq ($(UNAME_M),x86_64)
	ARCH = amd64
else ifneq ($(filter %86,$(UNAME_M)),)
	ARCH = i686
else ifeq ($(UNAME_M),aarch64)
	ARCH = arm64
else ifneq ($(filter %arm,$(UNAME_M)),)
	ARCH = arm
else
	ARCH = misc
endif
endif

# Autodetect OS
ifeq ($(OS),)
ifeq ($(UNAME_S),Linux)
	OS = lin
else ifeq ($(UNAME_S),Darwin)
	OS = mac
else ifneq ($(filter MINGW%,$(UNAME_S)),)
	OS = win
else
	OS = misc
endif
endif

EXT = so
CMAKE ?= cmake
PREFIX ?= build
BUILD_TYPE ?= ReleaseWithDebug
CMAKEFLAGS ?= -DCMAKE_BUILD_TYPE=$(BUILD_TYPE) -DBUILD_SHARED_LIBS=ON -DGLFW_BUILD_EXAMPLES=OFF -DGLFW_BUILD_TESTS=OFF -DGLFW_BUILD_DOCS=OFF

ifeq ($(OS),win)
	CMAKEFLAGS += -G "MSYS Makefiles"
	EXT = dll
else ifeq ($(OS),mac)
	EXT = dylib
else ifeq ($(OS),lin)
ifeq ($(ARCH),amd64)
	NATIVE_CFLAGS += -static-libgcc -include "$(ROOT_DIR)/glibcver/glibc_2.20_x64.h" -pthread
else ifeq ($(ARCH),i686)
	NATIVE_CFLAGS += -static-libgcc -include "$(ROOT_DIR)/glibcver/glibc_2.20_x86.h" -pthread
endif
	CMAKEFLAGS += -DGLFW_BUILD_WAYLAND=1 -DGLFW_BUILD_X11=1
endif
BUILDDIR ?= build-$(OS)-$(ARCH)

native:
	$(MAKE) build CFLAGS="${NATIVE_CFLAGS}"

win-amd64:
	$(MAKE) build OS=win ARCH=amd64 CMAKEFLAGS="${CMAKEFLAGS} -DCMAKE_TOOLCHAIN_FILE=CMake/x86_64-w64-mingw32.cmake"

win-i686:
	$(MAKE) build OS=win ARCH=i686 CMAKEFLAGS="${CMAKEFLAGS} -DCMAKE_TOOLCHAIN_FILE=CMake/i686-w64-mingw32.cmake"

build:
	mkdir -p $(BUILDDIR) ../static
	cmake -B $(BUILDDIR)/ -DCMAKE_C_FLAGS="$(CFLAGS)" $(CMAKEFLAGS)
	$(MAKE) -C $(BUILDDIR)
	@if [ "${OS}" = "win" ]; then \
		cp $(BUILDDIR)/src/glfw3.$(EXT) ../static/libglfw-$(OS)-$(ARCH).$(EXT); \
	else \
		cp $(BUILDDIR)/src/libglfw.$(EXT) ../static/libglfw-$(OS)-$(ARCH).$(EXT); \
	fi

clean:
	rm -r $(BUILDDIR)
