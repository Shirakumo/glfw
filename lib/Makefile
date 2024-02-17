ROOT_DIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
OS = misc
ARCH = misc
EXT = so

UNAME_M := $(shell uname -m)
ifeq ($(UNAME_M),x86_64)
	ARCH = amd64
else ifneq ($(filter %86,$(UNAME_M)),)
	ARCH = i686
else ifeq ($(UNAME_M),aarch64)
	ARCH = arm64
else ifneq ($(filter %arm,$(UNAME_M)),)
	ARCH = arm
endif

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
	ifeq ($(ARCH),amd64)
		CFLAGS += -static-libgcc -include "$(ROOT_DIR)/glibcver/glibc_2.9_x64.h"
	else ifeq ($(ARCH),i686)
		CFLAGS += -static-libgcc -include "$(ROOT_DIR)/glibcver/glibc_2.9_x86.h"
	endif
	CMAKEFLAGS += -DGLFW_BUILD_WAYLAND=1 -DGLFW_BUILD_X11=1
	OS = lin
endif
ifeq ($(UNAME_S),Darwin)
	OS = mac
	EXT = dylib
endif
ifneq ($(filter MINGW%,$(UNAME_S)),)
	OS = win
	EXT = dll
	CMAKEFLAGS += -G "MSYS Makefiles"
endif
all:
	mkdir -p build ../static
	cmake -B build/ -DCMAKE_BUILD_TYPE=ReleaseWithDebug -DBUILD_SHARED_LIBS=ON -DGLFW_BUILD_EXAMPLES=OFF -DGLFW_BUILD_TESTS=OFF -DGLFW_BUILD_DOCS=OFF -DCMAKE_C_FLAGS="$(CFLAGS)" $(CMAKEFLAGS)
	$(MAKE) -C build
	cp build/src/*glfw.$(EXT) ../static/libglfw-$(OS)-$(ARCH).$(EXT)
