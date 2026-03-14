# ac-native Makefile
# Builds the native AC piece runner, optionally with QuickJS-ng

SRCDIR   := src
BUILDDIR := build
QJSDIR   := $(BUILDDIR)/quickjs

# Use musl for static linking if available, else gcc
CC       ?= gcc
GIT_HASH   := $(shell git rev-parse --short HEAD 2>/dev/null || echo "unknown")
BUILD_TS   ?= $(shell date -u '+%Y-%m-%dT%H:%M')
BUILD_NAME := $(shell scripts/build-name.sh --bump 2>/dev/null || echo "local-build")
QUIRCDIR   := lib/quirc/lib
CFLAGS     := -O2 -Wall -Wextra -std=gnu11 -I$(QJSDIR) -I$(QUIRCDIR) -DAC_GIT_HASH=\"$(GIT_HASH)\" -DAC_BUILD_TS=\"$(BUILD_TS)\" -DAC_BUILD_NAME=\"$(BUILD_NAME)\"
LDFLAGS  := -Wl,--as-needed -lm -lpthread -lasound -lutil

# Static build: only when musl-gcc is available (musl has bundled static libc)
# On Fedora/standard gcc, dynamic linking is used and .so files are bundled in initramfs
ifdef STATIC
  ifeq ($(shell command -v musl-gcc 2>/dev/null),)
    # musl-gcc not available — skip -static (dynamic linking, bundle .so in initramfs)
    STATIC :=
  else
    LDFLAGS += -static
  endif
endif

# DRM headers — use pkg-config if available
DRM_CFLAGS := $(shell pkg-config --cflags libdrm 2>/dev/null || echo "-I/usr/include/libdrm -I/usr/include/drm")
DRM_LIBS   := $(shell pkg-config --libs libdrm 2>/dev/null || echo "-ldrm")
ALSA_CFLAGS := $(shell pkg-config --cflags alsa 2>/dev/null)
ALSA_LIBS   := $(shell pkg-config --libs alsa 2>/dev/null || echo "-lasound")
FLITE_LIBS  := -lflite -lflite_cmulex -lflite_usenglish -lflite_cmu_us_slt -lflite_cmu_us_kal

CFLAGS  += $(DRM_CFLAGS) $(ALSA_CFLAGS)
LDFLAGS += $(DRM_LIBS) $(ALSA_LIBS) $(FLITE_LIBS)

# SDL2 GPU-accelerated display (optional: make USE_SDL=1)
ifdef USE_SDL
  SDL_CFLAGS := $(shell pkg-config --cflags sdl2 2>/dev/null)
  SDL_LIBS   := $(shell pkg-config --libs sdl2 2>/dev/null || echo "-lSDL2")
  CFLAGS  += -DUSE_SDL $(SDL_CFLAGS)
  LDFLAGS += $(SDL_LIBS)
endif

# Source files
SSL_LIBS := $(shell pkg-config --libs openssl 2>/dev/null || echo "-lssl -lcrypto")
LDFLAGS  += $(SSL_LIBS)

SRCS := $(SRCDIR)/ac-native.c \
        $(SRCDIR)/drm-display.c \
        $(SRCDIR)/framebuffer.c \
        $(SRCDIR)/graph.c \
        $(SRCDIR)/font.c \
        $(SRCDIR)/color.c \
        $(SRCDIR)/input.c \
        $(SRCDIR)/audio.c \
        $(SRCDIR)/wifi.c \
        $(SRCDIR)/tts.c \
        $(SRCDIR)/ws-client.c \
        $(SRCDIR)/udp-client.c \
        $(SRCDIR)/camera.c \
        $(SRCDIR)/pty.c \
        $(SRCDIR)/js-bindings.c

OBJS := $(patsubst $(SRCDIR)/%.c,$(BUILDDIR)/%.o,$(SRCS))

# QuickJS-ng object files (compiled from source)
QJS_SRCS := $(QJSDIR)/quickjs.c $(QJSDIR)/libunicode.c $(QJSDIR)/libregexp.c $(QJSDIR)/cutils.c $(QJSDIR)/libbf.c
QJS_OBJS := $(patsubst $(QJSDIR)/%.c,$(BUILDDIR)/qjs-%.o,$(QJS_SRCS))

# Quirc QR decoder (compiled from vendored source)
QUIRC_SRCS := $(QUIRCDIR)/quirc.c $(QUIRCDIR)/decode.c $(QUIRCDIR)/identify.c $(QUIRCDIR)/version_db.c
QUIRC_OBJS := $(patsubst $(QUIRCDIR)/%.c,$(BUILDDIR)/quirc-%.o,$(QUIRC_SRCS))

TARGET := $(BUILDDIR)/ac-native

.PHONY: all clean quickjs test

all: quickjs $(TARGET)

$(TARGET): $(OBJS) $(QJS_OBJS) $(QUIRC_OBJS)
	$(CC) -o $@ $^ $(LDFLAGS)
	@echo "Built: $@ ($(shell wc -c < $@ | tr -d ' ') bytes)"

# All .c files depend on all project headers (simple but correct)
HEADERS := $(wildcard $(SRCDIR)/*.h)

# Track CFLAGS changes — force full rebuild when git hash, build name, or any flag changes.
# Make only watches source timestamps, not compiler flags, so without this the binary
# retains stale AC_BUILD_NAME / AC_GIT_HASH after new commits with no C source changes.
CFLAGS_FILE := $(BUILDDIR)/.cflags
CFLAGS_SIG  := $(shell echo '$(CFLAGS)' | md5sum | cut -d' ' -f1)
CFLAGS_PREV := $(shell cat $(CFLAGS_FILE) 2>/dev/null)
ifneq ($(CFLAGS_SIG),$(CFLAGS_PREV))
  $(shell mkdir -p $(BUILDDIR) && echo '$(CFLAGS_SIG)' > $(CFLAGS_FILE))
  $(shell rm -f $(BUILDDIR)/*.o)
endif

$(BUILDDIR)/%.o: $(SRCDIR)/%.c $(HEADERS) | $(BUILDDIR)
	$(CC) $(CFLAGS) -c -o $@ $<

$(BUILDDIR)/qjs-%.o: $(QJSDIR)/%.c | $(BUILDDIR)
	$(CC) $(CFLAGS) -DCONFIG_VERSION=\"0.8.0\" -c -o $@ $<

$(BUILDDIR)/quirc-%.o: $(QUIRCDIR)/%.c | $(BUILDDIR)
	$(CC) -O2 -Wall -std=gnu11 -I$(QUIRCDIR) -c -o $@ $<

$(BUILDDIR):
	mkdir -p $(BUILDDIR)

# Download and prepare QuickJS-ng
quickjs: $(QJSDIR)/quickjs.h

QJS_VERSION := 0.8.0
QJS_URL     := https://github.com/nicoffee/nicoffee-nicoffee/archive/refs/tags/v$(QJS_VERSION).tar.gz

$(QJSDIR)/quickjs.h:
	@echo "Fetching QuickJS..."
	mkdir -p $(QJSDIR)
	curl -sL "https://bellard.org/quickjs/quickjs-2024-01-13.tar.xz" -o $(BUILDDIR)/qjs.tar.xz && \
	  cd $(BUILDDIR) && tar xf qjs.tar.xz && \
	  cp quickjs-2024-01-13/*.c quickjs-2024-01-13/*.h quickjs/ || { \
	  echo "ERROR: Cannot fetch QuickJS." ; \
	  echo "  curl -L https://bellard.org/quickjs/quickjs-2024-01-13.tar.xz | tar xJ" ; \
	  exit 1 ; \
	}

clean:
	rm -f $(BUILDDIR)/*.o $(BUILDDIR)/qjs-*.o $(TARGET) $(BUILDDIR)/qjs.tar.xz
	rm -rf $(BUILDDIR)/quickjs $(BUILDDIR)/quickjs-*

# Run test piece locally (needs DRM access or QEMU)
test: $(TARGET)
	@echo "Running hello.mjs test piece..."
	$(TARGET) test-pieces/hello.mjs
