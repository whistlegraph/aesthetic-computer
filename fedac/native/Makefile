# ac-native Makefile
# Builds the native AC piece runner, optionally with QuickJS-ng

SRCDIR   := src
BUILDDIR := build
QJSDIR   := $(BUILDDIR)/quickjs

# Use musl for static linking if available, else gcc
CC       ?= gcc
CFLAGS   := -O2 -Wall -Wextra -std=gnu11 -I$(QJSDIR)
LDFLAGS  := -lm -lpthread

# For static build (production/initramfs):
#   make STATIC=1 CC=musl-gcc
ifdef STATIC
  LDFLAGS += -static
endif

# DRM headers — use pkg-config if available
DRM_CFLAGS := $(shell pkg-config --cflags libdrm 2>/dev/null || echo "-I/usr/include/libdrm -I/usr/include/drm")
DRM_LIBS   := $(shell pkg-config --libs libdrm 2>/dev/null || echo "-ldrm")

CFLAGS  += $(DRM_CFLAGS)
LDFLAGS += $(DRM_LIBS)

# Source files
SRCS := $(SRCDIR)/ac-native.c \
        $(SRCDIR)/drm-display.c \
        $(SRCDIR)/framebuffer.c \
        $(SRCDIR)/graph.c \
        $(SRCDIR)/font.c \
        $(SRCDIR)/color.c \
        $(SRCDIR)/input.c \
        $(SRCDIR)/js-bindings.c

OBJS := $(patsubst $(SRCDIR)/%.c,$(BUILDDIR)/%.o,$(SRCS))

# QuickJS-ng object files (compiled from source)
QJS_SRCS := $(QJSDIR)/quickjs.c $(QJSDIR)/libunicode.c $(QJSDIR)/libregexp.c $(QJSDIR)/cutils.c $(QJSDIR)/libbf.c
QJS_OBJS := $(patsubst $(QJSDIR)/%.c,$(BUILDDIR)/qjs-%.o,$(QJS_SRCS))

TARGET := $(BUILDDIR)/ac-native

.PHONY: all clean quickjs test

all: quickjs $(TARGET)

$(TARGET): $(OBJS) $(QJS_OBJS)
	$(CC) -o $@ $^ $(LDFLAGS)
	@echo "Built: $@ ($(shell wc -c < $@ | tr -d ' ') bytes)"

$(BUILDDIR)/%.o: $(SRCDIR)/%.c | $(BUILDDIR)
	$(CC) $(CFLAGS) -c -o $@ $<

$(BUILDDIR)/qjs-%.o: $(QJSDIR)/%.c | $(BUILDDIR)
	$(CC) $(CFLAGS) -DCONFIG_VERSION=\"0.8.0\" -c -o $@ $<

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
	rm -rf $(BUILDDIR)

# Run test piece locally (needs DRM access or QEMU)
test: $(TARGET)
	@echo "Running hello.mjs test piece..."
	$(TARGET) test-pieces/hello.mjs
