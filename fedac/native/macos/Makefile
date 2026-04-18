# ac-native macOS host Makefile
# Links SDL3 directly (unlike the Linux build which dlopens SDL3 as a fallback).
# Shares QuickJS sources with the Linux build at ../build/quickjs (fetched
# by the Linux Makefile or manually).

CC      ?= clang
BUILD   := build
TARGET  := $(BUILD)/ac-native-macos

# QuickJS sources live alongside the Linux build tree (shared fetch).
QJSDIR  := ../build/quickjs

SDL3_CFLAGS := $(shell pkg-config --cflags sdl3)
SDL3_LIBS   := $(shell pkg-config --libs sdl3)

CFLAGS  := -O2 -Wall -Wextra -std=gnu11 -I. -I../src -I$(QJSDIR) $(SDL3_CFLAGS)
LDFLAGS := $(SDL3_LIBS) -lm

# Host sources (this directory)
HOST_SRCS := main.c piece.c audio.c
HOST_OBJS := $(patsubst %.c,$(BUILD)/%.o,$(HOST_SRCS))

# QuickJS core object set (must match what the Linux Makefile uses at line 104)
QJS_SRCS  := quickjs.c libunicode.c libregexp.c cutils.c libbf.c
QJS_OBJS  := $(patsubst %.c,$(BUILD)/qjs-%.o,$(QJS_SRCS))

.PHONY: all clean run app app-run

all: $(TARGET)

# Paths for the .app bundle.
APP_NAME := Notepat
APP_DIR  := $(BUILD)/$(APP_NAME).app
PIECE_SRC := ../pieces/notepat.mjs
LIB_SRC   := ../../../system/public/aesthetic.computer/lib/percussion.mjs

# SDL3 dylib: source path + install-name string the binary links against.
SDL3_LIB_SRC  := /opt/homebrew/lib/libSDL3.0.dylib
SDL3_LIB_NAME := /opt/homebrew/opt/sdl3/lib/libSDL3.0.dylib

$(TARGET): $(HOST_OBJS) $(QJS_OBJS) | $(BUILD)
	$(CC) -o $@ $^ $(LDFLAGS)
	@echo "Built: $@ ($$(wc -c < $@ | tr -d ' ') bytes)"

$(BUILD)/%.o: %.c | $(BUILD)
	$(CC) $(CFLAGS) -c -o $@ $<

# QuickJS objects: compiled with the same CONFIG_VERSION the Linux side uses.
# Silence warnings QuickJS itself triggers — we don't own that code.
$(BUILD)/qjs-%.o: $(QJSDIR)/%.c | $(BUILD)
	$(CC) $(CFLAGS) -DCONFIG_VERSION=\"0.8.0\" -Wno-unused-parameter -Wno-sign-compare -Wno-implicit-fallthrough -c -o $@ $<

$(BUILD):
	mkdir -p $(BUILD)

run: $(TARGET)
	$(TARGET)

# Assemble a self-contained .app bundle with notepat + percussion lib +
# libSDL3 embedded. After copying the dylib in, we rewrite install names
# and rpaths so the binary resolves SDL3 through Contents/Frameworks rather
# than brew's /opt/homebrew. Ad-hoc codesign is required on arm64 after any
# install_name_tool modification — otherwise dyld refuses to load the
# tampered Mach-O.
app: $(TARGET) $(PIECE_SRC) $(LIB_SRC) Info.plist.in $(SDL3_LIB_SRC)
	rm -rf $(APP_DIR)
	mkdir -p $(APP_DIR)/Contents/MacOS
	mkdir -p $(APP_DIR)/Contents/Resources/lib
	mkdir -p $(APP_DIR)/Contents/Frameworks
	cp $(TARGET)    $(APP_DIR)/Contents/MacOS/notepat
	cp $(PIECE_SRC) $(APP_DIR)/Contents/Resources/piece.mjs
	cp $(LIB_SRC)   $(APP_DIR)/Contents/Resources/lib/percussion.mjs
	cp Info.plist.in $(APP_DIR)/Contents/Info.plist
	cp $(SDL3_LIB_SRC) $(APP_DIR)/Contents/Frameworks/libSDL3.0.dylib
	chmod +w $(APP_DIR)/Contents/Frameworks/libSDL3.0.dylib
	install_name_tool -id @rpath/libSDL3.0.dylib \
	    $(APP_DIR)/Contents/Frameworks/libSDL3.0.dylib
	install_name_tool -change $(SDL3_LIB_NAME) @rpath/libSDL3.0.dylib \
	    $(APP_DIR)/Contents/MacOS/notepat
	-install_name_tool -delete_rpath /opt/homebrew/lib \
	    $(APP_DIR)/Contents/MacOS/notepat 2>/dev/null
	install_name_tool -add_rpath @executable_path/../Frameworks \
	    $(APP_DIR)/Contents/MacOS/notepat
	codesign --force --sign - \
	    $(APP_DIR)/Contents/Frameworks/libSDL3.0.dylib
	codesign --force --sign - \
	    $(APP_DIR)/Contents/MacOS/notepat
	@echo "Bundle: $(APP_DIR)"
	@echo "--- install names ---"
	@otool -L $(APP_DIR)/Contents/MacOS/notepat | head -5

app-run: app
	open $(APP_DIR)

clean:
	rm -rf $(BUILD)
