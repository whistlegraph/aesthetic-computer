# pop/dsp/c — Aesthetic Computer DSP lib + CLI.
#
#   make            → ./acdsp (native binary)
#   make lib        → ./libacdsp.a
#   make wasm       → ./acdsp.wasm (+ .js) — requires emcc on PATH
#   make test       → ./acdsp + run test/bench.sh
#   make clean

CC      ?= cc
CFLAGS  ?= -O3 -std=c11 -Wall -Wextra -Wno-unused-parameter -ffast-math -DNDEBUG -fPIC
LDFLAGS ?= -lm

SRC_LIB := src/util.c src/wav.c src/biquad.c src/eq_graph.c src/comp_1176.c src/chain.c
SRC_CLI := src/cli.c
OBJ_LIB := $(SRC_LIB:.c=.o)
OBJ_CLI := $(SRC_CLI:.c=.o)

.PHONY: all lib wasm test clean
all: acdsp

acdsp: $(OBJ_LIB) $(OBJ_CLI)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

lib: libacdsp.a
libacdsp.a: $(OBJ_LIB)
	ar rcs $@ $^

src/%.o: src/%.c
	$(CC) $(CFLAGS) -Isrc -c -o $@ $<

# wasm target — single compile pass with emcc. Exports acdsp_process for
# the simplest "bring your own buffer" usage from JS. Memory growth so we
# can process full-song buffers.
EMCC_EXPORTS := _acdsp_process,_malloc,_free
wasm: acdsp.js
acdsp.js: $(SRC_LIB)
	@command -v emcc >/dev/null || { echo "emcc not found — install Emscripten"; exit 1; }
	emcc -O3 -Isrc $(SRC_LIB) \
	  -s WASM=1 \
	  -s ALLOW_MEMORY_GROWTH=1 \
	  -s EXPORTED_FUNCTIONS='[$(EMCC_EXPORTS)]' \
	  -s EXPORTED_RUNTIME_METHODS='["ccall","cwrap","HEAPF32","HEAPU8"]' \
	  -s MODULARIZE=1 -s EXPORT_NAME=AcdspModule \
	  -o acdsp.js

test: acdsp
	@./test/bench.sh

clean:
	rm -f $(OBJ_LIB) $(OBJ_CLI) acdsp libacdsp.a acdsp.js acdsp.wasm
