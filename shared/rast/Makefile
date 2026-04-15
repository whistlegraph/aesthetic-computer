## shared/rast — build the cross-platform rasterizer for every target.
##
## Targets:
##   test       → native test binary (gcc), runs the self-tests
##   native     → static archive (libac_rast.a) for linking into ac-native
##   wasm       → wasm modules for the web runtime (needs emscripten in PATH):
##                  raster-baseline.{wasm,js}  — no SIMD, no threads
##                  raster-simd.{wasm,js}      — SIMD, no threads
##                  raster-threads.{wasm,js}   — threads + SAB
##                  raster-full.{wasm,js}      — SIMD + threads
##   clean
##
## Pixel output must stay identical across test / native / every wasm
## variant. raster_test.c exercises the correctness path; a follow-up
## snapshot harness (golden PNG diff) covers the richer drift catch.

CC       ?= gcc
CFLAGS   ?= -O3 -Wall -Wextra -std=c99 -fPIC
AR       ?= ar
EMCC     ?= emcc

RAST_SRC := raster.c
RAST_OBJ := raster.o

.PHONY: all test native wasm clean

all: test

# ── self-test (gcc native) ───────────────────────────────────────────
raster_test: $(RAST_SRC) raster_test.c raster.h
	$(CC) $(CFLAGS) -o $@ $(RAST_SRC) raster_test.c -lm

test: raster_test
	./raster_test

# ── static archive for ac-native link ────────────────────────────────
$(RAST_OBJ): $(RAST_SRC) raster.h
	$(CC) $(CFLAGS) -c -o $@ $<

libac_rast.a: $(RAST_OBJ)
	$(AR) rcs $@ $^

native: libac_rast.a

# ── WASM variants (emscripten) ───────────────────────────────────────
# Each variant exports the same C API, so the JS loader probes features
# and picks the best one the browser can actually run. See raster.h for
# the exported entry points.
#
# Build flags (same across variants for everything NOT related to the
# capability being toggled, so variants stay pixel-identical):
#   -O3 — maximum inlining and loop opts
#   -flto — link-time optimization
#   -s STANDALONE_WASM=1 — emit a standalone module (no Emscripten JS shim)
#   -s EXPORTED_FUNCTIONS=['_ac_rast_triangle','_ac_rast_clear']
#   -s ENVIRONMENT='web,worker' — only ship web code paths
#
# Variant-specific flags:
#   baseline → (none)
#   simd     → -msimd128
#   threads  → -pthread -s PTHREAD_POOL_SIZE='navigator.hardwareConcurrency'
#   full     → -msimd128 -pthread …

EMCC_COMMON := -O3 -flto \
               -s STANDALONE_WASM=1 \
               -s EXPORTED_FUNCTIONS='["_ac_rast_triangle","_ac_rast_clear","_malloc","_free"]' \
               -s EXPORTED_RUNTIME_METHODS='["HEAPU8","HEAPU32","HEAPF32"]' \
               -s ENVIRONMENT='web,worker' \
               -s ALLOW_MEMORY_GROWTH=1 \
               -s MALLOC=emmalloc

raster-baseline.wasm: $(RAST_SRC) raster.h
	$(EMCC) $(EMCC_COMMON) -o $@ $(RAST_SRC)

raster-simd.wasm: $(RAST_SRC) raster.h
	$(EMCC) $(EMCC_COMMON) -msimd128 -o $@ $(RAST_SRC)

raster-threads.wasm: $(RAST_SRC) raster.h
	$(EMCC) $(EMCC_COMMON) -pthread \
	  -s USE_PTHREADS=1 -s PTHREAD_POOL_SIZE=8 \
	  -o $@ $(RAST_SRC)

raster-full.wasm: $(RAST_SRC) raster.h
	$(EMCC) $(EMCC_COMMON) -msimd128 -pthread \
	  -s USE_PTHREADS=1 -s PTHREAD_POOL_SIZE=8 \
	  -o $@ $(RAST_SRC)

wasm: raster-baseline.wasm raster-simd.wasm raster-threads.wasm raster-full.wasm

clean:
	rm -f raster_test $(RAST_OBJ) libac_rast.a \
	      raster-baseline.* raster-simd.* raster-threads.* raster-full.*
