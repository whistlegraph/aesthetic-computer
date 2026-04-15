# shared/rast — cross-platform triangle rasterizer

One C99 triangle rasterizer used by both **ac-native** (fedac/native) and the
**web runtime** (system/public/aesthetic.computer). Same pixel output on both
platforms — no more drift between what a piece looks like in a browser and
what it looks like on bare metal.

## Why

Before this, arena.mjs (and other 3D pieces) ran through two completely
independent rasterizers — `graph.mjs`'s JS triangle path on web, and
`graph3d.c`'s software rasterizer on native. Both implementations had to be
maintained in parallel and drifted in subtle ways: different pixel coverage
rules, different near-plane behaviour, different color precision. The fix
is structural: one source of truth, multiple build targets.

## Source

- `raster.h` — public API (pixel format, vertex struct, options, entry points)
- `raster.c` — implementation (~180 lines, pure C99, no libc allocs on the
  hot path, no mutable globals — fully re-entrant so worker threads can
  rasterize independent tiles against shared framebuffers)
- `raster_test.c` — self-tests (solid fill, perspective-correct color
  interpolation, depth occlusion, scissor clipping)

## Build

    make test        # run self-tests (gcc)
    make native      # libac_rast.a for linking into ac-native
    make wasm        # four WASM variants for the web runtime (needs emcc)
    make clean

The `wasm` target emits four modules with progressive feature sets. The
web loader probes browser capability and picks the best one available:

| Variant | Tradeoff |
|---|---|
| `raster-baseline.wasm` | ubiquitous; 2-3× faster than current JS |
| `raster-simd.wasm` | needs WASM SIMD (Chrome 91+, FF 89+, Safari 16.4+); 4-6× |
| `raster-threads.wasm` | needs SharedArrayBuffer (COOP+COEP on server); 8-15× |
| `raster-full.wasm` | SIMD + threads; 20-40× on multicore desktops |

See the [compatibility matrix](#cross-browser-support) below.

## Integration status

- [x] Pure-C rasterizer + self-tests
- [x] Makefile for native + wasm build targets
- [ ] Wire into `fedac/native/src/graph3d.c` (replace `rasterize_triangle`)
- [ ] emcc build variants produced + smoke-tested via wasmtime or node
- [ ] JS loader in `system/public/aesthetic.computer/lib/graph.mjs` that
      probes caps and instantiates the best module
- [ ] Replace `graph.mjs`'s JS triangle inner loop with a WASM call
- [ ] Golden PNG snapshot diff in CI (catches any drift between variants)
- [ ] Tile-binning + multi-worker pipeline (unlocks `threads`/`full` variants)

## Cross-browser support

Graceful fallback — every current browser gets at minimum the baseline
WASM (2-3× faster than today's JS). Better variants unlock automatically:

| Feature | Chrome/Edge | Firefox | Safari |
|---|---|---|---|
| Baseline WASM | 57+ (2017) | 52+ (2017) | 11+ (2017) |
| SharedArrayBuffer | 68+ (2018, isolated 91+) | 79+ (2020) | 15.2+ (Dec 2021) |
| Atomics | same as SAB | same | same |
| WASM SIMD (128-bit) | 91+ (2021) | 89+ (2021) | 16.4+ (Mar 2023) |
| WASM threads | same as SAB | same | same |

### Hosting prerequisites for the parallel path

The server must send these headers to enable `crossOriginIsolated`:

    Cross-Origin-Opener-Policy: same-origin
    Cross-Origin-Embedder-Policy: require-corp

The docs route already does this (see `system/netlify/functions/docs.js`).
Extending to the piece-hosting routes is a separate commit. If third-party
iframes (YouTube, Spotify, arbitrary user URLs) need to still work, use
`COEP: credentialless` instead of `require-corp` — same isolation, fewer
embedding breakages.

## Pixel format

All framebuffers use `uint32_t` per pixel packed as:

    (A << 24) | (R << 16) | (G << 8) | B

Matches `graph3d.c`'s existing convention and web's `Uint32Array` view over
a `Uint8ClampedArray`. The `AC_RAST_PACK(a,r,g,b)` macro constructs one.

## Depth buffer

32-bit float per pixel, parallel to the color buffer. Lower values =
closer to camera (matching `graph3d.c` but inverted from WebGL's default
— callers that pass post-perspective-divide Z in `[-1,+1]` should negate
before calling if they want standard z-buffer semantics).

Modes:

- `AC_RAST_DEPTH_RW` — read + write (standard opaque pass)
- `AC_RAST_DEPTH_READONLY` — read only (transparent / overlay pass)
- `AC_RAST_DEPTH_NONE` — no test / no write (painter's-order fallback,
  also what pieces without a depth buffer at all use)

## Parallelism plan

The rasterizer is designed so each triangle call is independent. A tile
binner — not yet implemented here — will:

1. Split screen into 64×64 tiles
2. Transform all vertices on the main thread
3. Bucket each triangle into the tiles it covers
4. Dispatch tiles to a worker pool (Web Workers on web, pthreads on native)
5. Each worker rasterizes its tiles using `ac_rast_triangle` with a
   scissor rect set to the tile bounds
6. Main thread waits on a completion counter, flips buffers, presents

Workers never touch each other's tiles, so no locking is needed beyond
the submit/complete atomics. SharedArrayBuffer holds the color + depth
backing store; workers write with normal pointer ops.
