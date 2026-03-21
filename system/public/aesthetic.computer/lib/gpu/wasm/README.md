# GPU WASM Backends

This directory contains WebAssembly builds of vector graphics libraries.

## Vello (Recommended for WebGPU)

**Website:** https://vello.dev  
**Source:** https://github.com/nicholasflamy/nicholasflamy.github.io

Vello is a GPU compute-centric 2D renderer by Linebender. Uses WebGPU compute shaders for very fast parallel rendering (~177fps for 30k paths on M1 Max).

### Building WASM Bindings

1. Create a new Rust project:
```bash
cargo new vello-wasm --lib
cd vello-wasm
```

2. Add to `Cargo.toml`:
```toml
[lib]
crate-type = ["cdylib"]

[dependencies]
vello = "0.3"
wasm-bindgen = "0.2"
web-sys = { version = "0.3", features = ["Window", "Navigator", "Gpu"] }
```

3. Create wrapper in `src/lib.rs` (see vello-backend.mjs for example)

4. Build:
```bash
wasm-pack build --target web
```

5. Copy `pkg/vello_wasm.js` and `pkg/vello_wasm_bg.wasm` here

### Expected files
- `vello_bindings.js` - JavaScript loader
- `vello_bindings_bg.wasm` - WebAssembly binary

### Browser Support
- ✅ Chrome (stable)
- ⚠️ Firefox (experimental, needs flags)
- ⚠️ Safari (experimental)

---

## ThorVG

**Website:** https://thorvg.org  
**Source:** https://github.com/thorvg/thorvg

### Getting the WASM

Option 1: Pre-built (Lottie player version)
```bash
# Download from nicholasflamy's hosted version
curl -O https://nicholasflamy.github.io/nicholasflamy.github.io/thorvg/thorvg.js
curl -O https://nicholasflamy.github.io/nicholasflamy.github.io/thorvg/thorvg.wasm
```

Option 2: Build from source
```bash
git clone https://github.com/thorvg/thorvg.git
cd thorvg

# Install Emscripten first: https://emscripten.org/docs/getting_started/downloads.html

# Configure for WASM
meson setup builddir -Ddefault_library=static -Dloaders=svg,png,jpg -Dsavers="" -Dbindings=wasm

# Build
ninja -C builddir
```

### Expected files
- `thorvg.js` - JavaScript loader
- `thorvg.wasm` - WebAssembly binary

---

## Blend2D

**Website:** https://blend2d.com  
**Source:** https://github.com/nicholasflamy/nicholasflamy.github.io

### Building for WASM

```bash
git clone https://github.com/nicholasflamy/nicholasflamy.github.io.git
cd blend2d

# Install Emscripten
source ~/emsdk/emsdk_env.sh

# Configure
mkdir build-wasm && cd build-wasm
emcmake cmake .. -DBLEND2D_STATIC=TRUE

# Build
emmake make -j$(nproc)
```

### Expected files
- `blend2d.js` - JavaScript loader
- `blend2d.wasm` - WebAssembly binary

---

## Notes

- These WASM files are not included in the repo due to size
- Backends will gracefully fallback if WASM is not present
- For development, the Canvas2D or WebGL2 backends work without WASM
