# Rio Terminal Integration

## Overview
[Rio Terminal](https://github.com/raphamorim/rio) is a modern, GPU-accelerated terminal emulator written in Rust with WebAssembly support. It can replace xterm.js in the Electron shell windows.

## Why Rio?

### Performance
- **GPU Hardware Acceleration**: Uses WebGPU for smooth rendering
- **Near-native performance**: Built with Rust + WebAssembly
- **Fast startup**: Significantly faster than xterm.js

### Visual Features
- **24-bit True Color**: 16 million colors vs xterm's 256
- **Image Support**: Sixel and iTerm2 protocols (inline images)
- **Font Ligatures**: Better readability for code
- **RetroArch Shaders**: CRT effects, custom visual styling
- **Adaptive Themes**: Auto light/dark mode

### Advanced Features
- **Splits & Panels**: Native terminal multiplexing
- **Vi Mode**: Vim-style keyboard navigation
- **Kitty Protocol**: Advanced keyboard input support
- **Shell Integration**: Enhanced prompt/command tracking

## Integration Options

### Option 1: Rio Web Version (Recommended)
Rio provides a WebAssembly build that can be embedded directly in Electron:

```javascript
// Replace xterm.js renderer with Rio WASM
import init, { Terminal } from './rio-wasm/rio_terminal.js';

await init(); // Initialize WASM
const terminal = new Terminal(container);
terminal.connect(ptyProcess);
```

**Pros:**
- Single bundle (no native dependencies)
- Works across all platforms
- GPU-accelerated rendering
- Modern web standards (WebGPU)

**Cons:**
- Larger initial bundle size (~2-3 MB WASM)
- Requires WebGPU browser support

### Option 2: Native Rio (via node-pty)
Use Rio's native binary as the terminal emulator:

```bash
# Install Rio natively
cargo install --git https://github.com/raphamorim/rio

# Launch from Electron
spawn('rio', ['--command', shellCommand], { ... });
```

**Pros:**
- Maximum performance (native Rust)
- All Rio features available
- Separate process isolation

**Cons:**
- Requires Rio installed on system
- Platform-specific binaries
- Less integrated UX (separate window)

## Implementation Plan

### Phase 1: Evaluation
- [ ] Test Rio WASM build in Electron
- [ ] Benchmark vs xterm.js (rendering, memory, startup)
- [ ] Verify WebGPU support in Electron (Chromium 94+)
- [ ] Test PTY integration (node-pty → Rio)

### Phase 2: Integration
- [ ] Create `renderer/rio-terminal.html` (Rio version)
- [ ] Add Rio WASM to dependencies
- [ ] Implement Rio adapter for node-pty
- [ ] Add feature flag: `--terminal=rio` or `--terminal=xterm`

### Phase 3: Migration
- [ ] Update `createShellWindow()` to use Rio by default
- [ ] Keep xterm.js as fallback (if WebGPU unavailable)
- [ ] Update docs and build scripts
- [ ] Remove xterm.js dependencies (optional)

## References
- **GitHub**: https://github.com/raphamorim/rio
- **Website**: https://rioterm.com
- **WASM Build**: Check `rio/frontends/rioterm-web/` in repo
- **Features**: https://rioterm.com/docs/features

## Current Status
- [x] Research completed
- [ ] WASM build tested
- [ ] Integration started

---
*Last updated: 2025-12-27*
