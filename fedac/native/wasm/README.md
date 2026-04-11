# AC Native WASM Prototype

This is a small browser-hosted prototype for running a slice of `ac-native`
offline in a single HTML file.

Current shape:

- Boots the real native [`prompt.mjs`](/workspaces/aesthetic-computer/fedac/native/pieces/prompt.mjs) piece
- Uses a tiny WebAssembly raster core for framebuffer clears and boxes
- Uses browser shims for keyboard input, config storage, theme, wifi, and piece jumps
- Generates a single offline HTML artifact

Build:

```bash
cd /workspaces/aesthetic-computer/fedac/native/wasm
npm install
npm run build
```

Output:

- `/workspaces/aesthetic-computer/fedac/native/build/ac-native-prompt-offline.html`
- `/workspaces/aesthetic-computer/system/public/ac-native-wasm/index.html`

Production route:

- `https://aesthetic.computer/ac-native-wasm/`

Notes:

- This is intentionally not full `ac-native` parity yet.
- Text rendering is still done by the browser canvas host.
- Native-only features like raw WiFi control, PTY, power management, and Linux device access are stubbed.
