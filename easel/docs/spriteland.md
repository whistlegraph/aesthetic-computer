# Spriteland

The desktop mediates the terminal grid through three visual layers:

1. `spriteland.js` caches background fabric and foreground wood tiles. Layout and theme changes repaint them; token updates do not.
2. Xterm renders selectable text with a transparent background. The terminal owns chat and control actions; its four bottom rows sit over the wooden rail. Native terminals retain their character-based fallback.
3. `companion-scene.js` composites the generated donkey poses, a functional QR easel, and a pixel-lettered version plate. `donkey.js` schedules pose changes, pauses when hidden, and supports reduced motion. The accessible QR button overlays the exact composited QR square.

The QR uses square black/white modules and a four-module white quiet zone. Wood, version text and all sixteen donkey poses stay outside that zone. Cmd+/− changes the common pixel scale; the name is twice the text size. The TV retains its own logical resolution.

The real Slab rock is a separate native overlay. Desktop markers identify `host_app`, `host_pid` and `host_window_id`; Slab matches the exact Aesel process/window and suppresses duplicate native name/QR/preview surfaces. The ledger registers `proxNamespace`, `proxName` and `proxIdentity`. The desktop reads the matching session's authoritative name; `prox:easel:<name>` resolves it, with `prox:easel:<host>:<name>` available for fleet ambiguity.

Validation:

- `node --test easel/test/donkey.test.mjs`
- Run `easel/test/preview-geometry.electron.cjs` with the desktop Electron binary for window resizing, hover/fullscreen geometry and QR zoom.
- Run `easel/test/companion-scene.electron.cjs` with `EASEL_DESKTOP=1 COLORTERM=truecolor` and Electron. It verifies every QR/quiet-zone pixel across all sixteen poses and saves normal/small/tiny full-window screenshots under `/tmp/easel-scene-*.png`.
- On macOS, `swift easel/test/decode-qr.swift EXPECTED_URL /tmp/easel-scene-normal.png /tmp/easel-scene-small.png /tmp/easel-scene-tiny.png` checks actual screenshot decoding with Vision. Set SDKROOT to a supported local SDK if necessary.

The September 15 integration check decoded the same watch URL at 16px, 14px and 12px text sizes. This is software-decoder evidence, not a guarantee for every physical phone camera, screen size or lighting condition.

The companion now draws only atlas columns 0–45 (the donkey), mirrored toward
content. The QR is a separate right-side button revealed by window hover or
keyboard focus, with its own quiet zone. Version text stays near the companion.
The title shows `@handle/proxName`. Renderer-measured title geometry travels over
validated IPC to a private `state/easel-layout/<pid>-<window>.json` sidecar; Slab
places the compact native rock immediately after the title using the same bounds
for drawing, pointer hits and occlusion. No fixed blank slot precedes the title.
