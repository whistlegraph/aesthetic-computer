# Standalone Easel

Install this directory's dependencies, then run `npm start -- --cwd /path/to/project --backend ac`.
The desktop shell runs Easel in a native PTY and uses the existing AC login.
On launch, Tab switches New media / Threads; arrows select and Enter opens.
Checkpointed update restarts resume directly.

GNU Unifont uses 8×16 CSS-pixel cells at its original size. View → Smooth Font
switches to Menlo. Retina displays map each CSS pixel to their backing pixels.
The window opens at 760×540 and fits the terminal when resized or tiled.

The terminal uses xterm's WebGL renderer with automatic fallback. Chromium
selects its GPU backend; Metal availability is checked through GPU diagnostics,
not assumed. Pass `--diagnostics /tmp/easel-diagnostics` to save GPU information
and a window screenshot. Desktop startup skips the terminal splash and loads
only the selected preview URL. Marker changes use a filesystem watcher.

The top-right QR follows the Piece URL and includes a four-module quiet zone.
Published Pictures also show their verified painting #code QR. Local artifacts
without a public URL do not show a QR.

Local app builds are available; signed public distribution is not published yet.

`/restart` (or Easel → Restart Easel) saves the active thread, transcript, draft,
model and workspace before replacing the app process. A busy turn or upload
finishes first. The local checkpoint is private (`0600`). Launching Easel again
focuses its existing window; `--restart` requests a saved restart.

`/update` is handled by the desktop host. Development builds reload current
source. Packaged builds use electron-updater's binary download and verification
pipeline, then checkpoint and install. macOS distribution still requires a
signed release and a published update feed; neither is published by this change.
`npm run dist:mac` prepares ZIP/DMG release artifacts.

The corner preview follows the current display’s aspect ratio and text size.
Fullscreen scales the same 128-pixel-high logical viewport without reflow.
Hover the preview to enlarge it; move away to shrink it. Click the QR to open
the piece in your browser. Header links highlight and show a hand on hover. View → Preview offers Compact, Zoom on Hover, and Always Open.
The version sits below the preview. Drag chat text to select it; right-click for
Copy, Paste, and Select All. ⌘W saves the session and closes the window.
⌘+/− changes text size; ⌘0 resets it. Slab’s status palettes and light/dark
setting recolor the terminal live.

Media previews support Picture PNG, Sound WAV with waveform/playback, Paper
source/PDF, and Game Boy ROMs through AC's bundled WasmBoy core. Game Boy building
uses AC's GBDK stack; the compiler is installed separately. See `../media/gameboy`.
