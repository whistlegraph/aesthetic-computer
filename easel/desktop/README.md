# Standalone Aesel

Install this directory's dependencies, then run `npm start -- --cwd /path/to/project --backend ac`.
The desktop shell runs Aesel in a native PTY and uses the existing AC login.
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

Linux x86-64 downloads: https://releases.aesthetic.computer/easel/desktop/index.html

Ubuntu/Debian: download `Easel-0.7.0-linux-amd64.deb`, then run
`sudo apt install ./Easel-0.7.0-linux-amd64.deb`. Launch **Aesel** from the app menu
or run `easel`. The installer configures Chromium's sandbox when user namespaces
are unavailable. Do not launch with `--no-sandbox`.

Other distributions can use the AppImage (`chmod +x` first), or extract the
tarball and run `./easel`. If FUSE is missing, AppImage supports
`--appimage-extract-and-run`. Systems restricting user namespaces need an
installed sandbox; on Ubuntu use the .deb. This initial release targets
Intel/AMD 64-bit, tested on Ubuntu 24.04; ARM is not included.

An AC account and acceptance of required transcript sharing are needed before
use. The normal hosted AC provider needs no Claude or Codex installation.
Bring-your-own Claude or Codex modes remain optional and require their CLI and
account.
Game Boy compilation requires GBDK installed separately.

Build on Linux with `npm install` and `npm run dist:linux`. Upload the resulting
three artifacts, `latest-linux.yml`, and `scripts/linux-download.html` (as
`index.html`) with `scripts/publish-linux.cjs DIST`, using the existing Spaces
credentials and AWS S3 SDK. Aesel uses the isolated `easel/desktop/` prefix;
never register it through AC Desktop's release API. AppImage supports the
binary update feed; .deb/tar users install a new package manually.
macOS downloads are available from https://releases.aesthetic.computer/easel/desktop/.
The Apple-silicon DMG and ZIP are signed and notarized; Intel builds are not
included in this first release.

`/restart` (or Aesel → Restart Aesel) saves the active thread, transcript, draft,
model and workspace before replacing the app process. A busy turn or upload
finishes first. The local checkpoint is private (`0600`). Launching Aesel again
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
