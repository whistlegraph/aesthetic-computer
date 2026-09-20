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
focuses its existing window; launching it with a directory (`--cwd`) opens that
workspace as another window of the running Studio, or focuses the window that
already has it. `--restart` requests a saved restart.

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

### The Studio

Aesel is one process with one Dock icon and any number of windows. File → New
Window (⌘N) opens a fresh workspace beside the current one; each window runs
its own agent, preview, and letter (𝔄, 𝔅, ℭ…), and the Slab menubar tells them
apart by window. Closing a window ends only its session; the app quits when the
last one closes. Restart App and binary updates checkpoint every open window
and reopen them all afterwards. `main.cjs` hosts the windows; the pure naming,
resume and addressing rules live in `studio.cjs`.

### Project apps (local macOS prototype)

App mode gives a project its own Dock name, icon and stable bundle ID. The
Projects menu and Dock menu share a registry of installed projects, mark running
apps, and open or focus the selected app. The existing Aesel installation remains
available. New Window from a project app opens in the Studio, not in the
project's own identity.

```sh
node easel/bin/piece-app.mjs --cwd /path/to/project --name sefo \
  --piece sefo.mjs --icon /path/to/piece-frame.png --open
```

The installer uses the installed `~/Applications/Aesel.app` as its base (override
with `--base`), creates an APFS clone in `~/Applications/Aesel Pieces/`, renames
and signs its native helpers, and verifies a local PTY launch before installation.
It requires the repo's desktop development dependencies for ASAR extraction.
These are locally signed prototypes, not notarized apps for distribution. The
runtime is frozen in each clone; binary auto-update is disabled so an Aesel
release cannot replace the project's identity. Existing apps must be closed
before rerunning the installer; replaced bundles are retained beside their
registry entry for rollback.

The project stores its identity in `.easel/app.json`; the shared registry is
`~/Library/Application Support/Easel/piece-apps/<id>/`. Renaming with the installer
preserves the bundle ID and installed path, so Dock shortcuts stay valid. The
current piece selects its existing saved session, including the case where a
project originally opened in a separate window was later reopened as the default
Aesel window. `--instance` explicitly selects a session identity when needed.
Reopening the app resumes its conversation and draft; opening it twice focuses
the existing process. Agent activity still appears in the prox ledger. The
registry currently provides discovery, liveness and launch/focus—not automatic
crash recovery or a separate supervisor daemon.
