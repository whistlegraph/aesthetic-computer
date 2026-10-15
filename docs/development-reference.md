# Development reference

Read the relevant section for the task. Commands and implementation details
should be checked against the current source before use. Shared working rules
live in [SCORE.md](../SCORE.md); environment selection lives in
[ENVIRONMENT.md](../ENVIRONMENT.md).

### Architecture

**Private products** — `sotce-net`, `choir`, `mime` (including the retiring
Whistlegraph app), `oskiewar`, and `nopaint` (No Paint / NeoPaint) live in
`aesthetic-computer-vault/products/`.
Original paths are ignored local links. Commit product edits in the vault;
keep the runtime, shared libraries and formats public. See
[`docs/private-products.md`](../docs/private-products.md) for the inventory,
workspace setup, release composition and checks.

**Frontend (system/)**
- `system/public/aesthetic.computer/` — Web client (Canvas + WebGL)
  - `bios.mjs` — Core runtime, loads pieces
  - `boot.mjs` — System initialization
  - `lib/disk.mjs` — Piece loader and lifecycle
  - `disks/*.mjs` — Individual pieces (programs)
  - `lib/*.mjs` — Shared libraries and utilities

**Backend**
- `session-server/` — Real-time multiplayer (Socket.io + geckos.io UDP). Hosted on its own DigitalOcean VPS; deployed via `fish session-server/deploy.fish` (sshes to the box, `git pull` from github main, restart node `session.mjs`). Houses `arena-manager.mjs` which is the authoritative pmove + snapshot pipeline for `disks/arena.mjs`.
- `lith/` — Production monolith deploy (Express + Caddy on a DigitalOcean VPS, pulled from the tangled knot `git@knot.aesthetic.computer:aesthetic.computer/core` via `lith/deploy.fish`). Express adapts the handlers in `system/netlify/functions/` as routes — the `netlify/functions/` path is historical; Netlify is no longer the host.
- `lith/mirror/` — knot↔github bidirectional mirror (systemd timer, every 60s). Lets us push to knot only while still letting downstream consumers (session-server VPS, mirrors) pull from github.
- `help/bridge/` — Local Express bridge on @jeffrey's macbook that spawns the host `claude` CLI and streams it as SSE; reached publicly through `help.aesthetic.computer` via the existing droplet proxy + autossh reverse tunnel. Powers the `aa` piece (admin-only phone-side chat with the macbook's claude). Auto-runs under launchd as `computer.aesthetic.aa-bridge` and `computer.aesthetic.aa-tunnel`.

**Arena auto-deploy (post-commit hook)** — Any commit on `main` that touches `disks/arena.mjs`, `lib/{arena-world,pmove,cam-doll}.mjs`, `session-server/{arena-manager,session}.mjs`, or `shared/` triggers a paired deploy in the background:
1. push HEAD to knot (`origin`) so lith pulls the right commit
2. `fish lith/deploy.fish` — pulls knot, restarts the monolith
3. wait ~75s for the knot→github mirror
4. `fish session-server/deploy.fish` — VPS pull from github + node restart

This is critical because `lib/pmove.mjs` is shared physics: client (lith) and server (session-server) MUST match, otherwise `arena-manager.mjs`'s reconciler will fight the client's prediction and movement glitches. Logs land in `.git/arena-auto-deploy.log`. Set `AC_NO_AUTO_DEPLOY=1` to skip (e.g. while bisecting).

**Languages**
- `kidlisp/` — KidLisp dialect (Lisp for generative art)
  - `compiler.mjs` — Parser and compiler
  - `spec/*.mjs` — Test specs

**Desktop (ac-electron/)**
- `ac-electron/main.js` — Electron main process: tray, menubar/notepat modes, multi-window orchestration, native bridges, auto-update.
- `ac-electron/preload.js` + `webview-preload.js` — Bridges between main process and the AC web client running in the embedded webview.
- `ac-electron/renderer/` — Native chrome (preferences, flip-view, notepat overlay) rendered outside the web client.
- `npm start` — Run against `https://aesthetic.computer` (prod) or `http://localhost:8888` (dev with `--dev`).
- `npm run build:mac` / `build:win` / `build:linux` — electron-builder packaging.
- `npm run release:host:mac` — Notarized signed mac build published to `silo.aesthetic.computer/desktop/`.
- **File associations:** mp3/wav/flac/ogg/m4a drop on the Dock icon (or onto a running window) opens the `play` piece. A loopback http server (`127.0.0.1:<ephemeral>`) streams the file with HTTP Range so scrubbing works.

**Bare Metal OS (fedac/native/)**
- `ac-os build` — Full build: binary → initramfs → kernel (produces `build/vmlinuz`)
- `ac-os flash` — Build + flash to USB
- `ac-os upload` — Build + upload OTA release (always rebuilds — never uploads stale kernels)
- `ac-os flash+upload` — Build + flash + upload
- **Important:** The kernel embeds the git hash and build name at compile time. `upload` without `build` would serve a stale kernel. The `ac-os` script enforces a full rebuild before every upload.

**Host Tooling (slab/)** — @jeffrey's macOS host, not a deployed service
- **Vault-backed domain MCP** (`toolchain/domains/domains-mcp.mjs`) — use
  `vault_status` and `domains_inspect` for AC Cloudflare/DNS work before
  asking for keys or a dashboard login. Cloudflare and Porkbun credentials resolve
  automatically inside the tools from `aesthetic-computer-vault`; values never
  belong in MCP replies or config. `domains_dns_set` previews changes and applies
  them with `apply: true` within the user's authorized task. Registrar delegation
  and Cloudflare zone records are separate. Install locally with
  `bash toolchain/mcp/install-daemons.sh domains` (loopback `:7782`). New MCP
  credential loaders use the service-scoped resolver in
  `toolchain/vault/credentials.mjs`; do not scan client credentials implicitly.
  Frame, Illy, web search, Slack, Instagram, X/Bluesky, and DMGify also use that
  resolver automatically. Analytics, Fleet, Memory, and Papers share its vault
  root lookup. `vault_status` discovers supported services without returning keys.
- **Cleaner** (`toolchain/macos/cleaner.sh`) — canonical safe fleet-Mac disk
  cleanup. “Call the cleaner” means run `cleaner --apply`; plain `cleaner` is
  report-only, and APFS snapshot thinning remains separately opt-in. Install
  with `toolchain/macos/cleaner.sh --install`. Fleet MCP exposes `fleet_cleaner`.
- **Unipointer** (`slab/deskflow-handoff/UNIPOINTER.md`) — canonical identifier
  for the seat's one logical pointer. Neo and Blueberry can exchange the
  physical Deskflow controller role without changing the unipointer's active
  machine or display-local position. Code and agents should use the literal
  identifier `unipointer` and state-record kind `unipointer-state`; every fleet
  host exposes `~/.local/bin/unipointer` for versioned JSON state.
- `slab/menubar-swift/` — native Swift menubar daemon (launchd `computer.slab.menubar`). Shows live Claude-session status, themes each Terminal.app/iTerm2 window by session state (working/awaiting/complete), tiles all windows into one grid, tints the desktop, and serves passphrases over a unix socket. Built + installed locally with `slab/menubar-swift/install.sh` (`swift build -c release` → universal arm64+x86_64 binary → app bundle + launchd agent); there is no remote deploy.
- **Swift build budget** — `~/.local/bin/swift` is the host build guard (`toolchain/macos/swift-guard.sh`): shell-invoked `swift build` commands inside this repo serialize per Swift package, run at lower priority, and default to two compiler jobs on ≤16 GB hosts (an explicit `--jobs` still wins). For Slab Menubar iteration use `slab/menubar-swift/build-dev.sh`; run its `install.sh` only for the final release install. Do not bypass the guard with `/usr/bin/swift build` unless deliberately diagnosing the guard itself.
- **Media render budget** — `~/.local/bin/{ffmpeg,ffprobe}` point at the repo QoS shims in `toolchain/shims/`, so shell/agent media work runs at utility priority and yields to the interactive UI under contention. Do not invoke the Homebrew binaries by absolute path unless deliberately bypassing QoS for a benchmark.
- **Tiling auto-fits the type:** `tileNow` sizes each Terminal window's font to the grid (Far/Near/Tiny modes) and drives `View ▸ Default Font Size` per window so a live window actually adopts it — a per-window zoom otherwise silently overrides the profile font and is invisible to AppleScript. Floors keep it legible (Far 10 / Near 9 / Tiny 8). iTerm2 has no AppleScript font property, so it tiles by bounds only.
- **Prox creatures (formerly prompt rocks)** (`slab/menubar-swift/Sources/SlabMenubar/PromptSigilOverlay.swift`) — egg creatures beside live local agent sessions, with stable session-seeded shells and pet names. `ProxCreatures.swift` persists portable appearance records in `~/.config/slab/creatures/`; active working time grows them slowly, and the existing throttled memoir inference may add a small feature. `ProxCreatureFrames.swift` renders breathing, blinking portraits under the shared local-time sun. Expressions, motion, and shadow color carry status; peer attention adds a blink and rattle. Click for the living memoir. Loopboy adds glow to the same creature. These remain click-through companion windows: `reposition` and `overlayAt` must both reject covered overlays. `prox_character` reads or exports appearance alone as JSON + PNG/GIF for reuse elsewhere.
- **Prompt rocks MCP** (`slab/bin/prox-mcp.mjs`, registered as `prox` in `.mcp.json`) — an MCP over the fleet handle ledger (`Ledger.swift`; `~/.config/slab/ledger/{local,peers/*}.json`, served per-machine on tailnet-only `:5252`) so any agent can `prox_list` every live session across machines, `prox_find` a `host:name` reference (e.g. `neo:regif`) to its status/subject/cwd/seed, `prox_poke` one (`POST /poke` → the target rock blinks + rattles), `prox_send` inbox messages and `prox_receive` pending replies, send `prox_artifact_ready` the output paths from an asynchronous render, and `prox_launch` a new Claude/Codex Terminal on a prompt host. `prox_wake` is a deprecated inbox-only alias and direct `/wake` is retired; peer messages never submit terminal input; `prox_artifact_ready` supplies the standard inspect/iterate/integrate/continue prompt itself. Launch is deliberately not a remote shell: the target accepts only those two fixed agents, a bounded prompt, and a cwd beneath that user's home. `prox_close` reads the session marker for tty+pid and closes locally only (refuses the calling session). This is how a `machine:promptname` handle resolves without an SSH crawl.
- **Loopboy** (`~/.config/slab/loopboy.json`, surfaced in the Slab menubar) — a reversible mode of an existing prox. `prox_bind_notification(handle, contact)` enters the mode for Claude, Codex, or Aesel; `prox_unbind_notification(handle)` leaves it. Both preserve the process, window, pet name, session ID, and conversation. Do not launch a replacement prox to change modes. New thread updates pulse the contact's rock and queue an inbox notification; `prox_loopboy_wait` receives updates through a waiting tool call, and the heartbeat is visual only. Loopboy never types, focuses a terminal, presses Return, resumes a process, or sends a reply automatically, even when an old route says `wake: true`. Mode records under `~/.local/share/slab/state/loopboy-modes/` override stale launch settings after an exit.
- **Paper MCP / `/papers` stack** (`slab/bin/paper-mcp.mjs`, registered as `paper`) — “use the papers stack,” “use `/papers`,” and similar requests name the studio's scholarly publishing workflow, not merely a request to export or prettify a PDF. Begin by consulting [`papers/SCORE.md`](../papers/SCORE.md), the public Platter index, relevant sub-platters, prior papers and bibliographies, and the underlying code/data/evidence. Unless the user names another mill lane, shape the result as an archival/arXiv-style LaTeX paper: title/byline/date, abstract, problem and context, related work, system or method, implementation, evidence/evaluation, ethics/privacy/limitations, conclusion, references, numbered/captioned figures and tables, and reproducible source/assets. Briefings, decks, cards, dossiers, and visual reports are distinct lanes and require explicit intent or strong task evidence. The MCP is the transport/build/inspection layer: `paper_list`/`paper_find` locate precedent, `paper_read` supports the Platter consult, `paper_build` runs XeLaTeX or Tectonic, `paper_figure_table_qa_check` supports mandatory visual inspection, and `paper_open` raises the result through `slab-pdf`. The shared loopback daemon is `:7777`; `toolchain/mcp/install-daemons.sh` registers it for Claude and Codex. A build may pass `notifyHandle` to queue its PDF path in the originating rock’s inbox through `prox_artifact_ready` (an inbox message, never a wake).
- `slab/bin/ac-passphrase` — pinentry-free secret fetch from the daemon (see [ENVIRONMENT.md](../ENVIRONMENT.md)).

**Other Projects**
- `tezos/` — NFT/blockchain experiments
- `grab/` — Media utilities
- `feed/` — RSS/content feeds

### Adding a Piece

**Published identity:** Whistlegraph Pad and MIME publications must have a stable
identifier in the global AC namespace, independent of the author's handle.
Use the content's canonical code, with its prefix or sigil where applicable;
titles and `@handle/name` aliases do not replace it. Handle changes must not
change the identifier. MIME cards place the global identifier at top left,
author handle at bottom left, and MIME type at bottom right.

**Clubs:** A user-administered club is an AC primitive with a global identity,
a goal or place, membership, and shared content. MIME is its first visual
interface; the AC prompt must expose the same club and member operations
through shared records and permissions. Pieces may belong to multiple clubs
without changing their global identifiers.

Every piece is a single `.mjs` (JS) or `.lisp` (KidLisp) file in
[`system/public/aesthetic.computer/disks/`](../system/public/aesthetic.computer/disks/).
Scaffold from the template:

```bash
npm run new <slug> "one-line description"
```

**Header convention** — the docs auto-scan reads lines 1–2:

```js
// Name, YY.MM.DD.HH.MM
// One-line description shown in `list` and prompt autocomplete.
```

**Lifecycle exports** (all optional except whichever ones you need):
`boot`, `paint`, `sim`, `act`, `leave`. Export `meta()` to opt the piece **into**
`list` and prompt autocomplete — omit it and the piece stays reachable at
`/<slug>` but hidden from indexes (good for drafts).

**Curating the entry** (richer `desc`, `colon` params/`examples`, force
`hidden: true`, or override an auto-entry): edit the `pieces` map in
[`system/netlify/functions/docs.js`](../system/netlify/functions/docs.js). Curated
entries always win over the auto-scan.

See [`WRITE-A-PIECE.md`](../WRITE-A-PIECE.md) for the end-user `source` / `publish`
flow and [`CLAUDE.md`](../CLAUDE.md) for the full piece API surface.

### Piece-Log Debugging (client-side errors)

Every piece load gets a fresh `pieceId` and a 2-second-batched wrapper around `console.log` / `warn` / `error` / `info` is installed in [`system/public/aesthetic.computer/lib/disk.mjs`](../system/public/aesthetic.computer/lib/disk.mjs) . Events are POSTed to `/api/piece-log` ([`netlify/functions/piece-log.mjs`](../system/netlify/functions/piece-log.mjs)) and stored in MongoDB in the `piece-runs` collection with phases `start` / `log` / `error` / `complete`.

This is the primary debug channel for problems you can't reproduce locally — silent synth failures, "worked for me but not for the user" bugs, hydration issues on specific hosts. Each record carries:

- `pieceId`, `slug`, `bootId`, `userAgent`, `host`, geo (from CF headers)
- `events[]` — the captured console output with `{level, at, elapsed, message}`, last 500 per run
- `error` — if the piece crashed, `{message, stack}`
- `summary` — on clean exit, `{duration, ...}`

**Inspecting from the CLI** (SSHes to lith, runs [`system/backend/piece-logs-cli.mjs`](../system/backend/piece-logs-cli.mjs) against the deployed env):

```fish
ac-piece-logs notepat                   # recent 20 runs of a slug
ac-piece-logs-events notepat --since 30 # include console events, last 30 min
ac-piece-logs-errors                    # status=error runs in the last hour
ac-piece-logs-grep "drumMode"           # full-text search across captured events
ac-piece-logs-json --slug notepat | jq  # raw JSON for scripting
```

The CLI ships with every `fish lith/deploy.fish`. If you add new telemetry, bump the payload in `disk.mjs` and the phase handler in `netlify/functions/piece-log.mjs`; no schema migration needed (MongoDB collection is schemaless).

### Pulling Chat Messages (clock / system channels)

Chat lives in MongoDB. Each channel is a separate collection:

- `chat-system` — the main `chat` piece (`/chat`)
- `chat-clock` — the `laer-klokken` / r8dio chat piece (connects via `client.connect("clock")` in [`disks/laer-klokken.mjs`](../system/public/aesthetic.computer/disks/laer-klokken.mjs))

**Public read endpoint:** [`/api/chat-messages`](../system/netlify/functions/chat-messages.mjs) (GET, 2-min Redis cache):

```fish
# Latest 100 clock-channel messages as chronological JSON (oldest → newest)
curl -s "https://aesthetic.computer/api/chat-messages?instance=clock&limit=100" | jq

# Just handle + text
curl -s "https://aesthetic.computer/api/chat-messages?instance=clock&limit=100" \
  | jq -r '.messages[] | "\(.when)  \(.from)  |  \(.text)"'

# Filter by sender or URL pattern (e.g. YouTube links from @prutti)
curl -s "https://aesthetic.computer/api/chat-messages?instance=clock&limit=100" \
  | jq -r '.messages[]
            | select((.from == "@prutti") or (.text | test("youtu\\.?be|youtube\\.com"; "i")))
            | "\(.when)  \(.from)  |  \(.text)"'
```

Query params:
- `instance` — `system` (default) or `clock`. Any other value hits `chat-system`.
- `limit` — up to **100** (over 100 returns HTTP 400). Sort is `when` descending, then reversed to chronological before returning.

Response shape: `{ instance, count, messages: [{ id, from, text, when, hearts }], nextBefore }`. `from` is resolved to `@handle` via the `@handles` collection, falling back to `"anon"` for unclaimed user ids. `hearts` joins the shared `hearts` collection (`type: "chat-<instance>"`). `nextBefore` is the oldest `when` in the page, ready to hand back as `before=` for the previous page.

**Going back further than 100 messages** — pass `before=<ISO>` to walk back (or use `nextBefore` from the previous response):

```fish
# All @prutti YouTube links in the clock channel, paginating back
set cursor ""
while true
    set url "https://aesthetic.computer/api/chat-messages?instance=clock&limit=100"
    test -n "$cursor"; and set url "$url&before=$cursor"
    set page (curl -s $url)
    test (echo $page | jq '.count') -eq 0; and break
    echo $page | jq -r '.messages[]
      | select(.from == "@prutti" and (.text | test("youtu"; "i")))
      | "\(.when)  \(.text)"'
    set cursor (echo $page | jq -r '.nextBefore')
end
```

Full docs and `curl`/JS/Python examples live at [`/api/chat-messages` on api.aesthetic.computer](https://api.aesthetic.computer) (served by [`system/netlify/functions/api-docs.mjs`](../system/netlify/functions/api-docs.mjs)).

If you ever need raw Mongo access (deleted messages, admin edits, heavier aggregations), go direct from lith:

```fish
# On lith (or any machine with backend creds loaded):
ac-host                                          # pick lith
# then in the ssh session:
cd aesthetic.computer/system
node -e '
  import("./backend/database.mjs").then(async ({ connect }) => {
    const { db, disconnect } = await connect();
    const rows = await db.collection("chat-clock")
      .find({ when: { $lt: new Date("2026-04-22T00:00:00Z") } })
      .sort({ when: -1 }).limit(500).toArray();
    console.log(JSON.stringify(rows, null, 2));
    await disconnect();
  });
'
```


## Hardware Probing on ac-native

When the onboard Claude is debugging a device (missing speakers, WiFi,
trackpad, etc.) the below surfaces are always available without needing
a separate diagnostic build. Probe from the prompt, a piece, or a shell
— whatever fits the task.

### Boot-time logs (USB pulled out then inspected on a host)

- `/mnt/pre-launch.log` — init script's full probe dump: GPU nodes,
  block devices, net ifaces, rfkill state, PCI devices with bound
  drivers, ACPI codecs, sound PCM names, GPIO chips + descriptor
  consumers, ASoC debugfs, MAX98357A amp state, and a subset of the
  running kernel config via `/proc/config.gz` (CONFIG_IKCONFIG=y).
- `/mnt/kmsg.log` — persistent `cat /dev/kmsg` for the entire boot
  session. Grep for any driver probe, dev_dbg, or warning.
- `/mnt/ac-native-stderr.log` — ac-native's ALSA init trace
  (device opened, mixer enumeration, volume sets, XRUN count).
- `/mnt/flash-last.log` — flash-thread telemetry from the last OTA.

### Runtime probing (piece APIs)

- `system.audio.listPcms()` → array of `{device, card, num, id, name}`.
  Skips HDMI. `active: true` marks the PCM ac-native's main audio
  thread opened.
- `system.audio.testPcm(device, freq_hz, duration_ms, volume)` — plays
  a short sine wave on an arbitrary ALSA device in a detached thread.
  Used by the `speaker` piece to find which PCM actually drives the
  onboard speakers vs headphone jack vs HDMI. Piece source:
  [speaker.mjs](../system/public/aesthetic.computer/disks/speaker.mjs).
- `system.firmware.{available, board, biosVersion, install}` —
  machines running MrChromebox coreboot can reflash from os.mjs's
  firmware panel (gated behind `/dev/mtd0` + `bios_vendor=coreboot`).

### Live sysfs hotspots

- `/proc/asound/card0/pcm*p/info` — per-PCM id/name. On SOF:
  pcm0p is usually "Speakers", pcm1p "Headset", pcm2-4p are HDMI.
- `/sys/bus/gpio/devices/` + `/sys/kernel/debug/gpio` — list every
  GPIO chip + which consumer holds which pin. `gpio-* | sdmode` shows
  the MAX98357A speaker-enable line.
- `/sys/bus/pci/devices/<BDF>/driver` — symlinks to each PCI device's
  driver. `drv=NONE` means the driver isn't bound (either missing
  config, missing firmware file, or missing ACPI device match).
- `/sys/bus/acpi/devices/<HID>:NN/physical_node/driver` — same but
  for ACPI-enumerated devices (audio codecs, embedded controller,
  pinctrl, etc).
- `/proc/config.gz` — `zcat /proc/config.gz | grep CONFIG_FOO=` tells
  you definitively whether a config survived `make olddefconfig`.

### Build-time canaries

`docker-build.sh` verifies six critical driver symbols (`jsl_pinctrl_
acpi_match`, `max98357a_sdmode_event`, `rt5682_i2c_probe`,
`i2c_dw_prepare_clk`, …) are linked into vmlinux via `nm`. When a new
config toggles on, a hash sentinel wipes the matching subsystem's
object files so kbuild actually rebuilds them. If your build fails at
"BUILD SANITY: critical driver symbols missing", wipe the persistent
docker volume: `docker volume rm ac-os-kbuild`.
