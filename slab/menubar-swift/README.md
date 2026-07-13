# slab-menubar (Swift)

Native macOS menu-bar app for the slab workflow — replaces `slab/bin/slab-menubar.py` (rumps/Python).

## Why swift

- One language across the whole slab stack (the lid-ambient synth is already Swift).
- No Python venv → no venv breakage, no numpy/sounddevice install fragility on a fresh Mac.
- Faster cold start, lower memory, no GIL.
- SF Symbols + native `NSStatusItem` give dynamic, template-aware icons that adapt to light/dark menu bars.
- The passphrase-modal feature (deferred in Python) ships here.

## What's in the menu bar

The status-item icon changes by state:

| State                                              | SF Symbol                           |
| -------------------------------------------------- | ----------------------------------- |
| idle (no work)                                     | `square.stack.3d.up` (outline)      |
| N active, lid open                                 | `square.stack.3d.up.fill` + `N`     |
| N active, lid closed, sleep disabled (working)     | `moon.zzz.fill` + `N`               |
| N active, lid closed, sleep NOT disabled (⚠ drift) | `exclamationmark.triangle.fill` + N |
| ambient synth running                              | `waveform.path.ecg`                 |

Menu contents are parity with the Python version:

- status line + active-prompt / active-subagent counts
- Tailnet submenu (click peer → opens `ssh <host>` in Terminal.app)
- Mail submenu (`mbsync` + `mu index`, open sync log)
- Stay-awake toggle / Sleep now
- Open daemon log / Open sounds folder
- Reload daemon / Quit menu bar

Refresh cadence: 2 s. Mail unread count refreshes every 30 s (15 ticks).

## Prompt rocks

`Sources/SlabMenubar/PromptSigilOverlay.swift` — the tumbling stones parked at
the top-right of each terminal window, one per live Claude session. When someone
says "prompt rocks", this is it.

- **Shape = identity.** Each rock is a 3D sigil grown from `sessionId + prompt`,
  so it re-forms when the session moves on to a new prompt. Frames are
  pre-rendered per rock (`SigilRockFrames`) and played back as a sprite sheet.
- **Motion = status.** Spin speed and direction encode working / awaiting /
  complete / stale. A poke from a peer (the ledger's "observed" note) makes the
  stone blink and rattle.
- **Name + bubble.** A deterministic pet name in Comic Sans bubble lettering
  sits under the stone; pointing at it reveals a card summarizing the prompt
  (one cheap `claude -p haiku` sentence, cached per seed).
- **A shared sun** lights every rock from the local time of day, so the whole
  wall of stones re-lights together.

The rocks are borderless, click-through `.floating` windows — they ride above
the normal-window stack so a busy wall of preview cards can't bury them. The
price is that **occlusion is hand-rolled**, in two places that must agree:
`reposition` hides a rock whose terminal corner is covered, and `overlayAt`
refuses the pointer to a rock that is hidden or covered *at the cursor*. Skip
either and a stone will wake up and pop its bubble through the window sitting on
top of it.

## Passphrase modal (IPC server)

The app listens on a Unix domain socket at `~/.ac-daemon.sock` (mode 0600). Any script on this user's account can request a passphrase.

### Request / response

```json
→ {"op":"passphrase","label":"vault-ssh","timeout":600}
← {"ok":true,"secret":"…","cached":false}
```

- `timeout` is the in-memory cache TTL in seconds (default 600).
- Repeated requests for the same `label` return the cached secret with `"cached":true` until TTL expires.
- Cancel: `← {"ok":false,"cancelled":true}`.
- Forget one: `→ {"op":"forget","label":"vault-ssh"}`. Forget all: omit `label`.
- Ping: `→ {"op":"ping"} → {"ok":true,"pong":true}`.

### Fish caller example

```fish
set req '{"op":"passphrase","label":"vault","timeout":600}'
set resp (echo $req | nc -U ~/.ac-daemon.sock)
set phrase (echo $resp | jq -r '.secret // empty')
```

The modal is a native `NSAlert` + `NSSecureTextField`, brought to the front with `NSApp.activate(ignoringOtherApps:)`.

## Layout

```
menubar-swift/
├── Package.swift
├── install.sh                          # build + install + launchctl
├── computer.slab.menubar.plist.tmpl    # launchd agent template (@HOME@ is substituted)
└── Sources/SlabMenubar/
    ├── main.swift                      # NSApplication bootstrap
    ├── AppDelegate.swift               # status item, timer, menu actions
    ├── Paths.swift                     # all path + tool-location constants
    ├── ShellRunner.swift               # Process wrapper (sync + async)
    ├── StateSnapshot.swift             # polls lid/pmset/prompt dirs/ambient flag
    ├── TailnetPeer.swift               # `tailscale status --json` parser
    ├── IconRenderer.swift              # SF Symbol picker per state
    ├── MenuBuilder.swift               # NSMenu construction
    ├── PassphraseModal.swift           # NSAlert + NSSecureTextField
    └── PassphraseServer.swift          # Unix-socket JSON server with in-memory cache
```

## Build & install

```
./install.sh
```

Idempotent. Re-run after edits to rebuild, replace the binary, and bounce the launch agent.

### Manual

```
swift build -c release
cp .build/release/slab-menubar-swift ~/.local/bin/slab-menubar
sed "s|@HOME@|$HOME|g" computer.slab.menubar.plist.tmpl > ~/Library/LaunchAgents/computer.slab.menubar.plist
launchctl unload ~/Library/LaunchAgents/computer.slab.menubar.plist 2>/dev/null || true
launchctl load   ~/Library/LaunchAgents/computer.slab.menubar.plist
```

Logs: `/tmp/slab-menubar.out`, `/tmp/slab-menubar.err`.

## Relation to slab/install.sh

The Swift menubar replaces the Python menubar — `slab/install.sh` still sets up the daemon, hooks, sounds, and sudoers. Running `menubar-swift/install.sh` after that repoints the menubar plist at the compiled binary. Run order for a fresh Mac:

```
slab/install.sh                  # daemon + hooks + sounds + python menubar
slab/menubar-swift/install.sh    # overlay: replace python menubar with swift
```

Uninstall is handled by `slab/uninstall.sh`, which removes `computer.slab.menubar.plist` regardless of which binary it points to.

## Future

- Hot reload via [Inject](https://github.com/krzysztofzablocki/Inject): split into host exec + dylib so edits land in-process.
- First-class callers: `devault.fish`, `lith/deploy.fish`, npm session scripts.
- Richer status: lid sensor sparkline, ambient-synth peak, SSH session count.
