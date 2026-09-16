# Easel interface contract

`easel` opens the standalone desktop app. `ac` opens Easel inside the current
terminal, with optional Slab integration. They are two hosts for the same making
interface and the same project data. Neither interface may introduce a separate
conversation model, command dialect, artifact history, or meaning for an action.

## Entry points

| Invocation | Behavior |
|---|---|
| `easel` | Activate the existing desktop window, or reopen its last workspace. |
| `easel .` / `easel DIRECTORY` | Select an existing workspace when starting the desktop app. |
| `easel --cwd DIRECTORY` | Explicit spelling of that workspace selection. |
| `ac` / `ac DIRECTORY` | Run the shared TUI in this terminal, defaulting to the current directory. |
| `easel --help` | Explain desktop launch options without starting the app. |

The macOS desktop launcher uses `open -a ~/Applications/Easel.app`, without
`-n`. It does not create another app instance. If the app is already running,
macOS activates it; new workspace/backend/model arguments are startup options,
not a promise to replace an active session. Quit the app before starting it with
a different workspace. A future explicit workspace-switch action should use the
same project selection contract in both hosts.

`bin/easel` remains the native terminal launcher for compatibility. The installed
`~/.local/bin/ac` points there; `~/.local/bin/easel` points to `bin/easel-desktop`.
`EASEL_DESKTOP_APP` can select another installed macOS app bundle. Other platforms
can use the native terminal interface; this desktop launcher is macOS-specific.

Ordinary launches open New media / Threads before connecting a provider. Tab
switches tabs; arrows choose and Enter opens. Native and desktop snapshots share
the thread catalog. Explicit resume/piece/prompt launches and checkpointed app
restarts bypass this chooser.

## Visible parity is required

Keep the same visible layout, labels, actions, keyboard shortcuts, and state:

- The conversation scrolls internally above a fixed composer/footer.
- EASEL opens `/about`; the @handle opens that user's profile.
- Medium, current artifact/version, and conversation engine remain visible.
- Picture, Sound, Piece, Paper, and Game Boy use the same `/medium` and artifact
  selection actions. `/new` starts fresh work in the current medium; `/new thread`
  starts a conversation while retaining the selected work.
- The artifact is always in the top-left preview. Its position and purpose do
  not change across media. The share QR occupies the opposite corner when one
  exists. Expand, collapse, and full-screen actions need equivalent visible
  controls and keyboard routes in each implementation. Hover enlarges
  the preview; ordinary left/right clicks belong to the artifact. Clicking the
  QR opens its destination in the browser.
- Pending work retains the last usable preview. A version becomes visually ready
  after its output loads, not when a token arrives. Failed work retains the last
  good artifact and reports the failure. Reduced-motion preferences suppress
  shake/particles without hiding the state change.
- Sound never autoplays on creation, refresh, or rollback. Game Boy uses the
  actual compiled cartridge, with the same joypad mapping in every emulator host.

Host primitives may differ: Electron supplies a window, PTY, clipboard, browser,
and embedded preview; a native terminal supplies its own text surface and Slab
can supply the preview/window controls. This permits different implementation
code, not a visibly different Easel product. Slab must remain optional: the
standalone app works without its daemon, and `ac` still works without overlays.

## Shared actions and storage

Both hosts execute the same `src/tui.mjs` and command implementations. `/about`,
`/backend`, `/model`, `/medium`, `/artifacts`, `/select`, `/artifact`, `/versions`,
`/rollback`, `/open`, `/export`, `/login`, `/logout`, `/whoami`, and interruption
retain their meaning. Picture `/publish` explicitly publishes its accepted PNG as an AC painting and
`/qr` shows the verified painting #code route. Piece `/publish` retains software
publication. `/autopublish`, `/live`, and `/performance` remain Piece-specific;
other media must not silently reinterpret these actions.
A missing tool gives the same capability/error explanation in both hosts.

An @handle uses the shared `~/.ac-token` sign-in, including its existing refresh
flow. Desktop is not a second identity. AC-hosted inference and a user's Claude
or Codex CLI remain explicit provider choices. Switching the provider/model
preserves the conversation handoff and selected artifact; it does not switch
media providers or claim to transfer private vendor reasoning state.

Workspaces, piece source, artifact manifests, accepted output bytes, thread
records, and revision history share their existing formats. Selecting the same
workspace must expose the same artifacts and versions. Host-specific window
geometry and last-workspace convenience files are not a second project store.
Concurrent writers need coordination; opening a project in two hosts is not
permission to overwrite one another's state.

Rollback restores recorded source/assets/output together without regenerating
paid media. `v1`, `v2`, etc. refer to accepted artifact revisions. A changed Game
Boy source may retain its last built ROM as a preview while explicitly reporting
that a build is needed; it must not export that ROM as if it matched new source.

A QR encodes an actually reachable published Piece URL, with the same identity,
label, and destination in desktop and Slab. A local-only Picture, Sound, Paper,
or ROM does not gain a pretend public URL. Export is separate from publication.
Picture now has that explicit action: `/publish` uploads only the accepted PNG
and exposes its server-issued #code after owner and byte verification. Sound,
Paper, and ROM publication still need an explicit shared action before either
host shows a public share QR for them.

## Current gaps and verification

This is the parity requirement, not a claim that every interaction already
matches. Current gaps include:

- Desktop and Slab implement preview expansion, focus, and full-screen controls
  separately. Their geometry, keyboard restoration, and hover behavior still
  need a paired visual/interaction regression check.
- Text selection, clipboard shortcuts, scrollback, font zoom, and context menus
  cross Electron/xterm versus native-terminal boundaries. Shared TUI scrolling
  exists, but complete host equivalence is not yet established.
- Native terminal launch currently defaults to Claude; desktop launch defaults
  to AC. Explicit `--backend` agrees, but default selection needs one shared rule.
- Desktop startup arguments cannot switch an already-running app's workspace.
- Simultaneous editing of the same workspace by desktop and native sessions is
  not a tested collaborative editing feature.
- Media-specific optional capabilities still apply: Paper needs TeX for PDF
  builds, Game Boy needs GBDK, and paid raster generation needs explicit provider
  credentials and user authorization. A host must not hide these limits.

On Blueberry, zsh currently finds `/usr/sbin/ac` before `~/.local/bin/ac` and has
no overriding `ac` function. Use `~/.local/bin/ac` or put `~/.local/bin` earlier
in PATH. No zsh configuration was overwritten. The tracked Fish integration now
routes `ac` to `~/.local/bin/ac` and `easel` to `~/.local/bin/easel`; already-open
Fish sessions retain their old function until they source the updated
`easel/shell/easel.fish` or start a new shell.

Release parity checks should exercise the same sequence in both hosts: sign in,
create/select each medium, make an update, inspect its preview, switch engines,
start a new thread, roll back, export, interrupt work, select/copy text, and
expand/exit the preview. Compare the visible controls and resulting state, not
only passing unit tests.

## Planned platform adapters

Windows and Linux are planned adapters, not currently shipped equivalents.
Windows/PowerShell should route `easel` to its desktop app and `ac` to the same
terminal core, with explicit argument passing and shared workspace/auth formats.
Linux should do the same under X11 and Wayland. Window placement, focus, global
shortcuts, clipboard, and overlay availability must be reported as host
capabilities; Wayland compositor restrictions must not be hidden behind controls
that silently fail. These adapters keep the same visible actions and state,
while replacing only the OS-specific window, terminal, and launch primitives.
