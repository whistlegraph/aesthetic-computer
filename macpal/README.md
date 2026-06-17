# MacPal

A friendly little character who lives in the corner of your screen.

This is the first, deliberately-tiny MacPal: **star** — a star who floats in a
corner, blinks and leans on his own, springs when you hover, squashes when you
press, and rides along when you drag him (snapping to the nearest corner with a
little clink on release). Click him and he **escapes** — collapses into a micro
chip tucked into his corner with his name beside him; click again and he tinks
back to full size. His corner, color, and collapsed state persist across launches.

It is the seed of a distributable MacPal (an eventual Mac App Store build).

## What he can do

- **Right-click menu** — *About MacPal*, *Gold Star* / *Silver Star* (live
  toggle, checkmarked), *Quit MacPal*.
- **Gold or silver** — a full silver pose set ships alongside the gold one; the
  choice is remembered and even the About masthead follows it.
- **About window** — modeled on the Menu Band About panel: star masthead,
  "MacPal", "a friendly little helper", and the dedication *"This MacPal was made
  for Fía and is maintained by @jeffrey"* (the í wears an acute accent **and** the
  live system-accent color), version, and copyright.
- **Menu Band compatibility** — MacPal creates/polls
  `~/.local/share/desktop-badge/note`, the existing signal file Menu Band writes
  `<seq> <noteName>` to when it plays. On each note the star opens his mouth and
  floats a pitch-colored ♪ — no Menu Band changes needed, they ship independently.
  New signal kinds can land in the same dir for future compatibility.

## Build & run (local dev)

```bash
./build.sh            # → build/MacPal.app (ad-hoc signed)
./build.sh --install  # build, copy to /Applications, and launch
```

On first launch MacPal adds itself to **Login Items** (via `SMAppService`) so the
pal is always there. Remove it any time in *System Settings → General → Login
Items* — it won't re-add itself.

## Package to hand to someone (AirDrop / email)

```bash
./package.sh              # sign (Developer ID) + notarize + staple
SKIP_NOTARIZE=1 ./package.sh   # signed only, faster (recipient right-click→Open)
```

Produces a **notarized, stapled** app that opens with no Gatekeeper block:

- `dist/MacPal.app` — signed bundle
- `dist/MacPal.zip` — ~900 KB, for email/download
- `~/Desktop/MacPal.app` — loose copy to **AirDrop**

The recipient just drags it to Applications and opens it (one normal
"downloaded from the internet, are you sure?" prompt the first time). Notary
credentials are read from `aesthetic-computer-vault/apple/app-specific-password.env`;
signing uses the *Developer ID Application: Jeffrey Scudder (FB5948YR3S)* cert.

## Icon

`make-icon.sh` renders `Resources/AppIcon.icns` (gold star on a sky-blue
squircle) from `star-glyph.svg` via `rsvg-convert` + `iconutil`. Re-run it after
changing the star art. `package.sh` regenerates it automatically if missing.

## Profiles & plugins

One binary, two lives, chosen by launch profile:

- **(no args)** → the **star**, for Fía, with the **affirmations** plugin (a
  caption @jeffrey can change remotely — see below). This is the App-Store build.
- **`--profile fuser --name panda --corner TL [--repo …]`** → the **fuser badge**
  for the fleet (neo / panda / chicken / blueberry): git status, Asana tasks,
  ⚡OVERTIME, and a live terminal pane, via the **fuser** plugin.

The avatar (float, poses, name wiggle, hover, drag-to-corner, collapse, Menu
Band sing) lives in `PalCore.swift` and is shared. Everything that stacks
*beneath the name* is a `PalPlugin`.

## Affirmations (remote status under the star)

The star polls `GET /api/macpal-status?to=fia` every ~45s and shows the message
under herself; a new one slides in with a hop and a soft chime, and the last is
cached so it persists offline. @jeffrey pushes one (admin-only) with:

```bash
node macpal/affirm.mjs "proud of you 💛"        # → to=fia, production
npm run affirm "great work today ✨" -- --to fia
node macpal/affirm.mjs "testing" --local         # → https://localhost:8888
node macpal/affirm.mjs --clear                   # blank the caption
```

Auth uses `~/.ac-token` (run `node tezos/ac-login.mjs` once). The endpoint is
`system/netlify/functions/macpal-status.mjs` (Redis-backed, served by lith).

## Layout

```
macpal/
├── build.sh                  swiftc Sources/*.swift → .app, sign, optional install
├── package.sh                Developer ID sign + notarize → AirDrop-ready
├── make-icon.sh              render Resources/AppIcon.icns from the star
├── affirm.mjs                CLI: push an affirmation to a star (admin-only)
├── Sources/
│   ├── main.swift            entry: parse profile → PalConfig → attach plugins
│   ├── PalCore.swift         shared avatar + PalPlugin protocol + About
│   ├── AffirmationsPlugin.swift   Fía: remote affirmation caption
│   └── FuserPlugin.swift     fleet: git · Asana · OVERTIME · terminal pane
├── Resources/
│   ├── Info.plist            bundle metadata (LSUIElement, min macOS 13)
│   ├── star-glyph*.svg       gold poses: home · blink · lean · sing  (bundled)
│   ├── star-silver*.svg      silver poses                            (bundled)
│   └── {neo,panda,chicken}-glyph*.svg   fleet glyphs (installer source, not bundled)
└── fuser/                    symlinks into the vault (client-sensitive deploy bits)
    ├── install.sh, deploy.sh        build the badge from Sources, flip the fleet
    └── badge-asana-sync.sh, …       (kept in aesthetic-computer-vault)
```

Per-user star state (corner, color, collapsed flag, last affirmation) lives in
`~/Library/Application Support/MacPal/`; the fuser badge uses
`~/.local/share/desktop-badge/`. Both share the Menu Band note signal at
`~/.local/share/desktop-badge/note`.

## Swap the avatar

Replace the `star-glyph*.svg` / `star-silver*.svg` files (128×128 viewBox,
transparent background — the app draws the accent drop-shadow) and rebuild. The
loader cycles through the `home`/`-2`/`-3` poses for the active color and uses
`-sing` for Menu Band note pings; one SVG is enough, the rest give it life.

## Lineage

MacPal began as the charm carved out of the fleet `desktop-badge`. The two have
now merged the other way: the badge's features (git status, Asana, OVERTIME, the
terminal pane) returned as the `FuserPlugin`, the shared avatar became
`PalCore`, and the star gained the `AffirmationsPlugin`. The old
`aesthetic-computer-vault/fuser/bin/desktop-badge/desktop-badge.swift` is
superseded by `macpal --profile fuser`.
