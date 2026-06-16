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

## Layout

```
macpal/
├── build.sh                  swiftc → .app bundle, sign, optional install
├── package.sh                Developer ID sign + notarize → AirDrop-ready
├── make-icon.sh              render Resources/AppIcon.icns from the star
├── Sources/
│   └── MacPal.swift          the whole app, one file
└── Resources/
    ├── Info.plist            bundle metadata (LSUIElement, min macOS 13)
    ├── star-glyph*.svg       gold poses: home · blink · lean · sing
    └── star-silver*.svg      silver poses: home · blink · lean · sing
```

Per-user state (chosen corner, color, collapsed flag) lives in
`~/Library/Application Support/MacPal/`.

## Swap the avatar

Replace the `star-glyph*.svg` / `star-silver*.svg` files (128×128 viewBox,
transparent background — the app draws the accent drop-shadow) and rebuild. The
loader cycles through the `home`/`-2`/`-3` poses for the active color and uses
`-sing` for Menu Band note pings; one SVG is enough, the rest give it life.

## Lineage

Carved down from the `desktop-badge` corner character in the slab/fuser tooling,
stripped of git status, Asana tasks, OVERTIME mode, the terminal pane, and the
display glow — keeping the charm (floating avatar, drag-to-corner snap,
click-to-collapse, idle pose cycling, name wiggle, hover spring, and the Menu
Band sing) plus a focused right-click menu (About / color / Quit).
