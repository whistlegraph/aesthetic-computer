# toolchain/macos

Host-specific helpers that only make sense on a macOS machine.

## sticky.mjs — the "sticky the X" workflow

Drops text into a macOS **Stickies** note, styled the way @jeffrey
likes it: **translucent**, **larger text**, **sized to fit the content
closely**, and **centered on screen**.

When @jeffrey says *"sticky the &lt;thing&gt;"* on a macOS host, run this
on that thing.

```bash
node toolchain/macos/sticky.mjs path/to/file.txt   # note from a file
node toolchain/macos/sticky.mjs --text "a note"    # note from a string
echo "a note" | node toolchain/macos/sticky.mjs    # note from stdin
```

Flags: `--bigger N` (font bumps over the Stickies default, default 3),
`--opaque` (skip translucency), `--no-center` (leave it where it lands).

Needs **Accessibility** permission for the host terminal — the script
drives Stickies with `osascript` / System Events keystrokes (⌘N, ⌘V,
font bumps, ⌥⌘T). If the paste lands empty, the content is still on the
clipboard; just ⌘V into the note by hand.

## chrome-shot.mjs — safe headless Chrome screenshots

Wraps `Google Chrome --headless=new --screenshot=…` so it actually exits.
`--headless=new` reliably writes the PNG but often refuses to quit; left
unattended these orphans pile up, each pinning a `/tmp/chrome-*` profile
dir, and eventually `open -a "Google Chrome"` stops opening windows at
all. This wrapper polls the output, kills Chrome the moment the file
stabilises, cleans up the ephemeral profile, and reaps any headless
Chromes older than 2 minutes before it starts.

```bash
node toolchain/macos/chrome-shot.mjs <url> <out.png> \
  [--size WxH] [--budget MS] [--wait MS] [--full-page]
```

Use this anywhere you'd otherwise type a raw `Google Chrome --headless=new …`
incantation (TL site screenshots, paper previews, etc.). Never spawn a
loose headless Chrome — always go through this script.

## to-phone.mjs — push files to the iPhone (zero taps)

Sends files from this Mac to @jeffrey's iPhone over the shared iCloud
account (same Apple ID on both) — no AirDrop, no accept prompt, no drag.
Two rails, auto-picked per file:

- **photos & videos** → imported into Photos → iCloud Photos syncs them to
  the iPhone **camera roll** (ready to post to Instagram, etc.)
- **everything else** → copied into **iCloud Drive** → shows up in the Files app

```bash
node toolchain/macos/to-phone.mjs <file...>
node toolchain/macos/to-phone.mjs --album "pals" *.png    # into a Photos album
node toolchain/macos/to-phone.mjs --drive report.pdf      # force iCloud Drive
node toolchain/macos/to-phone.mjs --folder pals doc.pdf   # Drive subfolder
```

Flags: `--photos` / `--drive` force a rail, `--album NAME` (Photos, created if
missing), `--folder NAME` (iCloud Drive subfolder, default `to-phone`). Needs
iCloud Photos + iCloud Drive on (both enabled on this host). Sync lands in a
few seconds. Complements `iphone-tap/` (which drives the phone via iPhone
Mirroring) — this one gets files *onto* the phone.
