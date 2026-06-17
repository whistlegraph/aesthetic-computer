# iphone-tap

A tiny macOS CLI that gives scripts **eyes and hands** on a real iPhone through
the system **iPhone Mirroring** app. It's the reusable primitive under in-house
iPhone automations — the first being Instagram grid archiving
(`portraits/jeffrey/bin/ig-archive-mirror.mjs`).

The tap + window-frame code is lifted from **YergerSnap** (`slab/yergersnap`),
which has driven Julia Yerger's stop-motion rig in production. This version adds
screen capture and on-device OCR so a caller can *see* the screen, not tap blind.

## Build

```bash
toolchain/macos/iphone-tap/build.sh
```

Unsigned, local-use. Grant the **terminal app that runs it** two permissions
(System Settings → Privacy & Security):

- **Accessibility** — read window frames + inject clicks (`frame`, `tap`)
- **Screen Recording** — capture pixels (`shot`)

## Commands (all print JSON)

| Command | Result |
|---|---|
| `iphone-tap frame [--window NAME]` | `{x,y,w,h}` window bounds (points) |
| `iphone-tap shot OUT.png [--window NAME]` | captures the window region to PNG |
| `iphone-tap ocr IN.png` | `{w,h,lines:[{text,conf,x,y,w,h}]}` — coords normalized **0–1, top-left** |
| `iphone-tap tap FX FY [--window NAME] [--no-activate]` | click at fraction `(FX,FY)` of the window |

OCR coordinates drop straight into `tap` — find a control's box, tap its center.

Default `--window` is `"iPhone Mirroring"`. Errors print `{"error":"..."}` and
exit non-zero so callers can branch.

## The black-capture gotcha

iPhone Mirroring often **captures as solid black** (Continuity privacy). Two ways
through, in order of preference:

1. **Grant Screen Recording** to the terminal — on recent macOS this is often
   enough for a real capture.
2. If it's still black, open a **QuickTime → File → New Movie Recording** with the
   iPhone (over USB) as the source. That window captures fine. Point `shot`/`ocr`
   at it with `--window "Movie Recording"` for *reading*, while `tap` still drives
   the interactive `"iPhone Mirroring"` window. Both are scaled views of the same
   phone screen, so a fraction found in one maps directly to the other.

## Smoke test

```bash
# open iPhone Mirroring first
./iphone-tap frame
./iphone-tap shot /tmp/phone.png && open /tmp/phone.png   # not black?
./iphone-tap ocr /tmp/phone.png                            # finds on-screen text?
```
