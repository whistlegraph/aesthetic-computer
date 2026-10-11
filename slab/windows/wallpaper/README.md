# slab wallpaper for Windows

The Windows counterpart of `slab/blueberry-wallpaper`, after jeffrey's verdict
on a drifting field ("too insane"): the still scattered-pals composition as
the background, and ONE animated element — a lavender pal mark in the lower
right that turns once every 40 seconds, bilinear and subpixel so it never
jitters, repainting only its own rectangle. The rest of the screen never
redraws.

- `wallpaper.c` — single-file Win32, software rendered into a 32-bit DIB.
- `pals_sprite.h` — alpha mask of `fedac/plymouth/pals.png` at 192 px wide;
  regenerate with `python3 gen-sprite.py`.
- `app.manifest` — embedded; a layered *child* window only works when the
  process declares Windows 8+ support.
- `build.ps1` / `deploy.sh [--run]` — same shape as `slab/menuband-win`.
  `deploy.sh` also converts `~\Pictures\ac-wallpaper.png` (the composition
  rendered on neo) to `ac-wallpaper.bmp` on the box, which the program loads
  with one `LoadImage` call and stretches to the screen; the slab gradient is
  drawn if the file is missing. `--run` relaunches inside the signed-in
  session through the ac-desk queue (`~\desk-queue\*.ps1`).
- Flags: `--static` (no animation at all), `--fps N` (pin the rate for
  measurements), `--float` (ordinary bottom-most window, to prove rendering).
- Autostart: HKCU `Run` value `AcWallpaper`. Single instance. Log with the
  window hierarchy at attach and a stats line every 10 s:
  `%TEMP%\slab-wallpaper.log`.

## Cost (CHUWI MiniBook X, 1920×1200 at 150 %)

| | |
|---|---|
| AC, 30 fps | 0.8 % of one core |
| battery, 15 fps | ~0.4 % |
| per frame | render 0.35 ms + blit 0.20 ms, box 104–170 px a side |
| working set | 26 MB |

Pauses while the session is locked, the display is off, or a full-screen app
is in front. Battery/AC is re-read every 3 s and on power broadcasts.

## How it sits behind the icons (Windows 11 24H2+, the "raised desktop")

Progman carries `WS_EX_NOREDIRECTIONBITMAP`, so an ordinary child window is
never composed. What works:

1. Send Progman `0x052C` with `wParam 0xD, lParam 1` **once**. Explorer
   reference-counts this; the old trailing `(0xD, 0)` / `(0, 0)` forms release
   the WorkerW and Explorer destroys it a millisecond later.
2. Be a **layered** child of Progman (`WS_EX_LAYERED`, alpha 255), z-ordered
   directly beneath `SHELLDLL_DefView` (the icons), and push Explorer's
   wallpaper `WorkerW` child to `HWND_BOTTOM`.

A plain Progman child, a child of the WorkerW, and a child of DefView all
render but stay invisible on 24H2. The classic (pre-24H2) WorkerW-sibling
layout is still handled as a fallback.
