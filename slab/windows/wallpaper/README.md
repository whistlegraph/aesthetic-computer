# slab live wallpaper for Windows

The Windows counterpart of `slab/blueberry-wallpaper`: the slab dark gradient
with the pals marks drifting up the screen, swaying and spinning in the five
tints, thirteen of them (the Mac's eight MarkSpec rows plus five smaller).

- `wallpaper.c` — single-file Win32, software rendered (32-bit DIB, dirty
  rectangles only), ~7 % of one core at 24 fps on a MiniBook X (the layered-child redirection is most of it; 30 fps cost ~12 %).
- `pals_sprite.h` — alpha mask of `fedac/plymouth/pals.png` at 192 px wide;
  regenerate with `python3 gen-sprite.py`.
- `app.manifest` — embedded; a layered *child* window only works when the
  process declares Windows 8+ support.
- `build.ps1` / `deploy.sh [--run]` — same shape as `slab/menuband-win`.
  `--run` relaunches inside the signed-in session through the ac-desk queue.

## How it sits behind the icons (Windows 11 24H2+, the "raised desktop")

Progman carries `WS_EX_NOREDIRECTIONBITMAP`, so an ordinary child window is
never composed. The recipe that works:

1. Send Progman `0x052C` with `wParam 0xD, lParam 1` **once**. Explorer
   reference-counts this; the old trailing `(0xD, 0)` / `(0, 0)` forms release
   the WorkerW and Explorer destroys it a millisecond later.
2. Be a **layered** child of Progman (`WS_EX_LAYERED`, alpha 255), z-ordered
   directly beneath `SHELLDLL_DefView` (the icons), and push Explorer's
   wallpaper `WorkerW` child to `HWND_BOTTOM`.

Modes 1–3 (`--mode N`) are the attempts that render but stay invisible on
24H2: plain Progman child, child of the WorkerW, child of DefView. `--float`
shows the animation as an ordinary bottom-most window for checking rendering.
Pauses while locked, display off, or a full-screen app is in front. Log:
`%TEMP%\slab-wallpaper.log` (window hierarchy at attach).
