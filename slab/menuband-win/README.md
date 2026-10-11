# Menu Band for Windows

The Mac app's menu-bar piano, rebuilt for the Windows 11 taskbar.

Windows 11 removed deskbands, so there is no supported way to put a control
inside the taskbar. The strip is instead a borderless, topmost,
non-activating layered window that parks itself just left of the system tray
and follows the taskbar through moves, DPI changes and full-screen apps.
Clicking a key never steals focus from the app you were using.

Sound is the same GM synthesis core the Mac app and AC OS run
(`fedac/native/src/gm_synth.c`, copied in by `deploy.sh`), rendered over
WASAPI with the polyphony, voice stealing and attack/release contour from
`MenuBandGMSynth.swift`.

- `menuband.c` — the whole app: synth host, WASAPI, strip rendering, taskbar tracking, tray menu.
- `build.ps1` — compiles with the VS 2022 Build Tools (`cl.exe`), no other dependencies.
- `deploy.sh` — from a Mac: copies the sources to a Windows box over ssh, builds, and with `--run` relaunches the strip inside the signed-in desktop session.

Play: click or drag across the keys. Scroll wheel shifts octave. Right-click
the strip or the tray icon to change instrument or quit.

Background and the Windows feasibility notes: https://aesthetic.computer/menuband/windows.html
