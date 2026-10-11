# slab on Windows

`slab-agent.ps1` is the `frame` agent for a Windows 11 machine. It speaks
the same file protocol as `FrameCapture.swift` inside the Mac menubar app,
so `frame <machine>` from a Mac works unchanged: the client ships its
request over `ssh host bash -s`, Git bash answers on Windows, and the agent
(resident in the signed-in desktop session) produces the envelope.

- Observe: GDI screen capture (foreground window by default, `screen` for
  all of it, `crop=x,y,w,h`), Windows.Media.Ocr text with click centres, a
  UI Automation walk of the foreground window as the `ax` tree, visible
  windows, cursor, DPI. JPEG thumb ≤ 1568 px.
- Act: `native-click=` / `native-drag=` through SendInput.
- Coordinates are physical pixels (`coordinateSpace: windows-physical-pixels`).

Install on the box: copy to `~\src\slab\slab-agent.ps1`, put Git's `bin`
on the machine PATH (for `bash -s`), start it inside the desktop session
(a scheduled task running as the user, or the HKCU Run key `SlabAgent`).
Register the machine in `~/.config/slab/puppet.json` with an `sshHost`.
Windows PowerShell 5.1 only; it is the one with WinRT.

Timings on a MiniBook X: ~0.7 s warm (OCR dominates), ~4 s cold.
