# AC Xbox Native Latency

Minimal x64 UWP shim for Xbox Developer Mode. It polls `Windows.Gaming.Input`,
submits a preallocated XAudio2 buffer, flashes Direct3D, and logs QPC timing.

- Face and shoulder buttons trigger the probe.
- Menu cycles 44.1, 48, and 96 kHz.
- Yellow marks a trigger; blue, green, and magenta identify sample rate.

GitHub Actions produces the signed package. Device Portal credentials remain
only on Blueberry and are never sent to GitHub.
