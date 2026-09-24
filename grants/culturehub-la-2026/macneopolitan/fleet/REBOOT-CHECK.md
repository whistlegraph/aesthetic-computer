# Next laptop OS test

No reboot has been requested yet. The live controls and new arrangement work
must be accounted for before rebooting any of the six laptops.

1. Finish or stop the shared cue. Verify native playback stopped, SUB READY,
   room blackout acknowledged and held-center RGB zero.
2. Save the chosen performance bundle, score, battery overlay, brightness
   controller and `performance-controls.json` in the USB overlay on both boot
   partitions. Preserve the previous boot image and checksum manifest.
3. Preserve each laptop's seat and display orientation. Current preference:
   audio-hit backlight mode, 0 when idle; 25% software master with hardware
   mixers at100; optional large-note display. Do not boot into automatic music.
4. Fix the separate Wi-Fi connection TTS before claiming the requested quiet
   boot: the current boot runtime still says “connected to CULTUREHUB LA”.
   The requested replacement is a brief sine melody; it has not been installed.
   The earlier personalized Jeffrey greeting patch is already on the USBs.
5. Test one laptop first, verify seat, power/bolt indicator, brightness0 at idle,
   volume, network, and readiness. Only then repeat across the remaining five.
6. Re-probe all native audio clocks after reboot; restart the corrected SUB
   server to load the new calibration. Verify score hash/duration, 25%, both
   channels, fullscreen and audio enabled before a spoken test cue.
7. Repeat a short audio/light test, then rerun the orchestral benchmark with
   identical score/effects/light settings if measuring a performance change.

The original accepted replay, full stress baseline, corrected SUB timing,
and newer candidates are distinct artifacts. See [CONNECTIONS.md](CONNECTIONS.md)
and the [baseline report](../../../../fedac/native/docs/performance/orchestral-stress-2026-09-24.md).
The Femrag++ sample segment requires its own transport/readiness check before
it can be included in a native fleet cue.
