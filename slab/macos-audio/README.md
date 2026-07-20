# AC Mac Audio

First-party, macOS-only synchronized audio infrastructure for JukeWizard,
MenuBand, and other Aesthetic Computer host apps. It intentionally owns a
small protocol instead of embedding a cross-platform multi-room server.

The sender captures one application through Core Audio's process-tap API,
converts it to 48 kHz stereo PCM, and sends timestamped sub-MTU UDP packets.
Every receiver continuously estimates the sender's monotonic clock and renders
the samples whose presentation timestamps match its hardware callback. Clock
drift is therefore corrected continuously rather than only at startup.

```sh
swift build -c release --package-path slab/macos-audio

# Non-disruptive channel test on neo:
slab/macos-audio/.build/release/ac-audio-room tone

# On each listener:
ac-audio-room receive --host neo.local --channel left  --name neo
ac-audio-room receive --host neo.local --channel right --name blueberry

# Capture only Spotify and mute its original direct output:
ac-audio-room send --app Spotify
```

The first per-app capture prompts for macOS **Screen & System Audio Recording**
permission. The default 700 ms presentation buffer favors stable Wi-Fi; this is
room playback infrastructure, not a live instrument monitor.

Use `send --keep-original` for a non-disruptive capture test; normal room mode
mutes the tapped app so its direct speakers do not echo the synchronized feed.

Receivers accept `--gain 0...2`; `--gain 0` is useful for silent network and
clock verification.

Wire protocol version 1 lives in `RoomProtocol.swift`. It is intentionally
independent from JukeWizard and MenuBand UI so both apps can use the same
sender/receiver types directly.
