# TrackDrum

TrackDrum turns the active Mac trackpad into Menu Band’s percussion surface.
Its borderless window is drawn at the touch device’s reported physical point
range, uses the production Metal membrane and audio engine, and quits with
Escape.

The app has one capability contract for every distribution channel:

- public AppKit `NSTouch` input while TrackDrum is frontmost;
- App Sandbox enabled;
- no private frameworks, global event taps, helper processes, login items, or
  persistent background mode;
- one bundle identifier: `computer.aestheticcomputer.trackdrum`.

Build the signed universal direct-download artifact:

```bash
./build-dmg.sh
```

Verify the App Store target without signing:

```bash
./build-app-store.sh --verify
```

Create an App Store archive and signed installer package:

```bash
./build-app-store.sh --archive
```

Both routes compile the same app sources and ship the same feature set. Only
the distribution signature and container differ.
