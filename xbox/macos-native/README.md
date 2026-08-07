# oskiewar for macOS

The installed macOS game is an AppKit application with no WebView. JavaScriptCore
runs the same `xbox/live/hello.js` lifecycle as Xbox; AppKit/CoreGraphics,
AVFoundation, and GameController provide native graphics, audio, and input.

```bash
npm run oskiewar:mac:build
npm run oskiewar:mac:install
```

`install` places `oskiewar.app` in `/Applications`, pins it to the Dock once,
and opens it. Re-running it safely replaces that app with the current shared
game source.
