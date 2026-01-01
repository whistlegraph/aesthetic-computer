# Aesthetic TV — tvOS App

A passive, cinematic playback surface for Aesthetic Computer tapes.

## Architecture

```
AestheticTV/
├── App/
│   ├── AestheticTVApp.swift    # Entry point
│   └── AppState.swift          # Global state
├── Models/
│   └── Tape.swift              # Tape data model
├── Services/
│   └── FeedService.swift       # API client
├── Player/
│   ├── PlayerView.swift        # AVPlayer wrapper
│   └── OverlayView.swift       # Metadata overlay
├── Views/
│   └── ContentView.swift       # Main view
└── Assets.xcassets/            # App icons & colors
```

## Building

1. Open in Xcode 15+ on macOS
2. Select "AestheticTV" target
3. Select Apple TV device or simulator
4. Build & Run

## Siri Remote Controls

| Button | Action |
|--------|--------|
| ▶︎ Play/Pause | Toggle playback |
| ← | Previous tape |
| → | Next tape |
| ↑ | Show overlay |
| ↓ | Hide overlay |

## API

The app fetches from:
```
GET https://aesthetic.computer/api/tv-tapes?limit=50
```

Response:
```json
{
  "tapes": [
    {
      "id": "abc",
      "title": "Tape ABC",
      "mp4": "https://at.aesthetic.computer/xrpc/...",
      "duration": 120,
      "resolution": "1280x720",
      "fps": 30,
      "created_at": "2025-01-01T00:00:00Z",
      "owner": "@jeffrey"
    }
  ],
  "total": 50,
  "lastUpdated": "2025-01-01T00:00:00Z"
}
```

## Development

### MVP Test
1. Run on Apple TV simulator
2. Verify tape feed loads
3. Test playback controls
4. Check overlay appears on swipe up

### Device Testing
1. Pair Apple TV: Settings → Remotes and Devices → Remote App and Devices
2. In Xcode: Window → Devices and Simulators → Add device
3. Select AestheticTV target → Apple TV device → Run
