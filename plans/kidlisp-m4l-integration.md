# KidLisp.com ↔ Max for Live Integration

## Overview
Connect KidLisp.com IDE to Ableton Live via Max for Live device using session-server WebSockets.

## Architecture

```
┌─────────────────┐                    ┌─────────────────┐
│  KidLisp.com    │                    │  M4L Device     │
│  (Browser IDE)  │                    │  (jweb~ in Max) │
└────────┬────────┘                    └────────┬────────┘
         │                                      │
         │  WebSocket                 WebSocket │
         │                                      │
         └──────────────┬───────────────────────┘
                        │
              ┌─────────▼─────────┐
              │  session-server   │
              │  (daw: channel)   │
              └───────────────────┘
```

## Device Publishing & Distribution

### Asset Location
M4L devices are published to:
```
https://assets.aesthetic.computer/m4l/
```

### Device Files
| Device | URL |
|--------|-----|
| AC KidLisp | `https://assets.aesthetic.computer/m4l/AC-KidLisp.amxd` |
| AC Notepat | `https://assets.aesthetic.computer/m4l/AC-Notepat.amxd` |
| AC Metronome | `https://assets.aesthetic.computer/m4l/AC-Metronome.amxd` |
| AC Prompt | `https://assets.aesthetic.computer/m4l/AC-Prompt.amxd` |

### Build & Deploy Process
```bash
# Build all devices
cd ac-m4l
python3 build.py --production

# Upload to assets bucket
npm run assets:sync:up -- --include "m4l/*"

# Or deploy specific device
aws s3 cp "AC KidLisp.amxd" s3://aesthetic-computer-assets/m4l/AC-KidLisp.amxd
```

### Versioning
- Keep versioned copies: `m4l/AC-KidLisp-v1.0.0.amxd`
- Latest always at: `m4l/AC-KidLisp.amxd`
- Version history in `ac-m4l/CHANGELOG.md`

### Download Link in KidLisp.com
The Ableton boot screen includes a direct download link:
```html
<a href="https://assets.aesthetic.computer/m4l/AC-KidLisp.amxd" download>
  Download AC KidLisp.amxd
</a>
```

## Session-Server Changes

### New Channel Type: `daw:`

```javascript
// Join a DAW channel
{ type: "daw:join", handle: "jeffrey", device: "browser" }  // from kidlisp.com
{ type: "daw:join", handle: "jeffrey", device: "AC KidLisp M4L" }  // from M4L

// List connected devices
{ type: "daw:clients" }
→ { type: "daw:clients", clients: [
    { handle: "jeffrey", device: "browser", id: 123 },
    { handle: "jeffrey", device: "AC KidLisp M4L", id: 456 }
  ]}

// Send code to all devices in channel
{ type: "daw:code", lisp: "(wipe red)" }

// Receive code (broadcast to all except sender)
{ type: "daw:code", from: "browser", lisp: "(wipe red)" }
```

## KidLisp.com Changes

### Ableton Platform Mode
When platform = "ableton":
1. Connect to session-server with `daw:join`
2. Show connected M4L devices in boot screen
3. Play button sends `daw:code` instead of running locally
4. Show connection status indicator

### UI Elements
- Boot screen shows "Connected Devices: AC KidLisp (Ableton)" 
- Orange indicator when M4L device connected
- "No devices connected" message when waiting

## Max for Live Device: "AC KidLisp"

### Device Structure
```
┌──────────────────────────────────────────────────┐
│  AC KidLisp                              [▶][■]  │
├──────────────────────────────────────────────────┤
│  ┌────────────────────────────────────────────┐  │
│  │                                            │  │
│  │        KidLisp Preview Canvas              │  │
│  │        (jweb~ running kidlisp runtime)     │  │
│  │                                            │  │
│  └────────────────────────────────────────────┘  │
│  Status: Connected to session-server             │
│  Channel: jeffrey                                │
└──────────────────────────────────────────────────┘
```

### Components
- `jweb~` - Loads headless KidLisp runtime + WebSocket client
- `plugout~` - Routes audio to Ableton mixer
- `live.text` - Play/Stop buttons
- Status display showing connection state

### M4L URL
```
https://aesthetic.computer/kidlisp-player?daw=1&channel=jeffrey
```

Or for local dev:
```
https://localhost:8888/kidlisp-player?daw=1&channel=jeffrey
```

## New Piece: `kidlisp-player`

A headless KidLisp runtime that:
1. Connects to session-server on load
2. Joins the `daw:` channel from URL param
3. Listens for `daw:code` messages
4. Executes received KidLisp code
5. Renders to canvas (visible in jweb~)
6. Outputs audio via Web Audio (captured by jweb~)

## Implementation Steps

### Phase 1: Session-Server DAW Channels
- [ ] Add `dawChannels` map to session-server
- [ ] Handle `daw:join`, `daw:leave`, `daw:clients`, `daw:code` messages
- [ ] Broadcast code to all channel members except sender

### Phase 2: kidlisp-player Piece  
- [ ] Create `/system/public/aesthetic.computer/disks/kidlisp-player.mjs`
- [ ] Embed KidLisp interpreter
- [ ] Connect to session-server WebSocket
- [ ] Execute received code on canvas

### Phase 3: M4L Device
- [ ] Create `AC KidLisp.amxd` in ac-m4l/
- [ ] jweb~ pointing to kidlisp-player
- [ ] Audio routing via plugout~
- [ ] Channel name input (live.text or live.menu)

### Phase 4: KidLisp.com Integration
- [ ] Modify Ableton platform mode to use daw: channels
- [ ] Show connected devices in boot screen
- [ ] Send code via WebSocket on Play

## Authentication / Security

- Channel names could be user handles (require login)
- Or use generated room codes (like session rooms)
- M4L device could show QR code to connect from kidlisp.com

## Future Ideas

- Bidirectional: M4L sends Ableton tempo/transport state to kidlisp.com
- Multiple M4L instances (different tracks) receiving same code
- Record KidLisp visuals as video in Ableton
- MIDI input from Ableton controls KidLisp params
