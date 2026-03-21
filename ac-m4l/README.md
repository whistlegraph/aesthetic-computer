# AC Max for Live Devices

This directory contains Max for Live device source files for Aesthetic Computer.

## AC Notepat

A Max for Live instrument that embeds the Aesthetic Computer notepat piece using `jweb~` (audio-enabled web browser).

### Building

```bash
# Build the .amxd file
python3 build.py

# Build and install to Ableton User Library (Mac)
python3 build.py --install
```

### Files

- `AC-Notepat.amxd.json` - Source JSON for the M4L device
- `AC Notepat.amxd` - Built M4L device (binary)
- `build.py` - Build script

### How it works

1. `jweb~` loads `https://localhost:8888/notepat?daw=1`
2. Web Audio from the page is captured by `jweb~`'s signal outlets
3. Audio routes through `plugout~` to Ableton's mixer

### Configuration

Key settings in the JSON source:

```json
{
  "openrect": [0.0, 0.0, 400.0, 169.0],  // Device width (height fixed at 169px by M4L)
  "devicewidth": 400.0,                   // Explicit device width
  "presentation_rect": [0.0, 0.0, 401.0, 170.0],  // jweb~ display size
  "latency": 32.0,                        // Audio latency buffer (ms)
  "url": "https://localhost:8888/notepat?daw=1"   // URL to load
}
```

### Requirements

- Ableton Live with Max for Live
- Max 9+ (for `jweb~` audio support)
- Aesthetic Computer dev server running on localhost:8888

### Development

To modify the device:
1. Edit `AC-Notepat.amxd.json`
2. Run `python3 build.py --install`
3. Reload the device in Ableton

Or edit directly in Max:
1. Open the device in Ableton
2. Click the wrench icon to edit in Max
3. Make changes and save
4. Extract updated JSON with: `cat "AC Notepat.amxd" | tail -c +37 > updated.json`
