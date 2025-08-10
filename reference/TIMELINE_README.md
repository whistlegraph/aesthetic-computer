# Ableton Live Timeline Viewer

A real-time visual timeline viewer for Ableton Live projects that parses XML files and displays live playback in the terminal.

## Quick Start

**One-command pipeline:**
```bash
cd reference
npm run timeline
```

This will automatically:
1. Find your `zzzZWAP_extracted.xml` file
2. Parse the Ableton project data
3. Start a live, real-time visual timeline in your terminal

## Features

- **Live Timeline Visualization**: See your project play in real-time with beat-accurate timing
- **Track Activity Display**: Visual representation of which tracks are active
- **Note Stream**: Rolling display of recent MIDI notes with pitch information
- **Progress Bar**: Visual progress through your song
- **Interactive Controls**: 
  - `SPACE` = Play/Pause
  - `Q` = Quit

## How It Works

1. **XML Parsing**: Uses a SAX parser to efficiently extract timeline data from Ableton's XML format
2. **Data Processing**: Organizes tracks, clips, MIDI notes, and timing information
3. **Real-time Playback**: Simulates playback using the project's tempo and timing
4. **Terminal Visualization**: Updates the display at 30fps for smooth animation

## What You'll See

```
🎵 Ableton Live Timeline Viewer
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
▶️  00:16.2 | Tempo: 120 BPM | Beat: 33.04
Progress: ██░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░ 6.2%
Active Notes: 2

Track Activity:
Track Name   │ Recent Notes
───────────────────────────────────
LOW          │                     
3 BASS       │ C2 F2               
8 TREM FX    │                     
9 FX 1       │                     
11 RATTLE HA │ C5 C5 C5            
A-Reverb     │                     
B-Delay      │                     

Recent Notes:
C5 C5 C5 C5 F2 C2

Controls: SPACE = play/pause, Q = quit
```

## Files

- `ableton-live-viewer.mjs` - Main viewer application
- `timeline.mjs` - Simple launcher script
- `package.json` - Updated with `npm run timeline` command

## Manual Usage

You can also run the viewer directly with a specific XML file:

```bash
node ableton-live-viewer.mjs /path/to/your/extracted.xml
```

## Requirements

- Node.js
- `saxes` package for XML parsing
- `chalk` package for colored terminal output

Dependencies are already installed in the reference directory.
