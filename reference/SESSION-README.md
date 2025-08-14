# Ableton Live Session View Visualizer

A real-time terminal visualization of Ableton Live projects that simulates the **session view** interface.

## Features

- **Session Grid**: Visual grid showing clips in session slots with real-time playback indicators
- **Track Activity**: Live meters showing activity levels for each track  
- **Aggregate Data Stream**: Combined output visualization showing how data flows through the session
- **Interactive Triggering**: Trigger clips and scenes like in Ableton Live
- **Auto-triggering**: Clips automatically trigger other clips for dynamic sessions

## Usage

### Quick Start
```bash
./session
```

### Manual Usage
```bash
node ableton-session-viewer.mjs [path-to-xml]
```

## Controls

- **SPACE**: Play/Pause the session
- **1-8**: Trigger scenes (horizontal rows)
- **T**: Random clip trigger
- **Q**: Quit

## Visual Elements

### Session Grid
- `[░░░]` - Empty/inactive clip
- `[▁▁▁]` - Low activity clip
- `[▃▃▃]` - Medium activity clip  
- `[▅▅▅]` - High activity clip
- `[███]` - Peak activity clip

### Track Meters
- Real-time activity bars showing note/audio output
- Percentage indicators
- Color-coded by track

### Aggregate Stream
- Bottom visualization showing combined session output
- Shows data flow and density over time
- Color-coded intensity levels

## Pipeline

The tool automatically:
1. Parses Ableton XML project files
2. Creates a session grid with clips distributed across tracks and scenes
3. Simulates real-time playback with clip triggering
4. Generates aggregated output visualization
5. Provides interactive session control

Perfect for analyzing project structure and visualizing how musical elements interact in real-time!
