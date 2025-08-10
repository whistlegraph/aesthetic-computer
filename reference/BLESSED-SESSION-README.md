# Ableton Live Session Viewer - Blessed Edition

## 🎯 Overview

This is an upgraded version of the Ableton Live Session Viewer that uses the **blessed** TUI library for smooth, professional terminal rendering without flickering.

## ✨ Key Improvements

### ✅ Fixed Issues from Original Version
- **No more flickering** - Uses blessed's efficient screen update system
- **Complete track labeling** - All tracks have proper labels and headers
- **Better layout management** - Professional boxed interface with organized sections
- **Smooth real-time updates** - Stable rendering without screen artifacts

### 🎛️ Interface Layout
```
┌─── Header: Title, Play Status, Beat Counter, Active Clips ───┐
│                                                             │
├─── Session Grid (75%) ───┬─── Track Activity (25%) ─────────┤
│ Track headers             │ Individual track level meters   │
│ 8x16 clip grid           │ Real-time activity monitoring    │
│ Color-coded clip states  │ Track name + percentage         │
│                          │                                 │
├─── Aggregate Output ─────┴─────────────────────────────────┤
│ Master level visualization                                  │
│ Overall system activity                                     │
├─── Controls ────────────────────────────────────────────────┤
│ Interactive key bindings and help text                     │
└─────────────────────────────────────────────────────────────┘
```

## 🚀 Usage

### Quick Start
```bash
# One-command launcher
./session

# Or run directly
node ableton-session-viewer-blessed.mjs [path-to-xml]
```

### Interactive Controls
- **SPACE** - Play/pause transport
- **1-8** - Trigger scene rows (launches clips in that row)
- **T** - Random clip triggering
- **R** - Reset all activity
- **Q/ESC** - Quit application

## 🎨 Visual Features

### Clip State Visualization
- `[░░░]` - Empty clip slot (gray)
- `[▁▁▁]` - Low activity (gray, 0-25%)
- `[▃▃▃]` - Medium activity (yellow, 25-50%)
- `[▅▅▅]` - High activity (green, 50-75%)
- `[███]` - Maximum activity (red, 75-100%)

### Track Activity Meters
- 20-character progress bars
- Color-coded intensity levels
- Real-time percentage display
- Individual track monitoring

### Aggregate Output
- Master level visualization
- System-wide activity aggregation
- Color-coded intensity levels

## 🔧 Technical Architecture

### Blessed TUI Components
- **Screen** - Main terminal interface manager
- **Boxes** - Bordered containers for each interface section
- **Efficient Rendering** - Only updates changed content
- **Proper Layout** - Responsive boxed design

### XML Parser Enhancements
- Supports `GroupTrack`, `AudioTrack`, `MidiTrack`
- Handles `ClipSlot` and `GroupTrackSlot`
- Parses `EffectiveName` for track labels
- Robust clip detection and metadata extraction

### Real-time Updates
- 100ms update intervals
- Activity decay simulation
- Random activity spikes
- Smooth transport simulation

## 📁 Files

- `ableton-session-viewer-blessed.mjs` - Main blessed-based application
- `session` - Simple launcher script
- `BLESSED-SESSION-README.md` - This documentation

## 🎵 Data Flow

```
Ableton XML → SaxesParser → Track/Clip Data → Blessed UI → Interactive Session View
```

1. **Parse** - Extract tracks, clips, and metadata from XML
2. **Visualize** - Render session grid with blessed components
3. **Interact** - Respond to user input for clip triggering
4. **Update** - Real-time activity simulation and display

## 🌟 Benefits

- **Professional appearance** - Clean, stable interface
- **Better usability** - Clear track labels and organized layout
- **Smooth performance** - No flickering or visual artifacts
- **Enhanced interactivity** - Responsive controls and feedback
- **Scalable design** - Handles multiple tracks and complex sessions

This blessed edition provides the stable, professional session view visualization you requested!
