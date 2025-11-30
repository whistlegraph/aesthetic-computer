# AC Notepat VST

A VST3/AU plugin that embeds Aesthetic Computer's notepat synthesizer directly in your DAW.

## Building on macOS

### Prerequisites
- Xcode (with command line tools)
- CMake (`brew install cmake`)
- Git

### Build Steps

```bash
cd ~/Desktop/code/aesthetic-computer/ac-vst
chmod +x build-mac.sh
./build-mac.sh
```

This will:
1. Clone JUCE 8.0.0 (if not already present)
2. Configure the project with CMake
3. Build VST3 and AU formats
4. Install to `~/Library/Audio/Plug-Ins/`

### Manual Installation

After building, plugins are located in:
- VST3: `build/ACNotepat_artefacts/Release/VST3/ACNotepat.vst3`
- AU: `build/ACNotepat_artefacts/Release/AU/ACNotepat.component`

Copy to:
- VST3: `~/Library/Audio/Plug-Ins/VST3/`
- AU: `~/Library/Audio/Plug-Ins/Components/`

## Usage in Ableton Live

1. Open Ableton Live
2. Go to Preferences → Plug-Ins
3. Ensure "Use VST3 Plug-In Custom Folder" is enabled
4. Rescan plugins
5. Find "AC Notepat" under Plug-Ins → VST3

## Features

- **Embedded WebView**: Runs the full notepat experience inside your DAW
- **MIDI Input**: Play notepat with your MIDI controller
- **Automatable Parameters**:
  - Room (reverb amount)
  - Wave Type (sine, triangle, sawtooth, square)
  - Octave Offset (-2 to +2)
  - Slide Mode

## MIDI Mapping

| MIDI Note | Notepat Key | Note |
|-----------|-------------|------|
| C3 (48)   | z           | C    |
| C#3 (49)  | a           | C#   |
| D3 (50)   | x           | D    |
| D#3 (51)  | s           | D#   |
| E3 (52)   | c           | E    |
| F3 (53)   | v           | F    |
| F#3 (54)  | d           | F#   |
| G3 (55)   | b           | G    |
| G#3 (56)  | f           | G#   |
| A3 (57)   | n           | A    |
| A#3 (58)  | g           | A#   |
| B3 (59)   | m           | B    |

## CC Mapping

| CC Number | Parameter    |
|-----------|--------------|
| CC1       | Room Amount  |
| CC74      | Cycle Wave   |

## Troubleshooting

### Plugin not showing in DAW
1. Make sure plugins are in the correct folder
2. Try rescanning plugins in DAW preferences
3. Check Console.app for any loading errors

### WebView not loading
- The plugin requires internet access to load aesthetic.computer
- For offline use, we'd need to bundle the notepat assets

### Audio not routing
- Currently audio comes from WebView's audio context
- Full audio bridge implementation is TODO

## Development

Edit source files in `Source/` then rebuild:
```bash
cd build
cmake --build . --config Release
```
