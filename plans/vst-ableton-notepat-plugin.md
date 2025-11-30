# VST Plugin: Aesthetic Computer Notepat for Ableton Live 12

## 🎯 Goal
Develop a modern VST3/CLAP audio plugin that embeds Aesthetic Computer's `notepat` instrument directly into Ableton Live 12, enabling the visual synthesizer experience within the DAW workflow.

## 🏗️ Architecture Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                     Ableton Live 12 (macOS Host)                │
│  ┌───────────────────────────────────────────────────────────┐  │
│  │              AC Notepat VST3/CLAP Plugin                  │  │
│  │  ┌─────────────────────┐  ┌────────────────────────────┐  │  │
│  │  │   Native UI Shell   │  │     Audio Processing       │  │  │
│  │  │   (WebView/CEF)     │  │  - MIDI → Note Conversion  │  │  │
│  │  │                     │  │  - Web Audio → DAW Audio   │  │  │
│  │  │  ┌───────────────┐  │  │  - Parameter Automation    │  │  │
│  │  │  │  Embedded AC  │  │  │                            │  │  │
│  │  │  │   notepat     │◄─┼──┤  MIDI In ──────────────►   │  │  │
│  │  │  │   (WebView)   │  │  │  Audio Out ─────────────►  │  │  │
│  │  │  └───────────────┘  │  └────────────────────────────┘  │  │
│  │  └─────────────────────┘                                  │  │
│  └───────────────────────────────────────────────────────────┘  │
│                                                                 │
│  Communication Options:                                         │
│  ─────────────────────                                          │
│  Option A: Local WebView (CEF/WebKit)                           │
│  Option B: Artery Bridge (CDP over SSH)                         │
│  Option C: OSC/WebSocket Bridge to external AC instance         │
└─────────────────────────────────────────────────────────────────┘
```

## 📦 Technology Stack

### Plugin Framework Options

1. **JUCE 8** (Recommended)
   - Industry standard for audio plugins
   - Native VST3, AU, CLAP support
   - Built-in WebView2/WebKit browser component
   - C++ with excellent macOS support
   
2. **iPlug2**
   - Lightweight alternative
   - Good WebView integration
   - Simpler build system
   
3. **CLAP + Rust** (Modern approach)
   - Using `clap-rs` or custom Rust bindings
   - WebView via `tauri` or `wry`
   - Better memory safety

### WebView Options for Embedding

1. **macOS Native WebKit** (via WKWebView)
   - Best performance on macOS
   - No additional dependencies
   
2. **CEF (Chromium Embedded Framework)**
   - More web API compatibility
   - Larger binary size
   
3. **Ultralight**
   - Lightweight WebKit alternative
   - Good for plugin contexts

## 🔧 Implementation Phases

### Phase 1: Development Environment Setup
- [ ] Set up Mac development via SSH from dev container
- [ ] Install Xcode, CMake, JUCE on Mac host
- [ ] Configure SSH tunnel for remote development
- [ ] Test Artery bridge connectivity to Mac

### Phase 2: Plugin Scaffold
- [ ] Create JUCE/iPlug2 project structure
- [ ] Implement basic VST3 plugin shell
- [ ] Add MIDI input handling
- [ ] Add audio output bus

### Phase 3: WebView Integration
- [ ] Embed WebView component in plugin UI
- [ ] Load Aesthetic Computer from localhost/remote
- [ ] Implement JavaScript ↔ Native bridge
- [ ] Handle MIDI → keyboard event translation

### Phase 4: Audio Bridge
- [ ] Capture Web Audio output from notepat
- [ ] Route to DAW audio bus
- [ ] Handle sample rate conversion
- [ ] Implement latency compensation

### Phase 5: DAW Integration
- [ ] Add automatable parameters (room, wave type, octave)
- [ ] Implement preset save/load
- [ ] Handle plugin state persistence
- [ ] Test with Ableton Live 12

### Phase 6: Polish & Release
- [ ] Code signing for macOS
- [ ] Notarization for Gatekeeper
- [ ] Installer creation
- [ ] Documentation

## 🌉 Artery Bridge Option (Quick Start)

For initial prototyping, we can use the existing Artery system to bridge between the dev container and Mac host:

```
┌──────────────────────┐        SSH Tunnel        ┌──────────────────────┐
│  Fedora Dev Container │ ◄──────────────────────► │   Mac Host (Ableton)  │
│                       │                          │                       │
│  ┌─────────────────┐  │     Port Forward        │  ┌─────────────────┐  │
│  │     Artery      │  │     (9222 CDP)          │  │   Chrome/Edge   │  │
│  │  (CDP Client)   │◄─┼─────────────────────────┼──│  with DevTools  │  │
│  └─────────────────┘  │                          │  └─────────────────┘  │
│                       │                          │         ▲             │
│                       │                          │         │             │
│                       │                          │  ┌──────┴──────────┐  │
│                       │                          │  │  AC in Browser  │  │
│                       │                          │  │   (notepat)     │  │
│                       │                          │  └─────────────────┘  │
│                       │                          │         ▲             │
│                       │      MIDI over OSC      │         │  OSC/MIDI   │
│  ┌─────────────────┐  │ ◄───────────────────────┼─────────┴───────────  │
│  │  OSC Server     │  │                          │  Ableton Live 12     │
│  └─────────────────┘  │                          │  (External MIDI)     │
└──────────────────────┘                          └──────────────────────┘
```

### SSH Configuration (from vault)

Using `aesthetic-computer-vault/ssh-friend.fish`:
```fish
# Connect to Mac host
./aesthetic-computer-vault/ssh-friend.fish

# Or run remote command
./aesthetic-computer-vault/ssh-friend.fish "open -a 'Google Chrome' 'https://aesthetic.computer/notepat'"
```

### Required Mac Host Setup

```bash
# 1. Enable Chrome DevTools remote debugging
/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome \
  --remote-debugging-port=9222 \
  --user-data-dir=/tmp/chrome-debug

# 2. Install Node.js for local OSC server (if needed)
brew install node

# 3. SSH tunnel from dev container
ssh -R 9222:localhost:9222 $SSH_FRIEND_USER@$SSH_FRIEND_HOST
```

## 📁 Proposed File Structure

```
aesthetic-computer/
├── plugins/
│   └── ac-notepat-vst/
│       ├── CMakeLists.txt
│       ├── JUCE/                    # JUCE submodule
│       ├── Source/
│       │   ├── PluginProcessor.cpp  # Audio processing
│       │   ├── PluginProcessor.h
│       │   ├── PluginEditor.cpp     # UI with WebView
│       │   ├── PluginEditor.h
│       │   ├── MIDIHandler.cpp      # MIDI → keyboard events
│       │   ├── MIDIHandler.h
│       │   ├── WebViewBridge.cpp    # JS ↔ Native communication
│       │   └── WebViewBridge.h
│       ├── Resources/
│       │   └── notepat-bundle/      # Bundled AC notepat assets
│       ├── Builds/
│       │   └── MacOSX/
│       └── README.md
├── artery/
│   ├── artery-vst-bridge.mjs        # New: VST ↔ Artery bridge
│   └── ...
└── plans/
    └── vst-ableton-notepat-plugin.md  # This file
```

## 🎹 MIDI to Notepat Mapping

```
MIDI Note → Notepat Key
─────────────────────────
C3 (48)  → 'z' (C)
C#3 (49) → 'a' (C#)  [shift octave down]
D3 (50)  → 'x' (D)
D#3 (51) → 's' (D#)
E3 (52)  → 'c' (E)
F3 (53)  → 'v' (F)
F#3 (54) → 'd' (F#)
G3 (55)  → 'b' (G)
G#3 (56) → 'f' (G#)
A3 (57)  → 'n' (A)
A#3 (58) → 'g' (A#)
B3 (59)  → 'm' (B)
C4 (60)  → ',' (C+1)
... (continues for upper octaves)

Special Controls:
─────────────────
CC1 (Mod Wheel)  → Room/Reverb amount
CC74 (Brightness) → Wave type cycle
Pitch Bend       → Slide mode glide
```

## 🔊 Audio Routing Strategy

### Option A: ScriptProcessorNode Capture
Intercept Web Audio output at the AudioContext level and send samples to native code via JavaScript bridge.

### Option B: MediaStreamDestination
Use `createMediaStreamDestination()` to get audio as MediaStream, then capture via WebRTC-like APIs.

### Option C: Audio Worklet → SharedArrayBuffer
Modern approach using AudioWorklet with SharedArrayBuffer for zero-copy audio transfer to native code.

## 🚀 Quick Start Commands

```bash
# 1. SSH into Mac host
cd /workspaces/aesthetic-computer
./aesthetic-computer-vault/ssh-friend.fish

# 2. On Mac: Start Chrome with DevTools
/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome \
  --remote-debugging-port=9222 "https://aesthetic.computer/notepat"

# 3. Back in dev container: Test Artery connection
cd artery
node test-notepat.mjs

# 4. Future: Build VST plugin on Mac
cd plugins/ac-notepat-vst
cmake -B build -G Xcode
cmake --build build --config Release
```

## 📋 Todo / Next Steps

### Immediate (Dev Container → Mac Bridge)
1. [ ] Set up ngrok TCP tunnel for SSH access from dev container to Mac
2. [ ] Or use Tailscale/ZeroTier for persistent VPN between dev container and Mac
3. [ ] Test Artery CDP bridge with remote port forwarding

### Development Workflow Options

**Option A: Full Remote Development**
- SSH into Mac, do all plugin development there
- Use VS Code Remote SSH extension
- Dev container just for coordination/testing

**Option B: Hybrid Development** 
- Write plugin code in dev container
- rsync/git push to Mac for building
- Test via Artery bridge

**Option C: Mac as Pure Runtime**
- Ableton + notepat runs on Mac with Chrome DevTools enabled
- ngrok exposes CDP port 9222 
- Artery in dev container controls it remotely
- Plugin is a simple MIDI→OSC forwarder

### Technical Tasks
1. [ ] Set up ngrok TCP tunnel: `ngrok tcp 22` on Mac
2. [ ] Create OSC server in Artery for MIDI reception
3. [ ] Prototype MIDI → keyboard event translation
4. [ ] Research JUCE WebView component on macOS
5. [ ] Create minimal VST3 scaffold project
6. [ ] Document Ableton Live 12 VST3 requirements

### Networking Setup (Mac Side)
```bash
# On Mac: Enable SSH
sudo systemsetup -setremotelogin on

# Expose SSH via ngrok (one-time setup)
ngrok tcp 22

# Or for CDP bridge (Chrome DevTools)
ngrok tcp 9222

# Start Chrome with remote debugging
/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome \
  --remote-debugging-port=9222 \
  "https://aesthetic.computer/notepat"
```

## 🔗 Resources

- [JUCE Framework](https://juce.com/)
- [CLAP Plugin Standard](https://cleveraudio.org/)
- [iPlug2](https://iplug2.github.io/)
- [Artery CDP Bridge](../artery/artery.mjs)
- [Notepat Instrument](../system/public/aesthetic.computer/disks/notepat.mjs)
- [Aesthetic Computer Vault](../aesthetic-computer-vault/README.md)

---

*Created: 2024-11-30*
*Status: Planning*
