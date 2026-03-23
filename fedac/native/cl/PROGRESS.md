# AC Native OS — Common Lisp Edition Progress

## Overview

A bare-metal Common Lisp OS runtime for AC Native, using SBCL + CFFI. Boots directly to notepat (musical keyboard instrument) on real ThinkPad hardware via UEFI.

## Architecture

- **Runtime**: SBCL 2.6.2 compiled to standalone binary (~12MB)
- **Graphics**: DRM/KMS framebuffer with pixel scaling (targets ~200px wide)
- **Audio**: ALSA PCM with 32-voice polyphonic synth (48kHz stereo S16LE)
- **Input**: evdev keyboard polling (full QWERTY)
- **Build**: Docker reproducible build (Fedora 43 + GCC 15 + Linux 6.19.9)
- **Boot**: UEFI → Linux kernel with embedded initramfs → SBCL binary as /init child

## What Works (2026-03-22)

### Core Runtime
- [x] DRM display init (card0/card1/card2 probe + fbdev fallback)
- [x] Double-buffered dumb buffers with page flip
- [x] Pixel scaling with optimized memcpy-based fb-copy-scaled
- [x] 60fps frame sync via clock_nanosleep
- [x] evdev input polling (keyboard)
- [x] ALSA audio output with mixer unmute + volume max
- [x] 5 oscillator types: sine, square, triangle, sawtooth, noise
- [x] Polyphonic synth (32 voices, per-voice pan/attack/decay/duration)
- [x] Audio thread with lock-based voice management

### Graphics
- [x] Immediate-mode primitives: wipe, ink, plot, line, box, circle
- [x] Alpha blending (per-pixel ARGB32)
- [x] 6x10 bitmap font rendering with scaling
- [x] Color system (make-color, pack-argb32, blend)

### Notepat Instrument
- [x] QWERTY keyboard → chromatic note mapping (2.5 octaves per layout)
- [x] Key down → synth voice, key up → kill voice
- [x] Per-note chromatic rainbow colors
- [x] Background color lerps to average of active notes
- [x] Trail effect: fading colored bars per note+octave on release
- [x] Active note indicators (bright full-width bars)
- [x] Octave control (1-9 keys, arrow up/down)
- [x] Wave type cycling (Tab: sine → triangle → sawtooth → square → noise)
- [x] Confirmation blip sound on wave switch
- [x] Stereo panning (lower notes left, higher notes right)
- [x] Status bar: wave type, octave, FPS counter
- [x] ESC triple-press to quit
- [x] Power button to quit
- [x] Quick mode (Shift toggles short attack for staccato)
- [x] Metronome (Space toggle, -/= BPM, clock-synced, pendulum visual)
- [x] Arrow left/right wave switching
- [x] Kill voices on octave/wave change (prevents orphaned notes)
- [x] Wave type selector bar (bottom of screen)
- [x] Swank REPL server (port 4005, auto-start)
- [x] IP address display on screen
- [x] WiFi auto-connect from init (reads saved creds from USB)
- [x] `ac-os scan` and `ac-os repl` commands for remote development

### Build System
- [x] Docker reproducible build (Dockerfile.builder)
- [x] AC_BUILD_LISP=1 flag swaps CL binary into initramfs
- [x] Oven OTA pipeline compatible
- [x] USB flash via privileged Docker container

## What's Next

### Short Term
- [ ] Audio: mixer unmute verification (silent on some boots)
- [ ] Audio: sample recording/playback (microphone capture + sample bank)
- [ ] Audio: echo/room effect
- [ ] Audio: pitch shift (trackpad or slider)
- [ ] Input: touch/trackpad support
- [ ] Display: boot animation / splash screen
- [ ] System: USB log writing for debug
- [ ] System: config.json reading (handle, credentials)

### Medium Term
- [ ] WiFi management (wpa_supplicant integration)
- [ ] OTA self-update
- [ ] Metronome (clock-synced)
- [ ] Text-to-speech (flite integration)
- [ ] Multiple pieces with piece switching (prompt → notepat)
- [ ] Touch grid UI for note pads

### Long Term
- [ ] KidLisp evaluator in CL (replace QuickJS)
- [ ] Network/WebSocket for multiplayer
- [ ] Full piece API surface (matching JS disk.mjs)

## File Map

| File | Purpose |
|------|---------|
| `main.lisp` | Entry point, notepat instrument loop |
| `packages.lisp` | All package/export definitions |
| `drm-display.lisp` | DRM/KMS init, dumb buffers, page flip, present |
| `drm-constants.lisp` | DRM ioctl numbers and struct defs |
| `framebuffer.lisp` | Pixel buffer alloc, clear, plot, blend, scaled copy |
| `graph.lisp` | Immediate-mode 2D: wipe, ink, plot, line, box, circle |
| `color.lisp` | ARGB32 color struct, packing, alpha blend |
| `font-data.lisp` | 6x10 BDF bitmap font (95 ASCII chars) |
| `font.lisp` | Font rendering and measurement |
| `audio.lisp` | ALSA init, mixer unmute, voice management, audio thread |
| `audio-synth.lisp` | Oscillators (sine/square/tri/saw/noise), voice struct |
| `alsa-bindings.lisp` | CFFI bindings for libasound |
| `input.lisp` | evdev device scanning, event polling |
| `input-keycodes.lisp` | Linux keycode constants (full QWERTY) |
| `syscalls.lisp` | CFFI wrappers for Linux syscalls |
| `util.lisp` | Logging, monotonic clock, frame sync |
| `config.lisp` | USB config.json reader |
| `ac-native.asd` | ASDF system definition |
| `build.lisp` | SBCL save-lisp-and-die build script |

## Hardware Tested

- ThinkPad X1 Nano Gen 2 (Intel i915)
- ThinkPad Yoga 11e Gen 5 (Intel i915)
