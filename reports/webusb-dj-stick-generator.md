# WebUSB DJ Stick Generator - Technical Feasibility Report

**Date:** January 31, 2026  
**Author:** AC Research  
**Status:** Research / Implementation Planning  
**Related Piece:** `stick.mjs`

---

## The Vision

Create an AC tool that can:
1. Detect when a USB stick is plugged in
2. Format it if needed
3. Write audio files directly to it (MP3s, WAVs)
4. Export clock recordings, generated tones, or any AC audio
5. Create instant "DJ Sticks" ready for CDJ/XDJ playback

---

## Technical Landscape

### Option 1: WebUSB API

**What it is:** Browser API for communicating with USB devices directly.

**The Problem:** Mass storage devices (USB sticks) are **explicitly blocked** by WebUSB for security reasons. The spec excludes:
- Mass Storage Class (0x08)
- HID devices (unless user grants permission)
- Smart cards, audio devices with protected content

```javascript
// This will NOT show USB flash drives
navigator.usb.requestDevice({ filters: [] })
```

**Verdict:** ❌ Cannot access USB sticks via WebUSB

---

### Option 2: File System Access API

**What it is:** Allows web apps to read/write to the local filesystem with user permission.

**How it could work:**
```javascript
// User picks a directory (could be mounted USB drive)
const dirHandle = await window.showDirectoryPicker({
  mode: "readwrite"
});

// Write files to it
const fileHandle = await dirHandle.getFileHandle("track.mp3", { create: true });
const writable = await fileHandle.createWritable();
await writable.write(mp3Data);
await writable.close();
```

**Limitations:**
- User must manually navigate to USB drive in picker
- No auto-detection of USB insertion
- No formatting capability
- Works only in Chromium browsers (Chrome, Edge, Arc)

**Verdict:** ✅ **Best web-only option** - functional but not seamless

---

### Option 3: Electron Native Bridge (AC Pane)

**What it is:** Use Node.js filesystem access in the Electron app.

**Capabilities:**
- Full filesystem access (read/write/format)
- Device detection via `usb` npm package
- Mount point monitoring
- No user picker needed (once permissions granted)

```javascript
// In Electron main process
const { exec } = require('child_process');
const fs = require('fs');
const usb = require('usb');

// Detect USB insertion
usb.on('attach', (device) => {
  // Get mount point, write files
});

// Format drive (requires elevated permissions)
exec('diskutil eraseDisk FAT32 DJSTICK /dev/disk2', callback);
```

**Architecture for AC:**
```
stick.mjs (browser) 
    ↓ IPC
Electron main.js
    ↓ Node.js
USB stick filesystem
```

**Verdict:** ✅ **Full capabilities** - requires AC Pane (Electron app)

---

### Option 4: Native Messaging + Helper App

**What it is:** Browser extension + native helper that handles USB.

**Too complex for our needs.** Skip this.

---

## Recommended Implementation Path

### Phase 1: Web-Only (Works Now)

Use File System Access API in `stick.mjs`:

1. **User clicks "Select USB Drive"**
2. Directory picker opens
3. User navigates to USB drive (shows as mounted volume)
4. AC writes files directly to that directory

```javascript
// stick.mjs implementation
async function selectOutputDirectory() {
  const dirHandle = await window.showDirectoryPicker({
    id: 'dj-stick-output',
    mode: 'readwrite',
    startIn: 'desktop'
  });
  return dirHandle;
}

async function writeTrackToStick(dirHandle, trackName, audioData) {
  const fileHandle = await dirHandle.getFileHandle(
    `${trackName}.wav`, 
    { create: true }
  );
  const writable = await fileHandle.createWritable();
  await writable.write(audioData);
  await writable.close();
}
```

### Phase 2: Electron Integration (Full Experience)

Add to AC Pane for seamless USB detection:

**New IPC handlers in `main.js`:**
```javascript
ipcMain.handle('usb:list-drives', async () => {
  // Return mounted USB drives
  // macOS: parse `diskutil list`
  // Windows: use `wmic logicaldisk`
  // Linux: parse `/proc/mounts` or `lsblk`
});

ipcMain.handle('usb:write-file', async (event, { drivePath, fileName, data }) => {
  const fullPath = path.join(drivePath, fileName);
  await fs.promises.writeFile(fullPath, Buffer.from(data));
  return { success: true, path: fullPath };
});

ipcMain.handle('usb:format-drive', async (event, { drivePath, label }) => {
  // Platform-specific format commands
  // Requires user confirmation + elevated permissions
});
```

**Expose in `preload.js`:**
```javascript
contextBridge.exposeInMainWorld('acUSB', {
  listDrives: () => ipcRenderer.invoke('usb:list-drives'),
  writeFile: (opts) => ipcRenderer.invoke('usb:write-file', opts),
  formatDrive: (opts) => ipcRenderer.invoke('usb:format-drive', opts)
});
```

---

## Audio Export Pipeline

### From Clock Recordings

```
Clock piece records audio
    ↓
AudioBuffer in memory
    ↓
Encode to WAV (easy) or MP3 (needs encoder)
    ↓
Write to USB stick
```

### MP3 Encoding Options

1. **lamejs** - Pure JS MP3 encoder (slow but works everywhere)
2. **ffmpeg.wasm** - Full ffmpeg in WebAssembly (heavy but powerful)
3. **Native ffmpeg** - In Electron, shell out to ffmpeg binary

Recommendation: **WAV for Phase 1** (simpler), **MP3 via lamejs for Phase 2**

### DJ-Ready File Structure

Pioneer/Denon decks work best with:
```
USB_STICK/
├── PIONEER/           (auto-created by rekordbox export)
├── MUSIC/
│   ├── 01-track.mp3
│   ├── 02-track.mp3
│   └── ...
└── _REKORDBOX_ANALYSIS/  (if using rekordbox)
```

For AC-generated sticks:
```
DJSTICK/
├── AC_TRACKS/
│   ├── clock-recording-2026-01-31.wav
│   ├── tone-440hz.wav
│   └── generated-beat-01.wav
└── README.txt         (generated by AC with track info)
```

---

## Security Considerations

1. **Formatting is destructive** - Always require explicit confirmation
2. **Don't auto-mount** - Let user choose the drive
3. **Validate drive type** - Check it's actually a removable drive
4. **Size limits** - Warn if drive is suspiciously large (HDD vs thumb drive)

---

## Browser Compatibility

| Feature | Chrome | Firefox | Safari | Electron |
|---------|--------|---------|--------|----------|
| File System Access | ✅ | ❌ | ❌ | ✅ |
| WebUSB | ✅ (no mass storage) | ❌ | ❌ | ✅ |
| Audio encoding | ✅ | ✅ | ✅ | ✅ |
| Native USB access | ❌ | ❌ | ❌ | ✅ |

**Recommendation:** Target Chrome for web, full features in AC Pane (Electron)

---

## Implementation Checklist

### Phase 1: `stick.mjs` Basic (Web)
- [x] Create piece scaffold
- [ ] Implement File System Access directory picker
- [ ] Add WAV generation from tone
- [ ] Add track list management UI
- [ ] Write WAV files to selected directory
- [ ] Add progress indicator

### Phase 2: Clock Integration
- [ ] Export clock recordings as WAV
- [ ] Add recording browser in stick.mjs
- [ ] Batch export multiple recordings

### Phase 3: Electron Bridge
- [ ] Add USB detection to AC Pane
- [ ] Create IPC handlers for drive operations
- [ ] Add format capability (with safeguards)
- [ ] Auto-detect USB insertion

### Phase 4: DJ Metadata
- [ ] Add BPM detection/tagging
- [ ] Generate waveform previews
- [ ] Write ID3 tags (for MP3)
- [ ] Optionally create rekordbox-compatible structure

---

## Related AC Components

- **clock.mjs** - Source of recordings to export
- **tape.mjs** - Another audio recording source
- **tone.mjs** - Generate test tones
- **Electron app** (`ac-electron/`) - Native bridge for full USB access

---

## Conclusion

**Web-only path:** Functional but requires user to manually select USB drive via picker. Good enough for a working tool.

**Electron path:** Full seamless experience with auto-detection, one-click formatting, and direct writes. This is the "DJ Stick Generator" dream.

Start with Phase 1 in `stick.mjs` to prove the concept, then enhance with Electron integration in AC Pane for the full kerpow experience.

---

## References

- [File System Access API](https://developer.mozilla.org/en-US/docs/Web/API/File_System_Access_API)
- [WebUSB API](https://developer.mozilla.org/en-US/docs/Web/API/WebUSB_API)
- [WebUSB Blocklist](https://chromium.googlesource.com/chromium/src/+/main/third_party/blink/renderer/modules/webusb/usb_blocklist.cc)
- [lamejs MP3 encoder](https://github.com/zhuker/lamejs)
- [Node.js USB package](https://github.com/node-usb/node-usb)
- [Pioneer rekordbox export format](https://rekordbox.com/en/support/faq/export/)
