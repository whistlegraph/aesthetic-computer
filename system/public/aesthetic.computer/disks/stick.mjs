// Stick, 2026.1.31.12.00.00 💾
// USB Stick Laboratory - Format, write, and create DJ sticks from AC

/* #region 📚 README 
  This piece explores WebUSB capabilities for:
  - Detecting USB mass storage devices
  - Formatting drives (FAT32)
  - Writing audio files (MP3s from clock, tones, recordings)
  - Creating "DJ Sticks" with custom track libraries
  
  WebUSB Limitations:
  - Mass storage devices are typically blocked by browsers
  - May need File System Access API + USB together
  - Consider Electron/native bridge for full access
#endregion */

/* #region 🏁 TODO 
  - [ ] Implement WebUSB device detection
  - [ ] Test File System Access API for USB drives
  - [ ] Create audio export pipeline (WAV → MP3)
  - [ ] Build track list UI
  - [ ] Add progress indicator for writes
  - [ ] Explore native bridge for Electron app
#endregion */

let status = "Ready - Click to request USB device";
let device = null;
let tracks = [];
let usbSupported = false;

// 🥾 Boot
function boot({ screen, resize }) {
  resize(256, 192);
  usbSupported = typeof navigator !== "undefined" && !!navigator.usb;
  if (!usbSupported) {
    status = "WebUSB not supported in this browser";
  }
}

// 🎨 Paint
function paint({ wipe, ink, text, screen }) {
  wipe("black");
  
  // Title
  ink("cyan").write("💾 STICK", { x: 8, y: 8 });
  ink("gray").write("USB Laboratory", { x: 8, y: 20 });
  
  // Status
  ink(device ? "lime" : "yellow").write(status, { x: 8, y: 40, width: screen.width - 16 });
  
  // USB Support indicator
  ink(usbSupported ? "green" : "red").write(
    usbSupported ? "✓ WebUSB Available" : "✗ WebUSB Unavailable", 
    { x: 8, y: 56 }
  );
  
  // Instructions
  ink("white").write("Controls:", { x: 8, y: 80 });
  ink("gray").write("[U] Request USB Device", { x: 8, y: 92 });
  ink("gray").write("[F] Open File Picker", { x: 8, y: 104 });
  ink("gray").write("[T] Add Test Tone Track", { x: 8, y: 116 });
  ink("gray").write("[W] Write to Stick", { x: 8, y: 128 });
  
  // Track list
  if (tracks.length > 0) {
    ink("magenta").write(`Tracks (${tracks.length}):`, { x: 8, y: 148 });
    tracks.slice(0, 3).forEach((track, i) => {
      ink("white").write(`${i + 1}. ${track.name}`, { x: 16, y: 160 + i * 12 });
    });
  }
}

// ✒ Act
async function act({ event: e, sound, download }) {
  if (e.is("keyboard:down:u")) {
    await requestUSBDevice();
  }
  
  if (e.is("keyboard:down:f")) {
    await openFilePicker();
  }
  
  if (e.is("keyboard:down:t")) {
    addTestTone(sound);
  }
  
  if (e.is("keyboard:down:w")) {
    await writeToStick(download);
  }
  
  if (e.is("touch")) {
    await requestUSBDevice();
  }
}

// 🔌 Request USB Device (WebUSB)
async function requestUSBDevice() {
  if (!usbSupported) {
    status = "WebUSB not available";
    return;
  }
  
  try {
    status = "Requesting USB device...";
    
    // Note: Mass storage devices are typically blocked
    // This will show available USB devices
    device = await navigator.usb.requestDevice({
      filters: [] // Empty = show all devices
    });
    
    status = `Connected: ${device.productName || device.serialNumber || "Unknown"}`;
    console.log("USB Device:", device);
    
    // Try to open the device
    await device.open();
    console.log("Device opened, configurations:", device.configurations);
    
  } catch (err) {
    if (err.name === "NotFoundError") {
      status = "No device selected";
    } else {
      status = `Error: ${err.message}`;
      console.error("USB Error:", err);
    }
  }
}

// 📂 Open File Picker (File System Access API)
async function openFilePicker() {
  try {
    // Try to get a directory handle (could be USB drive)
    if (window.showDirectoryPicker) {
      status = "Opening directory picker...";
      const dirHandle = await window.showDirectoryPicker({
        mode: "readwrite",
        startIn: "desktop"
      });
      
      status = `Opened: ${dirHandle.name}`;
      console.log("Directory handle:", dirHandle);
      
      // List contents
      for await (const entry of dirHandle.values()) {
        console.log(`  ${entry.kind}: ${entry.name}`);
      }
      
    } else {
      status = "File System Access API not supported";
    }
  } catch (err) {
    if (err.name === "AbortError") {
      status = "Cancelled";
    } else {
      status = `Error: ${err.message}`;
    }
  }
}

// 🎵 Add Test Tone Track
function addTestTone(sound) {
  const freq = 440 * (tracks.length + 1); // A4, A5, A6...
  tracks.push({
    name: `Tone ${freq}Hz`,
    type: "tone",
    frequency: freq,
    duration: 5
  });
  status = `Added tone track: ${freq}Hz`;
}

// 💾 Write to Stick (placeholder)
async function writeToStick(download) {
  if (tracks.length === 0) {
    status = "No tracks to write!";
    return;
  }
  
  status = "Generating audio files...";
  
  // For now, just download as a workaround
  // Real implementation would write directly to USB
  for (const track of tracks) {
    if (track.type === "tone") {
      // Generate WAV data for the tone
      const wavData = generateToneWAV(track.frequency, track.duration);
      // download would trigger a file save
      console.log(`Would write: ${track.name}.wav (${wavData.byteLength} bytes)`);
    }
  }
  
  status = `Prepared ${tracks.length} tracks (download mode)`;
}

// 🔊 Generate WAV file data for a sine tone
function generateToneWAV(frequency, durationSec) {
  const sampleRate = 44100;
  const numSamples = sampleRate * durationSec;
  const buffer = new ArrayBuffer(44 + numSamples * 2);
  const view = new DataView(buffer);
  
  // WAV header
  writeString(view, 0, "RIFF");
  view.setUint32(4, 36 + numSamples * 2, true);
  writeString(view, 8, "WAVE");
  writeString(view, 12, "fmt ");
  view.setUint32(16, 16, true); // PCM
  view.setUint16(20, 1, true);  // Audio format
  view.setUint16(22, 1, true);  // Mono
  view.setUint32(24, sampleRate, true);
  view.setUint32(28, sampleRate * 2, true); // Byte rate
  view.setUint16(32, 2, true);  // Block align
  view.setUint16(34, 16, true); // Bits per sample
  writeString(view, 36, "data");
  view.setUint32(40, numSamples * 2, true);
  
  // Audio data
  for (let i = 0; i < numSamples; i++) {
    const sample = Math.sin(2 * Math.PI * frequency * i / sampleRate);
    view.setInt16(44 + i * 2, sample * 0x7FFF, true);
  }
  
  return buffer;
}

function writeString(view, offset, string) {
  for (let i = 0; i < string.length; i++) {
    view.setUint8(offset + i, string.charCodeAt(i));
  }
}

// 💗 Beat
function beat({ sound }) {
  // Could sync with clock here
}

export { boot, paint, act, beat };
