// Stick, 2026.1.31.12.00.00 💾
// USB Stick Laboratory - Format, write, and create DJ sticks from AC

/* #region 📚 README 
  Generate audio files from clock melodies and export to USB sticks.
  
  Usage:
    stick                          - Interactive mode
    stick 30 clock ^c,,,,,,        - Render 30s of clock melody to track list
    stick 10 tone 440              - Render 10s of 440Hz tone
  
  Architecture:
    - Piece runs in worker, sends render requests to bios
    - Bios handles offline audio rendering via OfflineAudioContext
    - File System Access API for writing to USB (user picks folder)
    
  Workflow:
    1. Add tracks via command or UI
    2. Select output folder (USB stick)
    3. Render & write all tracks as WAV files
#endregion */

/* #region 🏁 TODO 
  - [x] Basic UI scaffold
  - [ ] Command parsing for "stick 30 clock ^c,,,,"
  - [ ] Bios: offline audio rendering handler
  - [ ] WAV encoding from rendered audio
  - [ ] File System Access API for folder selection
  - [ ] Progress indication during render
  - [ ] Preview playback before export
#endregion */

import { parseMelody } from "../lib/melody-parser.mjs";

let status = "Ready";
let tracks = [];
let selectedFolder = null;
let inputText = "";
let inputActive = false;
let rendering = false;
let renderProgress = 0;

// 🥾 Boot
function boot({ params, screen, resize }) {
  resize(320, 240);
  
  // Parse URL params: stick~30~clock~^c,,,,
  if (params.length >= 3) {
    const duration = parseFloat(params[0]);
    const source = params[1];
    const sourceParams = params.slice(2).join(" ");
    
    if (!isNaN(duration) && source) {
      addTrack({ duration, source, sourceParams });
      status = `Added: ${source} (${duration}s)`;
    }
  } else if (params.length > 0) {
    status = "Usage: stick <duration> <source> <params>";
  }
}

// 🎨 Paint
function paint({ wipe, ink, screen, ui: { TextInput } }) {
  wipe("black");
  
  // Title bar
  ink("cyan").write("💾 STICK", { x: 8, y: 8 });
  ink("gray").write("DJ Stick Generator", { x: 72, y: 8 });
  
  // Folder selection
  ink("white").write("Output:", { x: 8, y: 28 });
  const folderText = selectedFolder || "[F] Select Folder...";
  ink(selectedFolder ? "lime" : "yellow").write(folderText, { x: 60, y: 28, width: screen.width - 68 });
  
  // Input area
  ink("white").write("Add Track:", { x: 8, y: 48 });
  ink("gray").box(8, 60, screen.width - 16, 16);
  ink(inputActive ? "white" : "gray").write(
    inputText || "30 clock ^c,,,,,", 
    { x: 12, y: 64 }
  );
  
  // Track list
  const trackY = 88;
  ink("magenta").write(`Tracks (${tracks.length}):`, { x: 8, y: trackY });
  
  tracks.slice(0, 5).forEach((track, i) => {
    const y = trackY + 16 + i * 14;
    const prefix = track.rendering ? "⏳" : track.rendered ? "✓" : "○";
    const color = track.rendering ? "yellow" : track.rendered ? "lime" : "white";
    ink(color).write(`${prefix} ${i + 1}. ${track.name} (${track.duration}s)`, { x: 12, y });
    ink("red").write("[×]", { x: screen.width - 28, y });
  });
  
  if (tracks.length > 5) {
    ink("gray").write(`...and ${tracks.length - 5} more`, { x: 12, y: trackY + 16 + 5 * 14 });
  }
  
  // Progress bar (when rendering)
  if (rendering) {
    const barY = screen.height - 50;
    ink("gray").box(8, barY, screen.width - 16, 12);
    ink("cyan").box(8, barY, (screen.width - 16) * renderProgress, 12);
    ink("white").write(`Rendering: ${Math.round(renderProgress * 100)}%`, { x: 8, y: barY + 16 });
  }
  
  // Controls
  const ctrlY = screen.height - 28;
  ink("gray").write("[F]older  [Enter]Add  [W]rite  [P]review", { x: 8, y: ctrlY });
  
  // Status
  ink("yellow").write(status, { x: 8, y: screen.height - 12, width: screen.width - 16 });
}

// ✒ Act
async function act({ event: e, sound, download, send, needsPaint }) {
  // Keyboard: F - Select folder
  if (e.is("keyboard:down:f")) {
    await selectOutputFolder();
    needsPaint();
  }
  
  // Keyboard: Enter - Add track from input
  if (e.is("keyboard:down:enter") && inputText.trim()) {
    const parsed = parseTrackInput(inputText.trim());
    if (parsed) {
      addTrack(parsed);
      inputText = "";
      status = `Added: ${parsed.source} (${parsed.duration}s)`;
    } else {
      status = "Invalid format. Use: <seconds> <source> <params>";
    }
    needsPaint();
  }
  
  // Keyboard: W - Write all tracks to stick
  if (e.is("keyboard:down:w")) {
    if (tracks.length === 0) {
      status = "No tracks to write!";
    } else if (!selectedFolder) {
      status = "Select a folder first [F]";
    } else {
      await renderAndWrite(send, download);
    }
    needsPaint();
  }
  
  // Keyboard: P - Preview first track
  if (e.is("keyboard:down:p")) {
    if (tracks.length > 0) {
      previewTrack(tracks[0], sound);
    } else {
      status = "No tracks to preview";
    }
    needsPaint();
  }
  
  // Keyboard: T - Add test tone
  if (e.is("keyboard:down:t")) {
    const freq = 440 * (tracks.length + 1);
    addTrack({ duration: 5, source: "tone", sourceParams: String(freq) });
    status = `Added tone: ${freq}Hz`;
    needsPaint();
  }
  
  // Keyboard: Typing for input
  if (e.is("keyboard:down") && e.key?.length === 1) {
    inputText += e.key;
    inputActive = true;
    needsPaint();
  }
  if (e.is("keyboard:down:backspace")) {
    inputText = inputText.slice(0, -1);
    needsPaint();
  }
  
  // Touch: Tap to focus input area
  if (e.is("touch") && e.y > 48 && e.y < 88) {
    inputActive = true;
    needsPaint();
  }
}

// 🔔 Receive messages from bios
// Pending render promises keyed by track id
const pendingRenders = {};

function receiver({ content }) {
  if (content.type === "stick:render:progress") {
    renderProgress = content.progress;
    const track = tracks.find(t => t.id === content.id);
    if (track) {
      status = `Rendering ${track.name}: ${Math.round(content.progress * 100)}%`;
    }
  }
  if (content.type === "stick:render:complete") {
    const track = tracks.find(t => t.id === content.id);
    if (track) {
      track.rendered = true;
      track.rendering = false;
      track.audioData = content.audioData;
    }
    // Resolve pending promise
    if (pendingRenders[content.id]) {
      pendingRenders[content.id].resolve(content.audioData);
      delete pendingRenders[content.id];
    }
  }
  if (content.type === "stick:render:error") {
    status = `Render error: ${content.error}`;
    rendering = false;
    // Reject pending promise
    if (pendingRenders[content.id]) {
      pendingRenders[content.id].reject(new Error(content.error));
      delete pendingRenders[content.id];
    }
  }
}

// � Select Output Folder (File System Access API)
async function selectOutputFolder() {
  try {
    if (!window.showDirectoryPicker) {
      status = "Folder picker not supported (try Chrome)";
      return;
    }
    
    status = "Selecting folder...";
    const dirHandle = await window.showDirectoryPicker({
      id: "dj-stick-output",
      mode: "readwrite",
      startIn: "desktop"
    });
    
    selectedFolder = dirHandle.name;
    window._stickDirHandle = dirHandle; // Store for later use
    status = `Folder selected: ${selectedFolder}`;
    console.log("📂 Selected folder:", dirHandle);
    
  } catch (err) {
    if (err.name === "AbortError") {
      status = "Cancelled";
    } else {
      status = `Error: ${err.message}`;
    }
  }
}

// 🎵 Parse track input: "30 clock ^c,,,," or "10 tone 440"
function parseTrackInput(input) {
  const parts = input.split(/\s+/);
  if (parts.length < 2) return null;
  
  const duration = parseFloat(parts[0]);
  if (isNaN(duration) || duration <= 0) return null;
  
  const source = parts[1].toLowerCase();
  const sourceParams = parts.slice(2).join(" ");
  
  return { duration, source, sourceParams };
}

// ➕ Add track to list
let trackIdCounter = 0;
function addTrack({ duration, source, sourceParams }) {
  const id = ++trackIdCounter;
  const name = source === "clock" 
    ? `clock-${(sourceParams || "melody").slice(0, 12)}`
    : source === "tone"
    ? `tone-${sourceParams || 440}hz`
    : `${source}-${id}`;
    
  tracks.push({
    id,
    name,
    duration,
    source,
    sourceParams,
    rendered: false,
    rendering: false,
    audioData: null
  });
}

// 🔊 Preview track (play via real-time audio)
function previewTrack(track, sound) {
  if (track.source === "tone") {
    const freq = parseFloat(track.sourceParams) || 440;
    sound.synth({
      type: "sine",
      tone: freq,
      duration: Math.min(track.duration, 2), // Preview max 2 seconds
      volume: 0.5
    });
    status = `Preview: ${track.name}`;
  } else if (track.source === "clock") {
    // For clock, we'd need to trigger the melody player
    // This is a simplified preview - just play first note
    status = `Preview not yet implemented for clock`;
  }
}

// 💾 Render all tracks and write to folder
async function renderAndWrite(send, download) {
  rendering = true;
  renderProgress = 0;
  status = "Rendering tracks...";
  
  const dirHandle = window._stickDirHandle;
  
  for (let i = 0; i < tracks.length; i++) {
    const track = tracks[i];
    track.rendering = true;
    renderProgress = i / tracks.length;
    
    try {
      let audioData;
      
      if (track.source === "tone") {
        // Tones are rendered locally
        audioData = generateToneWAV(
          parseFloat(track.sourceParams) || 440, 
          track.duration
        );
      } else if (track.source === "clock") {
        // Clock melodies are rendered via bios using OfflineAudioContext
        status = `Rendering ${track.name}...`;
        
        // Create a promise that will be resolved by receiver()
        const renderPromise = new Promise((resolve, reject) => {
          pendingRenders[track.id] = { resolve, reject };
          
          // Set a timeout in case something goes wrong
          setTimeout(() => {
            if (pendingRenders[track.id]) {
              reject(new Error("Render timeout"));
              delete pendingRenders[track.id];
            }
          }, 60000); // 60 second timeout
        });
        
        // Send render request to bios
        send({
          type: "stick:render",
          content: {
            id: track.id,
            source: track.source,
            melody: track.sourceParams,
            duration: track.duration,
            sampleRate: 44100
          }
        });
        
        // Wait for bios to complete rendering
        audioData = await renderPromise;
        
        if (!audioData) {
          throw new Error("No audio data returned from render");
        }
      } else {
        audioData = generateToneWAV(440, track.duration);
      }
      
      track.audioData = audioData;
      track.rendered = true;
      track.rendering = false;
      
      // Write to folder
      if (dirHandle) {
        const filename = `${String(i + 1).padStart(2, "0")}-${track.name}.wav`;
        status = `Writing ${filename}...`;
        const fileHandle = await dirHandle.getFileHandle(filename, { create: true });
        const writable = await fileHandle.createWritable();
        await writable.write(new Blob([audioData], { type: "audio/wav" }));
        await writable.close();
        console.log(`💾 Wrote: ${filename}`);
      }
      
    } catch (err) {
      console.error(`Error rendering track ${track.name}:`, err);
      track.rendering = false;
      status = `Error: ${err.message}`;
    }
  }
  
  rendering = false;
  renderProgress = 1;
  status = `✓ Wrote ${tracks.length} tracks to ${selectedFolder}`;
}

// 🔊 Generate WAV file data for a sine tone
function generateToneWAV(frequency, durationSec) {
  const sampleRate = 44100;
  const numChannels = 2; // Stereo
  const numSamples = sampleRate * durationSec;
  const bytesPerSample = 2; // 16-bit
  const dataSize = numSamples * numChannels * bytesPerSample;
  const buffer = new ArrayBuffer(44 + dataSize);
  const view = new DataView(buffer);
  
  // WAV header
  writeString(view, 0, "RIFF");
  view.setUint32(4, 36 + dataSize, true);
  writeString(view, 8, "WAVE");
  writeString(view, 12, "fmt ");
  view.setUint32(16, 16, true);              // PCM chunk size
  view.setUint16(20, 1, true);               // Audio format (PCM)
  view.setUint16(22, numChannels, true);     // Stereo
  view.setUint32(24, sampleRate, true);      // Sample rate
  view.setUint32(28, sampleRate * numChannels * bytesPerSample, true); // Byte rate
  view.setUint16(32, numChannels * bytesPerSample, true);  // Block align
  view.setUint16(34, 16, true);              // Bits per sample
  writeString(view, 36, "data");
  view.setUint32(40, dataSize, true);
  
  // Audio data (stereo interleaved)
  let offset = 44;
  for (let i = 0; i < numSamples; i++) {
    // Apply fade in/out to avoid clicks
    let envelope = 1;
    const fadeLen = sampleRate * 0.01; // 10ms fade
    if (i < fadeLen) envelope = i / fadeLen;
    if (i > numSamples - fadeLen) envelope = (numSamples - i) / fadeLen;
    
    const sample = Math.sin(2 * Math.PI * frequency * i / sampleRate) * envelope;
    const intSample = Math.max(-32768, Math.min(32767, sample * 0x7FFF));
    
    // Left channel
    view.setInt16(offset, intSample, true);
    offset += 2;
    // Right channel
    view.setInt16(offset, intSample, true);
    offset += 2;
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
  // Could sync with clock here for live preview
}

export { boot, paint, act, beat, receiver };
