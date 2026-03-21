// Toss, 2024.11.24.01.04.50.399
// Play two oscillators at once.

/* 📝 Notes
  + Usage
  - toss - Default 2 strips starting at 220Hz
  - toss 5 - 5 strips with default frequencies
  - toss a d a t j - 5 strips with notes A4, D4, A4, C#5, E5 (notepat notation)
  - toss 4c 4e 4g 5c - 4 strips forming a C major chord across two octaves
  - toss c v d s e - Chromatic sequence: C4, C#4, D4, D#4, E4
  
  🎵 Notepat Keyboard Notation:
  Base octave (default 4): c d e f g a b - white keys
                           v s w r q - sharps (c# d# f# g# a#)
  Next octave up: h i j k l m n - white keys (c d e f g a b in octave 5)
                  t y u o p - sharps (c# d# f# g# a# in octave 5)
  
  Octave numbers: Use digits 0-9 to change the current octave
  Example: toss 3c d e 5c - C3, D3, E3, C5 (octave shifts sticky)
  
  - [x] the corner label dragging is broken / needs an exception in ui for the scrub event/
  - [] Add a horizontal line for low -> hi pitch.
  - [] Make a `safe` tap area to prevent scrolling or just require at least 2-3px deadzone before
       scrubbing is toggled, which can reset on lift.
  - [] Do full volume at center of strip and less on the edges.
  - [] Consider a bounce or inertia mechanic that works like a bouncing scroll.
  - [] Change sound type per strip.
  - [] Add ability to record a sample into a strip.
  - [] Map notes to strips or load certain presets.
  - [] Maybe rattle could be done with touch in addition to shaking the phone?
  + Done
  - [x] Add keyboard shortcuts for the strips.
  - [x] Add auto-resizing to buttons.
  - [x] Add attack and decay similar to `notepat`.
  - [x] Make it so there can be N strips and they can be layed out either
       "along" or "across" like `toss across` and have a number like `toss across 8` for 8 strips where dragging
       left and right changes the freq.
  - [x] Add vertical separator for the two strips.
*/

let bands = [];
let type;
const startTone = 220;
const attack = 0.005;
const killFade = 0.3; // Longer fade to hear pitch return
let scrollOffset = 0; // For animated dotted line

// Gamepad support
let connectedGamepad = null;
let gamepadBandMapping = [0, 1, 2, 3]; // Map buttons A(0), B(1), X(2), Y(3) to band indices
let selectedBand = 0; // Currently selected band for d-pad pitch control

const wavetypes = [
  "sine",
  "triangle",
  "sawtooth",
  "square",
  "stample",
];

// Stample sample support
let stampleSampleId = null;
let stampleSampleData = null;
let stampleSampleRate = null;
let fallbackSfx = null; // Fallback sound if no stample recorded

let matrixFont;

// First character is for upping the pitch, second is for triggering, third
// is for downing.
const shortcuts = [
  "1qa",
  "2ws",
  "3ed",
  "4rf",
  "5tg",
  "6yh",
  "7uj",
  "8ik",
  "9ol",
  "0p;",
  "-['",
  "=]\\",
];

const theme = [
  "red",
  "orange",
  "green",
  "blue",
  "brown",
  "gray",
  "aqua",
  "purple",
  "pink",
  "rose",
  "violet",
  "lilac",
];

const { min, max, abs } = Math;

const toneLow = 5;
const toneHigh = 1600 * 2;

// Convert notepat keyboard notation to note name with octave
// Based on the notepat notation system
function convertNotepatChar(char, currentOctave = 4) {
  // Base octave white keys
  if (char === 'c') return `${currentOctave}c`;
  if (char === 'd') return `${currentOctave}d`;
  if (char === 'e') return `${currentOctave}e`;
  if (char === 'f') return `${currentOctave}f`;
  if (char === 'g') return `${currentOctave}g`;
  if (char === 'a') return `${currentOctave}a`;
  if (char === 'b') return `${currentOctave}b`;
  
  // Base octave sharps
  if (char === 'v') return `${currentOctave}cs`; // c#
  if (char === 's') return `${currentOctave}ds`; // d#
  if (char === 'w') return `${currentOctave}fs`; // f#
  if (char === 'r') return `${currentOctave}gs`; // g#
  if (char === 'q') return `${currentOctave}as`; // a#
  
  // Next octave white keys
  if (char === 'h') return `${currentOctave + 1}c`;
  if (char === 'i') return `${currentOctave + 1}d`;
  if (char === 'j') return `${currentOctave + 1}e`;
  if (char === 'k') return `${currentOctave + 1}f`;
  if (char === 'l') return `${currentOctave + 1}g`;
  if (char === 'm') return `${currentOctave + 1}a`;
  if (char === 'n') return `${currentOctave + 1}b`;
  
  // Next octave sharps
  if (char === 't') return `${currentOctave + 1}cs`; // c#
  if (char === 'y') return `${currentOctave + 1}ds`; // d#
  if (char === 'u') return `${currentOctave + 1}fs`; // f#
  if (char === 'o') return `${currentOctave + 1}gs`; // g#
  if (char === 'p') return `${currentOctave + 1}as`; // a#
  
  // Check for octave number
  if (/[0-9]/.test(char)) return parseInt(char);
  
  return null;
}

function makeBands({ api, colon, sound }, count, noteChars = []) {
  bandCount = 0; // Will count actual bands created
  type = colon[0] || "sine";
  
  let currentOctave = 4;
  let noteIndex = 0;
  
  // First pass: count how many valid notes we actually have
  let validNoteCount = 0;
  let tempOctave = 4;
  for (let i = 0; i < noteChars.length; i++) {
    const converted = convertNotepatChar(noteChars[i], tempOctave);
    if (typeof converted === 'number') {
      tempOctave = converted; // Update octave for next note
    } else if (typeof converted === 'string') {
      validNoteCount++; // Found a valid note
    }
  }
  
  // Use the valid note count if we have notes, otherwise use the provided count
  const actualCount = validNoteCount > 0 ? validNoteCount : count;
  
  for (let i = 0; i < actualCount; i += 1) {
    let initialTone = startTone * (i / 2 + 1); // Default tone
    let hasNote = false; // Track if we successfully parsed a note
    
    // If we have note characters, try to parse one for this band
    if (noteIndex < noteChars.length) {
      const char = noteChars[noteIndex];
      const converted = convertNotepatChar(char, currentOctave);
      
      if (typeof converted === 'number') {
        // It's an octave change
        currentOctave = converted;
        noteIndex++;
        // Try the next character for the actual note
        if (noteIndex < noteChars.length) {
          const nextChar = noteChars[noteIndex];
          const nextConverted = convertNotepatChar(nextChar, currentOctave);
          if (typeof nextConverted === 'string') {
            const hz = sound.freq(nextConverted);
            if (hz) {
              initialTone = hz;
              hasNote = true;
              console.log(`🎵 Toss strip ${i}: ${nextChar} -> ${nextConverted} -> ${hz.toFixed(2)}Hz`);
            }
            noteIndex++;
          } else {
            // Octave without following note - don't create a band
            i--;
            continue;
          }
        } else {
          // Octave at end of string - don't create a band
          i--;
          continue;
        }
      } else if (typeof converted === 'string') {
        // It's a note name
        const hz = sound.freq(converted);
        if (hz) {
          initialTone = hz;
          hasNote = true;
          console.log(`🎵 Toss strip ${i}: ${char} -> ${converted} -> ${hz.toFixed(2)}Hz`);
        }
        noteIndex++;
      } else {
        // Unknown character, skip it
        noteIndex++;
        i--; // Don't count this as a band
        continue;
      }
    }
    
    bands[i] = {
      sound: undefined,
      tone: initialTone,
      originalTone: hasNote ? initialTone : null, // Track original tone if set via notes
      waveType: type,
      waveBtn: null,
      currentVolume: 1.0,
      returning: false,
      lastX: null, // Track last touch x position
    };
    bandCount++;
  }
  layoutBandButtons(api);
}

function layoutBandButtons({ screen, ui }) {
  const hw = screen.width / 2;
  const segWidth = screen.width / bandCount;
  const waveBtnHeight = 24; // Height of wave type button at bottom
  const gap = 1; // Small gap between scrub area and wave button
  
  for (let i = 0; i < bandCount; i += 1) {
    // Main scrubbing area (top part of strip) - with gap before wave button
    const button = new ui.Button(i * segWidth, 0, segWidth, screen.height - waveBtnHeight - gap);
    button.id = `toss-band-${i}`;  // Add identifier for debugging
    bands[i].btn = button;
    
    // Wave type button (bottom part of strip)
    const waveBtn = new ui.Button(i * segWidth, screen.height - waveBtnHeight, segWidth, waveBtnHeight);
    waveBtn.id = `toss-wave-${i}`;
    waveBtn.noRolloverActivation = true; // Prevent activation from scrubbing from main strip
    bands[i].waveBtn = waveBtn;
  }
}

let bandCount;

function boot({ api, params, sound, net, store }) {
  // Load MatrixChunky8 font
  if (api.Typeface) {
    matrixFont = new api.Typeface("MatrixChunky8");
    matrixFont.load(net.preload);
  }

  // Preload fallback sound for stample mode (like notepat does)
  net
    .preload("startup")
    .then((sfx) => {
      fallbackSfx = sfx;
      console.log("🎵 Toss: Loaded fallback sfx:", sfx);
    })
    .catch((err) => console.warn("🎵 Toss: Failed to load fallback sfx:", err));

  // Load stample sample from store (like notepat does) - runs async in background
  stampleSampleId = null;
  stampleSampleData = null;
  stampleSampleRate = null;

  console.log("🎵 Toss boot: store available?", !!store, "retrieve available?", !!store?.retrieve);

  if (store?.retrieve) {
    (async () => {
      try {
        const storedSample =
          store["stample:sample"] ||
          (await store.retrieve("stample:sample", "local:db"));
        console.log("🎵 Toss: Retrieved sample from store:", !!storedSample, storedSample?.data?.length);
        if (storedSample?.data?.length) {
          const storedId = storedSample.id || "stample";
          stampleSampleId = storedId;
          stampleSampleData = storedSample.data;
          stampleSampleRate = storedSample.sampleRate;
          sound?.registerSample?.(storedId, storedSample.data, storedSample.sampleRate);
          console.log("🎵 Toss loaded stample sample:", storedId, storedSample.data.length, "samples");
        } else {
          console.log("🎵 Toss: No stample sample found in store (record one in `stample` piece first)");
        }
      } catch (err) {
        console.warn("🎵 Toss: Failed to load stample sample:", err);
      }
    })();
  } else {
    console.log("🎵 Toss: store.retrieve not available");
  }

  let count;
  let noteChars = [];
  
  // Check if first param is ONLY a number (count) or contains note characters
  const firstParam = params[0];
  const parsedCount = parseInt(firstParam);
  
  // Only treat as count if it's purely a number with no other characters
  if (!isNaN(parsedCount) && parsedCount.toString() === firstParam && firstParam.length <= 2) {
    // First param is a pure number - use it as count
    count = min(shortcuts.length, parsedCount);
    // If there are additional params, treat them as note characters
    if (params.length > 1) {
      noteChars = params.slice(1).join('').split('');
    }
  } else {
    // First param is not a pure number - treat all params as notes
    noteChars = params.join('').split('');
    count = 0; // Will be determined by makeBands based on actual valid notes
  }
  
  // Default count if nothing specified
  if (!count) count = 2;
  
  makeBands({ api, ...api, sound }, count, noteChars);
}

function paint({ api, wipe, ink, line, screen, box, circle, pen, write, num, sound, help, poly }) {
  wipe("purple"); // Clear the background.
  
  // Guard: don't try to paint bands if they haven't been created yet
  if (!bands || bands.length === 0) return;
  
  // Get waveform data from speaker for visualization
  const waveformLeft = sound?.speaker?.waveforms?.left || [];
  // Handle amplitude - could be number or array
  const amplitudeRaw = sound?.speaker?.amplitudes?.left;
  const amplitude = typeof amplitudeRaw === "number" ? amplitudeRaw : 
                    (Array.isArray(amplitudeRaw) && amplitudeRaw.length > 0 ? amplitudeRaw[0] : 0);
  
  // Check if any band is actively playing
  const anyPlaying = bands.some(b => b?.btn?.down || b?.sound);
  
  bands.forEach((band, index) => {
    if (!band?.btn) return; // Guard against undefined bands
    const btn = band.btn;
    const isSelected = connectedGamepad && selectedBand === index;
    btn.paint((b) => {
      const hue = num.map(band.tone, toneLow, toneHigh, 0, 359.9);
      const colorA = num.hslToRgb(hue, 100, 30); // theme[index];
      const colorB = num.hslToRgb(hue, 50, 10); // theme[index];
      ink(b.down ? colorA : colorB).box(b.box.x, b.box.y, b.box.w, b.box.h);
      
      // Draw selection indicator for gamepad
      if (isSelected) {
        ink("white", 180).box(b.box.x + 2, b.box.y + 2, b.box.w - 4, 2);
        ink("white", 180).box(b.box.x + 2, b.box.y + b.box.h - 4, b.box.w - 4, 2);
      }
      
      // Draw separator line on left edge (except first strip)
      if (b.box.x !== 0) {
        ink("white", 128).line(b.box.x, b.box.y, b.box.x, b.box.y + b.box.h);
      }
      
      // Draw dotted line representing current tone position
      const isActive = b.down;
      
      // For note-based mode: use center line with volume-based opacity
      // For normal mode: position line based on Hz value
      let lineX, lineOpacity;
      
      if (band.originalTone !== null) {
        // Note-based mode: center line with volume feedback
        lineX = b.box.x + b.box.w / 2;
        lineOpacity = 64;
        if (isActive && band.lastX !== null) {
          const distanceFromCenter = abs(band.lastX - lineX);
          const maxDistance = b.box.w / 2;
          const volumeFactor = 1 - (distanceFromCenter / maxDistance) * 0.7;
          lineOpacity = 64 + volumeFactor * 191;
        }
      } else {
        // Normal mode: position based on Hz value
        lineX = num.map(band.tone, toneLow, toneHigh, b.box.x, b.box.x + b.box.w);
        lineOpacity = isActive ? 255 : 128;
      }
      
      ink("white", lineOpacity);
      const dashLength = 4;
      const gapLength = 4;
      
      // Vary scroll speed based on pitch deviation from original (note mode only)
      let scrollSpeed = 1;
      if (isActive && band.originalTone !== null) {
        const pitchDeviation = abs(band.tone - band.originalTone);
        scrollSpeed = 1 + pitchDeviation / 100;
      }
      
      const offset = isActive ? (scrollOffset * scrollSpeed) % (dashLength + gapLength) : 0;
      
      for (let dy = -offset; dy < b.box.h; dy += dashLength + gapLength) {
        const startY = max(b.box.y, b.box.y + dy);
        const endY = min(b.box.y + b.box.h, b.box.y + dy + dashLength);
        if (startY < endY) {
          line(lineX, startY, lineX, endY);
        }
      }
      const toneHeight = num.map(band.tone, toneLow, toneHigh, screen.height, 0);
      ink("cyan", 64).line(b.box.x, toneHeight, b.box.x + b.box.w, toneHeight);
      ink("white");
      const hzText = Math.round(band.tone).toString();
      write(hzText, {
        center: "x",
        x: b.box.x + b.box.w / 2,
        y: b.box.h / 2 - 8,
      }, undefined, undefined, false, "MatrixChunky8");
      ink("yellow");
      write(shortcuts[index][1], {
        center: "x",
        x: b.box.x + b.box.w / 2 + 2,
        y: b.box.h / 2 + 32,
        size: 2,
      });

      ink("green");
      write("+ " + shortcuts[index][0], {
        center: "x",
        x: b.box.x + b.box.w / 2,
        y: b.box.h / 2 - 22,
      });

      ink("red");
      write("- " + shortcuts[index][2], {
        center: "x",
        x: b.box.x + b.box.w / 2,
        y: b.box.h / 2 + 6,
      });
    });
    
    // Paint wave type button at bottom
    if (band.waveBtn) {
      band.waveBtn.paint((wb) => {
        // Use notepat-style colors: darker blue when down, dark background when up
        ink(wb.down ? [40, 40, 100] : "darkblue").box(wb.box.x, wb.box.y, wb.box.w, wb.box.h);
        
        // Draw separator line on left edge (except first strip)
        if (wb.box.x !== 0) {
          ink("white", 128).line(wb.box.x, wb.box.y, wb.box.x, wb.box.y + wb.box.h);
        }
        
        // Draw horizontal separator line above wave button
        ink("white", 128).line(wb.box.x, wb.box.y, wb.box.x + wb.box.w, wb.box.y);
        
        ink("orange");
        write(band.waveType, {
          center: "xy",
          x: wb.box.x + wb.box.w / 2,
          y: wb.box.y + wb.box.h / 2,
        }, undefined, undefined, false, "MatrixChunky8");
      });
    }
  });
  
  // Draw waveform overlay on top of active bands (AFTER all btn.paint calls)
  bands.forEach((band, index) => {
    if (!band?.btn) return;
    const b = band.btn;
    const isActive = b.down || band.sound;
    
    if (isActive) {
      const hue = num.map(band.tone, toneLow, toneHigh, 0, 359.9);
      const waveColor = num.hslToRgb(hue, 80, 75);
      
      // If we have waveform data, draw it
      if (waveformLeft.length > 0) {
        const waveformSamples = Math.min(64, Math.floor(b.box.w / 2));
        const resampled = help?.resampleArray 
          ? help.resampleArray(waveformLeft, waveformSamples)
          : waveformLeft.slice(0, waveformSamples);
        
        if (resampled.length > 1) {
          const xStep = b.box.w / (resampled.length - 1);
          const yMid = b.box.y + b.box.h / 2;
          const yMax = b.box.h / 2.5;
          
          ink(waveColor[0], waveColor[1], waveColor[2], 255);
          const points = resampled.map((v, i) => [
            b.box.x + i * xStep,
            yMid + v * yMax
          ]);
          poly(points);
        }
      } else {
        // Fallback: draw a simple animated sine wave when no waveform data
        const waveformSamples = 32;
        const xStep = b.box.w / (waveformSamples - 1);
        const yMid = b.box.y + b.box.h / 2;
        const yMax = b.box.h / 4;
        const phase = (scrollOffset / 10) + index * Math.PI / 2;
        const freq = band.tone / 100; // Scale frequency for visual
        
        ink(waveColor[0], waveColor[1], waveColor[2], 200);
        const points = [];
        for (let i = 0; i < waveformSamples; i++) {
          const x = b.box.x + i * xStep;
          const y = yMid + Math.sin(phase + i * freq * 0.3) * yMax * (amplitude > 0 ? 1 : 0.5);
          points.push([x, y]);
        }
        poly(points);
      }
    }
  });
  
  // Draw gamepad indicator if connected
  if (connectedGamepad) {
    drawGamepadDiagram({ ink, box, circle, write }, screen, bands);
  }
}

// Draw a simple gamepad diagram showing which buttons map to which bands
function drawGamepadDiagram({ ink, box, circle, write }, screen, bands) {
  const padW = 56;
  const padH = 28;
  const padX = screen.width - padW - 4;
  const padY = 4;
  
  // Background
  ink("dimgray").box(padX, padY, padW, padH);
  
  // ABXY buttons in diamond - map to first 4 bands
  const faceX = padX + 40;
  const faceY = padY + 14;
  const btnSize = 5;
  
  // A (bottom) - band 0
  const aActive = bands[0]?.btn?.down;
  ink(aActive ? "lime" : "green").box(faceX, faceY + 5, btnSize, btnSize);
  
  // B (right) - band 1  
  const bActive = bands[1]?.btn?.down;
  ink(bActive ? "red" : "darkred").box(faceX + 5, faceY, btnSize, btnSize);
  
  // X (left) - band 2
  const xActive = bands[2]?.btn?.down;
  ink(xActive ? "cyan" : "blue").box(faceX - 5, faceY, btnSize, btnSize);
  
  // Y (top) - band 3
  const yActive = bands[3]?.btn?.down;
  ink(yActive ? "yellow" : "olive").box(faceX, faceY - 5, btnSize, btnSize);
  
  // D-pad
  const dpadX = padX + 12;
  const dpadY = padY + 14;
  const dSize = 4;
  
  ink("gray").box(dpadX, dpadY - 4, dSize, dSize); // Up
  ink("gray").box(dpadX, dpadY + 4, dSize, dSize); // Down
  ink("gray").box(dpadX - 4, dpadY, dSize, dSize); // Left
  ink("gray").box(dpadX + 4, dpadY, dSize, dSize); // Right
  
  // Show selected band indicator
  ink("white", 128);
  write(`B${selectedBand}`, { x: padX + 2, y: padY + 2 });
}

function act({ event: e, api, sound, pens }) {
  // Resize
  if (e.is("reframed")) layoutBandButtons(api);

  // Gamepad connection/disconnection
  if (e.is("gamepad:connected")) {
    connectedGamepad = e.gamepad;
    console.log("🎮 Toss: Gamepad connected:", e.gamepad);
  }
  if (e.is("gamepad:disconnected")) {
    connectedGamepad = null;
    console.log("🎮 Toss: Gamepad disconnected");
  }

  // Gamepad button events - map A(0), B(1), X(2), Y(3) to bands
  // 8BitDo Micro: A=0, B=1, X=3, Y=4 (different mapping!)
  const buttonToBand = { 0: 0, 1: 1, 3: 2, 4: 3 }; // A->0, B->1, X->2, Y->3
  
  for (const [btn, bandIdx] of Object.entries(buttonToBand)) {
    const band = bands[bandIdx];
    if (!band) continue;
    
    if (e.is(`gamepad:0:button:${btn}:push`)) {
      connectedGamepad = true;
      if (!band.btn.down) {
        band.btn.down = true;
        const sampleId = stampleSampleId || fallbackSfx;
        if (band.waveType === "stample" && sampleId) {
          band.sound = sound.play(sampleId, {
            volume: 1,
            pitch: band.tone,
            loop: true,
          });
        } else {
          band.sound = sound.synth({
            type: band.waveType === "stample" ? "sine" : band.waveType,
            attack,
            tone: band.tone,
            duration: "🔁",
          });
        }
      }
    }
    
    if (e.is(`gamepad:0:button:${btn}:release`)) {
      band.btn.down = false;
      band.sound?.kill(killFade);
      band.sound = null;
    }
  }
  
  // D-pad for band selection and pitch control
  // D-pad: Up=12, Down=13, Left=14, Right=15
  if (e.is("gamepad:0:button:14:push")) {
    // Left - select previous band
    selectedBand = (selectedBand - 1 + bands.length) % bands.length;
    connectedGamepad = true;
  }
  if (e.is("gamepad:0:button:15:push")) {
    // Right - select next band
    selectedBand = (selectedBand + 1) % bands.length;
    connectedGamepad = true;
  }
  if (e.is("gamepad:0:button:12:push")) {
    // Up - increase pitch of selected band
    const band = bands[selectedBand];
    if (band) {
      band.tone = min(band.tone + 20, toneHigh);
      band.sound?.update?.({ tone: band.tone, duration: 0.05 });
    }
    connectedGamepad = true;
  }
  if (e.is("gamepad:0:button:13:push")) {
    // Down - decrease pitch of selected band
    const band = bands[selectedBand];
    if (band) {
      band.tone = max(band.tone - 20, toneLow);
      band.sound?.update?.({ tone: band.tone, duration: 0.05 });
    }
    connectedGamepad = true;
  }

  // Pointer input.
  bands.forEach((band, index) => {
    const btn = band.btn;
    const left = index === 0;

    btn.act(
      e,
      {
        down: (btn) => {
          // Calculate volume based on distance from center (full volume at center, quieter at edges)
          const centerX = btn.box.x + btn.box.w / 2;
          const distanceFromCenter = abs(e.x - centerX);
          const maxDistance = btn.box.w / 2;
          const volumeFactor = 1 - (distanceFromCenter / maxDistance) * 0.7; // 30% to 100% volume
          
          band.lastX = e.x; // Store touch position
          
          console.log("🎵 Toss band starting sound:", index, band.tone, "volume:", volumeFactor.toFixed(2), "waveType:", band.waveType, "stampleId:", stampleSampleId, "fallback:", fallbackSfx);
          
          // Use play() for stample, synth() for other wave types
          const sampleId = stampleSampleId || fallbackSfx;
          if (band.waveType === "stample" && sampleId) {
            band.sound = sound.play(sampleId, {
              volume: volumeFactor,
              pitch: band.tone, // Pitch in Hz (bios divides by basePitch 440)
              loop: true,
            });
          } else {
            band.sound = sound.synth({
              type: band.waveType === "stample" ? "sine" : band.waveType, // Fallback to sine if stample but no sample
              attack,
              tone: band.tone,
              duration: "🔁",
              volume: volumeFactor,
              decay: 0.9995, // Slower decay for smoother release
            });
          }
          band.currentVolume = volumeFactor;
        },
        up: () => {
          console.log("🔇 Toss band UP - stopping sound:", index, !!band.sound);
          band.sound?.kill(killFade);
          band.sound = null;
          band.lastX = null; // Clear touch position
          
          // If this band has an original tone set via notes, start returning to it
          if (band.originalTone !== null && band.tone !== band.originalTone) {
            band.returning = true;
          }
        },
        cancel: () => {
          console.log("🔇 Toss band CANCEL - stopping sound:", index, !!band.sound);
          band.sound?.kill(killFade);
          band.sound = null;
        },
        over: (btn) => {
          // Simple rollover - no complex activation logic
        },
        out: (btn) => {
          console.log("🔇 Toss band OUT - stopping sound:", index, !!band.sound);
          band.sound?.kill(killFade);
          band.sound = null;
        },
        scrub: (btn) => {
          // Stop return animation when actively scrubbing
          band.returning = false;
          
          band.lastX = e.x; // Store touch position
          
          band.tone -= e.delta.y;

          if (band.tone < toneLow) band.tone = toneLow;
          if (band.tone > toneHigh) band.tone = toneHigh;

          // Calculate volume based on distance from center
          const centerX = btn.box.x + btn.box.w / 2;
          const distanceFromCenter = abs(e.x - centerX);
          const maxDistance = btn.box.w / 2;
          const volumeFactor = 1 - (distanceFromCenter / maxDistance) * 0.7;
          
          // Update sound with new parameters (stample uses pitch, synth uses tone)
          if (band.waveType === "stample" && stampleSampleId) {
            band.sound?.update?.({
              pitch: band.tone, // Pitch in Hz
              volume: volumeFactor,
            });
          } else {
            band.sound?.update?.({
              tone: band.tone,
              duration: 0.05,
              volume: volumeFactor,
            });
          }
          band.currentVolume = volumeFactor;
        },
      },
      pens?.(),
    );
    
    // Wave type button interaction
    if (band.waveBtn) {
      band.waveBtn.act(e, {
        down: () => {
          // Cycle to next wave type
          const currentIndex = wavetypes.indexOf(band.waveType);
          const nextIndex = (currentIndex + 1) % wavetypes.length;
          band.waveType = wavetypes[nextIndex];
          console.log(`🎵 Toss band ${index}: Changed wave to ${band.waveType}`);
          
          // Play a short blip to confirm the change
          sound.synth({
            type: "triangle",
            tone: 880,
            duration: 0.05,
            attack: 0.001,
            volume: 0.3,
          });
        },
      }, pens?.());
    }
  });

  // Keyboard shortcuts.
  bands.forEach((band, index) => {
    if (index >= shortcuts.length) return;
    const [up, play, down] = shortcuts[index];

    if (e.is(`keyboard:down:${up}`)) band.upping = true;
    if (e.is(`keyboard:up:${up}`)) band.upping = false;
    if (e.is(`keyboard:down:${down}`)) band.downing = true;
    if (e.is(`keyboard:up:${down}`)) band.downing = false;

    if (e.is(`keyboard:down:arrowdown`) && band.sound) band.downing = true;
    if (e.is(`keyboard:up:arrowdown`) && band.sound) band.downing = false;

    if (e.is(`keyboard:down:arrowup`) && band.sound) band.upping = true;
    if (e.is(`keyboard:up:arrowup`) && band.sound) band.upping = false;

    if (e.is(`keyboard:down:${play}`)) {
      if (!band.btn.down) {
        band.btn.down = true;
        // Use play() for stample, synth() for other wave types
        const sampleId = stampleSampleId || fallbackSfx;
        if (band.waveType === "stample" && sampleId) {
          band.sound = sound.play(sampleId, {
            volume: 1,
            pitch: band.tone, // Pitch in Hz
            loop: true,
          });
        } else {
          band.sound = sound.synth({
            type: band.waveType === "stample" ? "sine" : band.waveType,
            attack,
            tone: band.tone,
            duration: "🔁",
          });
        }
      }
    }

    if (e.is(`keyboard:up:${play}`)) {
      band.btn.down = false;
      band.sound?.kill(killFade);
      band.sound = null;
    }
  });

  // Scrolling.
  if (e.is("scroll")) {
    console.log("Scroll:", e);
    // TODO: Check to see if cursor is on top of one of the bands to activate
    //       scrolling.
  }
}

function sim() {
  // Animate scrolling dotted line
  scrollOffset += 0.5;
  
  bands.forEach((band) => {
    // Auto-return to original tone with animation
    if (band.returning && band.originalTone !== null) {
      const diff = band.originalTone - band.tone;
      const speed = 0.8; // Slower speed for less snappy return
      
      if (abs(diff) < 0.5) {
        // Snap to original when close enough
        band.tone = band.originalTone;
        band.returning = false;
      } else {
        // Smoothly interpolate back to original with easing
        band.tone += diff * 0.08 * speed; // Slower interpolation
      }
    }
    
    if (band.upping) {
      band.tone = min(band.tone + 1, toneHigh);

      // Update with pitch for stample, tone for synth
      if (band.waveType === "stample" && stampleSampleId) {
        band.sound?.update?.({
          pitch: band.tone, // Pitch in Hz
        });
      } else {
        band.sound?.update?.({
          tone: band.tone,
          duration: 0.05,
        });
      }
    }

    if (band.downing) {
      band.tone = max(band.tone - 1, toneLow);

      // Update with pitch for stample, tone for synth
      if (band.waveType === "stample" && stampleSampleId) {
        band.sound?.update?.({
          pitch: band.tone, // Pitch in Hz
        });
      } else {
        band.sound?.update?.({
          tone: band.tone,
          duration: 0.05,
        });
      }
    }
  });
}

// 📚 Library

// function beat() {
//   // Runs once per system metronome (BPM) tick.
// }

// function leave() {
//  // Runs once before the piece is unloaded.
// }

// function preview({ ink, wipe }) {
// Render a custom thumbnail image.
// }

// function icon() {
// Render an application icon, aka favicon.
// }

// ⚠️ Also available: `brush` and `filter`.
