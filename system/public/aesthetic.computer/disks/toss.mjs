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

  // Load stample sample from store (like notepat does)
  stampleSampleId = null;
  stampleSampleData = null;
  stampleSampleRate = null;

  if (store?.retrieve) {
    const storedSample = store["stample:sample"];
    const loadSample = async () => {
      const sample = storedSample || (await store.retrieve("stample:sample", "local:db"));
      if (sample?.data?.length) {
        const storedId = sample.id || "stample";
        stampleSampleId = storedId;
        stampleSampleData = sample.data;
        stampleSampleRate = sample.sampleRate;
        sound?.registerSample?.(storedId, sample.data, sample.sampleRate);
        console.log("🎵 Toss loaded stample sample:", storedId, sample.data.length, "samples");
      }
    };
    loadSample();
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

function paint({ api, wipe, ink, line, screen, box, circle, pen, write, num }) {
  wipe("purple"); // Clear the background.
  bands.forEach((band, index) => {
    const btn = band.btn;
    btn.paint((b) => {
      const hue = num.map(band.tone, toneLow, toneHigh, 0, 359.9);
      const colorA = num.hslToRgb(hue, 100, 30); // theme[index];
      const colorB = num.hslToRgb(hue, 50, 10); // theme[index];
      ink(b.down ? colorA : colorB).box(b.box.x, b.box.y, b.box.w, b.box.h);
      
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
  const gap = 32;
}

function act({ event: e, api, sound, pens }) {
  // Resize
  if (e.is("reframed")) layoutBandButtons(api);

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
          
          console.log("🎵 Toss band starting sound:", index, band.tone, "volume:", volumeFactor.toFixed(2));
          
          // Use play() for stample, synth() for other wave types
          if (band.waveType === "stample" && stampleSampleId) {
            band.sound = sound.play(stampleSampleId, {
              volume: volumeFactor,
              pitch: band.tone / startTone, // Pitch relative to 220Hz base
              loop: true,
            });
          } else {
            band.sound = sound.synth({
              type: band.waveType,
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
              pitch: band.tone / startTone,
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
        if (band.waveType === "stample" && stampleSampleId) {
          band.sound = sound.play(stampleSampleId, {
            volume: 1,
            pitch: band.tone / startTone,
            loop: true,
          });
        } else {
          band.sound = sound.synth({
            type: band.waveType,
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
          pitch: band.tone / startTone,
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
          pitch: band.tone / startTone,
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
