// Melody Highlighter — shared note-aware coloring for melody strings.
//
// Extracted from disks/clock.mjs's `buildColoredMelodyStringUnified` so both
// the clock piece AND KidLisp's `(melody "...")` string literal can render the
// SAME per-note coloring. The output uses the `\color\char` escape format that
// both clock's HUD writer and KidLisp's `buildColoredKidlispString` already
// understand (named colors like "red"/"goldenrod"/"cyan", and RGB arrays
// emitted as `\r,g,b\char`).
//
// The clock version closed over module globals (hasFirstSynthFired, flash
// timers, recentlyMutated indices, the stample cache). Those are now passed in
// via `opts` so the function is pure (no side effects). The one side effect in
// the original — resetting recentlyMutated* when the mutation flash expires —
// is the CALLER's responsibility now (see clock.mjs wrapper).

import { getNoteColor } from "./note-colors.mjs";

const _now = () =>
  typeof performance !== "undefined" ? performance.now() : Date.now();

// Single hoisted HSV→RGB (the clock version inlined this twice).
export function hsvToRgb(h, s, v) {
  const c = v * s;
  const x = c * (1 - Math.abs(((h / 60) % 2) - 1));
  const m = v - c;
  let r, g, b;
  if (h >= 0 && h < 60) { r = c; g = x; b = 0; }
  else if (h >= 60 && h < 120) { r = x; g = c; b = 0; }
  else if (h >= 120 && h < 180) { r = 0; g = c; b = x; }
  else if (h >= 180 && h < 240) { r = 0; g = x; b = c; }
  else if (h >= 240 && h < 300) { r = x; g = 0; b = c; }
  else { r = c; g = 0; b = x; }
  return [
    Math.round((r + m) * 255),
    Math.round((g + m) * 255),
    Math.round((b + m) * 255),
  ];
}

// Pure version of clock's getCurrentNoteIndex — gates on the passed
// `timingHasStarted` instead of the module global `hasFirstSynthFired`.
export function computeCurrentNoteIndex(
  melodyState,
  trackIndex = 0,
  timingHasStarted = true,
) {
  if (!timingHasStarted || !melodyState) return -1;
  if (melodyState.type === "single" && melodyState.notes) {
    const totalNotes = melodyState.notes.length;
    return (melodyState.index - 1 + totalNotes) % totalNotes;
  } else if (melodyState.type === "parallel" && melodyState.trackStates) {
    const trackState = melodyState.trackStates[trackIndex];
    if (trackState && trackState.track) {
      const totalNotes = trackState.track.length;
      return (trackState.noteIndex - 1 + totalNotes) % totalNotes;
    }
  }
  return -1;
}

// Build a `\color\char` escape string coloring each note of `melodyString`.
//
// opts:
//   now                        : ms timestamp (default performance.now())
//   timingHasStarted           : has the first synth fired? (default false)
//   getCurrentNoteIndex        : (state, trackIndex) => index  (default: pure)
//   recentlyMutated            : { noteIndex, trackIndex }  (rainbow target)
//   mutationFlash              : { active }                 (mutation flashing?)
//   specialCharFlash           : boolean                    (green brace flash?)
//   triggeredAsteriskPositions : number[] | ["*"]           (white asterisk flash)
//   getStampleStatus           : (code) => {loaded,loading,error} | null
export function buildColoredMelodyString(melodyString, melodyState, opts = {}) {
  if (!melodyString) return "";

  const now = opts.now ?? _now();
  const timingHasStarted = opts.timingHasStarted ?? false;
  const getCurrentNoteIndex =
    opts.getCurrentNoteIndex ||
    ((s, t = 0) => computeCurrentNoteIndex(s, t, timingHasStarted));
  const {
    noteIndex: recentlyMutatedNoteIndex = -1,
    trackIndex: recentlyMutatedTrackIndex = -1,
  } = opts.recentlyMutated || {};
  const shouldFlashGreen = opts.specialCharFlash ?? false;
  const shouldFlashMutation = opts.mutationFlash?.active ?? false;
  const triggeredAsteriskPositions = opts.triggeredAsteriskPositions || [];
  const getStampleStatus = opts.getStampleStatus || (() => null);

  let coloredMelodyString = "";
  let noteCharPositions = [];
  let noteIndex = 0;
  let inWaveform = false;
  let currentNoteIndex = 0;

  // For sequential melodies, get the effective melodyState (currentSequenceState).
  let effectiveMelodyState = melodyState;
  let currentSequenceIndex = 0;
  let isSequentialMelody = melodyState && melodyState.type === "sequential";

  if (isSequentialMelody) {
    currentSequenceIndex = melodyState.currentSequence ?? 0;
    if (melodyState.currentSequenceState) {
      effectiveMelodyState = melodyState.currentSequenceState;
    } else if (melodyState.sequences && melodyState.sequences.length > 0) {
      effectiveMelodyState = melodyState.sequences[0].parsed;
    }
  }

  if (
    timingHasStarted &&
    effectiveMelodyState &&
    effectiveMelodyState.type === "single"
  ) {
    currentNoteIndex = getCurrentNoteIndex(effectiveMelodyState);
  } else if (
    timingHasStarted &&
    effectiveMelodyState &&
    effectiveMelodyState.type === "parallel"
  ) {
    // handled below per track
  } else {
    currentNoteIndex = -1;
  }

  // For sequential melodies, split by > to identify sequences.
  let sequences = [];
  let sequenceDelimiterPositions = [];
  if (isSequentialMelody) {
    let lastSplitPos = 0;
    for (let i = 0; i < melodyString.length; i++) {
      if (melodyString[i] === ">") {
        sequences.push(melodyString.substring(lastSplitPos, i).trim());
        sequenceDelimiterPositions.push(i);
        lastSplitPos = i + 1;
      }
    }
    sequences.push(melodyString.substring(lastSplitPos).trim());
  }

  // Split by spaces to get groups, then process each group.
  const groups = melodyString.trim().split(/\s+/);
  let groupStartPositions = [];
  let searchStart = 0;
  for (let groupIdx = 0; groupIdx < groups.length; groupIdx++) {
    const group = groups[groupIdx];
    const groupStart = melodyString.indexOf(group, searchStart);
    groupStartPositions.push(groupStart);
    searchStart = groupStart + group.length;
  }

  // Map groups to tracks within the current sequence (sequential only).
  let effectiveGroupToTrackMap = [];
  let currentSeqIdx = 0;
  let currentTrackInSeq = 0;
  if (isSequentialMelody) {
    for (let groupIdx = 0; groupIdx < groups.length; groupIdx++) {
      const group = groups[groupIdx];
      if (/^\d*>$/.test(group)) {
        currentSeqIdx++;
        currentTrackInSeq = 0;
        effectiveGroupToTrackMap.push({
          isDelimiter: true,
          sequenceIndex: currentSeqIdx - 1,
        });
      } else {
        effectiveGroupToTrackMap.push({
          isDelimiter: false,
          sequenceIndex: currentSeqIdx,
          trackIndex: currentTrackInSeq,
          isCurrentSequence: currentSeqIdx === currentSequenceIndex,
        });
        currentTrackInSeq++;
      }
    }
  }

  // First pass: identify all note character positions and their modifiers.
  for (let groupIdx = 0; groupIdx < groups.length; groupIdx++) {
    const group = groups[groupIdx];
    const groupStartChar = groupStartPositions[groupIdx];
    noteIndex = 0;

    let trackIdxForColoring = groupIdx;
    let isInCurrentSequence = true;

    if (isSequentialMelody && effectiveGroupToTrackMap[groupIdx]) {
      const mapping = effectiveGroupToTrackMap[groupIdx];
      if (mapping.isDelimiter) continue;
      trackIdxForColoring = mapping.trackIndex;
      isInCurrentSequence = mapping.isCurrentSequence;
    }

    for (let i = 0; i < group.length; i++) {
      const char = group[i];

      if (char === "{") { inWaveform = true; continue; }
      else if (char === "}") { inWaveform = false; continue; }
      else if (inWaveform) { continue; }

      // Octave-first notation (5f, 4c#, …) + notepat chars after octave numbers.
      if (
        /[0-9]/.test(char) &&
        i + 1 < group.length &&
        /[a-gvswrqhijklmntyuop]/i.test(group[i + 1])
      ) {
        let noteStart = i;
        let noteEnd = i + 1;
        if (noteEnd + 1 < group.length) {
          if (group[noteEnd + 1] === "#") noteEnd++;
        }
        while (noteEnd + 1 < group.length) {
          const nextChar = group[noteEnd + 1];
          if (
            nextChar === "." || nextChar === "-" || nextChar === "[" ||
            nextChar === "]" || nextChar === "," || nextChar === "*"
          ) noteEnd++;
          else break;
        }
        const noteIndexToUse = noteIndex;
        const trackIndexToUse = isSequentialMelody
          ? trackIdxForColoring
          : (effectiveMelodyState && effectiveMelodyState.type === "parallel" ? groupIdx : 0);
        for (let j = noteStart; j <= noteEnd; j++) {
          noteCharPositions.push({
            charIndex: groupStartChar + j,
            noteIndex: noteIndexToUse,
            trackIndex: trackIndexToUse,
            isInCurrentSequence,
          });
        }
        noteIndex++;
        i = noteEnd;
      }
      // Struck mode toggle (^) — skip.
      else if (char === "^") {
        continue;
      }
      // Dashes — only count if they lead to a note (relative octave like ---c).
      else if (char === "-") {
        let dashEnd = i;
        while (dashEnd + 1 < group.length && group[dashEnd + 1] === "-") dashEnd++;
        if (
          dashEnd + 1 < group.length &&
          /[a-gvswrqhijklmntyuop]/i.test(group[dashEnd + 1])
        ) {
          let noteStart = i;
          let noteEnd = dashEnd + 1;
          if (noteEnd + 1 < group.length && group[noteEnd + 1] === "#") noteEnd++;
          while (noteEnd + 1 < group.length) {
            const nextChar = group[noteEnd + 1];
            if (
              nextChar === "." || nextChar === "," || nextChar === "*" ||
              nextChar === "[" || nextChar === "]"
            ) noteEnd++;
            else break;
          }
          const noteIndexToUse = noteIndex;
          const trackIndexToUse = isSequentialMelody
            ? trackIdxForColoring
            : (effectiveMelodyState && effectiveMelodyState.type === "parallel" ? groupIdx : 0);
          for (let j = noteStart; j <= noteEnd; j++) {
            noteCharPositions.push({
              charIndex: groupStartChar + j,
              noteIndex: noteIndexToUse,
              trackIndex: trackIndexToUse,
              isInCurrentSequence,
            });
          }
          noteIndex++;
          i = noteEnd;
        } else {
          i = dashEnd;
          continue;
        }
      }
      // + relative octave modifier (must be followed by a note).
      else if (char === "+") {
        let plusEnd = i;
        while (plusEnd + 1 < group.length && group[plusEnd + 1] === "+") plusEnd++;
        if (
          plusEnd + 1 < group.length &&
          /[a-gvswrqhijklmntyuop]/i.test(group[plusEnd + 1])
        ) {
          let noteStart = i;
          let noteEnd = plusEnd + 1;
          if (noteEnd + 1 < group.length && group[noteEnd + 1] === "#") noteEnd++;
          while (noteEnd + 1 < group.length) {
            const nextChar = group[noteEnd + 1];
            if (
              nextChar === "." || nextChar === "," || nextChar === "*" ||
              nextChar === "[" || nextChar === "]"
            ) noteEnd++;
            else break;
          }
          const noteIndexToUse = noteIndex;
          const trackIndexToUse = isSequentialMelody
            ? trackIdxForColoring
            : (effectiveMelodyState && effectiveMelodyState.type === "parallel" ? groupIdx : 0);
          for (let j = noteStart; j <= noteEnd; j++) {
            noteCharPositions.push({
              charIndex: groupStartChar + j,
              noteIndex: noteIndexToUse,
              trackIndex: trackIndexToUse,
              isInCurrentSequence,
            });
          }
          noteIndex++;
          i = noteEnd;
        } else {
          i = plusEnd;
          continue;
        }
      }
      // Regular note letters (incl. notepat chars vswrq/hijklmn/tyuop).
      else if (/[a-g#_vswrqhijklmntyuop]/i.test(char)) {
        let noteStart = i;
        let noteEnd = i;
        if (noteEnd + 1 < group.length) {
          if (group[noteEnd + 1] === "#") noteEnd++;
        }
        while (noteEnd + 1 < group.length) {
          const nextChar = group[noteEnd + 1];
          if (
            nextChar === "." || nextChar === "-" || nextChar === "[" ||
            nextChar === "]" || nextChar === "," || nextChar === "*"
          ) noteEnd++;
          else break;
        }
        const noteIndexToUse = noteIndex;
        const trackIndexToUse = isSequentialMelody
          ? trackIdxForColoring
          : (effectiveMelodyState && effectiveMelodyState.type === "parallel" ? groupIdx : 0);
        for (let j = noteStart; j <= noteEnd; j++) {
          noteCharPositions.push({
            charIndex: groupStartChar + j,
            noteIndex: noteIndexToUse,
            trackIndex: trackIndexToUse,
            isInCurrentSequence,
          });
        }
        noteIndex++;
        i = noteEnd;
      }
    }
  }

  // ── Color the original melody string character by character ──
  let inWaveformForColoring = false;

  function getRedNoteColor() {
    return "red";
  }

  function getStaticNoteColor(noteCharData) {
    if (!noteCharData) return "gray";
    const stateToUse = effectiveMelodyState || melodyState;
    if (!stateToUse) return "gray";
    let note = null;
    if (stateToUse.type === "single" && stateToUse.notes) {
      note = stateToUse.notes[noteCharData.noteIndex];
    } else if (stateToUse.type === "parallel" && stateToUse.tracks) {
      const track = stateToUse.tracks[noteCharData.trackIndex];
      note = track && track[noteCharData.noteIndex];
    }
    if (!note) return "gray";
    const rgb = getNoteColor(note.note);
    return `rgb(${rgb[0]}, ${rgb[1]}, ${rgb[2]})`;
  }

  function isNoteMutated(noteCharData) {
    if (!noteCharData) return false;
    const stateToUse = effectiveMelodyState || melodyState;
    if (!stateToUse) return false;
    if (stateToUse.type === "single") {
      const currentNote = stateToUse.notes && stateToUse.notes[noteCharData.noteIndex];
      return currentNote && currentNote.isMutation;
    } else if (
      stateToUse.type === "parallel" &&
      stateToUse.trackStates &&
      noteCharData.trackIndex < stateToUse.trackStates.length
    ) {
      const track = stateToUse.tracks[noteCharData.trackIndex];
      const currentNote = track && track[noteCharData.noteIndex];
      return currentNote && currentNote.isMutation;
    }
    return false;
  }

  function getMutatedNoteColor(isCurrentlyPlaying = false, noteCharData = null) {
    const isRecentlyMutatedNote =
      noteCharData &&
      noteCharData.noteIndex === recentlyMutatedNoteIndex &&
      noteCharData.trackIndex === recentlyMutatedTrackIndex;
    const shouldFlashForThisNote = shouldFlashMutation && isRecentlyMutatedNote;
    if (shouldFlashForThisNote) {
      const time = now * 0.005;
      const hue = (time % 1) * 360;
      return hsvToRgb(hue, 1, 1);
    }
    if (isCurrentlyPlaying) return getRedNoteColor();
    return "goldenrod";
  }

  for (let i = 0; i < melodyString.length; i++) {
    const char = melodyString[i];
    let color = "yellow";

    // Sequence separator `>`.
    if (char === ">") {
      const prevChar = i > 0 ? melodyString[i - 1] : " ";
      if (/\d/.test(prevChar)) color = "white";
      else color = "cyan";
      coloredMelodyString += `\\${color}\\${char}`;
      continue;
    }

    // Loop-count digits before `>` (like "3>").
    if (/\d/.test(char)) {
      let lookAhead = i + 1;
      while (lookAhead < melodyString.length && /\d/.test(melodyString[lookAhead])) lookAhead++;
      if (lookAhead < melodyString.length && melodyString[lookAhead] === ">") {
        color = "magenta";
        coloredMelodyString += `\\${color}\\${char}`;
        continue;
      }
    }

    if (char === " ") {
      color = timingHasStarted ? "yellow" : "gray";
    } else if (char === "x") {
      // Disabled-group `x` prefix detection.
      let isGroupPrefix = false;
      if (i === 0) {
        isGroupPrefix = true;
      } else {
        let j = i - 1;
        while (j >= 0 && melodyString[j] === " ") j--;
        isGroupPrefix = j < 0 || melodyString[j] === " ";
      }
      if (isGroupPrefix) {
        color = "brown";
      } else {
        color = timingHasStarted ? "yellow" : "gray";
      }
    } else {
      // Determine if this char is inside a disabled group.
      let isInDisabledGroup = false;
      {
        let charCount = 0;
        const groups2 = melodyString.trim().split(/\s+/);
        for (let groupIdx = 0; groupIdx < groups2.length; groupIdx++) {
          const group = groups2[groupIdx];
          const groupStart = melodyString.indexOf(group, charCount);
          const groupEnd = groupStart + group.length;
          if (i >= groupStart && i < groupEnd) {
            isInDisabledGroup = group.startsWith("x");
            break;
          }
          charCount = groupEnd;
        }
      }

      if (isInDisabledGroup) {
        color = "gray";
      } else {
        const noteCharData = noteCharPositions.find((ncp) => ncp.charIndex === i);
        let isCurrentlyPlayingNote = false;

        if (noteCharData) {
          if (melodyState && melodyState.type === "single") {
            isCurrentlyPlayingNote = noteCharData.noteIndex === currentNoteIndex;
          } else if (
            melodyState &&
            melodyState.type === "parallel" &&
            melodyState.trackStates &&
            noteCharData.trackIndex < melodyState.trackStates.length
          ) {
            const currentPlayingIndex = getCurrentNoteIndex(
              melodyState,
              noteCharData.trackIndex,
            );
            isCurrentlyPlayingNote = noteCharData.noteIndex === currentPlayingIndex;
          } else if (
            melodyState &&
            melodyState.type === "sequential" &&
            melodyState.currentSequenceState
          ) {
            if (noteCharData.isInCurrentSequence) {
              const seqState = melodyState.currentSequenceState;
              if (seqState.type === "single" && seqState.notes) {
                const totalNotes = seqState.notes.length;
                const currentPlayingIndex = (seqState.index - 1 + totalNotes) % totalNotes;
                isCurrentlyPlayingNote = noteCharData.noteIndex === currentPlayingIndex;
              } else if (seqState.type === "parallel" && seqState.trackStates) {
                const trackState = seqState.trackStates[noteCharData.trackIndex];
                if (trackState && trackState.track) {
                  const totalNotes = trackState.track.length;
                  const currentPlayingIndex = (trackState.noteIndex - 1 + totalNotes) % totalNotes;
                  isCurrentlyPlayingNote = noteCharData.noteIndex === currentPlayingIndex;
                }
              }
            }
          }
        }

        if (char === "{") {
          inWaveformForColoring = true;
          color = shouldFlashGreen ? "green" : "yellow";
        } else if (char === "}") {
          inWaveformForColoring = false;
          color = shouldFlashGreen ? "green" : "yellow";
        } else if (inWaveformForColoring) {
          // Stample code {#code} vs regular waveform content.
          let lookBack = i - 1;
          let waveContent = "";
          while (lookBack >= 0 && melodyString[lookBack] !== "{") {
            waveContent = melodyString[lookBack] + waveContent;
            lookBack--;
          }
          waveContent = waveContent + char;
          if (
            waveContent.startsWith("#") ||
            (lookBack >= 0 && melodyString[lookBack + 1] === "#")
          ) {
            const hashIdx = waveContent.indexOf("#");
            const code = waveContent.substring(hashIdx + 1).replace(/[^a-zA-Z0-9]/g, "");
            const cached = getStampleStatus(code);
            if (cached && cached.loaded) color = "lime";
            else if (cached && cached.loading) color = "cyan";
            else if (cached && cached.error) color = [255, 100, 100];
            else color = "orange";
          } else {
            color = "cyan";
          }
        } else if (char === "*") {
          if (
            shouldFlashMutation &&
            (triggeredAsteriskPositions.includes(i) ||
              triggeredAsteriskPositions.includes("*"))
          ) {
            color = "white";
          } else {
            const time = now * 0.005;
            const hue = (time % 1) * 360;
            color = hsvToRgb(hue, 1, 1);
          }
        } else if (melodyState && melodyState.isFallback) {
          if (noteCharData) {
            if (isCurrentlyPlayingNote) {
              const isMutated = isNoteMutated(noteCharData);
              if (/[.,]/i.test(char)) {
                color = isMutated ? getMutatedNoteColor(true, noteCharData) : getRedNoteColor();
              } else if (/[s]/i.test(char)) {
                color = isMutated ? getMutatedNoteColor(true, noteCharData) : getRedNoteColor();
              } else if (char === "_") {
                color = isMutated ? getMutatedNoteColor(true, noteCharData) : getRedNoteColor();
              } else if (/[0-9+\-#<>]/i.test(char)) {
                color = "green";
              } else {
                color = isMutated ? getMutatedNoteColor(true, noteCharData) : getRedNoteColor();
              }
            } else {
              const isMutated = isNoteMutated(noteCharData);
              if (isMutated) {
                color = getMutatedNoteColor(false, noteCharData);
              } else {
                color = timingHasStarted ? "yellow" : getStaticNoteColor(noteCharData);
              }
            }
          } else {
            color = timingHasStarted ? "yellow" : "gray";
          }
        } else {
          if (noteCharData) {
            if (isCurrentlyPlayingNote) {
              const isMutated = isNoteMutated(noteCharData);
              if (/[.,]/i.test(char)) {
                color = isMutated ? getMutatedNoteColor(true, noteCharData) : getRedNoteColor();
              } else if (/[s]/i.test(char)) {
                color = isMutated ? getMutatedNoteColor(true, noteCharData) : getRedNoteColor();
              } else if (char === "_") {
                color = isMutated ? getMutatedNoteColor(true, noteCharData) : getRedNoteColor();
              } else if (/[0-9+\-#\[\]]/i.test(char)) {
                color = "green";
              } else {
                color = isMutated ? getMutatedNoteColor(true, noteCharData) : getRedNoteColor();
              }
            } else {
              const isMutated = isNoteMutated(noteCharData);
              if (isMutated) {
                color = getMutatedNoteColor(false, noteCharData);
              } else {
                color = timingHasStarted ? "yellow" : "gray";
              }
            }
          } else {
            color = timingHasStarted ? "yellow" : "gray";
          }
        }
      }
    }

    // Dim notes not in the current sequence (sequential melodies).
    const noteCharData2 = noteCharPositions.find((ncp) => ncp.charIndex === i);
    const shouldDim =
      isSequentialMelody && noteCharData2 && noteCharData2.isInCurrentSequence === false;
    if (shouldDim && color !== "cyan" && color !== "magenta" && color !== "white") {
      if (typeof color === "string") {
        color = "gray";
      } else if (Array.isArray(color)) {
        color = [
          Math.round(color[0] * 0.3),
          Math.round(color[1] * 0.3),
          Math.round(color[2] * 0.3),
        ];
      }
    }

    if (Array.isArray(color)) {
      coloredMelodyString += `\\${color[0]},${color[1]},${color[2]}\\${char}`;
    } else {
      coloredMelodyString += `\\${color}\\${char}`;
    }
  }

  return coloredMelodyString;
}
