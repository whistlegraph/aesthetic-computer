// Offline adapter for the shared melody language used by disks/clock.mjs.
// The browser piece owns transport/UTC concerns; this adapter preserves its
// parser, parallel-track rules, duration units, swing marks and waveform params
// while resolving them to deterministic seconds for an offline renderer.

import {
  buildMelodyState,
  noteToTone,
  parseSequentialMelody,
  sequenceDurationBeats,
} from "../../../system/public/aesthetic.computer/lib/melody-parser.mjs";

const NOTE_PC = { C: 0, "C#": 1, D: 2, "D#": 3, E: 4, F: 5, "F#": 6, G: 7, "G#": 8, A: 9, "A#": 10, B: 11 };

export function toneToMidi(tone) {
  const m = /^(\d)([A-G](?:#)?)$/.exec(tone);
  if (!m) return null;
  return (Number(m[1]) + 1) * 12 + NOTE_PC[m[2]];
}

export function compileClockPattern({
  source,
  startSec = 0,
  cycles = 1,
  unitSec,
  section = "pattern",
  voice = "clock",
  swingUnitSec = unitSec / 8,
}) {
  if (!(unitSec > 0)) throw new Error("unitSec must be positive");
  const parsed = parseSequentialMelody(source, 4);
  const state = buildMelodyState(parsed, { baseTempo: unitSec * 1000 });
  const events = [];
  let cursor = startSec;

  const segments = parsed.type === "sequential" ? parsed.sequences : [parsed];
  for (let cycle = 0; cycle < cycles; cycle++) {
    for (let sequenceIndex = 0; sequenceIndex < segments.length; sequenceIndex++) {
      const segment = segments[sequenceIndex];
      const repeats = Math.max(1, segment.loopCount || 1);
      const segmentUnits = sequenceDurationBeats(segment);
      const tracks = segment.tracks || [segment.notes || []];
      for (let repeat = 0; repeat < repeats; repeat++) {
        const segmentStart = cursor;
        tracks.forEach((track, trackIndex) => {
          let elapsedUnits = 0;
          for (let noteIndex = 0; noteIndex < track.length; noteIndex++) {
            const note = track[noteIndex];
            const durationUnits = note.duration || 2;
            const swing = note.swing === "early" ? -1 : note.swing === "late" ? 1 : 0;
            const swingSec = swing * (note.swingAmount || 0) * swingUnitSec;
            const isRest = note.note === "rest" || note.note === "_" || note.note === "-";
            if (!isRest) {
              const tone = noteToTone(note.note, note.octave);
              const toneShift = typeof note.toneShift === "number" ? note.toneShift : 0;
              const midi = toneToMidi(tone);
              events.push({
                type: "note",
                section,
                voice,
                trackIndex,
                sequenceIndex,
                cycle,
                repeat,
                noteIndex,
                startSec: Math.max(0, segmentStart + elapsedUnits * unitSec + swingSec),
                durationSec: (note.sonicDuration || durationUnits) * unitSec,
                timelineDurationSec: durationUnits * unitSec,
                note: note.note,
                octave: note.octave,
                tone,
                midi,
                frequency: midi === null ? null : 440 * 2 ** ((midi - 69) / 12) + toneShift,
                velocity: note.volume ?? 0.8,
                waveType: note.waveType || "sine",
                struck: !!note.struck,
                swing: note.swing || null,
                swingAmount: note.swingAmount || 0,
                toneShift,
                source,
              });
            }
            elapsedUnits += durationUnits;
          }
        });
        cursor += segmentUnits * unitSec;
      }
    }
  }

  events.sort((a, b) => a.startSec - b.startSec || a.trackIndex - b.trackIndex);
  return {
    source,
    parsedType: parsed.type,
    stateType: state.type,
    startSec,
    endSec: cursor,
    durationSec: cursor - startSec,
    events,
  };
}

