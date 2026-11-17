import assert from "node:assert/strict";
import { convertNotepatNotation } from "../system/public/aesthetic.computer/lib/notepat-convert.mjs";
import { parseSimultaneousMelody } from "../system/public/aesthetic.computer/lib/melody-parser.mjs";

function testPreservesWaveSpecifier() {
  const input = "{triangle}b";
  const converted = convertNotepatNotation(input);
  assert.equal(converted, "{triangle}b", "Waveform specifier should remain intact");

  const parsed = parseSimultaneousMelody(converted, 4);
  assert.equal(parsed.tracks.length, 1, "Should produce a single track");
  assert.equal(parsed.tracks[0][0].waveType, "triangle", "Parsed note should keep triangle waveform");
}

function testPreservesNoiseSpecifier() {
  const input = "{noise-white}b";
  const converted = convertNotepatNotation(input);
  assert.equal(converted, "{noise-white}b", "Noise waveform should remain intact");

  const parsed = parseSimultaneousMelody(converted, 4);
  assert.equal(parsed.tracks[0][0].waveType, "noise-white", "Parsed note should keep noise-white waveform");
}

function testDigitsInsideBracesDoNotAffectOctave() {
  const input = "{0.5}c";
  const converted = convertNotepatNotation(input);
  assert.equal(converted, "{0.5}c", "Volume spec should remain intact");

  const withNotePat = convertNotepatNotation("4c {0.5}v");
  assert.equal(withNotePat, "4c {0.5}4cs", "Digits in braces should not change octave tracking");
}

function testSpeechIsPreserved() {
  const input = '"ty" v';
  const converted = convertNotepatNotation(input);
  assert.equal(converted, '"ty" 4cs', "Speech text should remain unchanged while notepat after quotes converts");
}

function testBAfterNoteIsNoteNotFlat() {
  // `b` is always the note B in notepat notation: `gb` is G then B, not G♭.
  const converted = convertNotepatNotation("fgbijk");
  const notes = parseSimultaneousMelody(converted, 4).tracks[0].map((n) => `${n.note}${n.octave}`);
  assert.deepEqual(notes, ["f4", "g4", "b4", "d5", "e5", "f5"], "b after a note letter should stay the note B");
}

function run() {
  testPreservesWaveSpecifier();
  testPreservesNoiseSpecifier();
  testDigitsInsideBracesDoNotAffectOctave();
  testSpeechIsPreserved();
  testBAfterNoteIsNoteNotFlat();
  console.log("✅ clock-notepat-convert tests passed");
}

run();
