// wav-score — a small score, sung by a small synth into a WAV.
//
// A language model cannot write audio bytes, but it can write notes. A harness
// whose goal is audio/wav asks for a score as JSON and this renders it: one
// voice per note, an envelope against clicks, everything mixed and clipped
// into 16-bit mono. No dependencies, so a workshop laptop needs nothing new.
//
//   { "bpm": 96, "notes": [ { "at": 0, "beats": 1, "pitch": "C4", "wave": "sine", "gain": 0.6 } ] }
//
// `at` and `beats` count beats from the start; `pitch` is a note name ("C4",
// "F#3", "Bb5"), a MIDI number, or "noise" for a drum hit.

export const RATE = 22050;
export const MAX_SECONDS = 60;
export const MAX_NOTES = 2000;
export const WAVES = ["sine", "square", "saw", "triangle", "noise"];

const STEPS = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };

// A note name or MIDI number as a frequency in hertz; null for noise.
export function frequency(pitch) {
  if (pitch === "noise") return null;
  let midi = pitch;
  if (typeof pitch === "string") {
    const match = /^([a-g])([#b]?)(-?\d)$/i.exec(pitch.trim());
    if (!match) throw new Error(`unreadable pitch "${pitch}" (try C4, F#3 or a MIDI number)`);
    const [, letter, accidental, octave] = match;
    midi = 12 * (Number(octave) + 1) + STEPS[letter.toLowerCase()] + (accidental === "#" ? 1 : accidental === "b" ? -1 : 0);
  }
  if (!Number.isFinite(midi) || midi < 0 || midi > 127) throw new Error(`pitch out of range: ${pitch}`);
  return 440 * 2 ** ((midi - 69) / 12);
}

// What is wrong with a score, as sentences a model can fix. Empty means it plays.
export function scoreProblems(score) {
  if (!score || typeof score !== "object" || Array.isArray(score)) return ["The score must be a JSON object with bpm and notes."];
  const problems = [];
  if (!Number.isFinite(score.bpm) || score.bpm < 20 || score.bpm > 300) problems.push("bpm must be a number from 20 to 300.");
  if (!Array.isArray(score.notes) || !score.notes.length) return [...problems, "notes must be a non-empty array."];
  if (score.notes.length > MAX_NOTES) problems.push(`Use at most ${MAX_NOTES} notes.`);
  let end = 0;
  score.notes.slice(0, 20000).forEach((note, i) => {
    const at = `notes[${i}]`;
    if (!Number.isFinite(note?.at) || note.at < 0) problems.push(`${at}.at must be a beat number ≥ 0.`);
    if (!Number.isFinite(note?.beats) || note.beats <= 0) problems.push(`${at}.beats must be > 0.`);
    if (note?.wave !== undefined && !WAVES.includes(note.wave)) problems.push(`${at}.wave must be one of ${WAVES.join(", ")}.`);
    try { frequency(note?.pitch); } catch (error) { problems.push(`${at}: ${error.message}`); }
    end = Math.max(end, (note?.at || 0) + (note?.beats || 0));
  });
  if (Number.isFinite(score.bpm) && (end * 60) / score.bpm > MAX_SECONDS) problems.push(`The score lasts longer than ${MAX_SECONDS} seconds; shorten it.`);
  return problems.slice(0, 12);
}

function sample(wave, phase) {
  const t = phase % 1;
  if (wave === "square") return t < 0.5 ? 1 : -1;
  if (wave === "saw") return 2 * t - 1;
  if (wave === "triangle") return 1 - 4 * Math.abs(t - 0.5);
  return Math.sin(2 * Math.PI * t);
}

// The score as WAV bytes. Throws with the first problem when it cannot play.
export function renderScore(score, { rate = RATE } = {}) {
  const problems = scoreProblems(score);
  if (problems.length) throw new Error(problems[0]);
  const beat = 60 / score.bpm;
  const end = Math.max(...score.notes.map((n) => (n.at + n.beats) * beat)) + 0.25;
  const mix = new Float32Array(Math.ceil(end * rate));
  let seed = 1;
  const noise = () => ((seed = (seed * 1664525 + 1013904223) >>> 0) / 2 ** 31) - 1;
  for (const note of score.notes) {
    const hz = frequency(note.pitch);
    const wave = hz === null ? "noise" : note.wave || "sine";
    const gain = Math.min(1, Math.max(0, note.gain ?? 0.5));
    const start = Math.floor(note.at * beat * rate);
    const length = Math.floor(note.beats * beat * rate);
    const attack = Math.min(length / 4, rate * 0.01), release = Math.min(length / 2, rate * 0.08);
    for (let i = 0; i < length && start + i < mix.length; i++) {
      const envelope = Math.min(1, i / attack, (length - i) / release);
      const decay = wave === "noise" ? Math.exp((-6 * i) / length) : 1;
      const value = wave === "noise" ? noise() : sample(wave, (hz * i) / rate);
      mix[start + i] += value * envelope * decay * gain * 0.3;
    }
  }
  const bytes = Buffer.alloc(44 + mix.length * 2);
  bytes.write("RIFF", 0); bytes.writeUInt32LE(36 + mix.length * 2, 4); bytes.write("WAVE", 8);
  bytes.write("fmt ", 12); bytes.writeUInt32LE(16, 16); bytes.writeUInt16LE(1, 20); bytes.writeUInt16LE(1, 22);
  bytes.writeUInt32LE(rate, 24); bytes.writeUInt32LE(rate * 2, 28); bytes.writeUInt16LE(2, 32); bytes.writeUInt16LE(16, 34);
  bytes.write("data", 36); bytes.writeUInt32LE(mix.length * 2, 40);
  for (let i = 0; i < mix.length; i++) bytes.writeInt16LE(Math.round(Math.max(-1, Math.min(1, Math.tanh(mix[i]))) * 32767), 44 + i * 2);
  return bytes;
}
