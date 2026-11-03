// Small synth cues, no samples. Everything sits on one pentatonic scale so
// ring moves, picks and build progress play as a melody rather than beeps.

const SCALE = [0, 2, 4, 7, 9];
const note = (step, root = 392) =>
  root * 2 ** ((SCALE[((step % 5) + 5) % 5] + 12 * Math.floor(step / 5)) / 12);

export class Sfx {
  constructor() {
    this.audio = null;
    this.parts = new Map();
  }
  // Electron plays without a gesture; a browser waits for the first input.
  wake() {
    try {
      this.audio ||= new AudioContext();
      if (this.audio.state !== 'running') return this.audio.resume();
    } catch {}
    return Promise.resolve();
  }
  get ready() {
    return this.audio?.state === 'running';
  }
  voice(frequency, duration, { volume = 0.04, type = 'triangle', at = 0, slide = 0 } = {}) {
    if (!this.ready) return;
    const a = this.audio,
      t = a.currentTime + at;
    const o = a.createOscillator(),
      g = a.createGain();
    o.type = type;
    o.frequency.setValueAtTime(frequency, t);
    if (slide) o.frequency.exponentialRampToValueAtTime(frequency * slide, t + duration);
    g.gain.setValueAtTime(0.0001, t);
    g.gain.exponentialRampToValueAtTime(volume, t + 0.008);
    g.gain.exponentialRampToValueAtTime(0.0001, t + duration);
    o.connect(g).connect(a.destination);
    o.start(t);
    o.stop(t + duration + 0.02);
  }
  noise(duration, { volume = 0.03, at = 0, from = 1800, to = 300 } = {}) {
    if (!this.ready) return;
    const a = this.audio,
      t = a.currentTime + at;
    const buffer = a.createBuffer(1, Math.ceil(a.sampleRate * duration), a.sampleRate);
    const data = buffer.getChannelData(0);
    for (let i = 0; i < data.length; i++) data[i] = Math.random() * 2 - 1;
    const src = a.createBufferSource(),
      filter = a.createBiquadFilter(),
      g = a.createGain();
    src.buffer = buffer;
    filter.type = 'lowpass';
    filter.frequency.setValueAtTime(from, t);
    filter.frequency.exponentialRampToValueAtTime(to, t + duration);
    g.gain.setValueAtTime(volume, t);
    g.gain.exponentialRampToValueAtTime(0.0001, t + duration);
    src.connect(filter).connect(g).connect(a.destination);
    src.start(t);
  }

  // Game.
  jump() {
    this.voice(220, 0.09, { slide: 1.8, volume: 0.025, type: 'square' });
  }
  launch() {
    this.voice(65, 0.12, { volume: 0.04 });
    this.noise(0.12, { volume: 0.02 });
  }
  blast() {
    this.noise(0.35, { volume: 0.05, from: 2400, to: 120 });
  }
  frag(mine) {
    if (mine) [0, 2, 4].forEach((s, i) => this.voice(note(s + 5), 0.18, { at: i * 0.06 }));
    else this.voice(110, 0.3, { slide: 0.5 });
  }

  // Build ring.
  ringOpen() {
    this.voice(note(0), 0.08, { volume: 0.03 });
    this.voice(note(2), 0.12, { volume: 0.03, at: 0.05 });
  }
  ringMove(slot) {
    this.voice(note(slot), 0.035, { volume: 0.012, type: 'sine' });
  }
  ringPick(slot) {
    this.voice(note(slot + 5), 0.12, { volume: 0.035 });
    this.voice(note(slot + 7), 0.16, { volume: 0.02, at: 0.04, type: 'sine' });
  }
  ringBack() {
    this.voice(note(3), 0.08, { volume: 0.025, slide: 0.7 });
  }
  refuse() {
    this.voice(140, 0.1, { volume: 0.03, type: 'square' });
  }

  // Aesel building: a soft launch, a rising pluck per new part, a resolve.
  buildStart() {
    [0, 4, 7].forEach((s) => this.voice(note(s, 196), 0.5, { volume: 0.018, type: 'sine' }));
    this.noise(0.25, { volume: 0.012, from: 600, to: 3000 });
  }
  buildProgress(id, parts) {
    const seen = this.parts.get(id) ?? 0;
    if (parts <= seen) return;
    this.parts.set(id, parts);
    // At most one pluck per update, climbing with the part count.
    this.voice(note(parts + 2), 0.09, { volume: 0.022, type: 'square' });
  }
  buildDone(id) {
    this.parts.delete(id);
    [0, 2, 4, 7].forEach((s, i) =>
      this.voice(note(s + 5), 0.3, { volume: 0.03, at: i * 0.07 }),
    );
    this.voice(note(12), 0.6, { volume: 0.015, at: 0.28, type: 'sine' });
  }
  buildFail(id) {
    this.parts.delete(id);
    this.voice(note(4), 0.18, { volume: 0.03, slide: 0.6 });
    this.voice(note(0, 196), 0.3, { volume: 0.025, at: 0.12, slide: 0.7 });
  }

  // The dev strip: a quiet tick so a new line is heard as well as seen.
  dev(kind) {
    if (kind === 'do') this.voice(1320, 0.02, { volume: 0.006, type: 'sine' });
    else this.voice(note(9), 0.07, { volume: 0.01, type: 'sine' });
  }
}
