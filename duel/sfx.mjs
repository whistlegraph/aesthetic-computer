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

  // Items.
  shot() {
    this.voice(900, 0.12, { slide: 0.25, volume: 0.035, type: 'square' });
    this.noise(0.08, { volume: 0.03, from: 5000, to: 800 });
  }
  poke() {
    this.noise(0.14, { volume: 0.035, from: 800, to: 4000 });
  }
  toss() {
    this.noise(0.25, { volume: 0.025, from: 400, to: 2400 });
    this.voice(300, 0.2, { slide: 1.6, volume: 0.015 });
  }
  hit(mine) {
    this.voice(mine ? 120 : 180, 0.16, { slide: 0.6, volume: 0.05, type: 'square' });
    this.noise(0.1, { volume: 0.04, from: 1200, to: 200 });
  }
  pickup(mine) {
    if (!mine) return;
    this.voice(note(4), 0.07, { volume: 0.03 });
    this.voice(note(7), 0.1, { volume: 0.03, at: 0.05 });
  }
  ricochet(up = 1) {
    // oskiewar's ricochet: a hat tick and a high sine ping.
    this.noise(0.03, { volume: 0.03, from: 9000, to: 5000 });
    this.voice(1550 * up + Math.random() * 400, 0.05, { volume: 0.02, type: 'sine' });
  }
  headshot() {
    this.voice(1200, 0.06, { volume: 0.04, type: 'square' });
    this.voice(600, 0.25, { slide: 0.5, volume: 0.04, at: 0.04 });
  }
  bounce() {
    this.voice(140, 0.06, { slide: 0.7, volume: 0.02 });
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

// Objects that sing: each (tone ...) is an oscillator placed in the room,
// panned and attenuated from where you stand. Only the nearest few play.
export class SpatialTones {
  constructor(sfx, { voices = 8 } = {}) {
    this.sfx = sfx;
    this.max = voices;
    this.voices = new Map();
  }
  update(sources, listener, time, beat = 0) {
    const a = this.sfx.audio;
    if (!a || a.state !== 'running') return;
    if (!this.out) {
      this.out = a.createGain();
      this.out.gain.value = 0.5;
      this.out.connect(a.destination);
    }
    const L = a.listener,
      t = a.currentTime;
    const set = (param, value) => param?.setTargetAtTime(value, t, 0.05);
    if (L.positionX) {
      set(L.positionX, listener.x);
      set(L.positionY, listener.y);
      set(L.positionZ, listener.z);
      set(L.forwardX, listener.fx);
      set(L.forwardY, listener.fy);
      set(L.forwardZ, listener.fz);
      set(L.upX, 0);
      set(L.upY, 1);
      set(L.upZ, 0);
    } else {
      L.setPosition?.(listener.x, listener.y, listener.z);
      L.setOrientation?.(listener.fx, listener.fy, listener.fz, 0, 1, 0);
    }
    const nearest = sources
      .map((s) => ({ ...s, d: Math.hypot(s.x - listener.x, s.y - listener.y, s.z - listener.z) }))
      .filter((s) => s.d < 45)
      .sort((p, q) => p.d - q.d)
      .slice(0, this.max);
    const live = new Set(nearest.map((s) => s.key));
    for (const [key, v] of this.voices)
      if (!live.has(key)) {
        v.gain.gain.setTargetAtTime(0, t, 0.08);
        v.osc.stop(t + 0.5);
        this.voices.delete(key);
      }
    for (const s of nearest) {
      let v = this.voices.get(s.key);
      if (!v || v.wave !== s.wave) {
        if (v) v.osc.stop(t + 0.05);
        const osc = a.createOscillator(),
          gain = a.createGain(),
          pan = a.createPanner();
        osc.type = s.wave;
        pan.panningModel = 'HRTF';
        pan.distanceModel = 'inverse';
        pan.refDistance = 2;
        pan.maxDistance = 45;
        pan.rolloffFactor = 1.3;
        gain.gain.value = 0;
        osc.connect(gain).connect(pan).connect(this.out);
        osc.start();
        v = { osc, gain, pan, wave: s.wave };
        this.voices.set(s.key, v);
      }
      // Square and saw are loud for their amplitude; even them out.
      const level = s.gain(time, beat) * (s.wave === 'sine' || s.wave === 'triangle' ? 0.25 : 0.1);
      // Short time constants so strikes and steps land crisply on the beat.
      v.gain.gain.setTargetAtTime(level, t, 0.008);
      v.osc.frequency.setTargetAtTime(Math.max(20, Math.min(4000, s.frequency(time, beat))), t, 0.004);
      if (v.pan.positionX) {
        set(v.pan.positionX, s.x);
        set(v.pan.positionY, s.y);
        set(v.pan.positionZ, s.z);
      } else v.pan.setPosition?.(s.x, s.y, s.z);
    }
  }
  silence() {
    const t = this.sfx.audio?.currentTime || 0;
    for (const v of this.voices.values()) {
      v.gain.gain.setTargetAtTime(0, t, 0.05);
      v.osc.stop(t + 0.3);
    }
    this.voices.clear();
  }
}
