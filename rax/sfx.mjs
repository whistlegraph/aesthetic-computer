// One master gain over every voice and noise, so the world sits level with
// the prompt's own sounds (2026-11-03). Tune here, not per sound.
const MASTER = 1.8;
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
  // (`pan`: -1 the left ear, 1 the right.)
  voice(frequency, duration, { volume = 0.04, type = 'triangle', at = 0, slide = 0, pan = 0 } = {}) {
    if (!this.ready) return;
    volume *= MASTER;
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
    if (pan && a.createStereoPanner) { const p = a.createStereoPanner(); p.pan.value = Math.max(-1, Math.min(1, pan)); o.connect(g).connect(p).connect(a.destination); }
    else o.connect(g).connect(a.destination);
    o.start(t);
    o.stop(t + duration + 0.02);
  }
  // ✊✊ Both closed: the world is held. A fifth, one note in each ear, and a low one under them.
  hold() {
    this.voice(196, 0.22, { volume: 0.05, pan: -0.9 });
    this.voice(294, 0.22, { volume: 0.05, pan: 0.9 });
    this.voice(98, 0.3, { volume: 0.04, type: 'sine' });
  }
  // ✊ A hand closes or opens (in a headset): a short note in that hand's own ear, the left lower than the right.
  fist(side, closed) {
    const pan = side === 'left' ? -0.9 : 0.9, base = side === 'left' ? 196 : 294;
    this.voice(closed ? base : base * 1.5, closed ? 0.09 : 0.06, { volume: closed ? 0.045 : 0.02, type: closed ? 'triangle' : 'sine', slide: closed ? 0.7 : 1.2, pan });
  }
  positionedOutput(at) {
    const a = this.audio;
    if (!at || !a.createPanner) return a.destination;
    const p = a.createPanner();
    Object.assign(p, { panningModel: 'equalpower', distanceModel: 'inverse', refDistance: 2, maxDistance: 60, rolloffFactor: 1.4 });
    if (p.positionX) { p.positionX.value = at.x; p.positionY.value = at.y; p.positionZ.value = at.z; }
    else p.setPosition?.(at.x, at.y, at.z);
    p.connect(a.destination);
    return p;
  }
  sampleOutput(at) {
    const gain = this.audio.createGain();
    gain.gain.value = 0.35 * MASTER;
    gain.connect(this.positionedOutput(at));
    return gain;
  }
  // 🎹 A tone held until it is let go (a piece's `synth` with no end): → { kill(fade) }.
  // `at` { x, y, z }: where in the world it sounds from (the listener is the eye: SpatialTones keeps it).
  hold(frequency, { volume = 0.035, type = 'sine', at = null } = {}) {
    if (!this.ready || !(frequency > 0)) return { kill() {} };
    const a = this.audio, o = a.createOscillator(), g = a.createGain();
    o.type = ['sine', 'square', 'triangle', 'sawtooth'].includes(type) ? type : 'square';
    o.frequency.setValueAtTime(frequency, a.currentTime);
    g.gain.setValueAtTime(0.0001, a.currentTime);
    g.gain.exponentialRampToValueAtTime(volume * MASTER * (at ? 2.2 : 1), a.currentTime + 0.01);
    const out = this.positionedOutput(at);
    o.connect(g).connect(out);
    o.start();
    let gone = false;
    return {
      // (A voice bent while it is held: the pitch glides to the new one.)
      update: ({ tone } = {}) => { if (!gone && tone > 0) o.frequency.setTargetAtTime(tone, a.currentTime, 0.015); },
      kill: (fade = 0.05) => {
      if (gone) return;
      gone = true;
      const t = a.currentTime, f = Math.max(0.01, Number(fade) || 0.05);
      g.gain.cancelScheduledValues(t);
      g.gain.setValueAtTime(Math.max(0.0001, g.gain.value), t);
      g.gain.exponentialRampToValueAtTime(0.0001, t + f);
      o.stop(t + f + 0.03);
    } };
  }
  // 📷 A shutter: two short clicks, brighter going sharper (the picture's density changed).
  shutter(step = 1) {
    this.noise(0.016, { volume: 0.05, from: step < 0 ? 1800 : 2800, to: step < 0 ? 900 : 1600 });
    this.noise(0.02, { volume: 0.04, at: 0.045, from: step < 0 ? 1200 : 3400, to: step < 0 ? 600 : 2000 });
  }
  noise(duration, { volume = 0.03, at = 0, from = 1800, to = 300 } = {}) {
    if (!this.ready) return;
    volume *= MASTER;
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

  // 🛗 The car in motion: a motor's hum and the wind in the shaft, both
  // rising with its speed (0 to 1). Asked every frame; nothing at 0.
  // 🚁 A drone's whir: four rotors as two pairs a little apart in pitch, a
  // buzz with a flutter in it. `near` is how close it is (1: it is yours, or
  // at your ear; 0: none in earshot); `work` how hard it is flying, 0 to 1.
  drone(near, work = 0) {
    if (!this.ready) return;
    const a = this.audio, t = a.currentTime;
    if (!this.rotors) {
      if (near <= 0) return;
      // 🌬️ Air, not a buzz (2026-11-20): the sound of a cabin, as Menu Band's
      // Air makes it (MenuBandAirNoise.swift `cabin`): dark noise with a low
      // body and a little mid, breathing slowly, behind two gentle low-pass
      // stages. The rotors are only a soft tremble in it, and a faint low hum
      // that rises as it works.
      const rate = a.sampleRate, length = Math.floor(rate * 4), buffer = a.createBuffer(1, length, rate), data = buffer.getChannelData(0);
      const k = 44100 / rate;
      let brown = 0, dc = 0, low = 0, mid = 0;
      for (let i = 0; i < length; i++) {
        const white = Math.random() * 2 - 1;
        brown += Math.min(1, 0.004 * k) * (white - brown);
        dc += Math.min(1, 0.0007 * k) * (brown - dc);
        low += Math.min(1, 0.017 * k) * (white - low);
        mid += Math.min(1, 0.085 * k) * (white - mid);
        data[i] = Math.tanh(((brown - dc) * 12 * 0.65 + low * 2.5 + mid * 0.35) * (0.96 + 0.04 * Math.sin((2 * Math.PI * 0.25 * i) / rate)));
      }
      // (The loop's ends are eased into one another so it has no seam.)
      const fade = Math.floor(rate * 0.25);
      for (let i = 0; i < fade; i++) { const m = i / fade; data[i] = data[i] * m + data[length - fade + i] * (1 - m); }
      const air = a.createBufferSource(), out = a.createGain(), band = a.createBiquadFilter(), soft = a.createBiquadFilter(), flutter = a.createOscillator(), depth = a.createGain(), hum = a.createOscillator(), humGain = a.createGain();
      air.buffer = buffer;
      air.loop = true;
      air.loopEnd = (length - fade) / rate;
      out.gain.value = 0;
      band.type = 'lowpass';
      band.frequency.value = 420;
      band.Q.value = 0.5;
      soft.type = 'lowpass';
      soft.frequency.value = 900;
      soft.Q.value = 0.5;
      air.connect(band).connect(soft).connect(out);
      air.start();
      hum.type = 'sine';
      hum.frequency.value = 86;
      humGain.gain.value = 0.03;
      hum.connect(humGain).connect(out);
      hum.start();
      // The blades' beat: a slow, soft tremble in the loudness.
      flutter.frequency.value = 11;
      depth.gain.value = 0;
      flutter.connect(depth).connect(out.gain);
      flutter.start();
      out.connect(a.destination);
      this.rotors = { out, band, soft, hum, depth };
    }
    const n = Math.max(0, Math.min(1, near)), w = Math.max(0, Math.min(1, work)), r = this.rotors;
    // Working harder it opens up (more of the air is heard) and its hum rises a little.
    r.band.frequency.setTargetAtTime(320 + 380 * w, t, 0.4);
    r.soft.frequency.setTargetAtTime(700 + 600 * w, t, 0.4);
    r.hum.frequency.setTargetAtTime(86 + 34 * w, t, 0.2);
    // (Barely there: a hush at a hover, a little more when it works. It was eight times this and filled the room.)
    r.out.gain.setTargetAtTime(n ? (0.006 + 0.008 * w) * n * n * MASTER : 0, t, 0.3);
    r.depth.gain.setTargetAtTime(n ? 0.0006 * n * n * MASTER : 0, t, 0.3);
  }
  motion(travel = 0, turn = 0, onFoot = false) {
    if (!this.ready) return;
    const active = travel > 0 || turn > 0;
    if (!active && !this.motionActive) return;
    this.motionActive = active;
    const a = this.audio, t = a.currentTime;
    if (onFoot && travel > 0) {
      if (this.lastStrideAt == null || t - this.lastStrideAt > 0.2) this.stride = 1;
      else this.stride += (t - this.lastStrideAt) * (1.15 + 2 * travel);
      this.lastStrideAt = t;
      if (this.stride >= 1) {
        this.stride %= 1;
        this.leftFoot = !this.leftFoot;
        const pitch = this.leftFoot ? 184 : 165;
        this.voice(pitch, 0.105, { slide: 0.52, volume: 0.022 + 0.014 * travel });
        this.noise(0.055, { from: 1800 + 900 * travel, to: 420, volume: 0.012 + 0.008 * travel });
      }
    } else this.lastStrideAt = null;
    if (!this.motionVoices) {
      this.motionVoices = ['sine', 'triangle'].map(type => {
        const tone = a.createOscillator(), out = a.createGain();
        tone.type = type;
        out.gain.value = 0;
        tone.connect(out).connect(a.destination);
        tone.start();
        return { tone, out };
      });
    }
    // 🌬️ And the look's air: a band of noise that opens and brightens with the turn, over the hum.
    if (!this.lookAir && turn > 0) {
      const rate = a.sampleRate, buffer = a.createBuffer(1, rate, rate), data = buffer.getChannelData(0);
      for (let i = 0; i < rate; i++) data[i] = Math.random() * 2 - 1;
      const src = a.createBufferSource(), band = a.createBiquadFilter(), out = a.createGain();
      src.buffer = buffer;
      src.loop = true;
      band.type = 'bandpass';
      band.frequency.value = 500;
      band.Q.value = 0.7;
      out.gain.value = 0;
      src.connect(band).connect(out).connect(a.destination);
      src.start();
      this.lookAir = { band, out };
    }
    if (this.lookAir) {
      const gain = this.lookAir.out.gain;
      this.lookAir.band.frequency.setTargetAtTime(450 + 900 * turn, t, 0.05);
      gain.cancelScheduledValues(t);
      gain.setTargetAtTime(0.03 * Math.sqrt(turn) * MASTER, t, 0.04);
      if (turn) gain.setTargetAtTime(0, t + 0.15, 0.06);
    }
    const [body, head] = this.motionVoices;
    body.tone.frequency.setTargetAtTime(125 + 85 * travel, t, 0.09);
    head.tone.frequency.setTargetAtTime(150 + 110 * turn, t, 0.035); // (a low hum for the look: it was 360 to 620 Hz, a whine)
    // Give each frame a short audio-clock lease. A suspended render loop
    // releases both notes even if it never delivers the final idle frame.
    for (const [voice, level, fade] of [[body, onFoot ? 0 : 0.002 * travel, 0.08], [head, 0.012 * Math.sqrt(turn), 0.025]]) {
      const gain = voice.out.gain;
      gain.cancelScheduledValues(t);
      gain.setTargetAtTime(level * MASTER, t, fade);
      if (level) gain.setTargetAtTime(0, t + 0.15, fade);
    }
  }
  // 🎥 The eye's own servos, heard while it is carried to a thing and back
  // (dock.mjs `rates`): one for the lens closing or opening, one for its
  // travel, one for its turning, each 0 to 1 by how fast that is changing.
  // A small motor each: a saw through a narrow band and a low-pass, its pitch
  // rising with the rate. Asked every frame; a rate under a hundredth is
  // silence, and each comes in and goes out over a few hundredths of a second,
  // so none clicks. Very quiet: at its fastest under the drone's own hush.
  servo(lens = 0, travel = 0, turn = 0) {
    if (!this.ready) return;
    const a = this.audio, t = a.currentTime, asked = [lens, travel, turn];
    if (!this.servos) {
      if (!asked.some((v) => v > 0.01)) return;
      // (Where each sits when barely moving and at full rate, in hertz: the lens a small high motor, the travel a low one, the turning between.)
      this.servos = [[310, 560], [96, 170], [170, 300]].map(([low, high]) => {
        const motor = a.createOscillator(), band = a.createBiquadFilter(), soft = a.createBiquadFilter(), out = a.createGain();
        motor.type = 'sawtooth';
        motor.frequency.value = low;
        band.type = 'bandpass';
        band.frequency.value = low * 2;
        band.Q.value = 2.5;
        soft.type = 'lowpass';
        soft.frequency.value = 1500;
        out.gain.value = 0;
        motor.connect(band).connect(soft).connect(out).connect(a.destination);
        motor.start();
        return { motor, band, out, low, high };
      });
    }
    this.servos.forEach((s, i) => {
      const r = Math.max(0, Math.min(1, asked[i] || 0)), on = r > 0.01, pitch = s.low + (s.high - s.low) * Math.sqrt(r);
      if (on) { s.motor.frequency.setTargetAtTime(pitch, t, 0.06); s.band.frequency.setTargetAtTime(pitch * 2, t, 0.06); } // (let go, it fades at the pitch it had: no falling whine)
      s.out.gain.setTargetAtTime(on ? (0.004 + 0.006 * r) * MASTER : 0, t, on ? 0.04 : 0.09);
    });
  }
  // 📉 Going down: as the picture comes apart (`gone`, 0 to 1) a tone falls
  // and breaks up into steps, under a rising hiss; at the end only a low hum.
  down(gone) {
    if (!this.ready) return;
    const a = this.audio, t = a.currentTime;
    if (!this.fall) {
      if (gone <= 0) return;
      const tone = a.createOscillator(), toneGain = a.createGain(), chop = a.createOscillator(), chopGain = a.createGain();
      tone.type = 'square';
      toneGain.gain.value = 0;
      chop.type = 'square';
      chop.frequency.value = 9;
      chopGain.gain.value = 0;
      chop.connect(chopGain).connect(toneGain.gain);
      tone.connect(toneGain).connect(a.destination);
      const buffer = a.createBuffer(1, a.sampleRate, a.sampleRate), data = buffer.getChannelData(0);
      // (Held samples: a hiss with its bits taken away.)
      for (let i = 0, held = 0; i < data.length; i++) { if (i % 6 === 0) held = Math.round((Math.random() * 2 - 1) * 4) / 4; data[i] = held; }
      const hiss = a.createBufferSource(), hissGain = a.createGain();
      hiss.buffer = buffer;
      hiss.loop = true;
      hissGain.gain.value = 0;
      hiss.connect(hissGain).connect(a.destination);
      tone.start(); chop.start(); hiss.start();
      this.fall = { tone, toneGain, chop, chopGain, hissGain };
    }
    const g = Math.max(0, Math.min(1, gone)), f = this.fall;
    f.tone.frequency.setTargetAtTime(g ? 660 * 2 ** (-3.5 * g) : 660, t, 0.05);
    f.chop.frequency.setTargetAtTime(6 + 30 * g, t, 0.1);
    f.toneGain.gain.setTargetAtTime(g ? (0.02 - 0.012 * g) * MASTER : 0, t, 0.08);
    f.chopGain.gain.setTargetAtTime(g ? 0.012 * g * MASTER : 0, t, 0.08);
    f.hissGain.gain.setTargetAtTime(g ? 0.03 * Math.sin(Math.PI * Math.min(1, g * 1.2)) * MASTER : 0, t, 0.1);
  }
  // 📶 A shoddy line (0 to 1): a crackle of held-sample hiss that comes in
  // gusts, thicker as the line gets worse; nothing on a good line.
  shoddy(bad) {
    if (!this.ready) return;
    const a = this.audio, t = a.currentTime;
    if (!this.crackle) {
      if (bad <= 0.05) return;
      const buffer = a.createBuffer(1, a.sampleRate, a.sampleRate), data = buffer.getChannelData(0);
      // (Pops on silence: sparse held samples, the bits taken away.)
      for (let i = 0, held = 0; i < data.length; i++) { if (i % 9 === 0) held = Math.random() < 0.08 ? Math.round((Math.random() * 2 - 1) * 3) / 3 : held * 0.6; data[i] = held; }
      const src = a.createBufferSource(), band = a.createBiquadFilter(), gain = a.createGain();
      src.buffer = buffer;
      src.loop = true;
      band.type = 'bandpass';
      band.frequency.value = 2400;
      band.Q.value = 0.7;
      gain.gain.value = 0;
      src.connect(band).connect(gain).connect(a.destination);
      src.start();
      this.crackle = { gain, band };
    }
    const g = bad > 0.05 ? Math.min(1, bad) : 0, gust = g ? 0.4 + 0.6 * Math.random() : 0;
    this.crackle.gain.gain.setTargetAtTime(0.035 * g * gust * MASTER, t, 0.03);
    this.crackle.band.frequency.setTargetAtTime(3200 - 1800 * g, t, 0.1);
  }
  lift(speed) {
    if (!this.ready) return;
    const a = this.audio, t = a.currentTime;
    if (!this.car) {
      if (speed <= 0) return;
      const hum = a.createOscillator(), low = a.createBiquadFilter(), humGain = a.createGain();
      hum.type = 'sawtooth';
      low.type = 'lowpass';
      low.frequency.value = 220;
      humGain.gain.value = 0;
      hum.connect(low).connect(humGain).connect(a.destination);
      hum.start();
      const buffer = a.createBuffer(1, a.sampleRate * 2, a.sampleRate), data = buffer.getChannelData(0);
      for (let i = 0; i < data.length; i++) data[i] = Math.random() * 2 - 1;
      const wind = a.createBufferSource(), band = a.createBiquadFilter(), windGain = a.createGain();
      wind.buffer = buffer;
      wind.loop = true;
      band.type = 'bandpass';
      band.Q.value = 0.7;
      windGain.gain.value = 0;
      wind.connect(band).connect(windGain).connect(a.destination);
      wind.start();
      this.car = { hum, humGain, band, windGain };
    }
    const s = Math.max(0, Math.min(1, speed)), c = this.car;
    c.hum.frequency.setTargetAtTime(38 + 46 * s, t, 0.08);
    c.humGain.gain.setTargetAtTime(s ? (0.012 + 0.02 * s) * MASTER : 0, t, 0.12);
    c.band.frequency.setTargetAtTime(300 + 1500 * s, t, 0.1);
    c.windGain.gain.setTargetAtTime(0.05 * s * s * MASTER, t, 0.12);
  }

  // A chord of sines, each voice gliding from from × its pitch to its pitch,
  // a little detuned against its neighbours so the chord shimmers.
  chord(steps, { root = 196, duration = 1, from = 1, attack = 0.06, volume = 0.012, stagger = 0.03, at = 0 } = {}) {
    if (!this.ready) return;
    const a = this.audio;
    steps.forEach((step, i) => {
      const t = a.currentTime + at + i * stagger,
        f = root * 2 ** (step / 12) * 2 ** (((i % 2 ? 1 : -1) * 4) / 1200),
        o = a.createOscillator(),
        g = a.createGain();
      o.type = 'sine';
      o.frequency.setValueAtTime(f * from, t);
      o.frequency.exponentialRampToValueAtTime(f, t + duration * 0.55);
      g.gain.setValueAtTime(0.0001, t);
      g.gain.exponentialRampToValueAtTime(volume * MASTER, t + attack);
      g.gain.setValueAtTime(volume * MASTER, t + duration * 0.45);
      g.gain.exponentialRampToValueAtTime(0.0001, t + duration);
      o.connect(g).connect(a.destination);
      o.start(t);
      o.stop(t + duration + 0.02);
    });
  }
  // Into the room from the prompt: a wide chord sliding up into place as the
  // paper drops; back to the paper: the chord sinks and gathers (2026-11-08).
  enter() {
    this.chord([0, 7, 14, 16, 23], { duration: 1.5, from: 0.66, attack: 0.12, stagger: 0.045 });
  }
  exit() {
    this.chord([23, 16, 14, 7, 0], { root: 196, duration: 0.8, from: 1.5, attack: 0.03, stagger: 0.02, volume: 0.011 });
  }

  // Game.
  // 🌀 A drone warps home: a tone falling away, a breath of hiss, and one rising where it lands.
  warp() {
    this.voice(880, 0.3, { slide: 0.2, volume: 0.03, type: 'square' });
    this.noise(0.6, { volume: 0.025, from: 3200, to: 200 });
    this.voice(140, 0.3, { slide: 5, volume: 0.03, type: 'square', at: 0.35 });
  }
  jump() {
    this.voice(220, 0.09, { slide: 1.8, volume: 0.025, type: 'square' });
  }
  // 🦶 Coming down from a jump or a drop: a low thud and a scuff, heavier the harder the landing (`hard`, 0 to 1).
  land(hard = 0) {
    this.voice(118, 0.11, { slide: 0.45, volume: 0.02 + 0.02 * hard });
    this.noise(0.07, { from: 900 + 700 * hard, to: 220, volume: 0.01 + 0.012 * hard });
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
  // 🍄 A bop: a rubbery boing for the one who landed it, a flat thud under it.
  // Each bop of a chain rings a step higher.
  bop(mine, combo = 1) {
    this.voice((mine ? 330 : 220) * 2 ** (Math.min(12, combo - 1) / 12), 0.16, { slide: 1.9, volume: 0.05, type: 'square' });
    this.noise(0.06, { volume: 0.03, from: 300, to: 1800 });
  }
  // 🔗 A chain landing: its points counted in, a rising run as long as the chain.
  bank(combo = 1) {
    for (let i = 0; i < Math.min(6, combo + 1); i++) this.voice(note(i * 2 + 4), 0.07, { volume: 0.03, at: i * 0.045 });
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
  // ⎋ Escape's two steps: the mouse let go (one high ding), then to the prompt (two notes falling).
  escStep(step) {
    if (step === 1) return void this.voice(note(9), 0.14, { volume: 0.03, type: 'sine' });
    this.voice(note(7), 0.1, { volume: 0.03, type: 'sine' });
    this.voice(note(2), 0.18, { volume: 0.03, type: 'sine', at: 0.07 });
  }
  ringBack() {
    this.voice(note(3), 0.08, { volume: 0.025, slide: 0.7 });
  }
  refuse() {
    this.voice(140, 0.1, { volume: 0.03, type: 'square' });
  }

  // 🧱 A build asked for and on its way to the server: one small note going up, under the ring's own level (the launch below is the server's yes); said again after a silence, the same note lower.
  buildAsk(again = false) {
    this.voice(note(again ? 2 : 7), 0.09, { volume: 0.016, type: 'sine', slide: 1.25 });
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
    // Every frame touches ~30 params. Skip what has not moved and clear what
    // was queued before, so automation timelines never pile up (WebKit keeps
    // them, and a long timeline costs on every frame) (2026-11-08).
    const glide = (param, value, k, eps) => {
      if (!param || Math.abs(param.value - value) < eps) return;
      param.cancelScheduledValues(t);
      param.setTargetAtTime(value, t, k);
    };
    const set = (param, value) => glide(param, value, 0.05, 0.01);
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
        v.gain.gain.cancelScheduledValues(t);
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
      glide(v.gain.gain, level, 0.008, 0.0005);
      glide(v.osc.frequency, Math.max(20, Math.min(4000, s.frequency(time, beat))), 0.004, 0.25);
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
