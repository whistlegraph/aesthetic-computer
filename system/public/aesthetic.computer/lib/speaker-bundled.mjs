// public/aesthetic.computer/lib/sound/volume.mjs
var amount = {
  val: 1
};
function apply(f32) {
  return f32 * amount.val;
}
var volume2 = {
  amount,
  apply
};

// public/aesthetic.computer/lib/num.mjs
var {
  abs,
  round,
  floor,
  ceil,
  random,
  PI,
  min,
  max,
  sqrt,
  pow,
  atan2,
  sin,
  cos
} = Math;
function clamp(value, low, high) {
  return min(max(value, low), high);
}
function within(range, a, b) {
  return abs(a - b) < range;
}
var cssColors = {
  aliceblue: [240, 248, 255],
  antiquewhite: [250, 235, 215],
  aqua: [0, 255, 255],
  aquamarine: [127, 255, 212],
  azure: [240, 255, 255],
  beige: [245, 245, 220],
  bisque: [255, 228, 196],
  black: [0, 0, 0],
  blanchedalmond: [255, 235, 205],
  blue: [0, 0, 255],
  blueviolet: [138, 43, 226],
  brown: [165, 42, 42],
  burlywood: [222, 184, 135],
  cadetblue: [95, 158, 160],
  chartreuse: [127, 255, 0],
  chocolate: [210, 105, 30],
  coral: [255, 127, 80],
  cornflowerblue: [100, 149, 237],
  cornsilk: [255, 248, 220],
  crimson: [220, 20, 60],
  cyan: [0, 255, 255],
  darkblue: [0, 0, 139],
  darkcyan: [0, 139, 139],
  darkgoldenrod: [184, 134, 11],
  darkgray: [169, 169, 169],
  darkgrey: [169, 169, 169],
  darkgreen: [0, 100, 0],
  darkkhaki: [189, 183, 107],
  darkmagenta: [139, 0, 139],
  darkolivegreen: [85, 107, 47],
  darkorange: [255, 140, 0],
  darkorchid: [153, 50, 204],
  darkred: [139, 0, 0],
  darksalmon: [233, 150, 122],
  darkseagreen: [143, 188, 143],
  darkslateblue: [72, 61, 139],
  darkslategray: [47, 79, 79],
  darkslategrey: [47, 79, 79],
  darkturquoise: [0, 206, 209],
  darkviolet: [148, 0, 211],
  deeppink: [255, 20, 147],
  deepskyblue: [0, 191, 255],
  dimgray: [105, 105, 105],
  dimgrey: [105, 105, 105],
  dodgerblue: [30, 144, 255],
  firebrick: [178, 34, 34],
  floralwhite: [255, 250, 240],
  forestgreen: [34, 139, 34],
  fuchsia: [255, 0, 255],
  gainsboro: [220, 220, 220],
  ghostwhite: [248, 248, 255],
  gold: [255, 215, 0],
  goldenrod: [218, 165, 32],
  gray: [128, 128, 128],
  grey: [128, 128, 128],
  green: [0, 128, 0],
  greenyellow: [173, 255, 47],
  honeydew: [240, 255, 240],
  hotpink: [255, 105, 180],
  indianred: [205, 92, 92],
  indigo: [75, 0, 130],
  ivory: [255, 255, 240],
  khaki: [240, 230, 140],
  lavender: [230, 230, 250],
  lavenderblush: [255, 240, 245],
  lawngreen: [124, 252, 0],
  lemonchiffon: [255, 250, 205],
  lightblue: [173, 216, 230],
  lightcoral: [240, 128, 128],
  lightcyan: [224, 255, 255],
  lightgoldenrodyellow: [250, 250, 210],
  lightgray: [211, 211, 211],
  lightgrey: [211, 211, 211],
  lightgreen: [144, 238, 144],
  lightpink: [255, 182, 193],
  lightsalmon: [255, 160, 122],
  lightseagreen: [32, 178, 170],
  lightskyblue: [135, 206, 250],
  lightslategray: [119, 136, 153],
  lightslategrey: [119, 136, 153],
  lightsteelblue: [176, 196, 222],
  lightyellow: [255, 255, 224],
  lime: [0, 255, 0],
  limegreen: [50, 205, 50],
  linen: [250, 240, 230],
  magenta: [255, 0, 255],
  maroon: [128, 0, 0],
  mediumaquamarine: [102, 205, 170],
  mediumblue: [0, 0, 205],
  mediumorchid: [186, 85, 211],
  mediumpurple: [147, 112, 219],
  mediumseagreen: [60, 179, 113],
  mediumslateblue: [123, 104, 238],
  mediumspringgreen: [0, 250, 154],
  mediumturquoise: [72, 209, 204],
  mediumvioletred: [199, 21, 133],
  midnightblue: [25, 25, 112],
  mintcream: [245, 255, 250],
  mistyrose: [255, 228, 225],
  moccasin: [255, 228, 181],
  navajowhite: [255, 222, 173],
  navy: [0, 0, 128],
  oldlace: [253, 245, 230],
  olive: [128, 128, 0],
  olivedrab: [107, 142, 35],
  orange: [255, 165, 0],
  orangered: [255, 69, 0],
  orchid: [218, 112, 214],
  palegoldenrod: [238, 232, 170],
  palegreen: [152, 251, 152],
  paleturquoise: [175, 238, 238],
  palevioletred: [219, 112, 147],
  papayawhip: [255, 239, 213],
  peachpuff: [255, 218, 185],
  peru: [205, 133, 63],
  pink: [255, 192, 203],
  plum: [221, 160, 221],
  powderblue: [176, 224, 230],
  purple: [128, 0, 128],
  rebeccapurple: [102, 51, 153],
  red: [255, 0, 0],
  rosybrown: [188, 143, 143],
  royalblue: [65, 105, 225],
  saddlebrown: [139, 69, 19],
  salmon: [250, 128, 114],
  sandybrown: [244, 164, 96],
  seagreen: [46, 139, 87],
  seashell: [255, 245, 238],
  sienna: [160, 82, 45],
  silver: [192, 192, 192],
  skyblue: [135, 206, 235],
  slateblue: [106, 90, 205],
  slategray: [112, 128, 144],
  slategrey: [112, 128, 144],
  snow: [255, 250, 250],
  springgreen: [0, 255, 127],
  steelblue: [70, 130, 180],
  tan: [210, 180, 140],
  teal: [0, 128, 128],
  thistle: [216, 191, 216],
  tomato: [255, 99, 71],
  turquoise: [64, 224, 208],
  violet: [238, 130, 238],
  wheat: [245, 222, 179],
  white: [255, 255, 255],
  whitesmoke: [245, 245, 245],
  yellow: [255, 255, 0],
  yellowgreen: [154, 205, 50],
  // Custom brown colors for AC
  darkbrown: [101, 67, 33],
  darkerbrown: [62, 39, 35],
  darksienna: [139, 90, 43]
};
var rainbowColors = [
  cssColors.red,
  cssColors.orange,
  cssColors.yellow,
  cssColors.green,
  cssColors.blue,
  cssColors.indigo,
  cssColors.violet
];
var zebraColors = [
  cssColors.black,
  // [0, 0, 0]
  cssColors.white
  // [255, 255, 255]
];
var organizedColorIndex = [
  // 0-15: Standard 16 web colors (c0=black, c1=white)
  "black",
  "white",
  "red",
  "lime",
  "blue",
  "yellow",
  "cyan",
  "magenta",
  "silver",
  "gray",
  "maroon",
  "olive",
  "green",
  "purple",
  "teal",
  "navy",
  // 17-32: Additional reds and pinks
  "crimson",
  "darkred",
  "firebrick",
  "indianred",
  "lightcoral",
  "salmon",
  "darksalmon",
  "lightsalmon",
  "pink",
  "lightpink",
  "hotpink",
  "deeppink",
  "palevioletred",
  "mediumvioletred",
  "coral",
  "tomato",
  // 33-48: Oranges
  "orange",
  "darkorange",
  "orangered",
  "chocolate",
  "saddlebrown",
  "sienna",
  "brown",
  "rosybrown",
  "sandybrown",
  "goldenrod",
  "darkgoldenrod",
  "peru",
  "burlywood",
  "tan",
  "navajowhite",
  "bisque",
  // 49-64: Yellows and golds
  "gold",
  "palegoldenrod",
  "khaki",
  "darkkhaki",
  "moccasin",
  "wheat",
  "lemonchiffon",
  "lightgoldenrodyellow",
  "lightyellow",
  "beige",
  "cornsilk",
  "blanchedalmond",
  "papayawhip",
  "antiquewhite",
  "linen",
  "oldlace",
  // 65-80: Greens
  "forestgreen",
  "darkgreen",
  "darkolivegreen",
  "darkseagreen",
  "limegreen",
  "seagreen",
  "mediumseagreen",
  "springgreen",
  "mediumspringgreen",
  "palegreen",
  "lightgreen",
  "lawngreen",
  "chartreuse",
  "greenyellow",
  "yellowgreen",
  "olivedrab",
  // 81-96: Blues and cyans
  "aqua",
  "darkturquoise",
  "turquoise",
  "mediumturquoise",
  "paleturquoise",
  "lightcyan",
  "cadetblue",
  "steelblue",
  "lightsteelblue",
  "powderblue",
  "lightblue",
  "skyblue",
  "lightskyblue",
  "deepskyblue",
  "dodgerblue",
  "cornflowerblue",
  // 97-112: More blues
  "royalblue",
  "mediumblue",
  "darkblue",
  "midnightblue",
  "slateblue",
  "darkslateblue",
  "mediumslateblue",
  "mediumpurple",
  "blueviolet",
  "indigo",
  "darkorchid",
  "darkviolet",
  "mediumorchid",
  "thistle",
  "plum",
  "violet",
  // 113-128: Purples and magentas
  "orchid",
  "fuchsia",
  "darkmagenta",
  "mediumvioletred",
  "lavenderblush",
  "mistyrose",
  "lavender",
  "ghostwhite",
  "azure",
  "aliceblue",
  "mintcream",
  "honeydew",
  "seashell",
  "ivory",
  "floralwhite",
  "snow",
  // 129-144: Grays and remaining colors
  "gainsboro",
  "lightgray",
  "lightgrey",
  "darkgray",
  "darkgrey",
  "dimgray",
  "dimgrey",
  "lightslategray",
  "lightslategrey",
  "slategray",
  "slategrey",
  "darkslategray",
  "darkslategrey",
  "whitesmoke",
  "rebeccapurple"
];
var remainingColors = Object.keys(cssColors).filter(
  (color) => !organizedColorIndex.includes(color)
);
var completeColorIndex = [...organizedColorIndex, ...remainingColors];

// public/aesthetic.computer/lib/sound/formant.mjs
var { sin: sin2, cos: cos2, PI: PI2, max: max2, min: min2 } = Math;
var VOWELS = {
  a: [{ freq: 730, bw: 80, gain: 1 }, { freq: 1090, bw: 90, gain: 0.5 }, { freq: 2440, bw: 120, gain: 0.25 }],
  e: [{ freq: 530, bw: 60, gain: 1 }, { freq: 1840, bw: 90, gain: 0.5 }, { freq: 2480, bw: 120, gain: 0.2 }],
  i: [{ freq: 270, bw: 60, gain: 1 }, { freq: 2290, bw: 90, gain: 0.35 }, { freq: 3010, bw: 100, gain: 0.2 }],
  o: [{ freq: 570, bw: 80, gain: 1 }, { freq: 840, bw: 90, gain: 0.5 }, { freq: 2410, bw: 120, gain: 0.2 }],
  u: [{ freq: 300, bw: 60, gain: 1 }, { freq: 870, bw: 90, gain: 0.35 }, { freq: 2240, bw: 120, gain: 0.15 }]
};
function vowelBands(spec, scale = 1) {
  const bands = typeof spec === "string" ? VOWELS[spec.toLowerCase()] : spec;
  if (!bands) return null;
  return bands.map(({ freq, bw, gain = 1 }) => ({ freq: freq * scale, bw, gain }));
}
var FormantBank = class {
  #n = 0;
  #sr;
  #b0;
  #a1;
  #a2;
  // RBJ constant-peak bandpass: b1 = 0, b2 = -b0.
  #gain;
  #x1;
  #x2;
  #y1;
  #y2;
  // Direct Form I history, per band.
  #norm = 1;
  // `sr` defaults to the AudioWorklet global; pass one when running elsewhere.
  constructor(bands, sr = globalThis.sampleRate) {
    this.#sr = sr;
    this.set(bands);
  }
  set(bands) {
    const n = bands.length;
    if (n !== this.#n) {
      this.#n = n;
      this.#b0 = new Float64Array(n);
      this.#a1 = new Float64Array(n);
      this.#a2 = new Float64Array(n);
      this.#gain = new Float64Array(n);
      this.#x1 = new Float64Array(n);
      this.#x2 = new Float64Array(n);
      this.#y1 = new Float64Array(n);
      this.#y2 = new Float64Array(n);
    }
    const nyq = this.#sr * 0.45;
    let peak = 0;
    for (let i = 0; i < n; i++) {
      const freq = min2(max2(bands[i].freq, 20), nyq);
      const bw = max2(bands[i].bw, 1);
      const w0 = 2 * PI2 * freq / this.#sr;
      const alpha = sin2(w0) * (bw / freq) / 2;
      const a0 = 1 + alpha;
      this.#b0[i] = alpha / a0;
      this.#a1[i] = -2 * cos2(w0) / a0;
      this.#a2[i] = (1 - alpha) / a0;
      const gain = bands[i].gain ?? 1;
      this.#gain[i] = gain;
      peak = max2(peak, gain);
    }
    this.#norm = peak > 0 ? 1 / peak : 1;
  }
  process(x) {
    let out = 0;
    for (let i = 0; i < this.#n; i++) {
      const y = this.#b0[i] * (x - this.#x2[i]) - this.#a1[i] * this.#y1[i] - this.#a2[i] * this.#y2[i];
      this.#x2[i] = this.#x1[i];
      this.#x1[i] = x;
      this.#y2[i] = this.#y1[i];
      this.#y1[i] = y;
      out += y * this.#gain[i];
    }
    return out * this.#norm;
  }
};

// public/aesthetic.computer/lib/sound/synth.mjs
var { abs: abs2, floor: floor2, round: round2, sin: sin3, cos: cos3, PI: PI3, min: min3, max: max3, random: random2, pow: pow2 } = Math;
var Synth = class _Synth {
  // Generic for all instruments.
  playing = true;
  id;
  // Unique for every playing instrument.
  // 🍿 Shortest release any kill can ask for. Below ~2ms the ramp stops
  // rounding the waveform's corner and starts sounding like the cut it
  // replaced.
  static MIN_RELEASE = 2e-3;
  fading = false;
  // If we are fading and then stopping playback.
  fadeGain = 1;
  // Release multiplier — walks 1 → 0 and never back up.
  fadeStep = 0;
  // How much `fadeGain` drops per sample.
  fadeProgress;
  fadeDuration;
  type;
  // square, sine, triangle, sawtooth, sample, noise-white, custom
  #phase = 0;
  #frequency;
  #duration = 0;
  #attack = 0;
  #decay = 0;
  #decayStart;
  volume = 1;
  // 0 to 1
  #futureVolume = 1;
  #pan = 0;
  // -1 to 1
  #progress = 0;
  #wavelength;
  // Calculated from the frequency.
  #futureFrequency;
  #frequencyUpdatesTotal;
  #frequencyUpdatesLeft;
  #frequencyUpdateSlice;
  #volumeUpdatesTotal;
  #volumeUpdatesLeft;
  #volumeUpdateSlice;
  #sampleData;
  // Specific to `sample`.
  #sampleIndex = 0;
  #declick = 0;
  // Samples left in the post-relocation attack ramp
  #declickTotal = 1;
  #pitch = 1;
  // Independent pitch factor (1 = none) — tempo untouched
  #pitchPhase = 0;
  // Sweep phase of the two-tap delay-line shifter
  #sampleEndIndex = 0;
  #sampleStartIndex = 0;
  #sampleSpeed = 0.25;
  #sampleLoop = false;
  #preserveDuration = false;
  // If true, pitch shift without changing duration (granular)
  #targetDurationSamples = 0;
  // Original duration in samples when preserving
  #playedSamples = 0;
  // Track how many samples we've output
  // Time stretch + pitch shift fields
  #timeStretchEnabled = false;
  // If true, stretch sample to targetDuration, then pitch shift
  #targetDurationMs = 0;
  // Target duration in milliseconds (for time stretch mode)
  #timeStretchRatio = 1;
  // How much to stretch/compress time (>1 = slower, <1 = faster)
  #outputSamplesNeeded = 0;
  // How many output samples to produce
  // Granular pitch shifting fields
  #grainSize = 2048;
  // Size of each grain in samples (~46ms at 44100Hz)
  #grainOverlap = 4;
  // Number of overlapping grains (more = smoother)
  #grains = [];
  // Array of active grains
  #grainPhase = 0;
  // Phase for spawning new grains
  #sourcePosition = 0;
  // Position in source buffer (independent of output)
  #up = false;
  // Specific to `square`.
  #step = 0;
  // Specific to `noise-white` filtering
  #noiseFilterState1 = 0;
  #noiseFilterState2 = 0;
  #noiseFilterState3 = 0;
  #noiseFilterState4 = 0;
  // Specific to `harp` — Karplus-Strong plucked string.
  // Refs: Karplus & Strong (1983); Jaffe & Smith EKS (1983);
  //       Smith, "Physical Audio Signal Processing" — CCRMA Stanford.
  // Mirrors the C generate_harp_sample in fedac/native/src/audio.c.
  #harpBuf = null;
  // Float32Array — string delay line
  #harpW = 0;
  // write index
  #harpLp1 = 0;
  // 1-pole moving-average LPF state
  // Specific to `whistle` — Cook/STK digital waveguide flute model.
  // Mirrors the C generate_whistle_sample in fedac/native/src/audio.c.
  #whistleBoreBuf = null;
  // bore delay line
  #whistleBoreW = 0;
  #whistleJetBuf = null;
  // jet delay line
  #whistleJetW = 0;
  #whistleBreath = 0;
  // smoothed breath pressure
  #whistleVibratoPhase = 0;
  // 5 Hz LFO phase
  #whistleLp1 = 0;
  // 1-pole loop LPF state
  #whistleHpX1 = 0;
  // 1-pole DC blocker — last input
  #whistleHpY1 = 0;
  // 1-pole DC blocker — last output
  #whistleNoiseSeed = 0;
  // xorshift32 state
  // Custom waveform generation
  #customGenerator;
  // Function that generates waveform data
  #customBuffer = [];
  // Buffer for streaming waveform data
  #customBufferSize = 1024;
  // Size of the streaming buffer
  // 🐦 Expression — slide, vibrato, drift bend the pitch; noise, formant and
  // lowpass shape the source; tremolo rides the level. Everything stays off
  // (null / 0) unless asked for, so a plain voice renders exactly as before.
  #slideLeft = 0;
  // samples left in the exponential glide
  #slideRatio = 1;
  // per-sample frequency multiplier while sliding
  #slideTarget = 0;
  // snapped to at the end so rounding never leaves it off-pitch
  #vibrato = null;
  // { rate, depth, delay } — depth in semitones, delay in samples
  #vibratoPhase = 0;
  #drift = 0;
  // semitones of wander
  #driftPos = 0;
  // smoothed position, -1..1
  #driftTarget = 0;
  #driftCountdown = 0;
  // samples until the walk picks a new target
  #liveFrequency;
  // the modulated pitch the last sample was rendered at
  #noise = 0;
  // white-noise mix, 0..1
  #formant = null;
  // FormantBank
  #lowpass = null;
  // { cutoff, q } — RBJ lowpass, coefficients cached below
  #lpB0 = 0;
  #lpB1 = 0;
  #lpA1 = 0;
  #lpA2 = 0;
  #lpX1 = 0;
  #lpX2 = 0;
  #lpY1 = 0;
  #lpY2 = 0;
  #lpSweepLeft = 0;
  // samples left in the cutoff sweep
  #lpSweepRatio = 1;
  #lpSweepTarget = 0;
  #tremolo = null;
  // { rate, depth }
  #tremoloPhase = 0;
  constructor({ type, id, options, duration, attack, decay, volume, pan }) {
    if (type === "noise") type = "noise-white";
    this.type = type;
    if (id === void 0 || id === null || id === NaN)
      console.warn("\u23F0 No id for sound:", id, type);
    this.id = id;
    if (type === "square" || type === "sine" || type === "triangle" || type === "sawtooth") {
      this.#frequency = options.tone;
    } else if (type === "harp" || type === "pluck" || type === "guitar" || type === "string") {
      this.#frequency = options.tone;
      const N = 2048;
      this.#harpBuf = new Float32Array(N);
      const stringDelay = clamp(sampleRate / this.#frequency, 2, N - 2);
      const n = Math.floor(stringDelay);
      let last = 0;
      for (let i = 0; i < n; i++) {
        const white = random2() * 2 - 1;
        const filt = 0.5 * (white + last);
        last = white;
        this.#harpBuf[i] = filt;
      }
      this.#harpW = n;
      this.type = "harp";
    } else if (type === "whistle" || type === "ocarina" || type === "flute" || type === "skullwhistle" || type === "skull-whistle") {
      this.#frequency = options.tone;
      this.#whistleBoreBuf = new Float32Array(2048);
      this.#whistleJetBuf = new Float32Array(512);
      this.#whistleNoiseSeed = (Number(id) || 1) * 2654435761 >>> 0;
      this.type = "whistle";
    } else if (type === "sample") {
      this.#frequency = null;
      this.#sampleData = options.buffer;
      this.sampleLabel = options.label;
      this.#sampleSpeed = options.speed || 1;
      this.#sampleLoop = options.loop || false;
      this.#preserveDuration = options.preserveDuration || false;
      if (this.#preserveDuration) {
      }
      const sampleLength = this.#sampleData.channels?.[0]?.length ?? this.#sampleData.length;
      this.#sampleStartIndex = clamp(
        options.startSample,
        0,
        sampleLength - 1
      );
      this.#sampleEndIndex = clamp(
        options.endSample,
        0,
        sampleLength - 1
      );
      this.#sampleIndex = this.#sampleSpeed < 0 ? this.#sampleEndIndex : this.#sampleStartIndex;
      if (options.targetDuration > 0) {
        this.#timeStretchEnabled = true;
        this.#targetDurationMs = options.targetDuration;
        const sampleRate2 = options.sampleRate || 44100;
        this.#outputSamplesNeeded = Math.floor(this.#targetDurationMs / 1e3 * sampleRate2);
        const sourceSamples = this.#sampleEndIndex - this.#sampleStartIndex;
        this.#timeStretchRatio = sourceSamples / this.#outputSamplesNeeded;
        const minDurationMs = 50;
        if (this.#targetDurationMs < minDurationMs) {
          this.#targetDurationMs = minDurationMs;
          this.#outputSamplesNeeded = Math.floor(this.#targetDurationMs / 1e3 * sampleRate2);
          this.#timeStretchRatio = sourceSamples / this.#outputSamplesNeeded;
        }
        this.#playedSamples = 0;
        this.#sourcePosition = this.#sampleStartIndex;
        this.#grains = [];
        this.#grainPhase = 0;
        this.#grainSize = Math.min(2048, Math.floor(sourceSamples / 8));
        this.#grainSize = Math.max(256, this.#grainSize);
      } else if (this.#preserveDuration) {
        this.#targetDurationSamples = this.#sampleEndIndex - this.#sampleStartIndex;
        this.#playedSamples = 0;
        this.#sourcePosition = this.#sampleStartIndex;
        this.#grains = [];
        this.#grainPhase = 0;
        const sampleDuration = this.#targetDurationSamples;
        this.#grainSize = Math.min(2048, Math.floor(sampleDuration / 8));
        this.#grainSize = Math.max(256, this.#grainSize);
      }
    } else if (type === "custom") {
      this.#frequency = options.tone || 440;
      if (typeof options.generator === "string") {
        try {
          this.#customGenerator = eval(`(${options.generator})`);
        } catch (error) {
          console.error("\u{1F3A8} Failed to parse custom generator:", error);
          throw new Error("Invalid custom generator function string");
        }
      } else {
        this.#customGenerator = options.generator;
      }
      if (typeof this.#customGenerator !== "function") {
        throw new Error("Custom synth type requires a generator function");
      }
      this._fillCustomBuffer();
    } else if (type === "noise-white") {
      this.#frequency = options.tone;
      this.#noiseFilterState1 = 0;
      this.#noiseFilterState2 = 0;
      this.#noiseFilterState3 = 0;
      this.#noiseFilterState4 = 0;
    }
    this.#wavelength = sampleRate / this.#frequency;
    this.#futureFrequency = this.#frequency;
    this.#liveFrequency = this.#frequency;
    this.#attack = attack;
    this.#duration = this.type === "sample" ? Infinity : duration;
    this.#decay = decay;
    this.#decayStart = this.#duration - this.#decay;
    this.#pan = pan;
    this.volume = volume;
    this.#futureVolume = this.volume;
    if (this.type !== "sample") {
      const span = this.#duration < Infinity ? this.#duration / sampleRate : 0.25;
      this._express(options, span);
    }
  }
  // Parse the expression options shared by the constructor and update().
  // `span` is the fallback glide length in seconds.
  _express(o, span) {
    if (o.slide > 0 && this.#frequency > 0) {
      const seconds = o.slideDuration > 0 ? o.slideDuration : span;
      this.#slideTarget = o.slide;
      this.#slideLeft = max3(1, round2(seconds * sampleRate));
      this.#slideRatio = pow2(o.slide / this.#frequency, 1 / this.#slideLeft);
      this.#futureFrequency = o.slide;
    }
    if (o.vibrato !== void 0) {
      const v = typeof o.vibrato === "number" ? { depth: o.vibrato } : o.vibrato;
      this.#vibrato = v && (v.depth ?? 0.5) > 0 ? { rate: v.rate ?? 5, depth: v.depth ?? 0.5, delay: (v.delay ?? 0) * sampleRate } : null;
      this.#vibratoPhase = 0;
    }
    if (o.tremolo !== void 0) {
      const t = typeof o.tremolo === "number" ? { depth: o.tremolo } : o.tremolo;
      this.#tremolo = t && (t.depth ?? 0.3) > 0 ? { rate: t.rate ?? 6, depth: min3(1, t.depth ?? 0.3) } : null;
      this.#tremoloPhase = 0;
    }
    if (o.drift !== void 0) this.#drift = o.drift > 0 ? o.drift : 0;
    if (o.noise !== void 0) this.#noise = clamp(o.noise, 0, 1) || 0;
    if (o.formant !== void 0) {
      const f = o.formant;
      const bands = f == null ? null : typeof f === "string" || Array.isArray(f) ? vowelBands(f) : vowelBands(f.vowel ?? f.bands, f.scale ?? 1);
      if (!bands) this.#formant = null;
      else if (this.#formant) this.#formant.set(bands);
      else this.#formant = new FormantBank(bands, sampleRate);
    }
    if (o.lowpass !== void 0) {
      const lp = typeof o.lowpass === "number" ? { cutoff: o.lowpass } : o.lowpass;
      if (!lp || !(lp.cutoff > 0)) {
        this.#lowpass = null;
        this.#lpSweepLeft = 0;
      } else {
        const q = 0.7071 * pow2(2, clamp(lp.resonance ?? 0.2, 0, 1) * 4);
        const fresh = !this.#lowpass;
        if (fresh) this.#lpX1 = this.#lpX2 = this.#lpY1 = this.#lpY2 = 0;
        const from = fresh ? lp.cutoff : this.#lowpass.cutoff;
        const to = lp.sweep > 0 ? lp.sweep : lp.cutoff;
        this.#lowpass = { cutoff: from, q };
        this._lowpassCoefficients();
        if (to !== from) {
          const seconds = lp.sweepDuration > 0 ? lp.sweepDuration : span;
          this.#lpSweepTarget = to;
          this.#lpSweepLeft = max3(1, round2(seconds * sampleRate));
          this.#lpSweepRatio = pow2(to / from, 1 / this.#lpSweepLeft);
        } else {
          this.#lpSweepLeft = 0;
        }
      }
    }
  }
  _lowpassCoefficients() {
    const { cutoff, q } = this.#lowpass;
    const w0 = 2 * PI3 * min3(cutoff, sampleRate * 0.45) / sampleRate;
    const c = cos3(w0);
    const alpha = sin3(w0) / (2 * q);
    const a0 = 1 + alpha;
    const gain = q > 0.7071 ? 1 / Math.sqrt(q / 0.7071) : 1;
    this.#lpB0 = (1 - c) / 2 / a0 * gain;
    this.#lpB1 = (1 - c) / a0 * gain;
    this.#lpA1 = -2 * c / a0;
    this.#lpA2 = (1 - alpha) / a0;
  }
  next(channelIndex) {
    if (!this.playing) return 0;
    if (this.#frequencyUpdatesLeft > 0) {
      this.#frequency += this.#frequencyUpdateSlice;
      this.#wavelength = sampleRate / this.#frequency;
      this.#frequencyUpdatesLeft -= 1;
    }
    if (this.#volumeUpdatesLeft > 0) {
      this.volume += this.#volumeUpdateSlice;
      this.#volumeUpdatesLeft -= 1;
    }
    let freq = this.#frequency;
    if (this.#slideLeft > 0) {
      this.#slideLeft -= 1;
      this.#frequency = this.#slideLeft === 0 ? this.#slideTarget : this.#frequency * this.#slideRatio;
      freq = this.#frequency;
      this.#wavelength = sampleRate / freq;
    }
    if (this.#vibrato || this.#drift > 0) {
      let cents = 0;
      if (this.#vibrato) {
        const v = this.#vibrato;
        const amount2 = min3(1, max3(0, (this.#progress - v.delay) / (0.15 * sampleRate)));
        this.#vibratoPhase += 2 * PI3 * v.rate / sampleRate;
        if (this.#vibratoPhase > 2 * PI3) this.#vibratoPhase -= 2 * PI3;
        cents += v.depth * amount2 * sin3(this.#vibratoPhase);
      }
      if (this.#drift > 0) {
        if (this.#driftCountdown <= 0) {
          this.#driftTarget = random2() * 2 - 1;
          this.#driftCountdown = round2(sampleRate * (0.3 + random2() * 0.4));
        }
        this.#driftCountdown -= 1;
        this.#driftPos += (this.#driftTarget - this.#driftPos) * (2 * PI3 * 2 / sampleRate);
        cents += this.#drift * this.#driftPos;
      }
      freq *= pow2(2, cents / 12);
      this.#wavelength = sampleRate / freq;
    }
    this.#liveFrequency = freq;
    let value;
    if (this.type === "square") {
      const halfWavelength = this.#wavelength / 2;
      this.#step += 1;
      if (this.#step >= halfWavelength) {
        this.#up = !this.#up;
        this.#step -= halfWavelength;
      }
      value = this.#up ? 1 : -1;
    } else if (this.type === "sine") {
      const increment = 2 * PI3 * freq / sampleRate;
      this.#phase += increment;
      if (this.#phase > 2 * PI3) {
        this.#phase -= 2 * PI3;
      }
      value = sin3(this.#phase);
    } else if (this.type === "triangle") {
      const stepSize = 4 / this.#wavelength;
      const adjustedStep = (this.#step + this.#wavelength / 4) % this.#wavelength;
      value = 1 - abs2(adjustedStep * stepSize - 2);
      this.#step += 1;
      if (this.#step >= this.#wavelength) this.#step = 0;
    } else if (this.type === "sawtooth") {
      value = 2 * (this.#step / this.#wavelength) - 1;
      this.#step += 1;
      if (this.#step >= this.#wavelength) this.#step = 0;
    } else if (this.type === "noise-white") {
      const noise = random2() * 2 - 1;
      if (freq && freq > 0) {
        const normalizedFreq = freq * 2 / sampleRate;
        const clampedFreq = clamp(normalizedFreq, 1e-3, 0.99);
        const resonance = 0.1;
        const omega = clampedFreq * PI3;
        const sin8 = Math.sin(omega);
        const cos5 = Math.cos(omega);
        const alpha = sin8 / (2 * (1 / resonance));
        const b0 = (1 - cos5) / 2;
        const b1 = 1 - cos5;
        const b2 = (1 - cos5) / 2;
        const a0 = 1 + alpha;
        const a1 = -2 * cos5;
        const a2 = 1 - alpha;
        const output = (b0 * noise + b1 * this.#noiseFilterState1 + b2 * this.#noiseFilterState2 - a1 * this.#noiseFilterState3 - a2 * this.#noiseFilterState4) / a0;
        this.#noiseFilterState2 = this.#noiseFilterState1;
        this.#noiseFilterState1 = noise;
        this.#noiseFilterState4 = this.#noiseFilterState3;
        this.#noiseFilterState3 = output;
        value = output * 3.5;
      } else {
        value = noise;
      }
    } else if (this.type === "harp") {
      const N = this.#harpBuf.length;
      const stringDelay = clamp(sampleRate / freq, 2, N - 2);
      let rd = this.#harpW - stringDelay;
      while (rd < 0) rd += N;
      const i0 = floor2(rd) | 0;
      const i1 = (i0 + 1) % N;
      const f = rd - i0;
      const delayed = this.#harpBuf[i0] * (1 - f) + this.#harpBuf[i1] * f;
      const filtered = 0.5 * (delayed + this.#harpLp1);
      this.#harpLp1 = delayed;
      const stretch = this.#decay > 0 && this.#decay < 0.2 ? 0.99 : 0.9985;
      const decayed = filtered * stretch;
      this.#harpBuf[this.#harpW] = decayed;
      this.#harpW = (this.#harpW + 1) % N;
      value = 2.5 * decayed;
    } else if (this.type === "whistle") {
      const BORE_N = 2048, JET_N = 512;
      const env = 1;
      const breathTarget = 0.18 + 0.82 * Math.sqrt(env);
      const breathSlew = env > this.#whistleBreath ? 0.012 : 3e-3;
      this.#whistleBreath += (breathTarget - this.#whistleBreath) * breathSlew;
      this.#whistleVibratoPhase += 5 / sampleRate;
      if (this.#whistleVibratoPhase >= 1) this.#whistleVibratoPhase -= 1;
      const vibrato = sin3(2 * PI3 * this.#whistleVibratoPhase) * 0.03;
      let s = this.#whistleNoiseSeed;
      s ^= s << 13;
      s >>>= 0;
      s ^= s >>> 17;
      s ^= s << 5;
      s >>>= 0;
      this.#whistleNoiseSeed = s;
      const white = s / 4294967295 * 2 - 1;
      const breath = this.#whistleBreath * (1 + 0.08 * white + vibrato);
      const pitch = clamp(freq, 30, sampleRate * 0.2);
      let boreDelay = sampleRate / pitch;
      let jetDelay = boreDelay * 0.32;
      if (boreDelay > BORE_N - 2) boreDelay = BORE_N - 2;
      if (jetDelay > JET_N - 2) jetDelay = JET_N - 2;
      let rd = this.#whistleBoreW - boreDelay;
      while (rd < 0) rd += BORE_N;
      let i0 = floor2(rd) | 0;
      let i1 = (i0 + 1) % BORE_N;
      let frac = rd - i0;
      const boreOut = this.#whistleBoreBuf[i0] * (1 - frac) + this.#whistleBoreBuf[i1] * frac;
      this.#whistleLp1 = 0.35 * -boreOut + 0.65 * this.#whistleLp1;
      const temp = this.#whistleLp1;
      let pd = breath - 0.5 * temp;
      this.#whistleJetBuf[this.#whistleJetW] = pd;
      this.#whistleJetW = (this.#whistleJetW + 1) % JET_N;
      rd = this.#whistleJetW - jetDelay;
      while (rd < 0) rd += JET_N;
      i0 = floor2(rd) | 0;
      i1 = (i0 + 1) % JET_N;
      frac = rd - i0;
      pd = this.#whistleJetBuf[i0] * (1 - frac) + this.#whistleJetBuf[i1] * frac;
      pd = pd * (pd * pd - 1);
      if (pd > 1) pd = 1;
      if (pd < -1) pd = -1;
      const y = pd - this.#whistleHpX1 + 0.995 * this.#whistleHpY1;
      this.#whistleHpX1 = pd;
      this.#whistleHpY1 = y;
      const intoBore = y + 0.5 * temp;
      this.#whistleBoreBuf[this.#whistleBoreW] = intoBore;
      this.#whistleBoreW = (this.#whistleBoreW + 1) % BORE_N;
      value = 0.3 * intoBore;
    } else if (this.type === "sample") {
      const bufferData = this.#sampleData.channels[0];
      if (this.#preserveDuration) {
        this.#playedSamples++;
        const grainSpacing = this.#grainSize / this.#grainOverlap;
        this.#grainPhase++;
        if (this.#grainPhase >= grainSpacing && this.#sourcePosition < this.#sampleEndIndex) {
          this.#grainPhase = 0;
          this.#grains.push({
            sourceStart: this.#sourcePosition,
            position: 0
            // Position within grain (0 to grainSize)
          });
        }
        this.#sourcePosition += 1;
        value = 0;
        const activeGrains = [];
        for (const grain of this.#grains) {
          const grainProgress = grain.position / this.#grainSize;
          const envelope2 = 0.5 * (1 - Math.cos(2 * Math.PI * grainProgress));
          const sourceIdx = grain.sourceStart + grain.position * this.#sampleSpeed;
          if (sourceIdx >= this.#sampleEndIndex || sourceIdx < this.#sampleStartIndex) {
            grain.position++;
            if (grain.position < this.#grainSize) {
              activeGrains.push(grain);
            }
            continue;
          }
          const idx0 = floor2(sourceIdx);
          const idx1 = idx0 + 1 < this.#sampleEndIndex ? idx0 + 1 : idx0;
          const frac = sourceIdx - idx0;
          const sample0 = bufferData[idx0] || 0;
          const sample1 = bufferData[idx1] || 0;
          const interpolatedSample = sample0 + frac * (sample1 - sample0);
          value += interpolatedSample * envelope2;
          grain.position++;
          if (grain.position < this.#grainSize) {
            activeGrains.push(grain);
          }
        }
        this.#grains = activeGrains;
        value /= this.#grainOverlap / 2;
        if (this.#grains.length === 0 && this.#sourcePosition >= this.#sampleEndIndex) {
          this.playing = false;
          return 0;
        }
      } else if (this.#timeStretchEnabled) {
        this.#playedSamples++;
        const grainSpacing = this.#grainSize / this.#grainOverlap;
        this.#grainPhase++;
        if (this.#grainPhase >= grainSpacing && this.#sourcePosition < this.#sampleEndIndex) {
          this.#grainPhase = 0;
          this.#grains.push({
            sourceStart: this.#sourcePosition,
            position: 0
            // Position within grain (0 to grainSize)
          });
        }
        this.#sourcePosition += this.#timeStretchRatio;
        value = 0;
        const activeGrains = [];
        for (const grain of this.#grains) {
          const grainProgress = grain.position / this.#grainSize;
          const envelope2 = 0.5 * (1 - Math.cos(2 * Math.PI * grainProgress));
          const sourceIdx = grain.sourceStart + grain.position * this.#sampleSpeed;
          if (sourceIdx >= this.#sampleEndIndex || sourceIdx < this.#sampleStartIndex) {
            grain.position++;
            if (grain.position < this.#grainSize) {
              activeGrains.push(grain);
            }
            continue;
          }
          const idx0 = floor2(sourceIdx);
          const idx1 = idx0 + 1 < this.#sampleEndIndex ? idx0 + 1 : idx0;
          const frac = sourceIdx - idx0;
          const sample0 = bufferData[idx0] || 0;
          const sample1 = bufferData[idx1] || 0;
          const interpolatedSample = sample0 + frac * (sample1 - sample0);
          value += interpolatedSample * envelope2;
          grain.position++;
          if (grain.position < this.#grainSize) {
            activeGrains.push(grain);
          }
        }
        this.#grains = activeGrains;
        value /= this.#grainOverlap / 2;
        if (this.#playedSamples >= this.#outputSamplesNeeded || this.#grains.length === 0 && this.#sourcePosition >= this.#sampleEndIndex) {
          this.playing = false;
          return 0;
        }
      } else {
        if (this.#pitch !== 1 && this.#sampleLoop) {
          const W = 2048;
          this.#pitchPhase = (this.#pitchPhase + (this.#pitch - 1) * this.#sampleSpeed + W) % W;
          const offA = this.#pitchPhase;
          const offB = (offA + W / 2) % W;
          const range = this.#sampleEndIndex - this.#sampleStartIndex;
          const rd = (off) => {
            let ix = this.#sampleIndex - off;
            while (ix < this.#sampleStartIndex) ix += range;
            while (ix >= this.#sampleEndIndex) ix -= range;
            return bufferData[floor2(ix)];
          };
          const gA = Math.sin(Math.PI * offA / W);
          const gB = Math.sin(Math.PI * offB / W);
          value = rd(offA) * gA + rd(offB) * gB;
        } else {
          value = bufferData[floor2(this.#sampleIndex)];
        }
        if (this.#declick > 0) {
          value *= 1 - this.#declick / this.#declickTotal;
          this.#declick -= 1;
        }
        this.#sampleIndex += this.#sampleSpeed;
        if (this.#sampleLoop) {
          if (this.#sampleIndex > this.#sampleEndIndex) {
            const rangeLength = this.#sampleEndIndex - this.#sampleStartIndex;
            const overshoot = this.#sampleIndex - this.#sampleEndIndex;
            this.#sampleIndex = this.#sampleStartIndex + overshoot % rangeLength;
          } else if (this.#sampleIndex < this.#sampleStartIndex) {
            const rangeLength = this.#sampleEndIndex - this.#sampleStartIndex;
            const undershoot = this.#sampleStartIndex - this.#sampleIndex;
            this.#sampleIndex = this.#sampleEndIndex - undershoot % rangeLength;
          }
        } else {
          if (this.#sampleIndex >= this.#sampleEndIndex || this.#sampleIndex < 0) {
            this.playing = false;
            return 0;
          }
        }
      }
    } else if (this.type === "custom") {
      if (this.#customBuffer.length === 0) {
        this._fillCustomBuffer();
      }
      if (this.#customBuffer.length > 0) {
        value = this.#customBuffer.shift();
      } else {
        value = 0;
      }
      if (this.#customBuffer.length < this.#customBufferSize / 4) {
        this._fillCustomBuffer();
      }
    }
    if (this.type !== "sample") {
      if (this.#noise > 0) {
        value = value * (1 - this.#noise * 0.5) + (random2() * 2 - 1) * this.#noise;
      }
      if (this.#formant) value = this.#formant.process(value);
      if (this.#lowpass) {
        if (this.#lpSweepLeft > 0) {
          this.#lpSweepLeft -= 1;
          this.#lowpass.cutoff = this.#lpSweepLeft === 0 ? this.#lpSweepTarget : this.#lowpass.cutoff * this.#lpSweepRatio;
          this._lowpassCoefficients();
        }
        const y = this.#lpB0 * (value + this.#lpX2) + this.#lpB1 * this.#lpX1 - this.#lpA1 * this.#lpY1 - this.#lpA2 * this.#lpY2;
        this.#lpX2 = this.#lpX1;
        this.#lpX1 = value;
        this.#lpY2 = this.#lpY1;
        this.#lpY1 = y;
        value = y;
      }
      if (this.#tremolo) {
        const t = this.#tremolo;
        this.#tremoloPhase += 2 * PI3 * t.rate / sampleRate;
        if (this.#tremoloPhase > 2 * PI3) this.#tremoloPhase -= 2 * PI3;
        value *= 1 - t.depth * (0.5 + 0.5 * sin3(this.#tremoloPhase));
      }
    }
    if (this.#duration < Infinity) {
      if (this.type === "noise-white") {
        const sharpAttack = min3(1, this.#progress / (this.#attack * 0.1));
        if (sharpAttack) value *= sharpAttack;
        const decayProgress = (this.#progress - this.#decayStart) / (this.#decay * 0.05);
        const sharpDecay = min3(1, 1 - Math.pow(decayProgress, 3));
        value *= max3(0, sharpDecay);
      } else {
        const attack2 = min3(1, this.#progress / this.#attack);
        if (attack2) value *= attack2;
        const decay2 = min3(
          1,
          1 - (this.#progress - this.#decayStart) / this.#decay
        );
        value *= decay2;
      }
    } else {
      if (this.#attack > 0) {
        const attack2 = min3(1, this.#progress / this.#attack);
        value *= attack2;
      }
    }
    this.#progress += 1;
    if (this.#progress >= this.#duration) {
      this.playing = false;
      return 0;
    }
    let out = value * this.volume;
    if (this.fading) {
      this.fadeGain -= this.fadeStep;
      if (this.fadeGain <= 0) {
        this.fadeGain = 0;
        this.fading = false;
        this.playing = false;
        return 0;
      }
      this.fadeProgress += 1;
      out *= this.fadeGain;
    }
    return out;
  }
  update({ tone, volume: volume3, shift, sampleSpeed, samplePosition, sampleData, pitch, duration: duration2 = 0.1, ...expression }) {
    if (typeof pitch === "number" && pitch > 0) {
      this.#pitch = pitch;
      if (pitch === 1) this.#pitchPhase = 0;
    }
    if (this.type !== "sample") this._express(expression, duration2);
    if (typeof tone === "number" && tone > 0) {
      this.#futureFrequency = tone;
      this.#frequencyUpdatesTotal = duration2 * sampleRate;
      this.#frequencyUpdatesLeft = this.#frequencyUpdatesTotal;
      this.#frequencyUpdateSlice = (this.#futureFrequency - this.#frequency) / this.#frequencyUpdatesTotal;
    }
    if (typeof volume3 === "number") {
      this.#futureVolume = volume3;
      this.#volumeUpdatesTotal = duration2 * sampleRate;
      this.#volumeUpdatesLeft = this.#volumeUpdatesTotal;
      this.#volumeUpdateSlice = (this.#futureVolume - this.volume) / this.#volumeUpdatesTotal;
    }
    if (typeof shift === "number") {
      const oldSpeed = this.#sampleSpeed;
      this.#sampleSpeed += shift;
    }
    if (typeof sampleSpeed === "number") {
      this.#sampleSpeed = sampleSpeed;
    }
    if (typeof samplePosition === "number" && this.#sampleData) {
      const len = this.#sampleData.channels?.[0]?.length ?? this.#sampleData.length;
      this.#sampleIndex = floor2(samplePosition * len);
      this.#declickTotal = Math.max(1, Math.floor(sampleRate * 4e-3));
      this.#declick = this.#declickTotal;
    }
    if (sampleData && this.type === "sample") {
      const oldLength = this.#sampleData?.channels?.[0]?.length ?? this.#sampleData?.length ?? 1;
      const newLength = sampleData.channels?.[0]?.length ?? sampleData.length ?? 1;
      const progress = this.#sampleIndex / oldLength;
      this.#sampleData = sampleData;
      this.#sampleEndIndex = clamp(
        Math.floor(this.#sampleEndIndex / oldLength * newLength),
        0,
        newLength - 1
      );
      this.#sampleStartIndex = clamp(
        Math.floor(this.#sampleStartIndex / oldLength * newLength),
        0,
        newLength - 1
      );
      this.#sampleIndex = clamp(
        Math.floor(progress * newLength),
        this.#sampleStartIndex,
        this.#sampleEndIndex
      );
    }
  }
  // Stereo
  pan(channel, frame) {
    if (channel === 0) {
      if (this.#pan > 0) frame *= 1 - this.#pan;
    } else if (channel === 1) {
      if (this.#pan < 0) frame *= 1 - abs2(this.#pan);
    }
    return frame;
  }
  // Use a 25ms fade by default.
  //
  // 🍿 Two rules keep a release from clicking, and both are about the *step*
  // in amplitude, not the length of the tail:
  //
  //   1. Every kill ramps. `kill(0)` used to drop `playing` on the spot,
  //      cutting the waveform wherever it happened to be — a full-scale
  //      discontinuity, which is the definition of a click. The floor below
  //      is short enough to still read as "immediate" and long enough to
  //      round the corner.
  //   2. A kill on an already-fading voice may only *shorten* the tail.
  //      Notepat routinely kills the same voice twice (the button `up`
  //      handler, then `cleanupOrphanedSounds`); restarting the ramp meant
  //      the gain jumped from wherever it had faded to back up to 1 — a pop
  //      louder than the note. Carrying `fadeGain` across kills and only
  //      raising `fadeStep` makes the second call a no-op or a speed-up.
  kill(fade = 0.025) {
    const seconds = max3(fade || 0, _Synth.MIN_RELEASE);
    const step = this.fadeGain / (seconds * sampleRate);
    this.fadeStep = this.fading ? max3(this.fadeStep, step) : step;
    this.fading = true;
    this.fadeProgress = 0;
    this.fadeDuration = seconds * sampleRate;
  }
  // Return an integer from 0->1 representing the progress of this sound so far.
  progress() {
    if (this.type === "sample") {
      return (this.#sampleIndex - this.#sampleStartIndex) / (this.#sampleEndIndex - this.#sampleStartIndex);
    } else {
      return this.#progress / this.#duration;
    }
  }
  // Fill the custom buffer with generated waveform data
  // Note: Using underscore convention instead of # private method for AudioWorklet compatibility
  _fillCustomBuffer() {
    if (!this.#customGenerator) return;
    try {
      const bufferSize = this.#customBufferSize - this.#customBuffer.length;
      const newSamples = this.#customGenerator({
        frequency: this.#liveFrequency,
        // follows slide/vibrato/drift, block by block
        sampleRate,
        progress: this.#progress,
        time: this.#progress / sampleRate,
        samplesNeeded: bufferSize
      });
      if (Array.isArray(newSamples)) {
        const clampedSamples = newSamples.map((sample) => {
          if (sample > 1) return 1;
          if (sample < -1) return -1;
          return sample;
        });
        this.#customBuffer.push(...clampedSamples);
      }
    } catch (error) {
      console.warn("\u{1F3A8} Custom waveform generator error:", error);
      const silenceSamples = new Array(this.#customBufferSize).fill(0);
      this.#customBuffer.push(...silenceSamples);
    }
  }
  // Update custom generator function
  setCustomGenerator(generator) {
    if (this.type === "custom") {
      if (typeof generator === "string") {
        try {
          this.#customGenerator = eval(`(${generator})`);
        } catch (error) {
          console.error("\u{1F3A8} Failed to parse custom generator in setCustomGenerator:", error);
          return;
        }
      } else if (typeof generator === "function") {
        this.#customGenerator = generator;
      } else {
        console.error("\u{1F3A8} Invalid generator type in setCustomGenerator:", typeof generator);
        return;
      }
      this.#customBuffer.length = 0;
    }
  }
};

// public/aesthetic.computer/lib/sound/bubble.mjs
var Bubble = class {
  // Generic for all instruments.
  playing = true;
  fading = false;
  // If we are fading and then stopping playback.
  fadeProgress;
  fadeDuration;
  #volume = 1;
  // 0 to 1
  #pan2 = 0;
  // -1 to 1
  #radius;
  #rise;
  #amp;
  #decay2;
  #gain;
  #phaseStep;
  #phaseRise;
  #phase2;
  #lastOut;
  #depth = 1;
  #timestep;
  #out = 0;
  #maxOut = 1;
  #progress2 = 0;
  // TODO: This becomes binary?
  #QUIET = 1e-6;
  // Parameter update properties for smooth transitions
  #futureRadius;
  #futureRise;
  #futureVolume2;
  #futurePan;
  #radiusUpdatesTotal;
  #radiusUpdatesLeft;
  #radiusUpdateSlice;
  #riseUpdatesTotal;
  #riseUpdatesLeft;
  #riseUpdateSlice;
  #volumeUpdatesTotal2;
  #volumeUpdatesLeft2;
  #volumeUpdateSlice2;
  #panUpdatesTotal;
  #panUpdatesLeft;
  #panUpdateSlice;
  #sustain = false;
  constructor(radius, rise, volume3, pan2, id2) {
    this.id = id2;
    this.start(radius, rise, volume3, pan2);
  }
  start(radius = this.#radius, rise = this.#rise, volume3 = this.#volume, pan2 = this.#pan2) {
    this.#pan2 = pan2;
    this.#volume = volume3;
    this.#radius = radius * 1e-3;
    this.#rise = rise;
    this.#futureRadius = this.#radius;
    this.#futureRise = this.#rise;
    this.#futureVolume2 = this.#volume;
    this.#futurePan = this.#pan2;
    this.#timestep = 1 / sampleRate;
    this.#lastOut = this.#out;
    const pRadius = this.#radius * Math.sqrt(this.#radius);
    this.#amp = 17.2133 * pRadius * this.#depth;
    this.#decay2 = 0.13 / this.#radius + 72e-4 * pRadius;
    this.#gain = Math.exp(-this.#decay2 * this.#timestep);
    this.#phaseStep = 3 / this.#radius * this.#timestep;
    this.#phaseRise = this.#phaseStep * this.#decay2 * this.#rise * this.#timestep;
    this.#phase2 = 0;
  }
  update({ radius, rise, volume: volume3, pan: pan2, sustain, duration: duration2 = 0.1 }) {
    if (typeof sustain === "boolean") {
      this.#sustain = sustain;
      console.log(`\u{1F9CB} UPDATE: Sustain set to ${sustain} for bubble ${this.id || "unknown"}`);
    }
    if (typeof radius === "number" && radius > 0) {
      this.#futureRadius = radius * 1e-3;
      this.#radiusUpdatesTotal = duration2 * sampleRate;
      this.#radiusUpdatesLeft = this.#radiusUpdatesTotal;
      this.#radiusUpdateSlice = (this.#futureRadius - this.#radius) / this.#radiusUpdatesTotal;
    }
    if (typeof rise === "number") {
      this.#futureRise = rise;
      this.#riseUpdatesTotal = duration2 * sampleRate;
      this.#riseUpdatesLeft = this.#riseUpdatesTotal;
      this.#riseUpdateSlice = (this.#futureRise - this.#rise) / this.#riseUpdatesTotal;
    }
    if (typeof volume3 === "number") {
      this.#futureVolume2 = volume3;
      this.#volumeUpdatesTotal2 = duration2 * sampleRate;
      this.#volumeUpdatesLeft2 = this.#volumeUpdatesTotal2;
      this.#volumeUpdateSlice2 = (this.#futureVolume2 - this.#volume) / this.#volumeUpdatesTotal2;
    }
    if (typeof pan2 === "number") {
      this.#futurePan = pan2;
      this.#panUpdatesTotal = duration2 * sampleRate;
      this.#panUpdatesLeft = this.#panUpdatesTotal;
      this.#panUpdateSlice = (this.#futurePan - this.#pan2) / this.#panUpdatesTotal;
    }
  }
  // Sustain control methods
  setSustain(sustain) {
    this.#sustain = sustain;
    console.log(`\u{1F9CB} setSustain(${sustain}) for bubble ${this.id || "unknown"}`);
  }
  enableSustain() {
    this.#sustain = true;
    console.log(`\u{1F9CB} enableSustain() for bubble ${this.id || "unknown"}`);
  }
  disableSustain() {
    this.#sustain = false;
    console.log(`\u{1F9CB} disableSustain() for bubble ${this.id || "unknown"}`);
  }
  next() {
    if (!this.playing) return 0;
    if (this.#radiusUpdatesLeft > 0) {
      this.#radius += this.#radiusUpdateSlice;
      const pRadius = this.#radius * Math.sqrt(this.#radius);
      this.#amp = 17.2133 * pRadius * this.#depth;
      this.#decay2 = 0.13 / this.#radius + 72e-4 * pRadius;
      this.#gain = Math.exp(-this.#decay2 * this.#timestep);
      this.#phaseStep = 3 / this.#radius * this.#timestep;
      this.#phaseRise = this.#phaseStep * this.#decay2 * this.#rise * this.#timestep;
      this.#radiusUpdatesLeft -= 1;
    }
    if (this.#riseUpdatesLeft > 0) {
      this.#rise += this.#riseUpdateSlice;
      this.#phaseRise = this.#phaseStep * this.#decay2 * this.#rise * this.#timestep;
      this.#riseUpdatesLeft -= 1;
    }
    if (this.#volumeUpdatesLeft2 > 0) {
      this.#volume += this.#volumeUpdateSlice2;
      this.#volumeUpdatesLeft2 -= 1;
    }
    if (this.#panUpdatesLeft > 0) {
      this.#pan2 += this.#panUpdateSlice;
      this.#panUpdatesLeft -= 1;
    }
    if (!this.#sustain && this.#amp < this.#QUIET && this.#phase2 > 1) {
      this.playing = false;
      return 0;
    }
    let alpha = this.#phase2 < 1 ? this.#phase2 : 1;
    this.#out = (1 - alpha) * this.#lastOut + alpha * this.#amp * Math.sin(Math.PI * 2 * this.#phase2);
    this.#phase2 += this.#phaseStep;
    this.#phaseStep += this.#phaseRise;
    if (!this.#sustain) {
      this.#amp *= this.#gain;
    }
    this.#progress2 += 1;
    let out = this.#out * this.#volume * 1e3;
    if (out > this.#maxOut) this.#maxOut = out;
    out = out / this.#maxOut;
    if (this.fading) {
      if (this.fadeProgress < this.fadeDuration) {
        this.fadeProgress += 1;
        out *= 1 - this.fadeProgress / this.fadeDuration;
      } else {
        this.fading = false;
        this.playing = false;
        return 0;
      }
    }
    return out;
  }
  // Stereo
  pan(channel, frame) {
    if (channel === 0) {
      if (this.#pan2 > 0) {
        frame *= 1 - this.#pan2;
      }
    } else if (channel === 1) {
      if (this.#pan2 < 0) {
        frame *= 1 - Math.abs(this.#pan2);
      }
    }
    return frame;
  }
  // Use a 25ms fade by default.
  kill(fade = 0.025) {
    if (!fade) {
      this.playing = false;
    } else {
      this.fading = true;
      this.fadeProgress = 0;
      this.fadeDuration = fade * sampleRate;
    }
  }
};

// public/aesthetic.computer/lib/sound/fart.mjs
var Fart = class {
  // Generic for all instruments.
  playing = true;
  fading = false;
  // If we are fading and then stopping playback.
  fadeProgress;
  fadeDuration;
  #volume = 1;
  // 0 to 1
  #pan2 = 0;
  // -1 to 1
  #pressure;
  // 0 to 1 - how hard you squeeze
  #pitch2;
  // Hz - fundamental frequency
  #rasp;
  // 0 to 1 - noise component (0 = pure tone, 1 = mostly noise)
  #amp;
  #decay2;
  #gain;
  #phase2;
  #lastOut;
  #timestep;
  #out = 0;
  #maxOut = 1;
  #progress2 = 0;
  #QUIET = 1e-6;
  // Noise generation state
  #noiseState = 0;
  // Parameter update properties for smooth transitions
  #futurePressure;
  #futurePitch;
  #futureRasp;
  #futureVolume2;
  #futurePan;
  #pressureUpdatesTotal;
  #pressureUpdatesLeft;
  #pressureUpdateSlice;
  #pitchUpdatesTotal;
  #pitchUpdatesLeft;
  #pitchUpdateSlice;
  #raspUpdatesTotal;
  #raspUpdatesLeft;
  #raspUpdateSlice;
  #volumeUpdatesTotal2;
  #volumeUpdatesLeft2;
  #volumeUpdateSlice2;
  #panUpdatesTotal;
  #panUpdatesLeft;
  #panUpdateSlice;
  #sustain = false;
  constructor(pressure, pitch, rasp, volume3, pan2, id2) {
    this.id = id2;
    this.start(pressure, pitch, rasp, volume3, pan2);
  }
  start(pressure = this.#pressure, pitch = this.#pitch2, rasp = this.#rasp, volume3 = this.#volume, pan2 = this.#pan2) {
    this.#pan2 = pan2;
    this.#volume = volume3;
    this.#pressure = Math.max(0.01, Math.min(1, pressure));
    this.#pitch2 = Math.max(20, Math.min(8e3, pitch));
    this.#rasp = Math.max(0, Math.min(1, rasp));
    this.#futurePressure = this.#pressure;
    this.#futurePitch = this.#pitch2;
    this.#futureRasp = this.#rasp;
    this.#futureVolume2 = this.#volume;
    this.#futurePan = this.#pan2;
    this.#timestep = 1 / sampleRate;
    this.#lastOut = this.#out;
    this.#amp = 0.3 * this.#pressure;
    this.#decay2 = 0.8 + this.#pitch2 / 8e3 * 0.2;
    this.#gain = Math.exp(-this.#decay2 * this.#timestep);
    this.#phase2 = 0;
  }
  update({ pressure, pitch, rasp, volume: volume3, pan: pan2, sustain, duration: duration2 = 0.1 }) {
    if (typeof sustain === "boolean") {
      this.#sustain = sustain;
      console.log(`\u{1F4A8} UPDATE: Sustain set to ${sustain} for fart ${this.id || "unknown"}`);
    }
    if (typeof pressure === "number" && pressure >= 0) {
      this.#futurePressure = Math.max(0.01, Math.min(1, pressure));
      this.#pressureUpdatesTotal = duration2 * sampleRate;
      this.#pressureUpdatesLeft = this.#pressureUpdatesTotal;
      this.#pressureUpdateSlice = (this.#futurePressure - this.#pressure) / this.#pressureUpdatesTotal;
    }
    if (typeof pitch === "number" && pitch > 0) {
      this.#futurePitch = Math.max(20, Math.min(8e3, pitch));
      this.#pitchUpdatesTotal = duration2 * sampleRate;
      this.#pitchUpdatesLeft = this.#pitchUpdatesTotal;
      this.#pitchUpdateSlice = (this.#futurePitch - this.#pitch2) / this.#pitchUpdatesTotal;
    }
    if (typeof rasp === "number" && rasp >= 0) {
      this.#futureRasp = Math.max(0, Math.min(1, rasp));
      this.#raspUpdatesTotal = duration2 * sampleRate;
      this.#raspUpdatesLeft = this.#raspUpdatesTotal;
      this.#raspUpdateSlice = (this.#futureRasp - this.#rasp) / this.#raspUpdatesTotal;
    }
    if (typeof volume3 === "number") {
      this.#futureVolume2 = volume3;
      this.#volumeUpdatesTotal2 = duration2 * sampleRate;
      this.#volumeUpdatesLeft2 = this.#volumeUpdatesTotal2;
      this.#volumeUpdateSlice2 = (this.#futureVolume2 - this.#volume) / this.#volumeUpdatesTotal2;
    }
    if (typeof pan2 === "number") {
      this.#futurePan = pan2;
      this.#panUpdatesTotal = duration2 * sampleRate;
      this.#panUpdatesLeft = this.#panUpdatesTotal;
      this.#panUpdateSlice = (this.#futurePan - this.#pan2) / this.#panUpdatesTotal;
    }
  }
  // Sustain control methods
  setSustain(sustain) {
    this.#sustain = sustain;
    console.log(`\u{1F4A8} setSustain(${sustain}) for fart ${this.id || "unknown"}`);
  }
  enableSustain() {
    this.#sustain = true;
    console.log(`\u{1F4A8} enableSustain() for fart ${this.id || "unknown"}`);
  }
  disableSustain() {
    this.#sustain = false;
    console.log(`\u{1F4A8} disableSustain() for fart ${this.id || "unknown"}`);
  }
  // Linear congruential generator for pseudo-random noise
  _noise() {
    this.#noiseState = this.#noiseState * 1103515245 + 12345 & 2147483647;
    return this.#noiseState / 2147483647 * 2 - 1;
  }
  next() {
    if (!this.playing) return 0;
    if (this.#pressureUpdatesLeft > 0) {
      this.#pressure += this.#pressureUpdateSlice;
      this.#pressureUpdatesLeft -= 1;
    }
    if (this.#pitchUpdatesLeft > 0) {
      this.#pitch2 += this.#pitchUpdateSlice;
      this.#pitchUpdatesLeft -= 1;
    }
    if (this.#raspUpdatesLeft > 0) {
      this.#rasp += this.#raspUpdateSlice;
      this.#raspUpdatesLeft -= 1;
    }
    if (this.#volumeUpdatesLeft2 > 0) {
      this.#volume += this.#volumeUpdateSlice2;
      this.#volumeUpdatesLeft2 -= 1;
    }
    if (this.#panUpdatesLeft > 0) {
      this.#pan2 += this.#panUpdateSlice;
      this.#panUpdatesLeft -= 1;
    }
    if (!this.#sustain && this.#amp < this.#QUIET) {
      this.playing = false;
      return 0;
    }
    const phaseStep = this.#pitch2 / sampleRate * Math.PI * 2;
    const tone = Math.sin(this.#phase2) * this.#pressure;
    const noise = this._noise() * this.#rasp;
    const mixed = tone * (1 - this.#rasp) + noise;
    this.#out = this.#lastOut * 0.3 + mixed * this.#amp * 0.7;
    this.#lastOut = this.#out;
    this.#phase2 += phaseStep;
    if (this.#phase2 > Math.PI * 2) {
      this.#phase2 -= Math.PI * 2;
    }
    if (!this.#sustain) {
      this.#amp *= this.#gain;
    }
    this.#progress2 += 1;
    let out = this.#out * this.#volume;
    if (Math.abs(out) > this.#maxOut) this.#maxOut = Math.abs(out);
    out = out / this.#maxOut;
    if (this.fading) {
      if (this.fadeProgress < this.fadeDuration) {
        this.fadeProgress += 1;
        out *= 1 - this.fadeProgress / this.fadeDuration;
      } else {
        this.fading = false;
        this.playing = false;
        return 0;
      }
    }
    return out;
  }
  // Stereo panning
  pan(channel, frame) {
    if (channel === 0) {
      if (this.#pan2 > 0) {
        frame *= 1 - this.#pan2;
      }
    } else if (channel === 1) {
      if (this.#pan2 < 0) {
        frame *= 1 - Math.abs(this.#pan2);
      }
    }
    return frame;
  }
  // Use a 25ms fade by default.
  kill(fade = 0.025) {
    if (!fade) {
      this.playing = false;
    } else {
      this.fading = true;
      this.fadeProgress = 0;
      this.fadeDuration = fade * sampleRate;
    }
  }
};

// public/aesthetic.computer/lib/sound/organic/voice.mjs
var Voice = class {
  playing = true;
  fading = false;
  fadeGain = 1;
  // The mixer reads `volume` and `fadeGain` for auto-mixing.
  id;
  p;
  // Current params — the glided values a `render` reads.
  t = 0;
  // Seconds since the voice began.
  dt = 1 / sampleRate;
  #glides = [];
  // { key, step, left }
  #fadeStep = 0;
  #stiff;
  // Param names that snap instead of glide.
  constructor(params, id2, defaults, stiff = []) {
    this.id = id2;
    this.p = { volume: 1, pan: 0, ...defaults };
    for (const k in params) if (params[k] !== void 0) this.p[k] = params[k];
    if (this.p.duration === "\u{1F501}") this.p.duration = Infinity;
    this.#stiff = /* @__PURE__ */ new Set(["duration", ...stiff]);
  }
  get volume() {
    return this.p.volume;
  }
  // Numeric params glide over `duration` seconds; anything else (a vowel, a
  // direction) snaps, then `retune` lets the generator react.
  update({ duration: duration2 = 0.1, ...props }) {
    const n = Math.max(1, Math.round(duration2 * sampleRate));
    for (const key in props) {
      const v = props[key];
      if (v === void 0) continue;
      if (typeof v === "number" && typeof this.p[key] === "number" && !this.#stiff.has(key)) {
        this.#glides = this.#glides.filter((g) => g.key !== key);
        this.#glides.push({ key, step: (v - this.p[key]) / n, left: n });
      } else this.p[key] = v;
    }
    this.retune?.(props, duration2);
  }
  next() {
    if (!this.playing) return 0;
    for (let i = this.#glides.length - 1; i >= 0; i--) {
      const g = this.#glides[i];
      this.p[g.key] += g.step;
      if (--g.left <= 0) this.#glides.splice(i, 1);
    }
    let out = this.render(this.t) * this.p.volume;
    this.t += this.dt;
    if (this.fading) {
      this.fadeGain -= this.#fadeStep;
      if (this.fadeGain <= 0) {
        this.fadeGain = 0;
        this.fading = false;
        this.playing = false;
        return 0;
      }
      out *= this.fadeGain;
    } else if (this.t >= this.p.duration) {
      this.playing = false;
      return 0;
    }
    return out > 1 ? 1 : out < -1 ? -1 : out;
  }
  pan(channel, frame) {
    const p = this.p.pan;
    if (channel === 0 && p > 0) return frame * (1 - p);
    if (channel === 1 && p < 0) return frame * (1 + p);
    return frame;
  }
  kill(fade = 0.025) {
    if (!fade) {
      this.playing = false;
      return;
    }
    this.fading = true;
    this.#fadeStep = this.fadeGain / (fade * sampleRate);
  }
};

// public/aesthetic.computer/lib/sound/organic/parts.mjs
var { sin: sin4, cos: cos4, exp, log, PI: PI4, min: min4, max: max4 } = Math;
var clamp2 = (x, lo, hi) => x < lo ? lo : x > hi ? hi : x;
var smooth = (x) => x <= 0 ? 0 : x >= 1 ? 1 : x * x * (3 - 2 * x);
function glide(a, b, u) {
  if (u <= 0) return a;
  if (u >= 1) return b;
  return a * exp(u * log(b / a));
}
function envelope(t, attack2, release, duration2) {
  const a = attack2 > 0 ? smooth(t / attack2) : 1;
  const r = duration2 === Infinity || release <= 0 ? 1 : smooth((duration2 - t) / release);
  return a * r;
}
var Noise = class {
  #s;
  constructor(seed = 2654435769) {
    this.#s = seed >>> 0 || 1;
  }
  next() {
    let x = this.#s;
    x ^= x << 13;
    x >>>= 0;
    x ^= x >>> 17;
    x ^= x << 5;
    x >>>= 0;
    this.#s = x;
    return x / 2147483648 - 1;
  }
};
var Smoother = class {
  #a;
  y = 0;
  constructor(cutoff) {
    this.#a = 1 - exp(-2 * PI4 * cutoff / sampleRate);
  }
  next(x) {
    this.y += this.#a * (x - this.y);
    return this.y;
  }
};
var Biquad = class {
  #b0 = 1;
  #b1 = 0;
  #b2 = 0;
  #a1 = 0;
  #a2 = 0;
  #x1 = 0;
  #x2 = 0;
  #y1 = 0;
  #y2 = 0;
  lowpass(freq, q = 0.707) {
    const w0 = 2 * PI4 * min4(freq, sampleRate * 0.45) / sampleRate;
    const alpha = sin4(w0) / (2 * q);
    const c = cos4(w0);
    const a0 = 1 + alpha;
    this.#b0 = (1 - c) / 2 / a0;
    this.#b1 = (1 - c) / a0;
    this.#b2 = this.#b0;
    this.#a1 = -2 * c / a0;
    this.#a2 = (1 - alpha) / a0;
    return this;
  }
  bandpass(freq, q = 1) {
    const w0 = 2 * PI4 * min4(freq, sampleRate * 0.45) / sampleRate;
    const alpha = sin4(w0) / (2 * q);
    const a0 = 1 + alpha;
    this.#b0 = alpha / a0;
    this.#b1 = 0;
    this.#b2 = -this.#b0;
    this.#a1 = -2 * cos4(w0) / a0;
    this.#a2 = (1 - alpha) / a0;
    return this;
  }
  process(x) {
    const y = this.#b0 * x + this.#b1 * this.#x1 + this.#b2 * this.#x2 - this.#a1 * this.#y1 - this.#a2 * this.#y2;
    this.#x2 = this.#x1;
    this.#x1 = x;
    this.#y2 = this.#y1;
    this.#y1 = y;
    return y;
  }
};
var Glottis = class {
  #phase2 = 0;
  #noise2;
  #period = 1;
  // Per-cycle pitch multiplier, redrawn at each closure.
  #cycle = 0;
  open = 0;
  // 0..1 — how far the folds are apart, for aspiration noise.
  oq = 0.6;
  // Open quotient: fraction of the period the folds are open.
  jitter = 0;
  sub = 0;
  constructor(seed = 1) {
    this.#noise2 = new Noise(seed);
  }
  next(freq) {
    const tp = this.oq * 0.7;
    const tn = this.oq - tp;
    const p = this.#phase2;
    let out;
    if (p < tp) {
      const s = sin4(PI4 * p / tp);
      this.open = 0.5 * (1 - cos4(PI4 * p / tp));
      out = tn / tp * s;
    } else if (p < this.oq) {
      const q = (p - tp) / tn;
      this.open = cos4(PI4 * q / 2);
      out = -sin4(PI4 * q / 2);
    } else {
      this.open = 0;
      out = 0;
    }
    if (this.#cycle & 1) out *= 1 - this.sub;
    this.#phase2 += freq * this.#period / sampleRate;
    if (this.#phase2 >= 1) {
      this.#phase2 -= 1;
      this.#cycle += 1;
      this.#period = 1 + this.jitter * 0.08 * this.#noise2.next();
    }
    return out;
  }
};

// public/aesthetic.computer/lib/sound/organic/growl.mjs
var { sin: sin5, exp: exp2, PI: PI5 } = Math;
var GAIN = 1.1;
var Growl = class extends Voice {
  #glottis = new Glottis(7);
  #noise2 = new Noise(11);
  #wander = new Smoother(2);
  // Slow drift of the tremor rate.
  #body;
  #chest = new Biquad();
  // Keeps the fundamental the formants can't pass.
  #tremor = 0;
  constructor(params, id2) {
    super(params, id2, { pitch: 55, rasp: 0.6, size: 1, tremor: 0.5, duration: 1.2 }, ["size"]);
    this.#glottis.oq = 0.5;
    this.#body = new FormantBank(vowelBands("o", 1 / this.p.size));
    this.#chest.lowpass(220 / this.p.size, 1.2);
  }
  retune(props) {
    if (!("size" in props)) return;
    this.#body.set(vowelBands("o", 1 / this.p.size));
    this.#chest.lowpass(220 / this.p.size, 1.2);
  }
  render(t) {
    const p = this.p;
    const rasp = clamp2(p.rasp, 0, 1);
    this.#glottis.jitter = rasp;
    this.#glottis.sub = rasp * 0.5;
    const f = p.pitch * (1 + 0.18 * exp2(-t / 0.08));
    const pulse = this.#glottis.next(f);
    const hiss = this.#noise2.next() * this.#glottis.open;
    const src = pulse * (1 - 0.4 * rasp) + hiss * rasp * 0.7;
    const rate = 25 + this.#wander.next(this.#noise2.next()) * 400;
    this.#tremor += 2 * PI5 * rate / sampleRate;
    const trem = 1 - clamp2(p.tremor, 0, 1) * 0.6 * (0.5 + 0.5 * sin5(this.#tremor));
    const out = this.#body.process(src) * 0.7 + this.#chest.process(src) * 0.6;
    return out * trem * envelope(t, 0.06, 0.25, p.duration) * GAIN;
  }
};

// public/aesthetic.computer/lib/sound/organic/breath.mjs
var { exp: exp3, pow: pow3, abs: abs3, sqrt: sqrt2 } = Math;
var GAIN2 = 0.8;
var Breath = class extends Voice {
  #noise2 = new Noise(5);
  #lp = new Biquad();
  #chest = new Biquad().bandpass(650, 2);
  #cutoff = 0;
  // Cutoff the lowpass was last built for.
  constructor(params, id2) {
    super(params, id2, { pressure: 0.6, cutoff: 1200, direction: "out", duration: 0.8 });
  }
  // The reference length: a held breath keeps the first 0.8 s of shape.
  #length() {
    return this.p.duration === Infinity ? 0.8 : this.p.duration;
  }
  #shape(t) {
    const L = this.#length();
    if (this.p.direction === "in") {
      const rise2 = 0.85 * L;
      return t < rise2 ? pow3(smooth(t / rise2), 1.5) : 1;
    }
    const rise = 0.12 * L;
    return t < rise ? smooth(t / rise) : 0.3 + 0.7 * exp3(-(t - rise) / (0.35 * L));
  }
  render(t) {
    const p = this.p;
    const pressure = clamp2(p.pressure, 0, 1);
    const e = this.#shape(t);
    const fc = p.cutoff * (1 + 0.3 * pressure * e);
    if (abs3(fc - this.#cutoff) > this.#cutoff * 0.01) {
      this.#lp.lowpass(fc, 1.4);
      this.#cutoff = fc;
    }
    const n = this.#noise2.next();
    const out = this.#lp.process(n) + this.#chest.process(n) * 0.5;
    const release = p.direction === "in" ? 0.05 : 0.15 * this.#length();
    const even = fc > 1200 ? sqrt2(1200 / fc) : 1;
    return out * e * envelope(t, 0, release, p.duration) * (0.3 + 0.7 * pressure) * even * GAIN2;
  }
};

// public/aesthetic.computer/lib/sound/organic/howl.mjs
var { sin: sin6, exp: exp4, pow: pow4, max: max5, PI: PI6 } = Math;
var GAIN3 = 1.3;
var Howl = class extends Voice {
  #glottis = new Glottis(3);
  #noise2 = new Noise(9);
  #drift2 = new Smoother(0.7);
  #tract;
  constructor(params, id2) {
    super(
      params,
      id2,
      { pitch: 220, slide: 330, vowel: "o", scale: 1, vibrato: 0.4, rasp: 0.1, attack: 0.08, release: 0.3, duration: 1.5 },
      ["scale", "attack", "release"]
    );
    this.#glottis.oq = 0.65;
    this.#tract = new FormantBank(this.#bands());
  }
  #bands() {
    return vowelBands(this.p.vowel, this.p.scale) || vowelBands("a", this.p.scale);
  }
  retune(props, duration2) {
    if ("vowel" in props || "scale" in props) this.#tract.set(this.#bands());
    if ("pitch" in props && !("slide" in props)) this.update({ slide: props.pitch, duration: duration2 });
  }
  render(t) {
    const p = this.p;
    const rasp = clamp2(p.rasp, 0, 1);
    this.#glottis.jitter = 0.05 + rasp * 0.6;
    this.#glottis.sub = rasp * 0.45;
    const sustain = p.duration === Infinity ? 1 : max5(0.05, p.duration - p.attack - p.release);
    let f = glide(p.pitch, p.slide, (t - p.attack) / sustain);
    const k = t / 0.05;
    f *= 1 + 0.05 * k * exp4(1 - k);
    const vib = clamp2((t - 0.2) / 0.3, 0, 1) * p.vibrato * sin6(2 * PI6 * 5.5 * t);
    const drift = this.#drift2.next(this.#noise2.next()) * 25;
    f *= pow4(2, (vib + drift) / 12);
    const pulse = this.#glottis.next(f);
    const hiss = this.#noise2.next() * this.#glottis.open;
    const src = pulse * (1 - 0.35 * rasp) + hiss * rasp * 0.5;
    const out = this.#tract.process(src) + src * 0.08;
    return out * envelope(t, p.attack, p.release, p.duration) * GAIN3;
  }
};

// public/aesthetic.computer/lib/sound/organic/chirp.mjs
var { sin: sin7, exp: exp5, floor: floor3, min: min5, PI: PI7 } = Math;
var GAIN4 = 0.6;
var Chirp = class extends Voice {
  #phase2 = 0;
  #noise2 = new Noise(13);
  constructor(params, id2) {
    super(params, id2, { pitch: 2500, slide: 4200, count: 3, rate: 8, duration: 0.5, noise: 0.05 }, ["count", "rate"]);
  }
  render(t) {
    const p = this.p;
    const gap = 1 / p.rate;
    const len = min5(gap * 0.6, 0.09);
    const i = floor3(t / gap);
    const tau = t - i * gap;
    if (i >= p.count || i === p.count - 1 && tau >= len) {
      this.playing = false;
      return 0;
    }
    if (tau >= len) return 0;
    const f = glide(p.pitch, p.slide, tau / len);
    this.#phase2 += 2 * PI7 * f / sampleRate;
    if (this.#phase2 > 2 * PI7) this.#phase2 -= 2 * PI7;
    const env = smooth(tau / 4e-3) * exp5(-tau / (len * 0.35)) * smooth((len - tau) / 5e-3);
    return (sin7(this.#phase2) + this.#noise2.next() * p.noise) * env * GAIN4;
  }
};

// public/aesthetic.computer/lib/sound/organic.mjs
var ORGANICS = { growl: Growl, breath: Breath, howl: Howl, chirp: Chirp };
function createOrganic({ kind, id: id2, params = {} }) {
  const Kind = ORGANICS[kind];
  if (!Kind) {
    console.warn("\u{1F43E} Unknown organic:", kind);
    return null;
  }
  return new Kind(params, id2);
}

// public/aesthetic.computer/lib/speaker.mjs
var { abs: abs4, round: round3, floor: floor4 } = Math;
var delayTime = 0.12;
var feedback = 0.6;
var mix = 0.5;
var roomEnabled = false;
var roomMix = 0.5;
var roomFeedback = 0.6;
var glitchEnabled = false;
var glitchMix = 0.65;
var glitchCrush = 6;
var glitchRate = 1600;
var glitchJitter = 0.15;
var sampleStore = {};
var SpeakerProcessor = class extends AudioWorkletProcessor {
  // TODO: Fix current Firefox bug with private fields: https://bugzilla.mozilla.org/show_bug.cgi?id=1435826
  #ticks;
  #lastTime;
  #bpm;
  #bpmInSec;
  #running = {};
  #queue = [];
  #currentWaveformLeft = [];
  #currentWaveformRight = [];
  #currentAmplitudeLeft = [];
  #currentAmplitudeRight = [];
  // Memory monitoring
  #memoryCheckCounter = 0;
  #lastMemoryCheck = 0;
  // Analysis throttling
  #analysisCounter = 0;
  // Performance monitoring for mobile devices
  #performanceMode = "auto";
  // 'auto', 'low', 'disabled'
  #processingTimeHistory = [];
  #lastProcessingTime = 0;
  #lastTelemetryTime = 0;
  // Frequency analysis
  #frequencyBandsLeft = [];
  #frequencyBandsRight = [];
  #fftBufferLeft = [];
  #fftBufferRight = [];
  #fftSize = 512;
  // Reduced from 1024 for better mobile performance
  // Beat detection variables
  #energyHistory = [];
  // Track energy over time for beat detection
  #energyHistorySize = 20;
  // Reduced from 43 for better mobile performance
  #beatSensitivity = 1.15;
  // Lower, more sensitive threshold (was 1.3)
  #adaptiveThreshold = 1.15;
  // Dynamic threshold that adapts
  #lastBeatTime = 0;
  #beatCooldown = 0.08;
  // Shorter cooldown for more responsive detection (was 0.1)
  #currentBeat = false;
  #beatStrength = 0;
  #recentEnergyPeaks = [];
  // Track recent energy peaks for adaptive threshold
  #energyVariance = 0;
  // Track energy variance for dynamic sensitivity
  #mixDivisor = 1;
  // 🍿 Pop (discontinuity) listener state.
  #popPrev = 0;
  #popCount = 0;
  #popMax = 0;
  #popEvents = [];
  #popLastReport = 0;
  // Demand gate for frequency analysis (see get-frequencies).
  #lastFrequencyRequest = -10;
  #reverbLeft;
  #reverbRight;
  // Glitch effect state
  #glitchHoldCounter = 0;
  #glitchHoldSamples = 1;
  #glitchHeldLeft = 0;
  #glitchHeldRight = 0;
  // VST Bridge Mode - sends samples to plugin instead of Web Audio output
  #vstBridgeEnabled = false;
  #vstSampleBuffer = { left: [], right: [] };
  #vstBufferSize = 128;
  // Send samples in chunks
  constructor(options2) {
    console.log("\u{1F50A} Sound Synthesis Worklet CONSTRUCTOR, bpm:", options2.processorOptions.bpm);
    super();
    this.#lastTime = currentTime;
    this.#bpm = options2.processorOptions.bpm;
    this.#bpmInSec = 60 / this.#bpm;
    this.#ticks = this.#bpmInSec;
    console.log("\u{1F50A} Worklet initialized: bpm=", this.#bpm, "bpmInSec=", this.#bpmInSec, "ticks=", this.#ticks);
    volume2.amount.val = 0.9;
    this.#reverbLeft = new Reverb(sampleRate, delayTime, feedback, mix);
    this.#reverbRight = new Reverb(sampleRate, delayTime, feedback, mix);
    this.#glitchHoldSamples = Math.max(1, Math.floor(sampleRate / glitchRate));
    this.port.onmessage = (e) => {
      const msg = e.data;
      if (msg.type === "get-waveforms") {
        this.port.postMessage({
          type: "waveforms",
          content: {
            left: this.#currentWaveformLeft,
            right: this.#currentWaveformRight
          }
        });
        return;
      }
      if (msg.type === "get-amplitudes") {
        this.port.postMessage({
          type: "amplitudes",
          content: {
            left: this.#currentAmplitudeLeft,
            right: this.#currentAmplitudeRight
          }
        });
        return;
      }
      if (msg.type === "get-frequencies") {
        this.#lastFrequencyRequest = currentTime;
        this.port.postMessage({
          type: "frequencies",
          content: {
            left: this.#frequencyBandsLeft,
            right: this.#frequencyBandsRight,
            beat: {
              detected: this.#currentBeat,
              strength: this.#beatStrength,
              timestamp: currentTime
            }
          }
        });
        return;
      }
      if (msg.type === "volume") {
        const nextVolume = clamp(msg.value ?? msg.content ?? msg.data ?? 1, 0, 1);
        volume2.amount.val = nextVolume;
        return;
      }
      if (msg.type === "vst:enable") {
        this.#vstBridgeEnabled = true;
        console.log("\u{1F39B}\uFE0F VST Bridge Mode ENABLED - routing audio to plugin");
        this.#report("vst:enabled", { enabled: true, sampleRate });
        return;
      }
      if (msg.type === "vst:disable") {
        this.#vstBridgeEnabled = false;
        this.#vstSampleBuffer = { left: [], right: [] };
        console.log("\u{1F39B}\uFE0F VST Bridge Mode DISABLED - routing audio to Web Audio");
        this.#report("vst:disabled", { enabled: false });
        return;
      }
      if (msg.type === "vst:get-samples") {
        const samples = {
          left: [...this.#vstSampleBuffer.left],
          right: [...this.#vstSampleBuffer.right]
        };
        this.#vstSampleBuffer.left = [];
        this.#vstSampleBuffer.right = [];
        this.port.postMessage({ type: "vst:samples", content: samples });
        return;
      }
      if (msg.type === "performance:mode") {
        const mode = msg.content?.mode ?? msg.mode;
        if (mode === "auto" || mode === "low" || mode === "disabled") {
          this.#performanceMode = mode;
        }
        return;
      }
      if (msg.type === "beat:skip") {
        console.log("\u{1F3BC} Beat skipped");
        this.#ticks = 0;
        this.#report("metronome", currentTime);
        return;
      }
      if (msg.type === "beat") {
        const soundData = msg.content;
        if (soundData.bubbles) {
          soundData.bubbles.forEach((bubbleData) => {
            const bubble = new Bubble(
              bubbleData.radius,
              bubbleData.rise,
              bubbleData.volume,
              bubbleData.pan,
              bubbleData.id
            );
            if (bubbleData.id !== void 0) {
              this.#running[bubbleData.id] = bubble;
            }
            this.#queue.push(bubble);
          });
        }
        if (soundData.farts) {
          soundData.farts.forEach((fartData) => {
            const fart = new Fart(
              fartData.pressure,
              fartData.pitch,
              fartData.rasp,
              fartData.volume,
              fartData.pan,
              fartData.id
            );
            if (fartData.id !== void 0) {
              this.#running[fartData.id] = fart;
            }
            this.#queue.push(fart);
          });
        }
        if (soundData.organics) {
          for (const o of soundData.organics) this.#organic(o);
        }
        if (soundData.kills) {
          soundData.kills.forEach((killData) => {
            this.#running[killData.id]?.kill(killData.fade);
            this.#running[killData.id] = void 0;
            delete this.#running[killData.id];
          });
        }
        return;
      }
      if (msg.type === "new-bpm") {
        this.#bpm = msg.data;
        this.#bpmInSec = 60 / this.#bpm;
        return;
      }
      if (msg.type === "get-progress") {
        const soundInstance = this.#running[msg.content];
        const progressValue = soundInstance?.progress();
        if (soundInstance && progressValue !== void 0) {
          const playDuration = currentTime - (soundInstance.startTime || 0);
          if (progressValue === 0 && playDuration > 0.1) {
            console.warn(`\u{1F3B5} AUDIO_STOPPED: Sound ${msg.content} progress reset to 0 after ${playDuration.toFixed(3)}s`);
          }
        }
        this.#report("progress", {
          id: msg.content,
          progress: progressValue,
          duration: soundInstance?.totalDuration || 0,
          timestamp: currentTime
        });
        return;
      }
      if (msg.type === "update") {
        const soundInstance = this.#running[msg.data.id];
        soundInstance?.update(msg.data.properties);
        return;
      }
      if (msg.type === "bubble:update") {
        const soundInstance = this.#running[msg.content.id];
        soundInstance?.update(msg.content.properties);
        return;
      }
      if (msg.type === "fart:update") {
        const soundInstance = this.#running[msg.content.id];
        soundInstance?.update(msg.content.properties);
        return;
      }
      if (msg.type === "sample:update") {
        const { label, buffer } = msg.data;
        const runningIds = Object.keys(this.#running);
        const runningLabels = Object.values(this.#running).map((s) => s?.sampleLabel).filter(Boolean);
        if (sampleStore[label]) {
          sampleStore[label] = buffer;
        }
        let updatedCount = 0;
        Object.values(this.#running).forEach((sound) => {
          if (sound && sound.sampleLabel === label) {
            sound.update({ sampleData: buffer });
            updatedCount++;
          }
        });
        return;
      }
      if (msg.type === "update-generator") {
        this.#running[msg.data.id]?.setCustomGenerator(msg.data.generator);
        return;
      }
      if (msg.type === "kill") {
        this.#running[msg.data.id]?.kill(msg.data.fade);
        this.#running[msg.data.id] = void 0;
        delete this.#running[msg.data.id];
        return;
      }
      if (msg.type === "kill:all") {
        const running = this.#running;
        Object.keys(running).forEach((key) => {
          running[key]?.kill();
          delete running[key];
        });
        return;
      }
      if (msg.type === "room:toggle") {
        roomEnabled = !roomEnabled;
        console.log("\u{1F3E0} ROOM TOGGLE:", roomEnabled ? "ON" : "OFF");
        if (roomEnabled) {
          this.#reverbLeft = new Reverb(sampleRate, delayTime, roomFeedback, roomMix);
          this.#reverbRight = new Reverb(sampleRate, delayTime, roomFeedback, roomMix);
          console.log("\u{1F3E0} Reverb created with mix:", roomMix, "feedback:", roomFeedback);
        }
        this.#report("room:state", { enabled: roomEnabled, mix: roomMix, feedback: roomFeedback });
        return;
      }
      if (msg.type === "room:set") {
        const data = msg.data || {};
        if (data.enabled !== void 0) roomEnabled = data.enabled;
        if (data.mix !== void 0) roomMix = clamp(data.mix, 0, 1);
        if (data.feedback !== void 0) roomFeedback = clamp(data.feedback, 0, 0.95);
        this.#reverbLeft = new Reverb(sampleRate, delayTime, roomFeedback, roomMix);
        this.#reverbRight = new Reverb(sampleRate, delayTime, roomFeedback, roomMix);
        this.#report("room:state", { enabled: roomEnabled, mix: roomMix, feedback: roomFeedback });
        return;
      }
      if (msg.type === "room:get") {
        this.#report("room:state", { enabled: roomEnabled, mix: roomMix, feedback: roomFeedback });
        return;
      }
      if (msg.type === "glitch:toggle") {
        glitchEnabled = !glitchEnabled;
        console.log("\u{1F9E9} GLITCH TOGGLE:", glitchEnabled ? "ON" : "OFF");
        this.#report("glitch:state", {
          enabled: glitchEnabled,
          mix: glitchMix,
          crush: glitchCrush,
          rate: glitchRate,
          jitter: glitchJitter
        });
        return;
      }
      if (msg.type === "glitch:set") {
        const data = msg.data || {};
        if (data.enabled !== void 0) glitchEnabled = data.enabled;
        if (data.mix !== void 0) glitchMix = clamp(data.mix, 0, 1);
        if (data.crush !== void 0) glitchCrush = clamp(round3(data.crush), 2, 12);
        if (data.rate !== void 0) glitchRate = clamp(data.rate, 20, 8e3);
        if (data.jitter !== void 0) glitchJitter = clamp(data.jitter, 0, 1);
        this.#glitchHoldSamples = Math.max(1, Math.floor(sampleRate / glitchRate));
        this.#report("glitch:state", {
          enabled: glitchEnabled,
          mix: glitchMix,
          crush: glitchCrush,
          rate: glitchRate,
          jitter: glitchJitter
        });
        return;
      }
      if (msg.type === "glitch:get") {
        this.#report("glitch:state", {
          enabled: glitchEnabled,
          mix: glitchMix,
          crush: glitchCrush,
          rate: glitchRate,
          jitter: glitchJitter
        });
        return;
      }
      if (msg.type === "cache:clear") {
        for (const k in sampleStore) delete sampleStore[k];
        return;
      }
      if (msg.type === "sound") {
        let duration2, attack2, decay2;
        if (msg.data.beats === Infinity) {
          duration2 = Infinity;
          attack2 = msg.data.attack * sampleRate;
          decay2 = msg.data.decay * sampleRate;
        } else {
          const data = msg.data;
          if (data.beats) {
            duration2 = round3(sampleRate * (this.#bpmInSec * data.beats));
          } else if (data.options.buffer && !duration2) {
            if (typeof data.options.buffer === "string") {
              data.options.buffer = sampleStore[data.options.buffer];
            } else {
              sampleStore[data.options.label] = data.options.buffer;
            }
            let from = clamp(data.options.from || 0, 0, 1);
            let to = clamp(data.options.to || 1, 0, 1);
            if (from > to) {
              [from, to] = [to, from];
              data.options.speed = -(data.options.speed || 1);
            }
            const startSample = round3(from * data.options.buffer.length);
            const endSample = round3(to * data.options.buffer.length);
            data.options.startSample = startSample;
            data.options.endSample = endSample;
            duration2 = round3(
              (endSample - startSample) / abs4(data.options.speed || 1) / data.options.buffer.sampleRate * sampleRate
            );
          }
          attack2 = round3(duration2 * msg.data.attack || 0);
          decay2 = round3(duration2 * msg.data.decay || 0);
        }
        let synthOptions = { tone: msg.data.tone, ...msg.data.options };
        if (msg.data.type === "custom" && msg.data.generator) {
          synthOptions = { ...synthOptions, generator: msg.data.generator };
        }
        const sound = new Synth({
          type: msg.data.type,
          id: msg.data.id,
          options: synthOptions,
          duration: duration2,
          attack: attack2,
          decay: decay2,
          volume: msg.data.volume ?? 1,
          pan: msg.data.pan || 0
        });
        this.#running[msg.data.id] = sound;
        this.#queue.push(sound);
        if (msg.data.probe) {
          this.#report("audio:probe", {
            ...msg.data.probe,
            workletFrame: currentFrame,
            workletTime: currentTime,
            attackFrames: attack2,
            attackMs: attack2 / sampleRate * 1e3,
            queueLength: this.#queue.length,
            runningCount: Object.keys(this.#running).length,
            sampleRate
          });
        }
        return;
      }
      if (msg.type === "bubble") {
        const bubble = new Bubble(
          msg.data.radius,
          msg.data.rise,
          msg.data.volume,
          msg.data.pan,
          msg.data.id
        );
        if (msg.data.id !== void 0) {
          this.#running[msg.data.id] = bubble;
        }
        this.#queue.push(bubble);
        return;
      }
      if (msg.type === "fart") {
        const fart = new Fart(
          msg.data.pressure,
          msg.data.pitch,
          msg.data.rasp,
          msg.data.volume,
          msg.data.pan,
          msg.data.id
        );
        if (msg.data.id !== void 0) {
          this.#running[msg.data.id] = fart;
        }
        this.#queue.push(fart);
        return;
      }
      if (msg.type === "organic") {
        this.#organic(msg.data);
        return;
      }
    };
  }
  #organic(data) {
    const voice = createOrganic(data);
    if (!voice) return;
    if (data.id !== void 0) this.#running[data.id] = voice;
    this.#queue.push(voice);
  }
  process(inputs, outputs) {
    try {
      const time = currentTime;
      const startTime = time * 1e3;
      this.#memoryCheckCounter++;
      if (this.#memoryCheckCounter >= sampleRate * 2) {
        this.#memoryCheckCounter = 0;
        if (this.#processingTimeHistory.length > 0) {
          const avgProcessingTime = this.#processingTimeHistory.reduce((a, b) => a + b, 0) / this.#processingTimeHistory.length;
          if (avgProcessingTime > 2 && this.#performanceMode !== "disabled") {
            console.log("\u{1F6A8} Disabling frequency analysis due to severe performance issues");
            this.#performanceMode = "disabled";
          } else if (avgProcessingTime > 1 && this.#performanceMode === "auto") {
            console.log("\u{1F40C} Switching to low performance mode for mobile optimization");
            this.#performanceMode = "low";
          } else if (avgProcessingTime < 0.5 && this.#performanceMode === "low") {
            console.log("\u{1F680} Switching back to normal performance mode");
            this.#performanceMode = "auto";
          } else if (avgProcessingTime < 0.3 && this.#performanceMode === "disabled") {
            console.log("\u{1F49A} Re-enabling low performance mode");
            this.#performanceMode = "low";
          }
        }
        if (this.#fftBufferLeft?.length > this.#fftSize * 2) {
          console.warn("\u26A0\uFE0F FFT buffer growing too large!", this.#fftBufferLeft.length);
        }
        if (this.#energyHistory?.length > this.#energyHistorySize * 2) {
          console.warn("\u26A0\uFE0F Energy history growing too large!", this.#energyHistory.length);
        }
      }
      if (time - this.#lastTelemetryTime >= 0.25) {
        this.#lastTelemetryTime = time;
        this.port.postMessage({
          type: "telemetry",
          content: {
            queueLength: this.#queue.length,
            runningCount: Object.keys(this.#running).length,
            performanceMode: this.#performanceMode
          }
        });
      }
      const result = this.#processAudio(inputs, outputs, time);
      const processingTime = time * 1e3 - startTime;
      this.#processingTimeHistory.push(processingTime);
      if (this.#processingTimeHistory.length > 100) {
        this.#processingTimeHistory.shift();
      }
      return result;
    } catch (error) {
      console.error("\u{1F6A8} Audio Worklet Error in process():", error, error?.stack);
      return true;
    }
  }
  #processAudio(inputs, outputs, time) {
    if (this.#lastTime && Math.floor(time) !== Math.floor(this.#lastTime)) {
    }
    if (Math.floor(time * 10) % 50 === 0) {
    }
    let waveformLeft = [];
    let waveformRight = [];
    const previousTicks = this.#ticks;
    this.#ticks += time - this.#lastTime;
    this.#lastTime = time;
    if (this.#ticks >= this.#bpmInSec) {
      this.#ticks = 0;
      this.#report("metronome", time);
    }
    const output = outputs[0];
    if (!output || !output[0] || !output[1]) {
      console.error("\u{1F6A8} Invalid outputs:", outputs, output);
      return true;
    }
    let ampLeft = 0, ampRight = 0;
    const waveformSize = round3(sampleRate / 200);
    const waveformRate = 1;
    for (let i = this.#queue.length - 1; i >= 0; i--) {
      const instrument = this.#queue[i];
      if (!instrument.playing) {
        this.#report("killed", { id: instrument.id });
        if (instrument.id !== void 0) delete this.#running[instrument.id];
        this.#queue.splice(i, 1);
      }
    }
    for (let s = 0; s < output[0].length; s += 1) {
      let voices = 0;
      for (const instrument of this.#queue) {
        const amplitude = instrument.next(s);
        output[0][s] += instrument.pan(0, amplitude);
        output[1][s] += instrument.pan(1, amplitude);
        if (instrument.fading) {
          voices += instrument.volume * instrument.fadeGain;
        } else {
          if (instrument.type !== "sample") {
            voices += instrument.volume;
          }
        }
      }
      voices = Math.max(1, voices);
      if (voices > 1) {
        if (!within(1e-3, this.#mixDivisor, voices)) {
          if (this.#mixDivisor < voices) {
            this.#mixDivisor *= 1.005;
          } else {
            this.#mixDivisor *= 0.997;
          }
        }
      }
      output[0][s] = volume2.apply(output[0][s] / this.#mixDivisor);
      output[1][s] = volume2.apply(output[1][s] / this.#mixDivisor);
      {
        const d = Math.abs(output[0][s] - this.#popPrev);
        if (d > 0.3) {
          this.#popCount += 1;
          if (d > this.#popMax) this.#popMax = d;
          if (this.#popEvents.length < 16) {
            const tail = this.#queue[this.#queue.length - 1];
            this.#popEvents.push({
              d: +d.toFixed(4),
              prev: +this.#popPrev.toFixed(4),
              now: +output[0][s].toFixed(4),
              voices: this.#queue.length,
              div: +this.#mixDivisor.toFixed(3),
              fading: tail ? !!tail.fading : null,
              gain: tail?.fadeGain !== void 0 ? +tail.fadeGain.toFixed(4) : null
            });
          }
        }
        this.#popPrev = output[0][s];
      }
      if (glitchEnabled) {
        if (this.#glitchHoldCounter <= 0) {
          const jitter = glitchJitter ? (Math.random() - 0.5) * glitchJitter * 0.6 : 0;
          const rateSwing = (Math.random() * 0.3 - 0.15) * (0.4 + glitchJitter);
          const holdSamples = Math.max(
            1,
            Math.floor(this.#glitchHoldSamples * (1 + jitter + rateSwing))
          );
          this.#glitchHoldCounter = holdSamples;
          if (Math.random() < 0.08 + glitchJitter * 0.15) {
            this.#glitchHeldLeft = output[0][s];
            this.#glitchHeldRight = output[1][s];
          }
        }
        this.#glitchHoldCounter -= 1;
        const crushLevels = 2 ** Math.max(4, glitchCrush);
        const crushedLeft = Math.round(this.#glitchHeldLeft * crushLevels) / crushLevels;
        const crushedRight = Math.round(this.#glitchHeldRight * crushLevels) / crushLevels;
        const skipChance = 1e-3 + glitchJitter * 0.01;
        const skipMix = Math.random() < skipChance ? 0.2 : 0;
        const wetMix = glitchMix * (0.15 + Math.random() * 0.25);
        output[0][s] = output[0][s] * (1 - wetMix) + crushedLeft * wetMix;
        output[1][s] = output[1][s] * (1 - wetMix) + crushedRight * wetMix;
        output[0][s] *= 1 - skipMix;
        output[1][s] *= 1 - skipMix;
      }
      if (roomEnabled) {
        if (s === 0 && Math.floor(currentTime) !== this._lastReverbLogTime) {
          this._lastReverbLogTime = Math.floor(currentTime);
          console.log("\u{1F3E0} REVERB ACTIVE - processing sample, roomEnabled:", roomEnabled);
        }
        output[0][s] = this.#reverbLeft.processSample(output[0][s]);
        output[1][s] = this.#reverbRight.processSample(output[1][s]);
      }
      if (this.#vstBridgeEnabled) {
        if (!this._vstModeWarningLogged) {
          console.warn("\u26A0\uFE0F VST Bridge Mode is ACTIVE - Web Audio output is silenced!");
          this._vstModeWarningLogged = true;
        }
        this.#vstSampleBuffer.left.push(output[0][s]);
        this.#vstSampleBuffer.right.push(output[1][s]);
        if (this.#vstSampleBuffer.left.length >= this.#vstBufferSize) {
          this.#report("vst:samples", {
            left: this.#vstSampleBuffer.left.slice(),
            right: this.#vstSampleBuffer.right.slice(),
            sampleRate
          });
          this.#vstSampleBuffer.left = [];
          this.#vstSampleBuffer.right = [];
        }
        output[0][s] = 0;
        output[1][s] = 0;
      }
      ampLeft = abs4(output[0][s]) > ampLeft ? abs4(output[0][s]) : ampLeft;
      ampRight = abs4(output[1][s]) > ampRight ? abs4(output[1][s]) : ampRight;
      if (s % waveformRate === 0) {
        waveformLeft.push(output[0][s]);
        waveformRight.push(output[1][s]);
      }
    }
    this.#currentWaveformLeft.push(...waveformLeft);
    this.#currentWaveformRight.push(...waveformRight);
    if (this.#currentWaveformLeft.length > waveformSize) {
      const excess = this.#currentWaveformLeft.length - waveformSize;
      this.#currentWaveformLeft.splice(0, excess);
      this.#currentWaveformRight.splice(0, excess);
    }
    this.#currentAmplitudeLeft = ampLeft;
    this.#currentAmplitudeRight = ampRight;
    if (this.#popCount > 0 && currentTime - this.#popLastReport > 0.25) {
      this.#report("pops", {
        count: this.#popCount,
        max: +this.#popMax.toFixed(3),
        at: currentTime,
        events: this.#popEvents
      });
      this.#popCount = 0;
      this.#popMax = 0;
      this.#popEvents = [];
      this.#popLastReport = currentTime;
    }
    if (currentTime - (this.#lastFrequencyRequest ?? -10) > 2) {
      return true;
    }
    this.#fftBufferLeft.push(...output[0]);
    this.#fftBufferRight.push(...output[1]);
    if (this.#fftBufferLeft.length > this.#fftSize) {
      this.#fftBufferLeft = this.#fftBufferLeft.slice(-this.#fftSize);
      this.#fftBufferRight = this.#fftBufferRight.slice(-this.#fftSize);
    }
    this.#analysisCounter = (this.#analysisCounter || 0) + 1;
    if (this.#performanceMode === "disabled") {
      return true;
    }
    let analysisInterval, beatInterval;
    if (this.#performanceMode === "low") {
      analysisInterval = 32;
      beatInterval = 64;
    } else {
      analysisInterval = 16;
      beatInterval = 32;
    }
    if (this.#fftBufferLeft.length >= this.#fftSize && this.#analysisCounter % analysisInterval === 0) {
      this.#frequencyBandsLeft = this.#analyzeFrequencies(this.#fftBufferLeft);
      this.#frequencyBandsRight = this.#analyzeFrequencies(this.#fftBufferRight);
      if (this.#analysisCounter % beatInterval === 0) {
        this.#detectBeats(this.#fftBufferLeft);
      }
    }
    if (this._processCallCount <= 3) {
      console.log("\u{1F50A} #processAudio END call:", this._processCallCount);
    }
    return true;
  }
  // End of #processAudio method
  // === FREQUENCY ANALYSIS METHODS ===
  // Optimized FFT implementation for mobile performance
  #fft(buffer) {
    const N = buffer.length;
    if (N <= 1) return buffer.map((x) => ({ real: x, imag: 0 }));
    const powerOf2 = Math.min(512, Math.pow(2, Math.floor(Math.log2(N))));
    const input = buffer.slice(0, powerOf2);
    const result = input.map((x) => ({ real: x, imag: 0 }));
    for (let i = 0; i < powerOf2; i++) {
      let j = 0;
      for (let k = 0; k < Math.log2(powerOf2); k++) {
        j = j << 1 | i >> k & 1;
      }
      if (j > i) {
        [result[i], result[j]] = [result[j], result[i]];
      }
    }
    for (let len = 2; len <= powerOf2; len *= 2) {
      const w = { real: Math.cos(-2 * Math.PI / len), imag: Math.sin(-2 * Math.PI / len) };
      for (let i = 0; i < powerOf2; i += len) {
        let wn = { real: 1, imag: 0 };
        for (let j = 0; j < len / 2; j++) {
          const u = result[i + j];
          const v = {
            real: result[i + j + len / 2].real * wn.real - result[i + j + len / 2].imag * wn.imag,
            imag: result[i + j + len / 2].real * wn.imag + result[i + j + len / 2].imag * wn.real
          };
          result[i + j] = { real: u.real + v.real, imag: u.imag + v.imag };
          result[i + j + len / 2] = { real: u.real - v.real, imag: u.imag - v.imag };
          const temp = { real: wn.real * w.real - wn.imag * w.imag, imag: wn.real * w.imag + wn.imag * w.real };
          wn = temp;
        }
      }
    }
    return result;
  }
  // Analyze frequencies and return structured frequency bands
  #analyzeFrequencies(buffer) {
    if (buffer.length < this.#fftSize) return [];
    const windowedBuffer = buffer.slice(0, this.#fftSize);
    const fftResult = this.#fft(windowedBuffer);
    const magnitudes = fftResult.map(
      (complex) => Math.sqrt(complex.real * complex.real + complex.imag * complex.imag)
    );
    const bands = [
      { name: "subBass", min: 20, max: 100 },
      // Sub Bass & Bass combined
      { name: "lowMid", min: 100, max: 400 },
      // Low Mid combined
      { name: "mid", min: 400, max: 1e3 },
      // Mid range
      { name: "highMid", min: 1e3, max: 2500 },
      // High Mid combined  
      { name: "presence", min: 2500, max: 5e3 },
      // Presence
      { name: "treble", min: 5e3, max: 1e4 },
      // Treble combined
      { name: "air", min: 1e4, max: 16e3 },
      // Air frequencies
      { name: "ultra", min: 16e3, max: 2e4 }
      // Ultra high combined
    ];
    const binFreq = sampleRate / this.#fftSize;
    return bands.map((band) => {
      const startBin = Math.floor(band.min / binFreq);
      const endBin = Math.min(Math.floor(band.max / binFreq), magnitudes.length / 2);
      let sum = 0;
      let count = 0;
      for (let i = startBin; i < endBin; i++) {
        sum += magnitudes[i];
        count++;
      }
      const amplitude = count > 0 ? sum / count : 0;
      let scaledAmplitude = amplitude;
      if (scaledAmplitude > 0) {
        scaledAmplitude = Math.pow(scaledAmplitude, 0.7);
      }
      return {
        name: band.name,
        frequency: { min: band.min, max: band.max },
        amplitude: Math.min(0.9, scaledAmplitude),
        // Reverted to original 90% clamp
        binRange: { start: startBin, end: endBin }
      };
    });
  }
  // Beat detection using energy-based onset detection
  #detectBeats(buffer) {
    if (buffer.length < this.#fftSize) return;
    const fftData = this.#fft(buffer);
    let currentEnergy = 0;
    const bassEndBin = Math.floor(250 * this.#fftSize / sampleRate);
    for (let i = 1; i < Math.min(bassEndBin, fftData.length / 2); i++) {
      const complex = fftData[i] || { real: 0, imag: 0 };
      currentEnergy += complex.real * complex.real + complex.imag * complex.imag;
    }
    currentEnergy = Math.sqrt(currentEnergy / bassEndBin);
    this.#energyHistory.push(currentEnergy);
    if (this.#energyHistory.length > this.#energyHistorySize) {
      this.#energyHistory.shift();
    }
    if (this.#currentBeat && currentTime - this.#lastBeatTime > 0.05) {
      this.#currentBeat = false;
      this.#beatStrength = 0;
    }
    if (this.#energyHistory.length < this.#energyHistorySize) return;
    const avgEnergy = this.#energyHistory.reduce((sum, energy) => sum + energy, 0) / this.#energyHistory.length;
    const variance = this.#energyHistory.reduce((sum, energy) => sum + Math.pow(energy - avgEnergy, 2), 0) / this.#energyHistory.length;
    this.#energyVariance = Math.sqrt(variance);
    if (currentEnergy > avgEnergy) {
      this.#recentEnergyPeaks.push(currentEnergy);
      if (this.#recentEnergyPeaks.length > 20) {
        this.#recentEnergyPeaks.shift();
      }
    }
    let adaptiveMultiplier = 1;
    if (this.#energyVariance > 0 && avgEnergy > 0) {
      const normalizedVariance = Math.min(this.#energyVariance / 50, 1);
      const energyLevel = Math.min(avgEnergy / 30, 1);
      if (avgEnergy > 20) {
        adaptiveMultiplier = Math.max(0.4, 0.8 - normalizedVariance * 0.3);
      } else if (normalizedVariance > 0.3) {
        adaptiveMultiplier = Math.max(0.7, 1.1 - normalizedVariance * 0.4);
      } else {
        adaptiveMultiplier = 1 + normalizedVariance * 0.2;
      }
    }
    this.#adaptiveThreshold = this.#beatSensitivity * adaptiveMultiplier;
    const timeSinceLastBeat = currentTime - this.#lastBeatTime;
    let timeBasedSensitivity = 1;
    if (timeSinceLastBeat > 0.3) {
      timeBasedSensitivity = 1 + Math.min(0.4, (timeSinceLastBeat - 0.3) * 0.8);
    }
    const finalThreshold = this.#adaptiveThreshold / timeBasedSensitivity;
    const energyRatio = avgEnergy > 0 ? currentEnergy / avgEnergy : 0;
    if (Math.floor(currentTime * 1) % 8 === 0 && this.#energyHistory.length >= this.#energyHistorySize) {
    }
    if (energyRatio > finalThreshold && timeSinceLastBeat > this.#beatCooldown) {
      this.#currentBeat = true;
      this.#beatStrength = Math.min(1, (energyRatio - finalThreshold) / 2);
      this.#lastBeatTime = currentTime;
    }
  }
  // Send data back to the `bios`.
  #report(type2, content) {
    this.port.postMessage({ type: type2, content });
  }
};
registerProcessor("speaker-processor", SpeakerProcessor);
var Reverb = class {
  constructor(sampleRate2, delayTime2, feedback2, mix2) {
    this.sampleRate = sampleRate2;
    this.delayTime = delayTime2;
    this.feedback = feedback2;
    this.mix = mix2;
    this.tapDelays = [
      Math.floor(delayTime2 * 0.23 * sampleRate2),
      // ~28ms - first reflection
      Math.floor(delayTime2 * 0.41 * sampleRate2),
      // ~49ms - early reflection
      Math.floor(delayTime2 * 0.67 * sampleRate2),
      // ~80ms - early reflection  
      Math.floor(delayTime2 * sampleRate2),
      // ~120ms - main delay
      Math.floor(delayTime2 * 1.43 * sampleRate2),
      // ~172ms - late reflection
      Math.floor(delayTime2 * 1.97 * sampleRate2),
      // ~236ms - reverb tail
      Math.floor(delayTime2 * 2.71 * sampleRate2),
      // ~325ms - long tail
      Math.floor(delayTime2 * 3.47 * sampleRate2)
      // ~416ms - very long tail
    ];
    this.tapGains = [0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2];
    const maxDelay = Math.max(...this.tapDelays);
    this.delayBuffer = new Float32Array(maxDelay + 1);
    this.bufferIndex = 0;
    this.bufferSize = maxDelay + 1;
  }
  processSample(inputSample) {
    let wetSample = 0;
    for (let i = 0; i < this.tapDelays.length; i++) {
      const readIndex = (this.bufferIndex - this.tapDelays[i] + this.bufferSize) % this.bufferSize;
      wetSample += this.delayBuffer[readIndex] * this.tapGains[i];
    }
    wetSample *= 0.25;
    let outputSample = inputSample + wetSample * this.mix;
    if (outputSample > 0.95) {
      outputSample = 0.95 + 0.05 * Math.tanh((outputSample - 0.95) * 10);
    } else if (outputSample < -0.95) {
      outputSample = -0.95 + 0.05 * Math.tanh((outputSample + 0.95) * 10);
    }
    this.delayBuffer[this.bufferIndex] = inputSample + wetSample * this.feedback;
    this.bufferIndex = (this.bufferIndex + 1) % this.bufferSize;
    return outputSample;
  }
};
