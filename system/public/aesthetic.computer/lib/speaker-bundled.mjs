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

// public/aesthetic.computer/lib/help.mjs
var { floor } = Math;

// public/aesthetic.computer/lib/num.mjs
var {
  abs,
  round,
  floor: floor2,
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

// public/aesthetic.computer/lib/sound/synth.mjs
var { abs: abs2, floor: floor3, sin: sin2, PI: PI2, min: min2, max: max2, random: random2 } = Math;
var Synth = class {
  // Generic for all instruments.
  playing = true;
  id;
  // Unique for every playing instrument.
  fading = false;
  // If we are fading and then stopping playback.
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
  #sampleEndIndex = 0;
  #sampleStartIndex = 0;
  #sampleSpeed = 0.25;
  #sampleLoop = false;
  // Time stretch + pitch shift fields
  #timeStretchEnabled = false;
  #targetDurationMs = 0;
  #timeStretchRatio = 1;
  #outputSamplesNeeded = 0;
  #playedSamples = 0;
  // Granular synthesis fields
  #grainSize = 2048;
  #grainOverlap = 4;
  #grains = [];
  #grainPhase = 0;
  #sourcePosition = 0;
  #up = false;
  // Specific to `square`.
  #step = 0;
  // Specific to `noise-white` filtering
  #noiseFilterState1 = 0;
  #noiseFilterState2 = 0;
  #noiseFilterState3 = 0;
  #noiseFilterState4 = 0;
  // Custom waveform generation
  #customGenerator;
  // Function that generates waveform data
  #customBuffer = [];
  // Buffer for streaming waveform data
  #customBufferSize = 1024;
  // Size of the streaming buffer
  constructor({ type, id, options, duration, attack, decay, volume, pan }) {
    this.type = type;
    if (id === void 0 || id === null || id === NaN)
      console.warn("\u23F0 No id for sound:", id, type);
    this.id = id;
    if (type === "square" || type === "sine" || type === "triangle" || type === "sawtooth") {
      this.#frequency = options.tone;
    } else if (type === "sample") {
      this.#frequency = null;
      this.#sampleData = options.buffer;
      this.#sampleSpeed = options.speed || 1;
      this.#sampleLoop = options.loop || false;
      this.#sampleStartIndex = clamp(
        options.startSample,
        0,
        this.#sampleData.length - 1
      );
      this.#sampleEndIndex = clamp(
        options.endSample,
        0,
        this.#sampleData.length - 1
      );
      this.#sampleIndex = this.#sampleSpeed < 0 ? this.#sampleEndIndex : this.#sampleStartIndex;
      
      // Time stretch + pitch shift mode
      if (options.targetDuration > 0) {
        this.#timeStretchEnabled = true;
        this.#targetDurationMs = options.targetDuration;
        this.#outputSamplesNeeded = Math.floor((this.#targetDurationMs / 1000) * sampleRate);
        const sourceSamples = this.#sampleEndIndex - this.#sampleStartIndex;
        this.#timeStretchRatio = sourceSamples / this.#outputSamplesNeeded;
        console.log("🎤 SPEAKER-BUNDLED timeStretch init:", {
          targetDurationMs: this.#targetDurationMs,
          sourceSamples,
          outputSamplesNeeded: this.#outputSamplesNeeded,
          timeStretchRatio: this.#timeStretchRatio,
          speed: this.#sampleSpeed
        });
        
        console.log("🎤 BUNDLED timeStretch INIT:", {
          targetDurationMs: this.#targetDurationMs,
          sourceSamples,
          outputSamplesNeeded: this.#outputSamplesNeeded,
          timeStretchRatio: this.#timeStretchRatio,
          speed: this.#sampleSpeed,
          sampleRate
        });
        
        // Minimum duration check
        const minDurationMs = 50;
        if (this.#targetDurationMs < minDurationMs) {
          this.#targetDurationMs = minDurationMs;
          this.#outputSamplesNeeded = Math.floor((this.#targetDurationMs / 1000) * sampleRate);
          this.#timeStretchRatio = sourceSamples / this.#outputSamplesNeeded;
        }
        
        this.#playedSamples = 0;
        this.#sourcePosition = this.#sampleStartIndex;
        this.#grains = [];
        this.#grainPhase = 0;
        this.#grainSize = Math.min(2048, Math.floor(sourceSamples / 8));
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
    this.#attack = attack;
    this.#duration = this.type === "sample" ? Infinity : duration;
    this.#decay = decay;
    this.#decayStart = this.#duration - this.#decay;
    this.#pan = pan;
    this.volume = volume;
    this.#futureVolume = this.volume;
  }
  next(channelIndex) {
    if (this.#frequencyUpdatesLeft > 0) {
      this.#frequency += this.#frequencyUpdateSlice;
      this.#wavelength = sampleRate / this.#frequency;
      this.#frequencyUpdatesLeft -= 1;
    }
    if (this.#volumeUpdatesLeft > 0) {
      this.volume += this.#volumeUpdateSlice;
      this.#volumeUpdatesLeft -= 1;
    }
    let value;
    if (this.type === "square") {
      this.#step += 1;
      if (this.#step >= this.#wavelength) {
        this.#up = !this.#up;
        this.#step -= this.#wavelength;
      }
      value = this.#up ? 1 : -1;
    } else if (this.type === "sine") {
      const increment = 2 * PI2 * this.#frequency / sampleRate;
      this.#phase += increment;
      if (this.#phase > 2 * PI2) {
        this.#phase -= 2 * PI2;
      }
      value = sin2(this.#phase);
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
      if (this.#frequency && this.#frequency > 0) {
        const normalizedFreq = this.#frequency * 2 / sampleRate;
        const clampedFreq = clamp(normalizedFreq, 1e-3, 0.99);
        const resonance = 0.1;
        const omega = clampedFreq * PI2;
        const sin3 = Math.sin(omega);
        const cos2 = Math.cos(omega);
        const alpha = sin3 / (2 * (1 / resonance));
        const b0 = (1 - cos2) / 2;
        const b1 = 1 - cos2;
        const b2 = (1 - cos2) / 2;
        const a0 = 1 + alpha;
        const a1 = -2 * cos2;
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
    } else if (this.type === "sample") {
      const bufferData = this.#sampleData.channels[0];
      
      // Time stretch + pitch shift mode using granular synthesis
      if (this.#timeStretchEnabled) {
        this.#playedSamples++;
        
        // Log every 10000 samples to track progress without spam
        if (this.#playedSamples === 1 || this.#playedSamples % 10000 === 0) {
          console.log("🎤 BUNDLED timeStretch RENDER:", {
            playedSamples: this.#playedSamples,
            outputNeeded: this.#outputSamplesNeeded,
            sourcePos: this.#sourcePosition.toFixed(0),
            grains: this.#grains.length
          });
        }
        
        const grainSpacing = this.#grainSize / this.#grainOverlap;
        
        // Spawn new grain when needed
        this.#grainPhase++;
        if (this.#grainPhase >= grainSpacing && this.#sourcePosition < this.#sampleEndIndex) {
          this.#grainPhase = 0;
          this.#grains.push({
            sourceStart: this.#sourcePosition,
            position: 0,
          });
        }
        
        // Advance source position based on time stretch ratio
        this.#sourcePosition += this.#timeStretchRatio;
        
        // Mix all active grains
        value = 0;
        const activeGrains = [];
        
        for (const grain of this.#grains) {
          const grainProgress = grain.position / this.#grainSize;
          const envelope = 0.5 * (1 - Math.cos(2 * Math.PI * grainProgress));
          
          // Read from source at pitch-shifted rate
          const sourceIdx = grain.sourceStart + (grain.position * this.#sampleSpeed);
          
          if (sourceIdx >= this.#sampleEndIndex || sourceIdx < this.#sampleStartIndex) {
            grain.position++;
            if (grain.position < this.#grainSize) {
              activeGrains.push(grain);
            }
            continue;
          }
          
          // Linear interpolation
          const idx0 = floor3(sourceIdx);
          const idx1 = idx0 + 1 < this.#sampleEndIndex ? idx0 + 1 : idx0;
          const frac = sourceIdx - idx0;
          const sample0 = bufferData[idx0] || 0;
          const sample1 = bufferData[idx1] || 0;
          const interpolatedSample = sample0 + frac * (sample1 - sample0);
          
          value += interpolatedSample * envelope;
          grain.position++;
          
          if (grain.position < this.#grainSize) {
            activeGrains.push(grain);
          }
        }
        
        this.#grains = activeGrains;
        value /= (this.#grainOverlap / 2);
        
        // Stop when done
        if (this.#playedSamples >= this.#outputSamplesNeeded || 
            (this.#grains.length === 0 && this.#sourcePosition >= this.#sampleEndIndex)) {
          this.playing = false;
          return 0;
        }
      } else {
        // Normal sample playback
        value = bufferData[floor3(this.#sampleIndex)];
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
    if (this.#duration < Infinity) {
      if (this.type === "noise-white") {
        const sharpAttack = min2(1, this.#progress / (this.#attack * 0.1));
        if (sharpAttack) value *= sharpAttack;
        const decayProgress = (this.#progress - this.#decayStart) / (this.#decay * 0.05);
        const sharpDecay = min2(1, 1 - Math.pow(decayProgress, 3));
        value *= max2(0, sharpDecay);
      } else {
        const attack2 = min2(1, this.#progress / this.#attack);
        if (attack2) value *= attack2;
        const decay2 = min2(
          1,
          1 - (this.#progress - this.#decayStart) / this.#decay
        );
        value *= decay2;
      }
    } else {
      if (this.#attack > 0) {
        const attack2 = min2(1, this.#progress / this.#attack);
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
  update({ tone, volume: volume3, shift, sampleSpeed, samplePosition, duration: duration2 = 0.1 }) {
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
      this.#sampleSpeed += shift;
    }
    if (typeof sampleSpeed === "number") {
      this.#sampleSpeed = sampleSpeed;
    }
    if (typeof samplePosition === "number" && this.#sampleData) {
      this.#sampleIndex = floor3(samplePosition * this.#sampleData.length);
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
  kill(fade = 0.025) {
    if (!fade) {
      this.playing = false;
    } else {
      this.fading = true;
      this.fadeProgress = 0;
      this.fadeDuration = fade * sampleRate;
    }
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
        frequency: this.#frequency,
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

// public/aesthetic.computer/lib/speaker.mjs
var { abs: abs3, round: round2, floor: floor4 } = Math;
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
  #reverbLeft;
  #reverbRight;
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
    super();
    this.#lastTime = currentTime;
    this.#bpm = options2.processorOptions.bpm;
    this.#bpmInSec = 60 / this.#bpm;
    this.#ticks = this.#bpmInSec;
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
        this.#running[msg.data.id]?.update(msg.data.properties);
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
        console.log("🧩 GLITCH TOGGLE:", glitchEnabled ? "ON" : "OFF");
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
        if (data.crush !== void 0) glitchCrush = clamp(round2(data.crush), 2, 12);
        if (data.rate !== void 0) glitchRate = clamp(data.rate, 20, 8000);
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
            duration2 = round2(sampleRate * (this.#bpmInSec * data.beats));
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
            const startSample = round2(from * data.options.buffer.length);
            const endSample = round2(to * data.options.buffer.length);
            data.options.startSample = startSample;
            data.options.endSample = endSample;
            duration2 = round2(
              (endSample - startSample) / abs3(data.options.speed || 1) / data.options.buffer.sampleRate * sampleRate
            );
          }
          attack2 = round2(duration2 * msg.data.attack || 0);
          decay2 = round2(duration2 * msg.data.decay || 0);
        }
        let synthOptions = msg.data.options || { tone: msg.data.tone };
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
    };
  }
  process(inputs, outputs) {
    try {
      const currentTime2 = this.currentTime;
      const startTime = currentTime2 * 1e3;
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
      const result = this.#processAudio(inputs, outputs, currentTime2);
      const processingTime = currentTime2 * 1e3 - startTime;
      this.#processingTimeHistory.push(processingTime);
      if (this.#processingTimeHistory.length > 100) {
        this.#processingTimeHistory.shift();
      }
      return result;
    } catch (error) {
      console.error("\u{1F6A8} Audio Worklet Error:", error);
      return true;
    }
  }
  #processAudio(inputs, outputs, currentTime2) {
    if (Math.floor(currentTime2 * 10) % 50 === 0) {
      console.log(`\u{1F3B5} WORKLET_TIME: ${currentTime2.toFixed(6)}s, sampleRate=${sampleRate}, frame=${currentFrame}`);
    }
    let waveformLeft = [];
    let waveformRight = [];
    const previousTicks = this.#ticks;
    this.#ticks += currentTime2 - this.#lastTime;
    this.#lastTime = currentTime2;
    if (this.#ticks >= this.#bpmInSec) {
      console.log(`\u{1F3B5} BEAT: ${currentTime2.toFixed(6)}s, bpm=${this.#bpm}, interval=${this.#bpmInSec.toFixed(3)}s, tick_overflow=${(this.#ticks - this.#bpmInSec).toFixed(6)}s`);
      this.#ticks = 0;
      this.#report("metronome", currentTime2);
    }
    const output = outputs[0];
    let ampLeft = 0, ampRight = 0;
    const waveformSize = round2(sampleRate / 200);
    const waveformRate = 1;
    for (let s = 0; s < output[0].length; s += 1) {
      this.#queue = this.#queue.filter((instrument) => {
        if (!instrument.playing) {
          this.#report("killed", { id: instrument.id });
        }
        return instrument.playing;
      });
      let voices = 0;
      for (const instrument of this.#queue) {
        const amplitude = instrument.next(s);
        output[0][s] += instrument.pan(0, amplitude);
        output[1][s] += instrument.pan(1, amplitude);
        if (instrument.fading) {
          voices += instrument.volume * (1 - instrument.fadeProgress / instrument.fadeDuration);
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
      if (glitchEnabled) {
        if (this.#glitchHoldCounter <= 0) {
          const jitter = glitchJitter ? (Math.random() - 0.5) * glitchJitter * 0.6 : 0;
          const rateSwing = (Math.random() * 0.3 - 0.15) * (0.4 + glitchJitter);
          const holdSamples = Math.max(1, Math.floor(this.#glitchHoldSamples * (1 + jitter + rateSwing)));
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
        if (s === 0 && Math.floor(currentTime2) !== this._lastReverbLogTime) {
          this._lastReverbLogTime = Math.floor(currentTime2);
          console.log("\u{1F3E0} REVERB ACTIVE - processing sample, roomEnabled:", roomEnabled);
        }
        output[0][s] = this.#reverbLeft.processSample(output[0][s]);
        output[1][s] = this.#reverbRight.processSample(output[1][s]);
      }
      if (this.#vstBridgeEnabled) {
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
      ampLeft = abs3(output[0][s]) > ampLeft ? abs3(output[0][s]) : ampLeft;
      ampRight = abs3(output[1][s]) > ampRight ? abs3(output[1][s]) : ampRight;
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
    this.#fftBufferLeft.push(...output[0]);
    this.#fftBufferRight.push(...output[1]);
    if (this.#fftBufferLeft.length > this.#fftSize) {
      this.#fftBufferLeft = this.#fftBufferLeft.slice(-this.#fftSize);
      this.#fftBufferRight = this.#fftBufferRight.slice(-this.#fftSize);
    }
    this.#analysisCounter = (this.#analysisCounter || 0) + 1;
    if (this.#performanceMode === "disabled") {
      return;
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
