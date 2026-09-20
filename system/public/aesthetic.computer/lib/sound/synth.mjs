import { within, lerp, clamp } from "../num.mjs";
import { FormantBank, vowelBands } from "./formant.mjs";
const { abs, floor, round, sin, cos, PI, min, max, random, pow } = Math;

export default class Synth {
  // Generic for all instruments.
  playing = true;
  id; // Unique for every playing instrument.

  // 🍿 Shortest release any kill can ask for. Below ~2ms the ramp stops
  // rounding the waveform's corner and starts sounding like the cut it
  // replaced.
  static MIN_RELEASE = 0.002;

  fading = false; // If we are fading and then stopping playback.
  fadeGain = 1; // Release multiplier — walks 1 → 0 and never back up.
  fadeStep = 0; // How much `fadeGain` drops per sample.
  fadeProgress;
  fadeDuration;

  type; // square, sine, triangle, sawtooth, sample, noise-white, custom

  #phase = 0;
  #frequency;
  #duration = 0;
  #attack = 0;
  #decay = 0;
  #decayStart;

  volume = 1; // 0 to 1
  #futureVolume = 1;
  #pan = 0; // -1 to 1

  #progress = 0;

  #wavelength; // Calculated from the frequency.
  #futureFrequency;

  #frequencyUpdatesTotal;
  #frequencyUpdatesLeft;
  #frequencyUpdateSlice;

  #volumeUpdatesTotal;
  #volumeUpdatesLeft;
  #volumeUpdateSlice;

  #sampleData; // Specific to `sample`.
  #sampleIndex = 0;
  #declick = 0; // Samples left in the post-relocation attack ramp
  #declickTotal = 1;
  #pitch = 1; // Independent pitch factor (1 = none) — tempo untouched
  #pitchPhase = 0; // Sweep phase of the two-tap delay-line shifter
  #sampleEndIndex = 0;
  #sampleStartIndex = 0;
  #sampleSpeed = 0.25;
  #sampleLoop = false;
  #preserveDuration = false; // If true, pitch shift without changing duration (granular)
  #targetDurationSamples = 0; // Original duration in samples when preserving
  #playedSamples = 0; // Track how many samples we've output
  
  // Time stretch + pitch shift fields
  #timeStretchEnabled = false; // If true, stretch sample to targetDuration, then pitch shift
  #targetDurationMs = 0; // Target duration in milliseconds (for time stretch mode)
  #timeStretchRatio = 1; // How much to stretch/compress time (>1 = slower, <1 = faster)
  #outputSamplesNeeded = 0; // How many output samples to produce
  
  // Granular pitch shifting fields
  #grainSize = 2048; // Size of each grain in samples (~46ms at 44100Hz)
  #grainOverlap = 4; // Number of overlapping grains (more = smoother)
  #grains = []; // Array of active grains
  #grainPhase = 0; // Phase for spawning new grains
  #sourcePosition = 0; // Position in source buffer (independent of output)

  #up = false; // Specific to `square`.
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
  #harpBuf = null;       // Float32Array — string delay line
  #harpW = 0;            // write index
  #harpLp1 = 0;          // 1-pole moving-average LPF state

  // Specific to `whistle` — Cook/STK digital waveguide flute model.
  // Mirrors the C generate_whistle_sample in fedac/native/src/audio.c.
  #whistleBoreBuf = null;    // bore delay line
  #whistleBoreW = 0;
  #whistleJetBuf = null;     // jet delay line
  #whistleJetW = 0;
  #whistleBreath = 0;        // smoothed breath pressure
  #whistleVibratoPhase = 0;  // 5 Hz LFO phase
  #whistleLp1 = 0;           // 1-pole loop LPF state
  #whistleHpX1 = 0;          // 1-pole DC blocker — last input
  #whistleHpY1 = 0;          // 1-pole DC blocker — last output
  #whistleNoiseSeed = 0;     // xorshift32 state

  // Custom waveform generation
  #customGenerator; // Function that generates waveform data
  #customBuffer = []; // Buffer for streaming waveform data
  #customBufferSize = 1024; // Size of the streaming buffer

  // 🐦 Expression — slide, vibrato, drift bend the pitch; noise, formant and
  // lowpass shape the source; tremolo rides the level. Everything stays off
  // (null / 0) unless asked for, so a plain voice renders exactly as before.
  #slideLeft = 0; // samples left in the exponential glide
  #slideRatio = 1; // per-sample frequency multiplier while sliding
  #slideTarget = 0; // snapped to at the end so rounding never leaves it off-pitch
  #vibrato = null; // { rate, depth, delay } — depth in semitones, delay in samples
  #vibratoPhase = 0;
  #drift = 0; // semitones of wander
  #driftPos = 0; // smoothed position, -1..1
  #driftTarget = 0;
  #driftCountdown = 0; // samples until the walk picks a new target
  #liveFrequency; // the modulated pitch the last sample was rendered at
  #noise = 0; // white-noise mix, 0..1
  #formant = null; // FormantBank
  #lowpass = null; // { cutoff, q } — RBJ lowpass, coefficients cached below
  #lpB0 = 0; #lpB1 = 0; #lpA1 = 0; #lpA2 = 0;
  #lpX1 = 0; #lpX2 = 0; #lpY1 = 0; #lpY2 = 0;
  #lpSweepLeft = 0; // samples left in the cutoff sweep
  #lpSweepRatio = 1;
  #lpSweepTarget = 0;
  #tremolo = null; // { rate, depth }
  #tremoloPhase = 0;

  constructor({ type, id, options, duration, attack, decay, volume, pan }) {
    // console.log("New Synth:", arguments);
    // 🌊 Accept "noise" as an alias for "noise-white" so code that targets
    // the native AC synth (fedac/native/src/js-bindings.c also aliases both
    // strings to WAVE_NOISE) plays correctly on the web. Without this the
    // shared drum kit in lib/percussion.mjs falls through every noise
    // branch and silently drops snares/hats/claps/etc.
    if (type === "noise") type = "noise-white";
    this.type = type;
    if (id === undefined || id === null || id === NaN)
      console.warn("⏰ No id for sound:", id, type);
    this.id = id;

    if (
      type === "square" ||
      type === "sine" ||
      type === "triangle" ||
      type === "sawtooth"
    ) {
      this.#frequency = options.tone;
    } else if (type === "harp" || type === "pluck" ||
               type === "guitar" || type === "string") {
      // Karplus-Strong: pre-fill the delay line with one wavelength of
      // pre-smoothed white noise — the initial "pluck". Loop then decays
      // exponentially via the two-point moving-average filter + stretch.
      this.#frequency = options.tone;
      const N = 2048;
      this.#harpBuf = new Float32Array(N);
      const stringDelay = clamp(sampleRate / this.#frequency, 2, N - 2);
      const n = Math.floor(stringDelay);
      let last = 0;
      for (let i = 0; i < n; i++) {
        const white = random() * 2 - 1;
        const filt = 0.5 * (white + last);
        last = white;
        this.#harpBuf[i] = filt;
      }
      this.#harpW = n;
      this.type = "harp"; // normalize alias
    } else if (type === "whistle" || type === "ocarina" ||
               type === "flute" || type === "skullwhistle" ||
               type === "skull-whistle") {
      // Digital waveguide flute (Cook/STK). Bore delay = SR/freq; jet
      // delay = 0.32 × bore. Cubic nonlinearity drives the loop into
      // self-oscillation via DC breath pressure. Buffers stay zeroed —
      // breath pressure ramps the loop up from rest naturally.
      this.#frequency = options.tone;
      this.#whistleBoreBuf = new Float32Array(2048);
      this.#whistleJetBuf = new Float32Array(512);
      // id can arrive as a BigInt from the worklet message channel; coerce
      // to Number before mixing with the multiplier so the >>> 0 chain
      // doesn't throw "Cannot mix BigInt and other types".
      this.#whistleNoiseSeed = ((Number(id) || 1) * 2654435761) >>> 0;
      this.type = "whistle"; // normalize alias
    } else if (type === "sample") {
      this.#frequency = null; // 1; // TODO: This could be a low or high pass
      //                                    option here?

      this.#sampleData = options.buffer;
      this.sampleLabel = options.label; // Track the label for live buffer updates

      /*console.log("🎤 SYNTH sample init:", {
        hasBuffer: !!options.buffer,
        bufferType: typeof options.buffer,
        hasChannels: !!options.buffer?.channels,
        channelsLength: options.buffer?.channels?.length,
        channel0Length: options.buffer?.channels?.[0]?.length,
        label: options.label,
        speed: options.speed,
        loop: options.loop,
        preserveDuration: options.preserveDuration,
      });*/

      this.#sampleSpeed = options.speed || 1;
      this.#sampleLoop = options.loop || false;
      this.#preserveDuration = options.preserveDuration || false;

      if (this.#preserveDuration) {
        // console.log("🎤 SYNTH preserveDuration enabled - granular pitch shift (no time stretch)");
      }

      // console.log("Speed:", this.#sampleSpeed);

      // if (this.#sampleSpeed < 0)
      // this.#sampleIndex = this.#sampleData.length - 1; // Otherwise 0.
      // this.#sampleStartIndex = options.startSample;
      // this.#sampleEndIndex = options.endSample;
      // Check the bounds of the sample data.
      // Note: sampleData can be { channels: [[...], [...]] } or a flat Float32Array
      const sampleLength = this.#sampleData.channels?.[0]?.length ?? this.#sampleData.length;
      this.#sampleStartIndex = clamp(
        options.startSample,
        0,
        sampleLength - 1,
      );
      this.#sampleEndIndex = clamp(
        options.endSample,
        0,
        sampleLength - 1,
      );

      this.#sampleIndex =
        this.#sampleSpeed < 0 ? this.#sampleEndIndex : this.#sampleStartIndex;
      
      // Time stretch + pitch shift mode: stretch to target duration, then pitch shift
      // This is for speech synthesis where we want the sample to fit the note's duration
      if (options.targetDuration > 0) {
        this.#timeStretchEnabled = true;
        this.#targetDurationMs = options.targetDuration;
        
        // Calculate how many output samples we need
        const sampleRate = options.sampleRate || 44100;
        this.#outputSamplesNeeded = Math.floor((this.#targetDurationMs / 1000) * sampleRate);
        
        // Calculate the time stretch ratio
        // sourceSamples / outputSamples = how fast we read through source
        const sourceSamples = this.#sampleEndIndex - this.#sampleStartIndex;
        this.#timeStretchRatio = sourceSamples / this.#outputSamplesNeeded;
        
        // Minimum duration check - avoid stretching too much
        const minDurationMs = 50; // 50ms minimum
        if (this.#targetDurationMs < minDurationMs) {
          // console.log(`🎤 SYNTH: Target duration ${this.#targetDurationMs}ms too short, clamping to ${minDurationMs}ms`);
          this.#targetDurationMs = minDurationMs;
          this.#outputSamplesNeeded = Math.floor((this.#targetDurationMs / 1000) * sampleRate);
          this.#timeStretchRatio = sourceSamples / this.#outputSamplesNeeded;
        }
        
        /*console.log("🎤 SYNTH timeStretch enabled:", {
          targetDurationMs: this.#targetDurationMs,
          sourceSamples,
          outputSamplesNeeded: this.#outputSamplesNeeded,
          timeStretchRatio: this.#timeStretchRatio.toFixed(3),
          pitchShiftSpeed: this.#sampleSpeed.toFixed(3),
        });*/
        
        // Setup granular for combined time stretch + pitch shift
        this.#playedSamples = 0;
        this.#sourcePosition = this.#sampleStartIndex;
        this.#grains = [];
        this.#grainPhase = 0;
        
        // Adjust grain size based on source sample length
        this.#grainSize = Math.min(2048, Math.floor(sourceSamples / 8));
        this.#grainSize = Math.max(256, this.#grainSize);
      }
      // When preserving duration, set up granular pitch shifting
      else if (this.#preserveDuration) {
        this.#targetDurationSamples = this.#sampleEndIndex - this.#sampleStartIndex;
        this.#playedSamples = 0;
        this.#sourcePosition = this.#sampleStartIndex;
        this.#grains = [];
        this.#grainPhase = 0;
        // Adjust grain size based on sample length - smaller for short samples
        const sampleDuration = this.#targetDurationSamples;
        this.#grainSize = Math.min(2048, Math.floor(sampleDuration / 8));
        this.#grainSize = Math.max(256, this.#grainSize); // Minimum grain size
      }
    } else if (type === "custom") {
      this.#frequency = options.tone || 440; // Default frequency for custom waveforms
      
      // Handle generator function (could be a string from postMessage)
      if (typeof options.generator === "string") {
        try {
          // Convert string back to function
          this.#customGenerator = eval(`(${options.generator})`);
        } catch (error) {
          console.error("🎨 Failed to parse custom generator:", error);
          throw new Error("Invalid custom generator function string");
        }
      } else {
        this.#customGenerator = options.generator; // Direct function
      }

      if (typeof this.#customGenerator !== "function") {
        throw new Error("Custom synth type requires a generator function");
      }

      // Pre-fill the buffer with initial data
      this._fillCustomBuffer();
    } else if (type === "noise-white") {
      this.#frequency = options.tone; // Use the tone parameter for filtering
      // Initialize filter state variables for resonant filter
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
    // if (this.#type === "sample") console.log("⏱️ Sample duration:", this.#duration);

    this.#decay = decay;
    this.#decayStart = this.#duration - this.#decay;

    this.#pan = pan;

    this.volume = volume;
    this.#futureVolume = this.volume;

    // A slide or sweep with no duration of its own runs the length of the
    // note; a held voice has no length, so it takes a quarter second.
    if (this.type !== "sample") {
      const span = this.#duration < Infinity ? this.#duration / sampleRate : 0.25;
      this._express(options, span);
    }

    // console.log("〰️", this);
  }

  // Parse the expression options shared by the constructor and update().
  // `span` is the fallback glide length in seconds.
  _express(o, span) {
    if (o.slide > 0 && this.#frequency > 0) {
      const seconds = o.slideDuration > 0 ? o.slideDuration : span;
      this.#slideTarget = o.slide;
      this.#slideLeft = max(1, round(seconds * sampleRate));
      this.#slideRatio = pow(o.slide / this.#frequency, 1 / this.#slideLeft);
      this.#futureFrequency = o.slide;
    }
    if (o.vibrato !== undefined) {
      const v = typeof o.vibrato === "number" ? { depth: o.vibrato } : o.vibrato;
      this.#vibrato = v && (v.depth ?? 0.5) > 0
        ? { rate: v.rate ?? 5, depth: v.depth ?? 0.5, delay: (v.delay ?? 0) * sampleRate }
        : null;
      this.#vibratoPhase = 0;
    }
    if (o.tremolo !== undefined) {
      const t = typeof o.tremolo === "number" ? { depth: o.tremolo } : o.tremolo;
      this.#tremolo = t && (t.depth ?? 0.3) > 0
        ? { rate: t.rate ?? 6, depth: min(1, t.depth ?? 0.3) }
        : null;
      this.#tremoloPhase = 0;
    }
    if (o.drift !== undefined) this.#drift = o.drift > 0 ? o.drift : 0;
    if (o.noise !== undefined) this.#noise = clamp(o.noise, 0, 1) || 0;
    if (o.formant !== undefined) {
      const f = o.formant;
      const bands = f == null ? null
        : typeof f === "string" || Array.isArray(f) ? vowelBands(f)
        : vowelBands(f.vowel ?? f.bands, f.scale ?? 1);
      if (!bands) this.#formant = null;
      else if (this.#formant) this.#formant.set(bands);
      else this.#formant = new FormantBank(bands, sampleRate);
    }
    if (o.lowpass !== undefined) {
      const lp = typeof o.lowpass === "number" ? { cutoff: o.lowpass } : o.lowpass;
      if (!lp || !(lp.cutoff > 0)) {
        this.#lowpass = null;
        this.#lpSweepLeft = 0;
      } else {
        // resonance 0..1 → Q: 0.707 (flat) up to ~11 (a whistle on the knee).
        const q = 0.7071 * pow(2, clamp(lp.resonance ?? 0.2, 0, 1) * 4);
        const fresh = !this.#lowpass;
        if (fresh) this.#lpX1 = this.#lpX2 = this.#lpY1 = this.#lpY2 = 0;
        // A live voice glides to the new cutoff instead of stepping; a
        // fresh one starts there and only moves if asked to sweep.
        const from = fresh ? lp.cutoff : this.#lowpass.cutoff;
        const to = lp.sweep > 0 ? lp.sweep : lp.cutoff;
        this.#lowpass = { cutoff: from, q };
        this._lowpassCoefficients();
        if (to !== from) {
          const seconds = lp.sweepDuration > 0 ? lp.sweepDuration : span;
          this.#lpSweepTarget = to;
          this.#lpSweepLeft = max(1, round(seconds * sampleRate));
          this.#lpSweepRatio = pow(to / from, 1 / this.#lpSweepLeft);
        } else {
          this.#lpSweepLeft = 0;
        }
      }
    }
  }

  _lowpassCoefficients() {
    const { cutoff, q } = this.#lowpass;
    const w0 = (2 * PI * min(cutoff, sampleRate * 0.45)) / sampleRate;
    const c = cos(w0);
    const alpha = sin(w0) / (2 * q);
    const a0 = 1 + alpha;
    // Resonance lifts the knee by Q; pulling the whole band down by √Q keeps
    // a swept sawtooth from clipping while the peak still stands proud of the
    // passband — the same bass-loss a ladder filter has when you turn it up.
    const gain = q > 0.7071 ? 1 / Math.sqrt(q / 0.7071) : 1;
    this.#lpB0 = ((1 - c) / 2 / a0) * gain; // b2 = b0
    this.#lpB1 = ((1 - c) / a0) * gain;
    this.#lpA1 = (-2 * c) / a0;
    this.#lpA2 = (1 - alpha) / a0;
  }

  next(channelIndex) {
    // 🍿 Once a voice is done it stays done. The mixer only prunes the queue
    // between render quanta, so a note that finishes mid-block keeps getting
    // asked for samples until the next block starts. Without this guard those
    // leftover samples fall past the release branch (`fading` is already
    // false) and come back at FULL volume — up to 2.7ms of the note blaring
    // after its fade, then a hard cut to silence when the prune finally runs.
    // That step is the click you hear at the end of a pad.
    if (!this.playing) return 0;

    // 🚥 Intepolated Properties 🎼

    // 📊 Frequency
    if (this.#frequencyUpdatesLeft > 0) {
      this.#frequency += this.#frequencyUpdateSlice;
      this.#wavelength = sampleRate / this.#frequency;
      this.#frequencyUpdatesLeft -= 1;
    }

    // 📢 Volume
    if (this.#volumeUpdatesLeft > 0) {
      this.volume += this.#volumeUpdateSlice;
      this.#volumeUpdatesLeft -= 1;
    }

    // 🐦 Pitch expression — one effective frequency drives every source.
    // The slide moves `#frequency` itself (a constant ratio per sample is an
    // exponential glide, which is how a throat moves), so a later linear
    // update({tone}) still departs from wherever the pitch actually is.
    // Vibrato and drift only bend the sample being rendered.
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
        // Fade in over 150ms once the delay has passed — a straight singer
        // lands the note first, then lets it shimmer.
        const amount = min(1, max(0, (this.#progress - v.delay) / (0.15 * sampleRate)));
        this.#vibratoPhase += (2 * PI * v.rate) / sampleRate;
        if (this.#vibratoPhase > 2 * PI) this.#vibratoPhase -= 2 * PI;
        cents += v.depth * amount * sin(this.#vibratoPhase);
      }
      if (this.#drift > 0) {
        // A random walk that picks a new target about twice a second and
        // eases toward it through a ~2 Hz one-pole, so the pitch wanders
        // rather than jitters.
        if (this.#driftCountdown <= 0) {
          this.#driftTarget = random() * 2 - 1;
          this.#driftCountdown = round(sampleRate * (0.3 + random() * 0.4));
        }
        this.#driftCountdown -= 1;
        this.#driftPos += (this.#driftTarget - this.#driftPos) * ((2 * PI * 2) / sampleRate);
        cents += this.#drift * this.#driftPos;
      }
      freq *= pow(2, cents / 12);
      this.#wavelength = sampleRate / freq;
    }
    this.#liveFrequency = freq;

    // 🎸🎙️ Waveform Sources 🎹
    let value;
    if (this.type === "square") {
      // 🟥 Square Wave
      // `#wavelength` is a FULL period, so the level flips at every HALF of
      // it. Flipping on the full period gave a square an octave below its
      // requested tone — measurably: a `square` asked for 261.63 Hz came out
      // at 130 Hz, while sine/triangle/sawtooth all landed on pitch. Both
      // sibling engines already do this right (fedac/native/src/audio.c's
      // WAVE_SQUARE and MenuBandPercussion.swift both take `phase < 0.5`),
      // so the web synth was the lone outlier in a drum kit that is supposed
      // to sound identical across all three.
      const halfWavelength = this.#wavelength / 2;
      this.#step += 1;
      if (this.#step >= halfWavelength) {
        this.#up = !this.#up;
        this.#step -= halfWavelength;
      }
      value = this.#up ? 1 : -1;
    } else if (this.type === "sine") {
      // 🟣 Sine Wave
      // Generate using a 'Phase Increment' method.
      const increment = (2 * PI * freq) / sampleRate;
      this.#phase += increment;
      if (this.#phase > 2 * PI) {
        this.#phase -= 2 * PI;
      }
      value = sin(this.#phase);
    } else if (this.type === "triangle") {
      // 📐 Triangle Wave - starts at 0 to avoid clicks
      const stepSize = 4 / this.#wavelength;
      // Offset by quarter wavelength to start at 0 instead of -1
      const adjustedStep = (this.#step + this.#wavelength / 4) % this.#wavelength;
      value = 1 - abs(adjustedStep * stepSize - 2);
      this.#step += 1;
      if (this.#step >= this.#wavelength) this.#step = 0;
    } else if (this.type === "sawtooth") {
      // 🪚 Sawtooth Wave
      value = 2 * (this.#step / this.#wavelength) - 1;
      this.#step += 1;
      if (this.#step >= this.#wavelength) this.#step = 0;
    } else if (this.type === "noise-white") {
      // 🌊 Filtered White Noise - responds to frequency/octave
      // Generate white noise
      const noise = random() * 2 - 1;
      
      // Apply resonant low-pass filter centered on the frequency
      // This makes the noise "pitched" by emphasizing frequencies around the tone
      if (freq && freq > 0) {
        // Calculate filter coefficients based on frequency
        // Normalize frequency to 0-1 range (0 = DC, 1 = Nyquist frequency)
        const normalizedFreq = (freq * 2) / sampleRate;
        const clampedFreq = clamp(normalizedFreq, 0.001, 0.99);
        
        // Sharp resonant filter coefficients
        const resonance = 0.1; // High resonance for sharp, distinct pitched effect
        const omega = clampedFreq * PI;
        const sin = Math.sin(omega);
        const cos = Math.cos(omega);
        const alpha = sin / (2 * (1 / resonance));
        
        // Biquad low-pass filter coefficients
        const b0 = (1 - cos) / 2;
        const b1 = 1 - cos;
        const b2 = (1 - cos) / 2;
        const a0 = 1 + alpha;
        const a1 = -2 * cos;
        const a2 = 1 - alpha;
        
        // Apply filter (Direct Form I)
        const output = (b0 * noise + b1 * this.#noiseFilterState1 + b2 * this.#noiseFilterState2 - a1 * this.#noiseFilterState3 - a2 * this.#noiseFilterState4) / a0;
        
        // Update filter state (store input and output history)
        this.#noiseFilterState2 = this.#noiseFilterState1;
        this.#noiseFilterState1 = noise;
        this.#noiseFilterState4 = this.#noiseFilterState3;
        this.#noiseFilterState3 = output;
        
        // Boost the filtered output more for sharper response
        value = output * 3.5;
      } else {
        // Fallback to unfiltered white noise if no frequency
        value = noise;
      }
      // 🚩 TODO: Also add pink and brownian noise.
    } else if (this.type === "harp") {
      // 🪕 Karplus-Strong plucked string. Read delayed sample → average
      // with previous → multiply by stretch S → write back. Output
      // boosted ×2.5 because the LPF + stretch leave raw amplitude well
      // below the oscillators at the same `volume`. Mirror of C
      // generate_harp_sample (fedac/native/src/audio.c).
      const N = this.#harpBuf.length;
      const stringDelay = clamp(sampleRate / freq, 2, N - 2);
      // Fractional-delay read with linear interpolation.
      let rd = this.#harpW - stringDelay;
      while (rd < 0) rd += N;
      const i0 = floor(rd) | 0;
      const i1 = (i0 + 1) % N;
      const f = rd - i0;
      const delayed = this.#harpBuf[i0] * (1 - f) + this.#harpBuf[i1] * f;
      const filtered = 0.5 * (delayed + this.#harpLp1);
      this.#harpLp1 = delayed;
      // Short-pluck variant when caller passes small decay.
      const stretch = (this.#decay > 0 && this.#decay < 0.2) ? 0.990 : 0.9985;
      const decayed = filtered * stretch;
      this.#harpBuf[this.#harpW] = decayed;
      this.#harpW = (this.#harpW + 1) % N;
      value = 2.5 * decayed;
    } else if (this.type === "whistle") {
      // 🎶 Digital waveguide flute (Cook/STK). See C generate_whistle_sample
      // for full algorithm notes — same code translated to JS.
      const BORE_N = 2048, JET_N = 512;
      const env = 1; // attack/release handled by outer envelope below
      const breathTarget = 0.18 + 0.82 * Math.sqrt(env);
      const breathSlew = env > this.#whistleBreath ? 0.012 : 0.003;
      this.#whistleBreath += (breathTarget - this.#whistleBreath) * breathSlew;
      this.#whistleVibratoPhase += 5 / sampleRate;
      if (this.#whistleVibratoPhase >= 1) this.#whistleVibratoPhase -= 1;
      const vibrato = sin(2 * PI * this.#whistleVibratoPhase) * 0.03;
      // xorshift32 for deterministic noise (matches C engine).
      let s = this.#whistleNoiseSeed;
      s ^= s << 13; s >>>= 0;
      s ^= s >>> 17;
      s ^= s << 5;  s >>>= 0;
      this.#whistleNoiseSeed = s;
      const white = (s / 0xFFFFFFFF) * 2 - 1;
      const breath = this.#whistleBreath * (1 + 0.08 * white + vibrato);
      const pitch = clamp(freq, 30, sampleRate * 0.20);
      let boreDelay = sampleRate / pitch;
      let jetDelay = boreDelay * 0.32;
      if (boreDelay > BORE_N - 2) boreDelay = BORE_N - 2;
      if (jetDelay > JET_N - 2)  jetDelay  = JET_N - 2;
      // Frac read — bore.
      let rd = this.#whistleBoreW - boreDelay;
      while (rd < 0) rd += BORE_N;
      let i0 = floor(rd) | 0;
      let i1 = (i0 + 1) % BORE_N;
      let frac = rd - i0;
      const boreOut = this.#whistleBoreBuf[i0] * (1 - frac) + this.#whistleBoreBuf[i1] * frac;
      this.#whistleLp1 = 0.35 * (-boreOut) + 0.65 * this.#whistleLp1;
      const temp = this.#whistleLp1;
      let pd = breath - 0.5 * temp;
      this.#whistleJetBuf[this.#whistleJetW] = pd;
      this.#whistleJetW = (this.#whistleJetW + 1) % JET_N;
      // Frac read — jet.
      rd = this.#whistleJetW - jetDelay;
      while (rd < 0) rd += JET_N;
      i0 = floor(rd) | 0;
      i1 = (i0 + 1) % JET_N;
      frac = rd - i0;
      pd = this.#whistleJetBuf[i0] * (1 - frac) + this.#whistleJetBuf[i1] * frac;
      // Cubic nonlinearity — limit-cycle generator.
      pd = pd * (pd * pd - 1);
      if (pd > 1) pd = 1;
      if (pd < -1) pd = -1;
      // 1-pole DC blocker.
      const y = pd - this.#whistleHpX1 + 0.995 * this.#whistleHpY1;
      this.#whistleHpX1 = pd;
      this.#whistleHpY1 = y;
      const intoBore = y + 0.5 * temp;
      this.#whistleBoreBuf[this.#whistleBoreW] = intoBore;
      this.#whistleBoreW = (this.#whistleBoreW + 1) % BORE_N;
      value = 0.3 * intoBore;
    } else if (this.type === "sample") {
      const bufferData = this.#sampleData.channels[0];

      // Log at various points to verify playback continues
      // const idx = floor(this.#sampleIndex);
      // if (idx === 0 || idx === 100 || idx === 1000 || idx === 5000 || idx === 10000) {
      //   console.log("🎤 SYNTH sample at index:", idx, "value:", bufferData?.[idx], "speed:", this.#sampleSpeed, "vol:", this.volume);
      // }

      // Handle preserveDuration mode: GRANULAR pitch shift without time stretch
      if (this.#preserveDuration) {
        this.#playedSamples++;
        
        // Granular synthesis: mix overlapping grains
        const grainSpacing = this.#grainSize / this.#grainOverlap;
        
        // Spawn new grain when needed (but only if source material remains)
        this.#grainPhase++;
        if (this.#grainPhase >= grainSpacing && this.#sourcePosition < this.#sampleEndIndex) {
          this.#grainPhase = 0;
          
          // Create a new grain starting at current source position
          this.#grains.push({
            sourceStart: this.#sourcePosition,
            position: 0, // Position within grain (0 to grainSize)
          });
        }
        
        // Advance source position at normal speed (1:1 with output)
        this.#sourcePosition += 1;
        
        // Mix all active grains
        value = 0;
        const activeGrains = [];
        
        for (const grain of this.#grains) {
          // Calculate envelope (Hann window for smooth crossfade)
          const grainProgress = grain.position / this.#grainSize;
          const envelope = 0.5 * (1 - Math.cos(2 * Math.PI * grainProgress));
          
          // Read from source at pitched rate
          const sourceIdx = grain.sourceStart + (grain.position * this.#sampleSpeed);
          
          // Skip this grain's contribution if it's past the end of source material
          // (no looping - just let grains fade out naturally)
          if (sourceIdx >= this.#sampleEndIndex || sourceIdx < this.#sampleStartIndex) {
            // Grain has exhausted source material - let it die naturally
            grain.position++;
            if (grain.position < this.#grainSize) {
              activeGrains.push(grain);
            }
            continue;
          }
          
          // Linear interpolation for smoother pitch shifting
          const idx0 = floor(sourceIdx);
          const idx1 = idx0 + 1 < this.#sampleEndIndex ? idx0 + 1 : idx0;
          const frac = sourceIdx - idx0;
          const sample0 = bufferData[idx0] || 0;
          const sample1 = bufferData[idx1] || 0;
          const interpolatedSample = sample0 + frac * (sample1 - sample0);
          
          value += interpolatedSample * envelope;
          
          // Advance grain position
          grain.position++;
          
          // Keep grain if still active
          if (grain.position < this.#grainSize) {
            activeGrains.push(grain);
          }
        }
        
        this.#grains = activeGrains;
        
        // Normalize by overlap count to prevent clipping
        value /= (this.#grainOverlap / 2);
        
        // Stop when all grains are done OR we've reached target duration
        if (this.#grains.length === 0 && this.#sourcePosition >= this.#sampleEndIndex) {
          this.playing = false;
          return 0;
        }
      }
      // Time stretch + pitch shift mode: stretch sample to fill target duration, then pitch shift
      else if (this.#timeStretchEnabled) {
        this.#playedSamples++;
        
        // Granular synthesis: mix overlapping grains
        const grainSpacing = this.#grainSize / this.#grainOverlap;
        
        // Spawn new grain when needed
        this.#grainPhase++;
        if (this.#grainPhase >= grainSpacing && this.#sourcePosition < this.#sampleEndIndex) {
          this.#grainPhase = 0;
          
          // Create a new grain starting at current source position
          this.#grains.push({
            sourceStart: this.#sourcePosition,
            position: 0, // Position within grain (0 to grainSize)
          });
        }
        
        // Advance source position based on time stretch ratio
        // timeStretchRatio < 1 = stretching (slower source read = longer output)
        // timeStretchRatio > 1 = compressing (faster source read = shorter output)
        this.#sourcePosition += this.#timeStretchRatio;
        
        // Mix all active grains
        value = 0;
        const activeGrains = [];
        
        for (const grain of this.#grains) {
          // Calculate envelope (Hann window for smooth crossfade)
          const grainProgress = grain.position / this.#grainSize;
          const envelope = 0.5 * (1 - Math.cos(2 * Math.PI * grainProgress));
          
          // Read from source at pitch-shifted rate within the grain
          // Time stretch is handled by #sourcePosition advancement
          // Pitch shift is handled by reading grains at a different rate
          const sourceIdx = grain.sourceStart + (grain.position * this.#sampleSpeed);
          
          // Skip this grain's contribution if it's past the end of source material
          if (sourceIdx >= this.#sampleEndIndex || sourceIdx < this.#sampleStartIndex) {
            grain.position++;
            if (grain.position < this.#grainSize) {
              activeGrains.push(grain);
            }
            continue;
          }
          
          // Linear interpolation for smoother playback
          const idx0 = floor(sourceIdx);
          const idx1 = idx0 + 1 < this.#sampleEndIndex ? idx0 + 1 : idx0;
          const frac = sourceIdx - idx0;
          const sample0 = bufferData[idx0] || 0;
          const sample1 = bufferData[idx1] || 0;
          const interpolatedSample = sample0 + frac * (sample1 - sample0);
          
          value += interpolatedSample * envelope;
          
          // Advance grain position
          grain.position++;
          
          // Keep grain if still active
          if (grain.position < this.#grainSize) {
            activeGrains.push(grain);
          }
        }
        
        this.#grains = activeGrains;
        
        // Normalize by overlap count to prevent clipping
        value /= (this.#grainOverlap / 2);
        
        // Stop when we've output enough samples OR source is exhausted and grains done
        if (this.#playedSamples >= this.#outputSamplesNeeded || 
            (this.#grains.length === 0 && this.#sourcePosition >= this.#sampleEndIndex)) {
          this.playing = false;
          return 0;
        }
      }
      // Normal (non-granular) sample playback
      else {
        if (this.#pitch !== 1 && this.#sampleLoop) {
          // 🎼 Independent pitch: two read taps sweep a short window
          // BEHIND the main head (which still advances at #sampleSpeed,
          // so tempo/grid stay untouched) and crossfade sinusoidally —
          // the classic delay-line pitch shifter. Clean for ±few
          // semitones, exactly what the p-/p+ pads reach.
          const W = 2048;
          this.#pitchPhase =
            (this.#pitchPhase + (this.#pitch - 1) * this.#sampleSpeed + W) % W;
          const offA = this.#pitchPhase;
          const offB = (offA + W / 2) % W;
          const range = this.#sampleEndIndex - this.#sampleStartIndex;
          const rd = (off) => {
            let ix = this.#sampleIndex - off;
            while (ix < this.#sampleStartIndex) ix += range;
            while (ix >= this.#sampleEndIndex) ix -= range;
            return bufferData[floor(ix)];
          };
          const gA = Math.sin((Math.PI * offA) / W);
          const gB = Math.sin((Math.PI * offB) / W);
          value = rd(offA) * gA + rd(offB) * gB;
        } else {
          value = bufferData[floor(this.#sampleIndex)];
        }
        // 🍿 Declick: after a samplePosition relocation (chop loops, beat
        // jumps), ramp back in over ~4ms so the jump never pops.
        if (this.#declick > 0) {
          value *= 1 - this.#declick / this.#declickTotal;
          this.#declick -= 1;
        }
        this.#sampleIndex += this.#sampleSpeed;
        
        // Handle looping
        if (this.#sampleLoop) {
          if (this.#sampleIndex > this.#sampleEndIndex) {
            // Calculate the range length for proper modulo operation
            const rangeLength = this.#sampleEndIndex - this.#sampleStartIndex;
            const overshoot = this.#sampleIndex - this.#sampleEndIndex;
            this.#sampleIndex = this.#sampleStartIndex + (overshoot % rangeLength); // Loop forwards. ➡️
          } else if (this.#sampleIndex < this.#sampleStartIndex) {
            const rangeLength = this.#sampleEndIndex - this.#sampleStartIndex;
            const undershoot = this.#sampleStartIndex - this.#sampleIndex;
            this.#sampleIndex = this.#sampleEndIndex - (undershoot % rangeLength); // Loop backwards. ⬅️
          }
        } else {
          // Normal mode: stop when sample ends
          if (
            this.#sampleIndex >= this.#sampleEndIndex ||
            this.#sampleIndex < 0
          ) {
            this.playing = false;
            return 0;
          }
        }
      }
    } else if (this.type === "custom") {
      // 🎨 Custom Waveform Generation
      // Ensure buffer has data available
      if (this.#customBuffer.length === 0) {
        this._fillCustomBuffer();
      }
      
      // Get the next value from our buffer
      if (this.#customBuffer.length > 0) {
        value = this.#customBuffer.shift();
      } else {
        value = 0; // Silence if no data available
      }
      
      // Refill buffer if it's running low
      if (this.#customBuffer.length < this.#customBufferSize / 4) {
        this._fillCustomBuffer();
      }
    }

    // 🐦 Timbre expression — breath into the source, then the throat, then
    // the mouth, then the level. Samples keep their own shape.
    if (this.type !== "sample") {
      if (this.#noise > 0) {
        value = value * (1 - this.#noise * 0.5) + (random() * 2 - 1) * this.#noise;
      }
      if (this.#formant) value = this.#formant.process(value);
      if (this.#lowpass) {
        if (this.#lpSweepLeft > 0) {
          this.#lpSweepLeft -= 1;
          this.#lowpass.cutoff = this.#lpSweepLeft === 0
            ? this.#lpSweepTarget : this.#lowpass.cutoff * this.#lpSweepRatio;
          this._lowpassCoefficients();
        }
        const y = this.#lpB0 * (value + this.#lpX2) + this.#lpB1 * this.#lpX1
          - this.#lpA1 * this.#lpY1 - this.#lpA2 * this.#lpY2;
        this.#lpX2 = this.#lpX1; this.#lpX1 = value;
        this.#lpY2 = this.#lpY1; this.#lpY1 = y;
        value = y;
      }
      if (this.#tremolo) {
        const t = this.#tremolo;
        this.#tremoloPhase += (2 * PI * t.rate) / sampleRate;
        if (this.#tremoloPhase > 2 * PI) this.#tremoloPhase -= 2 * PI;
        value *= 1 - t.depth * (0.5 + 0.5 * sin(this.#tremoloPhase));
      }
    }

    // 🦈 Attack & Decay Computation 📉
    // Only use attack or decay envelopes on self-terminating sounds.
    if (this.#duration < Infinity) {
      if (this.type === "noise-white") {
        // Much sharper attack/decay for noise-white to enable shorter, snappier sounds
        const sharpAttack = min(1, this.#progress / (this.#attack * 0.1)); // 10x faster attack
        if (sharpAttack) value *= sharpAttack;

        // Much sharper decay with exponential curve for crisp cutoff
        const decayProgress = (this.#progress - this.#decayStart) / (this.#decay * 0.05); // 20x faster decay
        const sharpDecay = min(1, 1 - Math.pow(decayProgress, 3)); // Cubic decay for sharp cutoff
        value *= max(0, sharpDecay);
      } else {
        // Standard attack/decay for other waveforms
        // Attack Envelope (0-1)
        const attack = min(1, this.#progress / this.#attack);
        if (attack) value *= attack;

        // Decay Envelope (0-1)
        const decay = min(
          1,
          1 - (this.#progress - this.#decayStart) / this.#decay,
        );
        // console.log(this.#progress, attack, decay);

        value *= decay;
      }
    } else {
      // TODO:
      // Attack will be in number of sampleFrames here... please calculate.
      if (this.#attack > 0) {
        // Calculate attack envelope using the number of frames passed
        const attack = min(1, this.#progress / this.#attack);
        value *= attack;
      }
    }

    // 🎠 Track the overall progress of the sound.

    // (Some sounds will have an Infinity duration and are killable)
    // (Samples wouldn't use this system.)

    this.#progress += 1;
    if (this.#progress >= this.#duration) {
      this.playing = false;
      // console.log("🛑 Synth finished.");
      return 0;
    }

    let out = value * this.volume;

    // 🔊 Debug: Log sample output periodically
    // if (this.type === "sample" && this.#progress % 4800 === 0) {
    //   console.log("🔊 SYNTH sample final output:", { progress: this.#progress, value, volume: this.volume, out, playing: this.playing, fading: this.fading });
    // }

    // ➰💀 "Fade 2 kill." - 25.02.15.00.14
    // The release rides a gain that only ever walks downhill. Tracking the
    // gain itself (rather than a position along a ramp) is what lets a second
    // kill() land safely — see kill().
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

  update({ tone, volume, shift, sampleSpeed, samplePosition, sampleData, pitch, duration = 0.1, ...expression }) {
    // 🎼 Live independent pitch factor (tempo-preserving shifter).
    if (typeof pitch === "number" && pitch > 0) {
      this.#pitch = pitch;
      if (pitch === 1) this.#pitchPhase = 0;
    }
    // 🐦 slide / vibrato / tremolo / drift / noise / lowpass / formant —
    // each replaces its setting; slide and a sweep-less lowpass glide from
    // where the voice is now over `duration`.
    if (this.type !== "sample") this._express(expression, duration);
    if (typeof tone === "number" && tone > 0) {
      this.#futureFrequency = tone;
      this.#frequencyUpdatesTotal = duration * sampleRate;
      this.#frequencyUpdatesLeft = this.#frequencyUpdatesTotal;
      this.#frequencyUpdateSlice =
        (this.#futureFrequency - this.#frequency) / this.#frequencyUpdatesTotal;
    }
    if (typeof volume === "number") {
      this.#futureVolume = volume;
      this.#volumeUpdatesTotal = duration * sampleRate;
      this.#volumeUpdatesLeft = this.#volumeUpdatesTotal;
      this.#volumeUpdateSlice =
        (this.#futureVolume - this.volume) / this.#volumeUpdatesTotal;
    }

    // Shift sample speed incrementally.
    if (typeof shift === "number") {
      const oldSpeed = this.#sampleSpeed;
      this.#sampleSpeed += shift;
      // console.log(`🔊 SYNTH shift: old=${oldSpeed.toFixed(4)}, shift=${shift.toFixed(4)}, new=${this.#sampleSpeed.toFixed(4)}, index=${this.#sampleIndex}, start=${this.#sampleStartIndex}, end=${this.#sampleEndIndex}`);
    }

    if (typeof sampleSpeed === "number") {
      // console.log(`🔊 SYNTH sampleSpeed: setting to ${sampleSpeed}`);
      this.#sampleSpeed = sampleSpeed;
    }

    if (typeof samplePosition === "number" && this.#sampleData) {
      const len = this.#sampleData.channels?.[0]?.length ?? this.#sampleData.length;
      this.#sampleIndex = floor(samplePosition * len);
      // 🍿 Arm the declick ramp — a relocated read head lands mid-wave;
      // ~4ms of attack keeps it from popping.
      this.#declickTotal = Math.max(1, Math.floor(sampleRate * 0.004));
      this.#declick = this.#declickTotal;
    }

    // 🔄 Live buffer swap - update sample data while maintaining playback position
    if (sampleData && this.type === "sample") {
      const oldLength = this.#sampleData?.channels?.[0]?.length ?? this.#sampleData?.length ?? 1;
      const newLength = sampleData.channels?.[0]?.length ?? sampleData.length ?? 1;
      const progress = this.#sampleIndex / oldLength; // 0 to 1 progress
      this.#sampleData = sampleData;
      // Recalculate indices for new buffer length
      this.#sampleEndIndex = clamp(
        Math.floor((this.#sampleEndIndex / oldLength) * newLength),
        0,
        newLength - 1,
      );
      this.#sampleStartIndex = clamp(
        Math.floor((this.#sampleStartIndex / oldLength) * newLength),
        0,
        newLength - 1,
      );
      // Maintain relative playback position
      this.#sampleIndex = clamp(
        Math.floor(progress * newLength),
        this.#sampleStartIndex,
        this.#sampleEndIndex,
      );
    }

    // console.log("🟠 Update properties:", arguments);
  }

  // Stereo
  pan(channel, frame) {
    if (channel === 0) {
      if (this.#pan > 0) frame *= 1 - this.#pan; // Left Channel
    } else if (channel === 1) {
      if (this.#pan < 0) frame *= 1 - abs(this.#pan); // Right Channel
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
    const seconds = max(fade || 0, Synth.MIN_RELEASE);
    const step = this.fadeGain / (seconds * sampleRate);
    this.fadeStep = this.fading ? max(this.fadeStep, step) : step;
    this.fading = true;
    this.fadeProgress = 0;
    this.fadeDuration = seconds * sampleRate;
  }

  // Return an integer from 0->1 representing the progress of this sound so far.
  progress() {
    if (this.type === "sample") {
      // return this.#progress;
      // this.#sampleIndex // current index
      // this.#sampleData.length // total length
      // this.#sampleStartIndex // start
      // this.#sampleEndIndex // end

      // return this.#sampleIndex / this.#sampleData.length;
      return (
        (this.#sampleIndex - this.#sampleStartIndex) /
        (this.#sampleEndIndex - this.#sampleStartIndex)
      );
    } else {
      return this.#progress / this.#duration;
    }
  }

  // Fill the custom buffer with generated waveform data
  // Note: Using underscore convention instead of # private method for AudioWorklet compatibility
  _fillCustomBuffer() {
    if (!this.#customGenerator) return;
    
    try {
      // Generate new samples for the buffer
      const bufferSize = this.#customBufferSize - this.#customBuffer.length;
      const newSamples = this.#customGenerator({
        frequency: this.#liveFrequency, // follows slide/vibrato/drift, block by block

        sampleRate: sampleRate,
        progress: this.#progress,
        time: this.#progress / sampleRate,
        samplesNeeded: bufferSize
      });
      
      if (Array.isArray(newSamples)) {
        // Clamp values to [-1, 1] range
        const clampedSamples = newSamples.map(sample => {
          if (sample > 1) return 1;
          if (sample < -1) return -1;
          return sample;
        });
        this.#customBuffer.push(...clampedSamples);
      }
    } catch (error) {
      console.warn('🎨 Custom waveform generator error:', error);
      // Fill with silence on error
      const silenceSamples = new Array(this.#customBufferSize).fill(0);
      this.#customBuffer.push(...silenceSamples);
    }
  }
  // Update custom generator function
  setCustomGenerator(generator) {
    if (this.type === "custom") {
      if (typeof generator === "string") {
        try {
          // Convert string back to function
          this.#customGenerator = eval(`(${generator})`);
        } catch (error) {
          console.error("🎨 Failed to parse custom generator in setCustomGenerator:", error);
          return;
        }
      } else if (typeof generator === 'function') {
        this.#customGenerator = generator;
      } else {
        console.error("🎨 Invalid generator type in setCustomGenerator:", typeof generator);
        return;
      }
      
      this.#customBuffer.length = 0; // Clear buffer to force regeneration
    }
  }
}
