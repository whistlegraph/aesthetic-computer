import { within, lerp, clamp } from "../num.mjs";
const { abs, floor, sin, PI, min, max, random } = Math;

export default class Synth {
  // Generic for all instruments.
  playing = true;
  id; // Unique for every playing instrument.

  fading = false; // If we are fading and then stopping playback.
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

  // Custom waveform generation
  #customGenerator; // Function that generates waveform data
  #customBuffer = []; // Buffer for streaming waveform data
  #customBufferSize = 1024; // Size of the streaming buffer

  constructor({ type, id, options, duration, attack, decay, volume, pan }) {
    // console.log("New Synth:", arguments);
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

    // this.#frequency = tone || 1; // Frequency in samples.
    // ❤️‍🔥 TODO: Calculate slide based on frequency...
    this.#wavelength = sampleRate / this.#frequency;
    this.#futureFrequency = this.#frequency;

    this.#attack = attack;

    this.#duration = this.type === "sample" ? Infinity : duration;
    // if (this.#type === "sample") console.log("⏱️ Sample duration:", this.#duration);

    this.#decay = decay;
    this.#decayStart = this.#duration - this.#decay;

    this.#pan = pan;

    this.volume = volume;
    this.#futureVolume = this.volume;

    // console.log("〰️", this);
  }

  next(channelIndex) {
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

    // 🎸🎙️ Waveform Sources 🎹
    let value;
    if (this.type === "square") {
      // 🟥 Square Wave
      this.#step += 1;
      if (this.#step >= this.#wavelength) {
        this.#up = !this.#up;
        this.#step -= this.#wavelength;
      }
      value = this.#up ? 1 : -1;
    } else if (this.type === "sine") {
      // 🟣 Sine Wave
      // Generate using a 'Phase Increment' method.
      const increment = (2 * PI * this.#frequency) / sampleRate;
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
      if (this.#frequency && this.#frequency > 0) {
        // Calculate filter coefficients based on frequency
        // Normalize frequency to 0-1 range (0 = DC, 1 = Nyquist frequency)
        const normalizedFreq = (this.#frequency * 2) / sampleRate;
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
        value = bufferData[floor(this.#sampleIndex)];
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
    if (this.fading) {
      if (this.fadeProgress < this.fadeDuration) {
        this.fadeProgress += 1;
        // Apply the fade envelope to the output.
        out *= 1 - this.fadeProgress / this.fadeDuration;
      } else {
        this.fading = false;
        this.playing = false;
        return 0;
      }
    }

    return out;
  }

  update({ tone, volume, shift, sampleSpeed, samplePosition, sampleData, duration = 0.1 }) {
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
  kill(fade = 0.025) {
    if (!fade) {
      this.playing = false;
    } else {
      // Fade over 'fade' seconds, before stopping playback.
      this.fading = true;
      this.fadeProgress = 0;
      this.fadeDuration = fade * sampleRate; // Convert seconds to samples.
    }
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
        frequency: this.#frequency,
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
