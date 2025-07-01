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

      this.#sampleSpeed = options.speed || 1;
      this.#sampleLoop = options.loop || false;

      // console.log("Speed:", this.#sampleSpeed);

      // if (this.#sampleSpeed < 0)
      // this.#sampleIndex = this.#sampleData.length - 1; // Otherwise 0.
      // this.#sampleStartIndex = options.startSample;
      // this.#sampleEndIndex = options.endSample;
      // Check the bounds of the sample data.
      this.#sampleStartIndex = clamp(
        options.startSample,
        0,
        this.#sampleData.length - 1,
      );
      this.#sampleEndIndex = clamp(
        options.endSample,
        0,
        this.#sampleData.length - 1,
      );

      this.#sampleIndex =
        this.#sampleSpeed < 0 ? this.#sampleEndIndex : this.#sampleStartIndex;    } else if (type === "custom") {
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
      this.#fillCustomBuffer();
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
      // 📐 Triangle Wave
      const stepSize = 4 / this.#wavelength;
      value = 1 - abs((this.#step % this.#wavelength) * stepSize - 2);
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

      // const index = floor(this.#sampleIndex);
      // let nextIndex;

      // if (this.#sampleSpeed > 0) {
      //   nextIndex = min(index + 1, bufferData.length - 1);
      // } else {
      //   nextIndex = max(index - 1, 0);
      // }

      // const t = this.#sampleIndex - index;
      // value = (1 - t) * bufferData[index] + t * bufferData[nextIndex];

      value = bufferData[floor(this.#sampleIndex)];

      this.#sampleIndex += this.#sampleSpeed;
      // Handle looping and stopping
      if (this.#sampleLoop) {
        if (this.#sampleIndex > this.#sampleEndIndex) {
          this.#sampleIndex =
            this.#sampleStartIndex + (this.#sampleIndex % this.#sampleEndIndex); // Loop forwards. ➡️
        } else if (this.#sampleIndex < this.#sampleStartIndex) {
          this.#sampleIndex =
            this.#sampleEndIndex - (this.#sampleStartIndex - this.#sampleIndex); // Loop backwards. ⬅️
        }
      } else {
        // console.log(this.#sampleIndex, this.#sampleEndIndex);
        if (
          this.#sampleIndex >= this.#sampleEndIndex ||
          this.#sampleIndex < 0
        ) {
          this.playing = false;
          // console.log("🛑 Sample finished.", this.#sampleIndex, this.#sampleEndIndex);
          return 0;
        }
      }
    } else if (this.type === "custom") {
      // 🎨 Custom Waveform Generation
      // Ensure buffer has data available
      if (this.#customBuffer.length === 0) {
        this.#fillCustomBuffer();
      }
      
      // Get the next value from our buffer
      if (this.#customBuffer.length > 0) {
        value = this.#customBuffer.shift();
      } else {
        value = 0; // Silence if no data available
      }
      
      // Refill buffer if it's running low
      if (this.#customBuffer.length < this.#customBufferSize / 4) {
        this.#fillCustomBuffer();
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

  update({ tone, volume, shift, sampleSpeed, samplePosition, duration = 0.1 }) {
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
      this.#sampleSpeed += shift;
      // console.log("New sample speed:", this.#sampleSpeed, this.#sampleIndex, this.#sampleEndIndex);
    }

    if (typeof sampleSpeed === "number") {
      this.#sampleSpeed = sampleSpeed;
    }

    if (typeof samplePosition === "number" && this.#sampleData) {
      this.#sampleIndex = floor(samplePosition * this.#sampleData.length);
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
  #fillCustomBuffer() {
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
