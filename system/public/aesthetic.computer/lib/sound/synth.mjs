import { within, lerp, clamp } from "../num.mjs";
const { abs, floor, sin, PI, min, max, random } = Math;

export default class Synth {
  // Generic for all instruments.
  playing = true;
  id; // Unique for every playing instrument.

  fading = false; // If we are fading and then stopping playback.
  fadeProgress;
  fadeDuration;

  type; // square, sine, triangle, sawtooth, sample, noise-white

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
        this.#sampleSpeed < 0 ? this.#sampleEndIndex : this.#sampleStartIndex;
    } else if (type === "noise-white") {
      this.#frequency = null; //undefined;
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
      // 🌊  White Noise
      value = random() * 2 - 1;
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
    }

    // 🦈 Attack & Decay Computation 📉
    // Only use attack or decay envelopes on self-terminating sounds.
    if (this.#duration < Infinity) {
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
}
