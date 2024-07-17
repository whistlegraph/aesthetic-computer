import { noteOrFreq } from "./note.mjs";
import { within, lerp } from "../num.mjs";
const { abs, floor, sin, PI } = Math;

export default class Sound {
  // Generic for all instruments.
  playing = true;

  #fading = false; // If we are fading and then stopping playback.
  #fadeProgress;
  #fadeDuration;

  #duration = 0;
  #attack = 0;
  #decay = 0;
  #decayStart;

  #volume = 1; // 0 to 1
  #futureVolume = 1;
  #pan = 0; // -1 to 1

  #progress = 0;

  #wavelength; // Calculated from the frequency.
  #futureWavelength;

  #type; // `square` or `sine`

  #up = false; // Specific to Square.
  #step = 0;

  constructor({ type, tone, duration, attack, decay, volume, pan }) {
    this.#type = type;

    const frequency = noteOrFreq(tone || 1); // Frequency in samples, divided by 2 yields the period length.
    this.#wavelength = sampleRate / frequency;// / 2;
    this.#futureWavelength = this.#wavelength;

    this.#duration = duration;
    this.#attack = attack;
    this.#decay = decay;
    this.#pan = pan;
    this.#volume = volume;
    this.#futureVolume = this.#volume;

    this.#decayStart = this.#duration - this.#decay;
  }

  // Update certain properties whilst playing.
  // update({ tone, volume }) {
  //   console.log("Update received:", tone);
  //   if (tone) {
  //     // Set futureWavelength for ramping up to in `next`.
  //     // TODO: Also add a timing delay here?
  //     this.#futureWavelength = sampleRate / noteOrFreq(tone || 1);
  //   }
  //   if (typeof volume === "number") this.#futureVolume = volume;
  // }

  update({ tone, volume }) {
    if (typeof tone === "number" && tone > 0) {
      // Set futureWavelength for ramping up to in `next`.
      this.#futureWavelength = sampleRate / noteOrFreq(tone) / 2;
    }
    if (typeof volume === "number") this.#futureVolume = volume;
  }

  // Stereo
  pan(channel, frame) {
    if (channel === 0) {
      // Left Channel
      if (this.#pan > 0) {
        frame *= 1 - this.#pan;
      }
    } else if (channel === 1) {
      // Right Channel
      if (this.#pan < 0) {
        frame *= 1 - abs(this.#pan);
      }
    }
    return frame;
  }

  next() {
    // Channel is either 0 or 1
    // Generic for all instruments.
    let value;

    // Lerp wavelength & volume towards their future goals.
    if (!within(0.001, this.#wavelength, this.#futureWavelength)) {
      this.#wavelength = lerp(this.#wavelength, this.#futureWavelength, 0.001);
    }

    if (!within(0.001, this.#volume, this.#futureVolume)) {
      this.#volume = lerp(this.#volume, this.#futureVolume, 0.025);
    }

    // Generate square wave as we step through the wavelength.

    // Generate different waveforms as we step through the wavelength.
    if (this.#type === "square") {
      // Square Wave
      this.#step += 1;
      if (this.#step >= this.#wavelength) {
        this.#up = !this.#up;
        this.#step -= this.#wavelength;
      }
      value = this.#up ? 1 : -1;
    } else if (this.#type === "sine") {
      // Sine Wave
      const angle = (Math.PI * this.#step) / (this.#wavelength / 2);
      value = Math.sin(angle);
      this.#step += 1;

      if (this.#step >= 2 * this.#wavelength) {
        this.#step = 0;
      }
    } else if (this.#type === "triangle") {
      // Triangle Wave
      const stepSize = 4 / this.#wavelength;
      value = 1 - Math.abs((this.#step % this.#wavelength) * stepSize - 2);
      this.#step += 1;

      if (this.#step >= this.#wavelength) {
        this.#step = 0;
      }
    } else if (this.#type === "sawtooth") {
      // Sawtooth Wave
      value = 2 * (this.#step / this.#wavelength) - 1;
      this.#step += 1;

      if (this.#step >= this.#wavelength) {
        this.#step = 0;
      }
    } else if (this.#type === "noise-white") {
      // TODO: Also add pink and brownian noise.
      // White Noise
      value = Math.random() * 2 - 1;
    }

    // Only use attack or decay envelopes on self-terminating sounds.
    if (this.#duration < Infinity) {
      // Attack Envelope (0-1)
      const attack = Math.min(1, this.#progress / this.#attack);
      if (attack) value *= attack;

      // Decay Envelope (0-1)
      const decay = Math.min(
        1,
        1 - (this.#progress - this.#decayStart) / this.#decay,
      );
      value *= decay;
    }

    // Track the overall progress of the sound.
    // (Some sounds will have an Infinity duration and are killable)
    this.#progress += 1;
    if (this.#progress >= this.#duration) {
      this.playing = false;
      return 0;
    }

    let out = value * this.#volume;

    // "Fade out to kill" - 24.07.01.20.36
    if (this.#fading) {
      if (this.#fadeProgress < this.#fadeDuration) {
        this.#fadeProgress += 1;
        // Apply the fade envelope to the output.
        out *= 1 - this.#fadeProgress / this.#fadeDuration;
      } else {
        this.#fading = false;
        this.playing = false;
        return 0;
      }
    }

    return out;
  }

  // Use a 25ms fade by default.
  kill(fade = 0.025) {
    if (!fade) {
      this.playing = false;
    } else {
      // Fade over 'fade' seconds, before stopping playback.
      this.#fading = true;
      this.#fadeProgress = 0;
      this.#fadeDuration = fade * sampleRate; // Convert seconds to samples.
    }
  }
}
