import { noteOrFreq } from "./note.mjs";

export default class Square {
  // Generic for all instruments.
  playing = true;

  #duration = 0;
  #attack = 0;
  #decay = 0;
  #decayStart;

  #volume = 1; // 0 to 1
  #pan = 0; // -1 to 1

  #progress = 0;

  // Specific to Square.
  #wavelength = 1024; // Calculated from the frequency.
  #up = false;
  #step = 0;

  constructor(tone, duration, attack, decay, volume, pan) {
    let frequency = noteOrFreq(tone);

    // Tuning adjustment.
    // Measured on MacBook Air with iOS App. - Aug 16, 5:56pm.
    const tuning = 50;
    frequency += tuning;

    // Frequency in samples, divided by 2 yields the period length.
    this.#wavelength = sampleRate / frequency / 2;

    this.#duration = duration;
    this.#attack = attack;
    this.#decay = decay;
    this.#pan = pan;
    this.#volume = volume;

    this.#decayStart = this.#duration - this.#decay;
    /*
    console.log("Volume: ", volume);
    console.log(
      "Duration:",
      this.#duration,
      "Attack:",
      this.#attack,
      "Decay:",
      this.#decay
    );
     */
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
        frame *= 1 - Math.abs(this.#pan);
      }
    }
    return frame;
  }

  next() {
    // Channel is either 0 or 1
    // Generic for all instruments.

    // Unmodified Value (either 1 or -1)
    let value = this.#up ? 1 : -1;

    // Attack Envelope (0-1)
    const attack = Math.min(1, this.#progress / this.#attack);
    value *= attack;

    // Decay Envelope (0-1)
    const decay = Math.min(
      1,
      1 - (this.#progress - this.#decayStart) / this.#decay
    );
    value *= decay;

    //if (this.#progress % 32 === 0) {
    //console.log(attack);
    // TODO: How can I plot this sample graphically?
    //}

    // Generate square wave as we step through the wavelength.
    if (this.#step < this.#wavelength) {
      this.#step += 1;
    } else {
      this.#up = !this.#up;
      this.#step = 0;
    }

    // Track the overall progress of the sound.
    this.#progress += 1;
    if (this.#progress >= this.#duration) {
      this.playing = false;
      return 0;
    }

    return value * this.#volume;
  }
}
