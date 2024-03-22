// Bubble 2022.02.04.17.48 [Sage: @mxsage + Jeffrey]
// A translation of C code that physically models a simple bubble sound.
// See also: https://github.com/SkAT-VG/SDT/blob/master/src/SDT/SDTLiquids.h

export default class Bubble {
  // Generic for all instruments.
  playing = true;

  #volume = 1; // 0 to 1
  #pan = 0; // -1 to 1

  #radius;
  #rise;
  #amp;
  #decay;
  #gain;
  #phaseStep;
  #phaseRise;
  #phase;
  #lastOut;
  #depth = 1;
  #timestep;

  #out = 0;
  #maxOut = 1;

  #progress = 0; // TODO: This becomes binary?

  #QUIET = 0.000001;

  constructor(radius, rise, volume, pan) {
    this.start(radius, rise, volume, pan);
  }

  start(
    radius = this.#radius,
    rise = this.#rise,
    volume = this.#volume,
    pan = this.#pan
  ) {
    this.#pan = pan;
    this.#volume = volume;
    this.#radius = radius * 0.001;
    this.#rise = rise;
    this.#timestep = 1 / sampleRate;
    this.#lastOut = this.#out;
    const pRadius = this.#radius * Math.sqrt(this.#radius);
    this.#amp = 17.2133 * pRadius * this.#depth;
    this.#decay = 0.13 / this.#radius + 0.0072 * pRadius;
    this.#gain = Math.exp(-this.#decay * this.#timestep);
    this.#phaseStep = (3.0 / this.#radius) * this.#timestep;
    this.#phaseRise =
      this.#phaseStep * this.#decay * this.#rise * this.#timestep;
    this.#phase = 0;
    //this.#maxOut = 1;
  }

  next() {
    // get sound value
    if (this.#amp < this.#QUIET && this.#phase > 1.0) {
      //console.log("DONE!", this.#progress);
      this.playing = false;
      return 0;
    }

    let alpha = this.#phase < 1.0 ? this.#phase : 1.0;
    this.#out =
      (1.0 - alpha) * this.#lastOut +
      alpha * this.#amp * Math.sin(Math.PI * 2 * this.#phase);
    this.#phase += this.#phaseStep;
    this.#phaseStep += this.#phaseRise;
    this.#amp *= this.#gain;

    this.#progress += 1;

    // *** Normalization to a max of 1 / -1
    // TODO: Keep track of a normalization curve. 2022.02.04.16.42
    const out = this.#out * this.#volume * 1000;
    if (out > this.#maxOut) this.#maxOut = out;

    // if (Math.abs(out) > 1 && this.#progress % 100 === 0) {
    //  console.error("Bubble clipped!", out);
    //}

    return out / this.#maxOut;
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
}
