// Bubble 2022.02.04.17.48 [Sage: @mxsage + Jeffrey]
// A translation of C code that physically models a simple bubble sound.
// See also: https://github.com/SkAT-VG/SDT/blob/master/src/SDT/SDTLiquids.h

export default class Bubble {
  // Generic for all instruments.
  playing = true;
  fading = false; // If we are fading and then stopping playback.
  fadeProgress;
  fadeDuration;

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

  // Parameter update properties for smooth transitions
  #futureRadius;
  #futureRise;
  #futureVolume;
  #futurePan;
  
  #radiusUpdatesTotal;
  #radiusUpdatesLeft;
  #radiusUpdateSlice;
  
  #riseUpdatesTotal;
  #riseUpdatesLeft;
  #riseUpdateSlice;
  
  #volumeUpdatesTotal;
  #volumeUpdatesLeft;
  #volumeUpdateSlice;
    #panUpdatesTotal;
  #panUpdatesLeft;
  #panUpdateSlice;
  
  #sustain = false;
  
  constructor(radius, rise, volume, pan, id) {
    this.id = id; // Store the ID for tracking
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
    
    // Initialize future values for parameter updates
    this.#futureRadius = this.#radius;
    this.#futureRise = this.#rise;
    this.#futureVolume = this.#volume;
    this.#futurePan = this.#pan;
    
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

  update({ radius, rise, volume, pan, sustain, duration = 0.1 }) {
    // Sustain updates (immediate change, no interpolation needed)
    if (typeof sustain === "boolean") {
      this.#sustain = sustain;
      console.log(`🧋 UPDATE: Sustain set to ${sustain} for bubble ${this.id || 'unknown'}`);
    }
    
    // Radius updates (affects fundamental frequency and timbre)
    if (typeof radius === "number" && radius > 0) {
      this.#futureRadius = radius * 0.001;
      this.#radiusUpdatesTotal = duration * sampleRate;
      this.#radiusUpdatesLeft = this.#radiusUpdatesTotal;
      this.#radiusUpdateSlice =
        (this.#futureRadius - this.#radius) / this.#radiusUpdatesTotal;
    }
    
    // Rise updates (affects surface tension and bubble behavior)
    if (typeof rise === "number") {
      this.#futureRise = rise;
      this.#riseUpdatesTotal = duration * sampleRate;
      this.#riseUpdatesLeft = this.#riseUpdatesTotal;
      this.#riseUpdateSlice =
        (this.#futureRise - this.#rise) / this.#riseUpdatesTotal;
    }
    
    // Volume updates
    if (typeof volume === "number") {
      this.#futureVolume = volume;
      this.#volumeUpdatesTotal = duration * sampleRate;
      this.#volumeUpdatesLeft = this.#volumeUpdatesTotal;
      this.#volumeUpdateSlice =
        (this.#futureVolume - this.#volume) / this.#volumeUpdatesTotal;
    }
    
    // Pan updates
    if (typeof pan === "number") {
      this.#futurePan = pan;
      this.#panUpdatesTotal = duration * sampleRate;
      this.#panUpdatesLeft = this.#panUpdatesTotal;
      this.#panUpdateSlice =
        (this.#futurePan - this.#pan) / this.#panUpdatesTotal;
    }
  }

  // Sustain control methods
  setSustain(sustain) {
    this.#sustain = sustain;
    console.log(`🧋 setSustain(${sustain}) for bubble ${this.id || 'unknown'}`);
  }
  
  enableSustain() {
    this.#sustain = true;
    console.log(`🧋 enableSustain() for bubble ${this.id || 'unknown'}`);
  }
  
  disableSustain() {
    this.#sustain = false;
    console.log(`🧋 disableSustain() for bubble ${this.id || 'unknown'}`);
  }

  next() {
    // Interpolated parameter updates
    
    // Radius updates (recalculate physical properties when radius changes)
    if (this.#radiusUpdatesLeft > 0) {
      this.#radius += this.#radiusUpdateSlice;
      
      // Recalculate dependent physical properties
      const pRadius = this.#radius * Math.sqrt(this.#radius);
      this.#amp = 17.2133 * pRadius * this.#depth;
      this.#decay = 0.13 / this.#radius + 0.0072 * pRadius;
      this.#gain = Math.exp(-this.#decay * this.#timestep);
      this.#phaseStep = (3.0 / this.#radius) * this.#timestep;
      this.#phaseRise = this.#phaseStep * this.#decay * this.#rise * this.#timestep;
      
      this.#radiusUpdatesLeft -= 1;
    }
    
    // Rise updates (affects phase rise behavior)
    if (this.#riseUpdatesLeft > 0) {
      this.#rise += this.#riseUpdateSlice;
      
      // Update phase rise calculation
      this.#phaseRise = this.#phaseStep * this.#decay * this.#rise * this.#timestep;
      
      this.#riseUpdatesLeft -= 1;
    }
    
    // Volume updates
    if (this.#volumeUpdatesLeft > 0) {
      this.#volume += this.#volumeUpdateSlice;
      this.#volumeUpdatesLeft -= 1;
    }
    
    // Pan updates
    if (this.#panUpdatesLeft > 0) {
      this.#pan += this.#panUpdateSlice;
      this.#panUpdatesLeft -= 1;
    }    // get sound value - only stop if not in sustain mode
    if (!this.#sustain && this.#amp < this.#QUIET && this.#phase > 1.0) {
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
    
    // Only apply amplitude decay if not in sustain mode
    if (!this.#sustain) {
      this.#amp *= this.#gain;
    }

    this.#progress += 1;

    // *** Normalization to a max of 1 / -1
    // TODO: Keep track of a normalization curve. 2022.02.04.16.42
    let out = this.#out * this.#volume * 1000;
    if (out > this.#maxOut) this.#maxOut = out;

    // if (Math.abs(out) > 1 && this.#progress % 100 === 0) {
    //  console.error("Bubble clipped!", out);
    //}

    out = out / this.#maxOut;

    // ➰💀 "Fade 2 kill." - Apply fading if necessary
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
}
