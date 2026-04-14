// 💨 Fart 2025.04.14
// Physical modeling of a fart sound using pressure, pitch, and rasp parameters.
// Based on procedural audio synthesis principles from CompuFart.

export default class Fart {
  // Generic for all instruments.
  playing = true;
  fading = false; // If we are fading and then stopping playback.
  fadeProgress;
  fadeDuration;

  #volume = 1; // 0 to 1
  #pan = 0; // -1 to 1

  #pressure; // 0 to 1 - how hard you squeeze
  #pitch; // Hz - fundamental frequency
  #rasp; // 0 to 1 - noise component (0 = pure tone, 1 = mostly noise)

  #amp;
  #decay;
  #gain;
  #phase;
  #lastOut;
  #timestep;

  #out = 0;
  #maxOut = 1;

  #progress = 0;

  #QUIET = 0.000001;

  // Noise generation state
  #noiseState = 0;

  // Parameter update properties for smooth transitions
  #futurePressure;
  #futurePitch;
  #futureRasp;
  #futureVolume;
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

  #volumeUpdatesTotal;
  #volumeUpdatesLeft;
  #volumeUpdateSlice;

  #panUpdatesTotal;
  #panUpdatesLeft;
  #panUpdateSlice;

  #sustain = false;

  constructor(pressure, pitch, rasp, volume, pan, id) {
    this.id = id; // Store the ID for tracking
    this.start(pressure, pitch, rasp, volume, pan);
  }

  start(
    pressure = this.#pressure,
    pitch = this.#pitch,
    rasp = this.#rasp,
    volume = this.#volume,
    pan = this.#pan
  ) {
    this.#pan = pan;
    this.#volume = volume;
    this.#pressure = Math.max(0.01, Math.min(1, pressure)); // Clamp 0.01-1
    this.#pitch = Math.max(20, Math.min(8000, pitch)); // Clamp pitch to audible range
    this.#rasp = Math.max(0, Math.min(1, rasp)); // Clamp 0-1

    // Initialize future values for parameter updates
    this.#futurePressure = this.#pressure;
    this.#futurePitch = this.#pitch;
    this.#futureRasp = this.#rasp;
    this.#futureVolume = this.#volume;
    this.#futurePan = this.#pan;

    this.#timestep = 1 / sampleRate;
    this.#lastOut = this.#out;

    // Amplitude envelope: pressure controls initial amplitude
    this.#amp = 0.3 * this.#pressure;

    // Decay rate: lower pitch = longer sustain
    this.#decay = 0.8 + (this.#pitch / 8000) * 0.2; // Faster decay for higher pitches
    this.#gain = Math.exp(-this.#decay * this.#timestep);

    this.#phase = 0;
  }

  update({ pressure, pitch, rasp, volume, pan, sustain, duration = 0.1 }) {
    // Sustain updates (immediate change, no interpolation needed)
    if (typeof sustain === "boolean") {
      this.#sustain = sustain;
      console.log(`💨 UPDATE: Sustain set to ${sustain} for fart ${this.id || 'unknown'}`);
    }

    // Pressure updates (affects amplitude and energy)
    if (typeof pressure === "number" && pressure >= 0) {
      this.#futurePressure = Math.max(0.01, Math.min(1, pressure));
      this.#pressureUpdatesTotal = duration * sampleRate;
      this.#pressureUpdatesLeft = this.#pressureUpdatesTotal;
      this.#pressureUpdateSlice =
        (this.#futurePressure - this.#pressure) / this.#pressureUpdatesTotal;
    }

    // Pitch updates (affects frequency)
    if (typeof pitch === "number" && pitch > 0) {
      this.#futurePitch = Math.max(20, Math.min(8000, pitch));
      this.#pitchUpdatesTotal = duration * sampleRate;
      this.#pitchUpdatesLeft = this.#pitchUpdatesTotal;
      this.#pitchUpdateSlice =
        (this.#futurePitch - this.#pitch) / this.#pitchUpdatesTotal;
    }

    // Rasp updates (affects noise/tone balance)
    if (typeof rasp === "number" && rasp >= 0) {
      this.#futureRasp = Math.max(0, Math.min(1, rasp));
      this.#raspUpdatesTotal = duration * sampleRate;
      this.#raspUpdatesLeft = this.#raspUpdatesTotal;
      this.#raspUpdateSlice =
        (this.#futureRasp - this.#rasp) / this.#raspUpdatesTotal;
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
    console.log(`💨 setSustain(${sustain}) for fart ${this.id || 'unknown'}`);
  }

  enableSustain() {
    this.#sustain = true;
    console.log(`💨 enableSustain() for fart ${this.id || 'unknown'}`);
  }

  disableSustain() {
    this.#sustain = false;
    console.log(`💨 disableSustain() for fart ${this.id || 'unknown'}`);
  }

  // Linear congruential generator for pseudo-random noise
  _noise() {
    this.#noiseState = (this.#noiseState * 1103515245 + 12345) & 0x7fffffff;
    return (this.#noiseState / 0x7fffffff) * 2 - 1; // Range: -1 to 1
  }

  next() {
    // Interpolated parameter updates

    // Pressure updates (affects amplitude)
    if (this.#pressureUpdatesLeft > 0) {
      this.#pressure += this.#pressureUpdateSlice;
      this.#pressureUpdatesLeft -= 1;
    }

    // Pitch updates (affects frequency for next cycle)
    if (this.#pitchUpdatesLeft > 0) {
      this.#pitch += this.#pitchUpdateSlice;
      this.#pitchUpdatesLeft -= 1;
    }

    // Rasp updates (affects noise/tone balance)
    if (this.#raspUpdatesLeft > 0) {
      this.#rasp += this.#raspUpdateSlice;
      this.#raspUpdatesLeft -= 1;
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
    }

    // Stop if amplitude is quiet and not sustaining
    if (!this.#sustain && this.#amp < this.#QUIET) {
      this.playing = false;
      return 0;
    }

    // Calculate phase step from current pitch
    const phaseStep = (this.#pitch / sampleRate) * Math.PI * 2;

    // Generate tone component (sine wave)
    const tone = Math.sin(this.#phase) * this.#pressure;

    // Generate noise component
    const noise = this._noise() * this.#rasp;

    // Mix tone and noise
    const mixed = tone * (1 - this.#rasp) + noise;

    // Apply amplitude envelope with smoothing
    this.#out = this.#lastOut * 0.3 + mixed * this.#amp * 0.7;
    this.#lastOut = this.#out;

    // Advance phase
    this.#phase += phaseStep;
    if (this.#phase > Math.PI * 2) {
      this.#phase -= Math.PI * 2;
    }

    // Only apply amplitude decay if not in sustain mode
    if (!this.#sustain) {
      this.#amp *= this.#gain;
    }

    this.#progress += 1;

    // Normalization to a max of 1 / -1
    let out = this.#out * this.#volume;
    if (Math.abs(out) > this.#maxOut) this.#maxOut = Math.abs(out);

    out = out / this.#maxOut;

    // Apply fading if necessary
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

  // Stereo panning
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
