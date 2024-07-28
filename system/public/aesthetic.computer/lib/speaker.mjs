/* global currentFrame, sampleRate, currentTime */

// import * as sine from "./sound/sine.js";
import * as volume from "./sound/volume.mjs";
import Sound from "./sound/sound.mjs";
import Bubble from "./sound/bubble.mjs";

const { abs, round } = Math;

// Helpful Info:

// For basic audio waveform algorithms see: https://github.com/Flarp/better-oscillator/blob/master/worklet.js
// And read about: https://en.wikipedia.org/wiki/Oscillator_sync#Hard_Sync

// Retrieve the currentFrame or currentTime (in seconds);
// console.log("Current frame:", currentFrame, currentTime);
// See also: https://developer.mozilla.org/en-US/docs/Web/API/AudioWorkletGlobalScope

// Also, many parameters can be used and configured:
// https://developer.mozilla.org/en-US/docs/Web/API/AudioWorkletNode/parameters
// TODO: Use parameters to change properties of square over time and eventually add more nodes.

class SoundProcessor extends AudioWorkletProcessor {
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

  constructor(options) {
    if (options.debug) console.log("🔊 Sound Synthesis Worklet Started");

    super();

    this.#lastTime = currentTime;

    this.#bpm = options.processorOptions.bpm;
    this.#bpmInSec = 60 / this.#bpm;
    this.#ticks = this.#bpmInSec;

    volume.amount.val = 1; //0.25; // Set global volume.

    // Change BPM, or queue up an instrument note.
    this.port.onmessage = (e) => {
      const msg = e.data;
      // Send waveform data to `bios`.
      if (msg.type === "get-waveforms") {
        this.port.postMessage({
          type: "waveforms",
          content: {
            left: this.#currentWaveformLeft,
            right: this.#currentWaveformRight,
          },
        });
        return;
      }

      if (msg.type === "get-amplitudes") {
        this.port.postMessage({
          type: "amplitudes",
          content: {
            left: this.#currentAmplitudeLeft,
            right: this.#currentAmplitudeRight,
          },
        });
        return;
      }

      // New BPM
      if (msg.type === "new-bpm") {
        this.#bpm = msg.data;
        this.#bpmInSec = 60 / this.#bpm;
        console.log("🎼 New BPM:", this.#bpm);
        return;
      }

      // Update properties of an existing sound, if found.
      if (msg.type === "update") {
        // console.log("got update!", this.#running, msg.data.id);
        this.#running[msg.data.id]?.update(msg.data.properties);
        return;
      }

      // 💀 Kill an existing sound.
      if (msg.type === "kill") {
        // Try and kill the sound if it exists, linearly over 'fade' seconds.
        this.#running[msg.data.id]?.kill(msg.data.fade);
        this.#running[msg.data.id] = undefined; // then compact the array.
        delete this.#running[msg.data.id];
        return;
      }

      // Kill all pervasive sounds.
      if (msg.type === "kill:all") {
        const running = this.#running;
        Object.keys(running).forEach((key) => {
          running[key]?.kill();
          delete running[key];
        });
        return;
      }

      // 📢 Sound
      // Fires just once and gets recreated on every call.

      // TODO: 🔥 Smooth out the 'beats' / 'duration' interface switch here.

      if (msg.type === "sound") {
        let duration, attack, decay;

        if (msg.data.beats === Infinity) {
          duration = Infinity;
        } else {
          // TODO: This should be settable not via beats as well...
          duration = round(sampleRate * (this.#bpmInSec * msg.data.beats));

          attack = round(duration * msg.data.attack); // Measured in frames.
          decay = round(duration * msg.data.decay);
        }

        // Trigger the sound...
        const sound = new Sound({
          type: msg.data.type,
          tone: msg.data.tone,
          duration,
          attack,
          decay,
          volume: msg.data.volume || 1,
          pan: msg.data.pan || 0,
        });

        if (duration === Infinity && msg.data.id > -1n) {
          this.#running[msg.data.id] = sound; // Index by the unique id.
        }

        this.#queue.push(sound);
        return;
      }

      // Bubble works similarly to Square.
      if (msg.type === "bubble") {
        this.#queue.push(
          new Bubble(
            msg.data.radius,
            msg.data.rise,
            msg.data.volume,
            msg.data.pan,
          ),
        );
        return;
      }
    };
  }

  process(inputs, outputs) {
    // 0️⃣ Waveform Tracking
    let waveformLeft = [];
    let waveformRight = [];

    // 1️⃣ Metronome
    this.#ticks += currentTime - this.#lastTime;
    this.#lastTime = currentTime;

    // const timeTillNextBeat = this.#ticks / this.#bpmInSec;

    if (this.#ticks >= this.#bpmInSec) {
      this.#ticks = 0;
      this.#report("metronome", currentTime);
    }

    // 2️⃣ Sound generation
    // const input = inputs[0];
    const output = outputs[0];

    let ampLeft = 0,
      ampRight = 0;

    // We assume two channels. (0 and 1)
    for (let s = 0; s < output[0].length; s += 1) {
      // Remove any finished instruments from the queue.
      this.#queue = this.#queue.filter((instrument) => {
        return instrument.playing;
      });

      // Loop through every instrument in the queue and add it to the output.
      for (const instrument of this.#queue) {
        // For now, all sounds are maxed out and mixing happens by dividing by the total length.

        // TODO: Actually add the sounds here instead of replacing them.
        const amplitude = instrument.next(); // this.#queue.length;

        output[0][s] += instrument.pan(0, amplitude);
        output[1][s] += instrument.pan(1, amplitude);
      }

      // Mix all sound through global volume.
      // output[0][s] = volume.apply(normalizeSample(output[0][s] / this.#queue.length)/* / this.#queue.length*/);
      // output[1][s] = volume.apply(normalizeSample(output[1][s] / this.#queue.length)/* / this.#queue.length*/);

      // console.log(this.#queue.length);

      // output[0][s] = volume.apply((output[0][s] / (this.#queue.length || 1)));
      // output[1][s] = volume.apply((output[1][s] / (this.#queue.length || 1)));

      // while (abs(output[0][s]) > 1) {
      //   output[0][s] *= 0.4;
      //   output[1][s] *= 0.4;
      // }

      output[0][s] = volume.apply(mix(output[0][s]));
      output[1][s] = volume.apply(mix(output[1][s]));

      // output[0][s] = output[0][s];
      // output[1][s] = output[1][s];

      // if (output[0][s] > 1) {
      //   console.log(output[0][s]);
      // }

      // Track the current amplitude of both channels, and get waveform data.
      ampLeft = abs(output[0][s]) > ampLeft ? abs(output[0][s]) : ampLeft;
      ampRight = abs(output[1][s]) > ampRight ? abs(output[1][s]) : ampRight;

      if (s % 8 === 0) {
        waveformLeft.push(output[0][s]); // Cap every 8th value. (Usually 16)
        waveformRight.push(output[1][s]);
      }
    }

    this.#currentWaveformLeft = waveformLeft.slice(0);
    this.#currentWaveformRight = waveformRight.slice(0);
    this.#currentAmplitudeLeft = ampLeft;
    this.#currentAmplitudeRight = ampRight;
    return true;
  }

  // Send data back to the `bios`.
  #report(type, content) {
    this.port.postMessage({ type, content });
  }
}

registerProcessor("sound-processor", SoundProcessor);

// Global Mixing and Compression
const threshold = 1.0; // This ends up being the max output amplitude.
const squash = 4.0; // How much to squeeze... 8 seems good?

function compressor(sample, maxAmplitude = threshold, ratio = squash) {
  if (abs(sample) > maxAmplitude) {
    const overThreshold = abs(sample) - maxAmplitude;
    return sample > 0
      ? maxAmplitude + overThreshold / ratio
      : -maxAmplitude - overThreshold / ratio;
  }
  return sample;
}

function limiter(sample, maxAmplitude = threshold) {
  if (abs(sample) > maxAmplitude) {
    return sample > 0 ? maxAmplitude : -maxAmplitude;
  }
  return sample;
}

function mix(sample, maxAmplitude = threshold) {
  return limiter(compressor(sample, maxAmplitude), maxAmplitude);
}
