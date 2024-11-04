/* global currentFrame, sampleRate, currentTime */

// import * as sine from "./sound/sine.js";
import * as volume from "./sound/volume.mjs";
import Synth from "./sound/synth.mjs";
import Bubble from "./sound/bubble.mjs";
import { lerp, within } from "./num.mjs";

const { abs, round, floor } = Math;

// Helpful Info:

// For basic audio waveform algorithms see: https://github.com/Flarp/better-oscillator/blob/master/worklet.js
// And read about: https://en.wikipedia.org/wiki/Oscillator_sync#Hard_Sync

// Retrieve the currentFrame or currentTime (in seconds);
// console.log("Current frame:", currentFrame, currentTime);
// See also: https://developer.mozilla.org/en-US/docs/Web/API/AudioWorkletGlobalScope

// Also, many parameters can be used and configured:
// https://developer.mozilla.org/en-US/docs/Web/API/AudioWorkletNode/parameters
// TODO: Use parameters to change properties of square over time and eventually add more nodes.

// Global reverb constants!
const delayTime = 0.1; // 200ms delay
const feedback = 0.4; // 50% feedback
const mix = 0.25; // 30% wet/dry mix

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

  #mixDivisor = 1;

  #reverbLeft;
  #reverbRight;

  constructor(options) {
    // if (options.processorOptions.debug) console.log("🔊 Sound Synthesis Worklet Started");
    // console.log("🎼 Sample rate:", sampleRate);

    super();

    this.#lastTime = currentTime;

    this.#bpm = options.processorOptions.bpm;
    this.#bpmInSec = 60 / this.#bpm;
    this.#ticks = this.#bpmInSec;

    volume.amount.val = 0.9; // Set global volume.

    this.#reverbLeft = new Reverb(sampleRate, delayTime, feedback, mix);
    this.#reverbRight = new Reverb(sampleRate, delayTime, feedback, mix);

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

      // Reset the metronome beat.
      if (msg.type === "beat:skip") {
        console.log("🎼 Beat skipped");
        this.#ticks = 0;
        this.#report("metronome", currentTime);
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

          attack = msg.data.attack * sampleRate;
          decay = msg.data.decay * sampleRate;
        } else {
          // TODO: This should be settable not via beats as well...
          duration = round(sampleRate * (this.#bpmInSec * msg.data.beats));

          attack = round(duration * msg.data.attack); // Measured in frames.
          decay = round(duration * msg.data.decay);
        }

        // Trigger the sound...
        const sound = new Synth({
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

    const waveformSize = round(sampleRate / 200); // * 0.06); // Sample size.
    // TODO:                                ^ Find good rate.
    const waveformRate = 1; // Sample interval.

    // We assume two channels. (0 and 1)
    for (let s = 0; s < output[0].length; s += 1) {
      // Remove any finished instruments from the queue.
      this.#queue = this.#queue.filter((instrument) => {
        return instrument.playing;
      });

      let voices = 0;

      // Loop through every instrument in the queue and add it to the output.
      for (const instrument of this.#queue) {
        // For now, all sounds are maxed out and mixing happens by dividing by the total length.
        const amplitude = instrument.next(); // this.#queue.length;
        // 😱 TODO: How can I mix reverb into the instrument here?

        output[0][s] += instrument.pan(0, amplitude);
        output[1][s] += instrument.pan(1, amplitude);

        if (instrument.fading) {
          voices +=
            instrument.volume *
            (1 - instrument.fadeProgress / instrument.fadeDuration);
        } else {
          // console.log(instrument, "Volume:", instrument.volume)
          voices += instrument.volume; //instrument.volume;
        }
      }

      // Auto-mixing for voices.
      voices = Math.max(1, voices);

      // TODO: 🟢 These divisor value need to be consistent in duration with
      //          varying sample rates.
      if (voices > 1) {
        if (!within(0.001, this.#mixDivisor, voices)) {
          if (this.#mixDivisor < voices) {
            this.#mixDivisor *= 1.005;
          } else {
            this.#mixDivisor *= 0.997;
          }
        }
      }

      // if (voices > 1 && this.#mixDivisor < voices) {
      // }

      // if (this.voices > 1 && this.#mixDivisor )

      // if (voices > 1)

      // } else {
      //  this.#mixDivisor = voices;
      // }

      // if (this.#queue.length > 0) console.log(output[0][s], voices, this.#mixDivisor);

      // Apply reverb to the amplitude
      //const rL = this.#reverbLeft.processSample(output[0][s]);
      //const rR = this.#reverbRight.processSample(output[1][s]);
      //output[0][s] = volume.apply(rL / this.#mixDivisor);
      //output[1][s] = volume.apply(rR / this.#mixDivisor);

      output[0][s] = volume.apply(output[0][s] / this.#mixDivisor);
      output[1][s] = volume.apply(output[1][s] / this.#mixDivisor);

      // Track the current amplitude of both channels, and get waveform data.
      ampLeft = abs(output[0][s]) > ampLeft ? abs(output[0][s]) : ampLeft;
      ampRight = abs(output[1][s]) > ampRight ? abs(output[1][s]) : ampRight;

      if (s % waveformRate === 0) {
        waveformLeft.push(output[0][s]);
        waveformRight.push(output[1][s]);
      }
    }

    this.#currentWaveformLeft.push(...waveformLeft);
    this.#currentWaveformRight.push(...waveformRight);

    // Remove old samples from the beginning if the buffer exceeds max size.
    if (this.#currentWaveformLeft.length > waveformSize) {
      const excess = this.#currentWaveformLeft.length - waveformSize;
      this.#currentWaveformLeft.splice(0, excess);
      this.#currentWaveformRight.splice(0, excess);
    }

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
// const threshold = 1; // This ends up being the max output amplitude.

// function compressor(sample, maxAmplitude = threshold, ratio = 4) {
//   if (abs(sample) > maxAmplitude) {
//     const overThreshold = abs(sample) - maxAmplitude;
//     return sample > 0
//       ? maxAmplitude + overThreshold / ratio
//       : -maxAmplitude - overThreshold / ratio;
//   }
//   return sample;
// }

// function limiter(sample, maxAmplitude = threshold) {
//   if (abs(sample) > maxAmplitude) {
//     return sample > 0 ? maxAmplitude : -maxAmplitude;
//   }
//   return sample;
// }

// function mix(sample, maxAmplitude = threshold) {
//   // return sample;
//   //return compressor(sample, maxAmplitude);
//   return limiter(compressor(sample, maxAmplitude), maxAmplitude);
// }

class Reverb {
  constructor(sampleRate, delayTime, feedback, mix) {
    this.sampleRate = sampleRate;
    this.delayTime = delayTime;
    this.feedback = feedback;
    this.mix = mix;

    // Convert delay time to samples
    this.delaySamples = Math.floor(delayTime * sampleRate);

    // Initialize the delay buffer
    this.delayBuffer = new Float32Array(this.delaySamples);
    this.bufferIndex = 0;
  }

  processSample(inputSample) {
    // Get the delayed sample from the buffer
    const delayedSample = this.delayBuffer[this.bufferIndex];
    // Calculate the output sample (dry + wet mix)
    const outputSample =
      inputSample * (1 - this.mix) + delayedSample * this.mix;
    // Write the current sample + feedback into the buffer
    this.delayBuffer[this.bufferIndex] =
      inputSample + delayedSample * this.feedback;
    // Advance the buffer index
    this.bufferIndex = (this.bufferIndex + 1) % this.delaySamples;
    return outputSample;
  }
}
