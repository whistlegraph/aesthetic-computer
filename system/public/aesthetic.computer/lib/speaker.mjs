/* global currentFrame, sampleRate, currentTime */

// import * as sine from "./sound/sine.js";
import * as volume from "./sound/volume.mjs";
import Sound from "./sound/sound.mjs";
import Bubble from "./sound/bubble.mjs";

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

  constructor(options) {
    if (options.debug) console.log("🔊 Sound Synthesis Worklet Started");

    super();

    this.#lastTime = currentTime;

    this.#bpm = options.processorOptions.bpm;
    this.#bpmInSec = 60 / this.#bpm;
    this.#ticks = this.#bpmInSec;

    volume.amount.val = 0.25; // Set global volume.

    // Change BPM, or queue up an instrument note.
    this.port.onmessage = (e) => {
      const msg = e.data;

      // New BPM
      if (msg.type === "new-bpm") {
        this.#bpm = msg.data;
        this.#bpmInSec = 60 / this.#bpm;
        console.log("🎼 New BPM:", this.#bpm);
        return;
      }

      // Update properties of an existing sound, if found.
      if (msg.type === "update") {
        this.#running[msg.data.id]?.update(msg.data.properties);
      }

      // 💀 Kill an existing sound.
      if (msg.type === "kill") {
        this.#running[msg.data]?.kill(); // Try and kill the sound if it exists,
        this.#running[msg.data] = undefined; // then compact the array.
        delete this.#running[msg.data];
        return;
      }

      // Kill all pervasive sounds.
      if (msg.type === "kill:all") {
        const running = this.#running;
        Object.keys(running).forEach((key) => {
          running[key]?.kill();
          delete runnning[key];
        });
        return;
      }

      // 📢 Sound
      // Fires just once and gets recreated on every call.
      if (msg.type === "sound") {
        let duration, attack, decay;

        if (msg.data.beats === Infinity) {
          duration = Infinity;
          attack = msg.data.attack; // Measured in seconds in `Sound`.
          decay = msg.data.decay;
        } else {
          duration = Math.round(sampleRate * (this.#bpmInSec * msg.data.beats));
          attack = Math.round(durationInFrames * msg.data.attack); // Measured in frames.
          decay = Math.round(durationInFrames * msg.data.decay);
        }

        // Trigger the sound...
        const sound = new Sound({
          type: msg.data.type,
          tone: msg.data.tone,
          duration,
          attack,
          decay,
          volume: msg.data.volume,
          pan: msg.data.pan,
        });

        if (duration === Infinity && msg.data.id > -1) {
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
            msg.data.pan
          )
        );
        return;
      }
    };
  }

  process(inputs, outputs) {
    // 1️⃣ Metronome
    this.#ticks += currentTime - this.#lastTime;
    this.#lastTime = currentTime;

    // const timeTillNextBeat = this.#ticks / this.#bpmInSec;

    if (this.#ticks >= this.#bpmInSec) {
      this.#ticks = 0;
      this.#report({ currentTime });
      // TODO: Add in amplitude.
    }

    // 2️⃣ Sound generation
    // const input = inputs[0];
    const output = outputs[0];

    // We assume two channels. (0 and 1)
    for (let frame = 0; frame < output[0].length; frame += 1) {
      // Remove any finished instruments from the queue.
      this.#queue = this.#queue.filter((instrument) => {
        return instrument.playing;
      });

      // Loop through every instrument in the queue and add it to the output.
      for (const instrument of this.#queue) {
        // For now, all sounds are maxed out and mixing happens by dividing by the total length.

        // TODO: Actually add the sounds here instead of replacing them.
        const amplitude = instrument.next(); // this.#queue.length;

        output[0][frame] += instrument.pan(0, amplitude);
        output[1][frame] += instrument.pan(1, amplitude);
      }

      // Mix all sound through global volume.
      output[0][frame] = volume.apply(output[0][frame]);
      output[1][frame] = volume.apply(output[1][frame]);
    }

    return true;
  }

  // Send data back to the `bios`.
  #report(content) {
    this.port.postMessage(content);
  }
}

registerProcessor("sound-processor", SoundProcessor);
