/* global currentFrame, sampleRate, currentTime */

// import * as sine from "./sound/sine.js";
import * as volume from "./sound/volume.mjs";
import Square from "./sound/square.mjs";
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

  #queue = [];

  #bubble;

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
      }

      // Square
      // Square fires just once and gets recreated on every call.
      if (msg.type === "square") {
        const durationInFrames = Math.round(
          sampleRate * (this.#bpmInSec * msg.data.beats)
        );

        const attackInFrames = Math.round(durationInFrames * msg.data.attack);
        const decayInFrames = Math.round(durationInFrames * msg.data.decay);

        this.#queue.push(
          new Square(
            msg.data.tone,
            durationInFrames,
            attackInFrames,
            decayInFrames,
            msg.data.volume,
            msg.data.pan
          )
        );

        /*
        console.log(
          "🎼 Square:",
          msg.data.tone,
          "Beats:",
          msg.data.beats,
          "Attack:",
          msg.data.attack,
          "Decay:",
          msg.data.decay,
          "Volume:",
          msg.data.volume,
          "Pan:",
          msg.data.pan
        );
        */
      }

      // Bubble is a singleton, only one can ever exist. They automatically
      // layer or get destroyed / recreated.
      if (msg.type === "bubble") {
        this.#bubble = new Bubble(
          msg.data.radius,
          msg.data.rise,
          msg.data.volume,
          msg.data.pan
        );
        this.#queue.push(this.#bubble);
      }

      /*
        if (!this.#queue.includes(this.#bubble)) {
          this.#bubble = new Bubble(
            msg.data.radius,
            msg.data.rise,
            msg.data.volume,
            msg.data.pan
          );
          this.#queue.push(this.#bubble);
        } else {
          this.#bubble.start(
            msg.data.radius,
            msg.data.rise,
            msg.data.volume,
            msg.data.pan
          );
        }

        console.log(
          "🎼 Bubble Radius:",
          msg.data.radius,
          "Rise:",
          msg.data.rise,
          "Volume:",
          msg.data.volume,
          "Pan:",
          msg.data.pan
        );
      }
       */

      // Sample

      // Triangle

      // Sine

      // Saw
    };
  }

  process(inputs, outputs) {
    // 1️⃣ Metronome
    this.#ticks += currentTime - this.#lastTime;
    this.#lastTime = currentTime;

    // const timeTillNextBeat = this.#ticks / this.#bpmInSec;

    if (this.#ticks >= this.#bpmInSec) {
      this.#ticks = 0;
      this.port.postMessage(currentTime);
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

  // #report() {
  //this.port.postMessage()
  // }
}

registerProcessor("sound-processor", SoundProcessor);
