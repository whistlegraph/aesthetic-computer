/* global currentFrame, sampleRate, currentTime */

// import * as sine from "./sound/sine.js";
import * as volume from "./sound/volume.mjs";
import Synth from "./sound/synth.mjs";
import Bubble from "./sound/bubble.mjs";
import { lerp, within, clamp } from "./num.mjs";

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

const sampleStore = {}; // A global store for sample buffers previously added.

class SpeakerProcessor extends AudioWorkletProcessor {
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

  // Frequency analysis
  #frequencyBandsLeft = [];
  #frequencyBandsRight = [];
  #fftBufferLeft = [];
  #fftBufferRight = [];
  #fftSize = 256; // Must be power of 2

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

      if (msg.type === "get-frequencies") {
        this.port.postMessage({
          type: "frequencies",
          content: {
            left: this.#frequencyBandsLeft,
            right: this.#frequencyBandsRight,
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

      // Process beat message with sound arrays (sounds, bubbles, kills)
      if (msg.type === "beat") {
        const soundData = msg.content;
        
        // Process bubbles array
        if (soundData.bubbles) {
          soundData.bubbles.forEach(bubbleData => {
            const bubble = new Bubble(
              bubbleData.radius,
              bubbleData.rise,
              bubbleData.volume,
              bubbleData.pan,
              bubbleData.id,
            );
            
            // Track bubble by ID if provided
            if (bubbleData.id !== undefined) {
              this.#running[bubbleData.id] = bubble;
            }
            
            this.#queue.push(bubble);
          });
        }
        
        // Process sounds array (existing logic should handle this via other messages)
        
        // Process kills array
        if (soundData.kills) {
          soundData.kills.forEach(killData => {
            this.#running[killData.id]?.kill(killData.fade);
            this.#running[killData.id] = undefined;
            delete this.#running[killData.id];
          });
        }
        
        return;
      }

      // New BPM
      if (msg.type === "new-bpm") {
        this.#bpm = msg.data;
        this.#bpmInSec = 60 / this.#bpm;
        // console.log("🎼 New BPM:", this.#bpm);
        return;
      }

      if (msg.type === "get-progress") {
        // ⏰ TODO: Compute actual sound progress from 0->1 here by looking
        // up the id with `msg.content`.
        // console.log(
        //   "Progress needed for:",
        //   msg.content,
        //   this.#running[msg.content],
        // );
        this.#report("progress", {
          id: msg.content,
          progress: this.#running[msg.content]?.progress(),
        });
        return;
      }

      // Update properties of an existing sound, if found.
      if (msg.type === "update") {
        // console.log("📻 Got sound update!", msg.data);
        this.#running[msg.data.id]?.update(msg.data.properties);
        return;
      }

      // Update custom generator for a custom synth
      if (msg.type === "update-generator") {
        // console.log("🎨 Got custom generator update!", msg.data);
        this.#running[msg.data.id]?.setCustomGenerator(msg.data.generator);
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

      // Clear the full sample cache.
      if (msg.type === "cache:clear") {
        for (const k in sampleStore) delete sampleStore[k];
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
          const data = msg.data;

          if (data.beats) {
            duration = round(sampleRate * (this.#bpmInSec * data.beats));
          } else if (data.options.buffer && !duration) {
            // Read the 'from' and 'to' values from msg.data.options here which
            // range from 0->1 to determine the start and stop inside of the sample
            // buffer.

            if (typeof data.options.buffer === "string") {
              data.options.buffer = sampleStore[data.options.buffer];
            } else {
              sampleStore[data.options.label] = data.options.buffer;
            }

            // console.log("Buffer:", data.options.buffer, data.options.label);

            // Ensure 'from' and 'to' are within the valid range [0,1]
            let from = clamp(data.options.from || 0, 0, 1);
            let to = clamp(data.options.to || 1, 0, 1);

            // Swap if from > to so that we always travel in the correct direction
            if (from > to) {
              [from, to] = [to, from];
              data.options.speed = -(data.options.speed || 1); // Reverse speed direction
            }

            // Compute the sample range
            const startSample = round(from * data.options.buffer.length);
            const endSample = round(to * data.options.buffer.length);

            // Add the sample range into options.
            data.options.startSample = startSample;
            data.options.endSample = endSample;

            // Compute duration based on the selected range
            duration = round(
              ((endSample - startSample) /
                abs(data.options.speed || 1) /
                data.options.buffer.sampleRate) *
                sampleRate,
            );
          }

          attack = round(duration * msg.data.attack || 0); // Measured in frames.
          decay = round(duration * msg.data.decay || 0);

          // console.log(
          //   "📻",
          //   "Start sample:",
          //   data.options.startSample,
          //   "End sample:",
          //   data.options.endSample,
          //   "Duration:",
          //   duration,
          //   "Speed",
          //   data.options.speed,
          // );
        }
        // console.log(msg.data);

        // Trigger the sound...

        // TODO: Use the `when` value to trigger the sound here.        //if (msg.data.when === "now") {
          // Prepare options with generator for custom type
          let synthOptions = msg.data.options || { tone: msg.data.tone };
          if (msg.data.type === "custom" && msg.data.generator) {
            synthOptions = { ...synthOptions, generator: msg.data.generator };
          }
          
          const sound = new Synth({
            type: msg.data.type,
            id: msg.data.id,
            options: synthOptions,
            duration,
            attack,
            decay,
            volume: msg.data.volume ?? 1,
            pan: msg.data.pan || 0,
          });

          // console.log("🔊🚩 Sound ID:", msg.data.id);

          // if (duration === Infinity && msg.data.id > -1n) {
          this.#running[msg.data.id] = sound; // Index by the unique id.
          // }

          this.#queue.push(sound);
        //} else {
          // Wait and then trigger...
          // console.log("😊 Waiting on sound:", msg.data.when);
        // }

        return;
      }

      //if (msg.type === "sample") {
      //  console.log("Play sample:", msg.data.buffer);

      // const sound = new Synth({
      //   type: "sample",
      //   options: {
      //     buffer: msg.data.buffer,
      //   },
      //   duration,
      //   attack,
      //   decay,
      //   volume: msg.data.volume ?? 1,
      //   pan: msg.data.pan || 0,
      // });

      // this.#queue.push(sound);
      //  return;
      // }      // Bubble works similarly to Square.
      if (msg.type === "bubble") {
        const bubble = new Bubble(
          msg.data.radius,
          msg.data.rise,
          msg.data.volume,
          msg.data.pan,
          msg.data.id,
        );
        
        // Track bubble by ID if provided
        if (msg.data.id !== undefined) {
          this.#running[msg.data.id] = bubble;
        }
        
        this.#queue.push(bubble);
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
        if (!instrument.playing) {
          this.#report("killed", { id: instrument.id });
        } // Send a kill message back.
        return instrument.playing;
      });

      let voices = 0;

      // Loop through every instrument in the queue and add it to the output.
      for (const instrument of this.#queue) {
        // For now, all sounds are maxed out and mixing happens by dividing by the total length.
        const amplitude = instrument.next(s); // this.#queue.length;
        // 😱 TODO: How can I mix reverb into the instrument here?

        output[0][s] += instrument.pan(0, amplitude);
        output[1][s] += instrument.pan(1, amplitude);

        if (instrument.fading) {
          voices +=
            instrument.volume *
            (1 - instrument.fadeProgress / instrument.fadeDuration);
        } else {
          // console.log(instrument, "Volume:", instrument.volume)
          if (instrument.type !== "sample") {
            voices += instrument.volume; // instrument.volume;
          }
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

    // === FREQUENCY ANALYSIS ===
    // Add samples to FFT buffers
    this.#fftBufferLeft.push(...output[0]);
    this.#fftBufferRight.push(...output[1]);

    // Keep buffer size manageable
    while (this.#fftBufferLeft.length > this.#fftSize) {
      this.#fftBufferLeft.shift();
      this.#fftBufferRight.shift();
    }

    // Perform frequency analysis when we have enough samples
    if (this.#fftBufferLeft.length >= this.#fftSize) {
      this.#frequencyBandsLeft = this.#analyzeFrequencies(this.#fftBufferLeft);
      this.#frequencyBandsRight = this.#analyzeFrequencies(this.#fftBufferRight);
    }

    return true;
  }

  // === FREQUENCY ANALYSIS METHODS ===
  
  // Simple FFT implementation for frequency analysis
  #fft(buffer) {
    const N = buffer.length;
    if (N <= 1) return buffer.map(x => ({ real: x, imag: 0 }));
    
    // Ensure power of 2
    const powerOf2 = Math.pow(2, Math.floor(Math.log2(N)));
    const input = buffer.slice(0, powerOf2);
    
    // Recursive FFT
    const even = this.#fft(input.filter((_, i) => i % 2 === 0));
    const odd = this.#fft(input.filter((_, i) => i % 2 === 1));
    
    const result = new Array(powerOf2);
    for (let k = 0; k < powerOf2 / 2; k++) {
      const t = { 
        real: Math.cos(-2 * Math.PI * k / powerOf2) * odd[k].real - Math.sin(-2 * Math.PI * k / powerOf2) * odd[k].imag,
        imag: Math.sin(-2 * Math.PI * k / powerOf2) * odd[k].real + Math.cos(-2 * Math.PI * k / powerOf2) * odd[k].imag
      };
      result[k] = { 
        real: even[k].real + t.real, 
        imag: even[k].imag + t.imag 
      };
      result[k + powerOf2 / 2] = { 
        real: even[k].real - t.real, 
        imag: even[k].imag - t.imag 
      };
    }
    return result;
  }

  // Analyze frequencies and return structured frequency bands
  #analyzeFrequencies(buffer) {
    if (buffer.length < this.#fftSize) return [];
    
    // Apply window function (Hanning) to reduce spectral leakage
    const windowed = buffer.map((sample, i) => 
      sample * (0.5 - 0.5 * Math.cos(2 * Math.PI * i / buffer.length))
    );
    
    // Perform FFT
    const fftResult = this.#fft(windowed);
    
    // Calculate magnitude spectrum
    const magnitudes = fftResult.map(complex => 
      Math.sqrt(complex.real * complex.real + complex.imag * complex.imag)
    );
    
    // Define frequency bands (in Hz)
    const bands = [
      { name: 'bass', min: 20, max: 250 },      // Bass
      { name: 'lowMid', min: 250, max: 500 },   // Low Mid
      { name: 'mid', min: 500, max: 2000 },     // Mid
      { name: 'highMid', min: 2000, max: 4000 }, // High Mid
      { name: 'treble', min: 4000, max: 8000 }, // Treble
      { name: 'ultra', min: 8000, max: 20000 }  // Ultra High
    ];
    
    // Calculate bin frequency resolution
    const binFreq = sampleRate / this.#fftSize;
    
    // Analyze each frequency band
    return bands.map(band => {
      const startBin = Math.floor(band.min / binFreq);
      const endBin = Math.min(Math.floor(band.max / binFreq), magnitudes.length / 2);
      
      let sum = 0;
      let count = 0;
      for (let i = startBin; i < endBin; i++) {
        sum += magnitudes[i];
        count++;
      }
      
      const amplitude = count > 0 ? sum / count : 0;
      
      return {
        name: band.name,
        frequency: { min: band.min, max: band.max },
        amplitude: Math.min(1, amplitude * 10), // Scale and clamp to 0-1
        binRange: { start: startBin, end: endBin }
      };
    });
  }

  // Send data back to the `bios`.
  #report(type, content) {
    this.port.postMessage({ type, content });
  }
}

registerProcessor("speaker-processor", SpeakerProcessor);

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
