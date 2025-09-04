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

  // Memory monitoring
  #memoryCheckCounter = 0;
  #lastMemoryCheck = 0;
  
  // Analysis throttling
  #analysisCounter = 0;
  
  // Performance monitoring for mobile devices
  #performanceMode = 'auto'; // 'auto', 'low', 'disabled'
  #processingTimeHistory = [];
  #lastProcessingTime = 0;

  // Frequency analysis
  #frequencyBandsLeft = [];
  #frequencyBandsRight = [];
  #fftBufferLeft = [];
  #fftBufferRight = [];
  #fftSize = 512; // Reduced from 1024 for better mobile performance

  // Beat detection variables
  #energyHistory = []; // Track energy over time for beat detection
  #energyHistorySize = 20; // Reduced from 43 for better mobile performance
  #beatSensitivity = 1.15; // Lower, more sensitive threshold (was 1.3)
  #adaptiveThreshold = 1.15; // Dynamic threshold that adapts
  #lastBeatTime = 0;
  #beatCooldown = 0.08; // Shorter cooldown for more responsive detection (was 0.1)
  #currentBeat = false;
  #beatStrength = 0;
  #recentEnergyPeaks = []; // Track recent energy peaks for adaptive threshold
  #energyVariance = 0; // Track energy variance for dynamic sensitivity

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
            beat: {
              detected: this.#currentBeat,
              strength: this.#beatStrength,
              timestamp: currentTime
            }
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
        // ⏰ Compute actual sound progress from 0->1 here by looking up the id
        const soundInstance = this.#running[msg.content];
        const progressValue = soundInstance?.progress();
        
        // 🎵 PROGRESS SYNC LOGGING - Critical for timeline sync accuracy
        if (soundInstance && progressValue !== undefined) {
          const playDuration = currentTime - (soundInstance.startTime || 0);
          console.log(`🎵 PROGRESS_REPORT: id=${msg.content}, progress=${progressValue.toFixed(6)}, duration=${playDuration.toFixed(3)}s, time=${currentTime.toFixed(6)}s`);
          
          // Detect if sound has stopped unexpectedly (progress goes to 0 while playing)
          if (progressValue === 0 && playDuration > 0.1) {
            console.warn(`🎵 AUDIO_STOPPED: Sound ${msg.content} progress reset to 0 after ${playDuration.toFixed(3)}s`);
          }
        }
        
        this.#report("progress", {
          id: msg.content,
          progress: progressValue,
          duration: soundInstance?.totalDuration || 0,
          timestamp: currentTime
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
    try {
      const currentTime = this.currentTime; // Get current time from AudioWorkletProcessor
      const startTime = currentTime * 1000; // Use currentTime instead of performance.now()
      
      // Memory monitoring - check every 2 seconds
      this.#memoryCheckCounter++;
      if (this.#memoryCheckCounter >= sampleRate * 2) { // Every 2 seconds
        this.#memoryCheckCounter = 0;
        
        // Performance monitoring - check if we're running slow
        if (this.#processingTimeHistory.length > 0) {
          const avgProcessingTime = this.#processingTimeHistory.reduce((a, b) => a + b, 0) / this.#processingTimeHistory.length;
          
          // If average processing time is > 2ms, disable frequency analysis completely
          if (avgProcessingTime > 2.0 && this.#performanceMode !== 'disabled') {
            console.log('🚨 Disabling frequency analysis due to severe performance issues');
            this.#performanceMode = 'disabled';
          } else if (avgProcessingTime > 1.0 && this.#performanceMode === 'auto') {
            console.log('🐌 Switching to low performance mode for mobile optimization');
            this.#performanceMode = 'low';
          } else if (avgProcessingTime < 0.5 && this.#performanceMode === 'low') {
            console.log('🚀 Switching back to normal performance mode');
            this.#performanceMode = 'auto';
          } else if (avgProcessingTime < 0.3 && this.#performanceMode === 'disabled') {
            console.log('💚 Re-enabling low performance mode');
            this.#performanceMode = 'low';
          }
        }
        
        // Log buffer sizes and memory usage
        console.log(`🧠 Memory Check:
        FFT Buffer Left: ${this.#fftBufferLeft?.length || 0}
        FFT Buffer Right: ${this.#fftBufferRight?.length || 0}
        Energy History: ${this.#energyHistory?.length || 0}
        Queue length: ${this.#queue?.length || 0}
        Running instruments: ${Object.keys(this.#running || {}).length}
        Performance mode: ${this.#performanceMode}
        Avg processing time: ${this.#processingTimeHistory.length > 0 ? (this.#processingTimeHistory.reduce((a, b) => a + b, 0) / this.#processingTimeHistory.length).toFixed(3) : 0}ms`);
          
        // Check for memory leaks in buffers
        if (this.#fftBufferLeft?.length > this.#fftSize * 2) {
          console.warn('⚠️ FFT buffer growing too large!', this.#fftBufferLeft.length);
        }
        if (this.#energyHistory?.length > this.#energyHistorySize * 2) {
          console.warn('⚠️ Energy history growing too large!', this.#energyHistory.length);
        }
      }
      
      const result = this.#processAudio(inputs, outputs, currentTime);
      
      // Track processing time for performance monitoring
      const processingTime = (currentTime * 1000) - startTime;
      this.#processingTimeHistory.push(processingTime);
      if (this.#processingTimeHistory.length > 100) {
        this.#processingTimeHistory.shift(); // Keep only last 100 measurements
      }
      
      return result;
    } catch (error) {
      console.error('🚨 Audio Worklet Error:', error);
      return true; // Keep processor alive
    }
  }

  #processAudio(inputs, outputs, currentTime) {
    
    // 🎵 TIMELINE SYNC LOGGING - Track audio worklet timing for mini timeline sync
    // Log audio timing every few seconds to monitor sync drift
    if (Math.floor(currentTime * 10) % 50 === 0) { // Every 5 seconds
      console.log(`🎵 WORKLET_TIME: ${currentTime.toFixed(6)}s, sampleRate=${sampleRate}, frame=${currentFrame}`);
    }
    
    // 0️⃣ Waveform Tracking
    let waveformLeft = [];
    let waveformRight = [];

    // 1️⃣ Metronome
    const previousTicks = this.#ticks;
    this.#ticks += currentTime - this.#lastTime;
    this.#lastTime = currentTime;

    // const timeTillNextBeat = this.#ticks / this.#bpmInSec;

    if (this.#ticks >= this.#bpmInSec) {
      // 🎵 BEAT SYNC LOGGING - Critical for timeline sync
      console.log(`🎵 BEAT: ${currentTime.toFixed(6)}s, bpm=${this.#bpm}, interval=${this.#bpmInSec.toFixed(3)}s, tick_overflow=${(this.#ticks - this.#bpmInSec).toFixed(6)}s`);
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

    // === FREQUENCY ANALYSIS (OPTIMIZED) ===
    // Add samples to FFT buffers
    this.#fftBufferLeft.push(...output[0]);
    this.#fftBufferRight.push(...output[1]);

    // Keep buffer size manageable (more efficient slice method)
    if (this.#fftBufferLeft.length > this.#fftSize) {
      this.#fftBufferLeft = this.#fftBufferLeft.slice(-this.#fftSize);
      this.#fftBufferRight = this.#fftBufferRight.slice(-this.#fftSize);
    }

    // Reduce analysis frequency to improve performance on mobile devices
    this.#analysisCounter = (this.#analysisCounter || 0) + 1;
    
    // Skip all frequency analysis if performance mode is disabled
    if (this.#performanceMode === 'disabled') {
      return; // Exit early, no frequency analysis
    }
    
    // Adaptive analysis frequency based on performance mode
    let analysisInterval, beatInterval;
    if (this.#performanceMode === 'low') {
      analysisInterval = 32; // Very infrequent analysis for slow devices
      beatInterval = 64;     // Very infrequent beat detection
    } else {
      analysisInterval = 16; // Normal reduced interval
      beatInterval = 32;     // Normal beat detection interval
    }
    
    // Only analyze when buffer is full and at the appropriate interval
    if (this.#fftBufferLeft.length >= this.#fftSize && this.#analysisCounter % analysisInterval === 0) {
      this.#frequencyBandsLeft = this.#analyzeFrequencies(this.#fftBufferLeft);
      this.#frequencyBandsRight = this.#analyzeFrequencies(this.#fftBufferRight);
      
      // Beat detection even less frequently to preserve audio performance
      if (this.#analysisCounter % beatInterval === 0) {
        this.#detectBeats(this.#fftBufferLeft);
      }
    }    return true;
  } // End of #processAudio method

  // === FREQUENCY ANALYSIS METHODS ===
  
  // Optimized FFT implementation for mobile performance
  #fft(buffer) {
    const N = buffer.length;
    if (N <= 1) return buffer.map(x => ({ real: x, imag: 0 }));
    
    // Ensure power of 2 and use smaller size for mobile
    const powerOf2 = Math.min(512, Math.pow(2, Math.floor(Math.log2(N))));
    const input = buffer.slice(0, powerOf2);
    
    // Use iterative FFT instead of recursive for better performance
    const result = input.map(x => ({ real: x, imag: 0 }));
    
    // Bit-reverse permutation
    for (let i = 0; i < powerOf2; i++) {
      let j = 0;
      for (let k = 0; k < Math.log2(powerOf2); k++) {
        j = (j << 1) | ((i >> k) & 1);
      }
      if (j > i) {
        [result[i], result[j]] = [result[j], result[i]];
      }
    }
    
    // Iterative FFT
    for (let len = 2; len <= powerOf2; len *= 2) {
      const w = { real: Math.cos(-2 * Math.PI / len), imag: Math.sin(-2 * Math.PI / len) };
      for (let i = 0; i < powerOf2; i += len) {
        let wn = { real: 1, imag: 0 };
        for (let j = 0; j < len / 2; j++) {
          const u = result[i + j];
          const v = {
            real: result[i + j + len / 2].real * wn.real - result[i + j + len / 2].imag * wn.imag,
            imag: result[i + j + len / 2].real * wn.imag + result[i + j + len / 2].imag * wn.real
          };
          result[i + j] = { real: u.real + v.real, imag: u.imag + v.imag };
          result[i + j + len / 2] = { real: u.real - v.real, imag: u.imag - v.imag };
          const temp = { real: wn.real * w.real - wn.imag * w.imag, imag: wn.real * w.imag + wn.imag * w.real };
          wn = temp;
        }
      }
    }
    return result;
  }

  // Analyze frequencies and return structured frequency bands
  #analyzeFrequencies(buffer) {
    if (buffer.length < this.#fftSize) return [];
    
    // Simplified windowing - use rectangular window for better mobile performance
    const windowedBuffer = buffer.slice(0, this.#fftSize);
    
    // Perform FFT
    const fftResult = this.#fft(windowedBuffer);
    
    // Calculate magnitude spectrum with reduced precision for mobile
    const magnitudes = fftResult.map(complex => 
      Math.sqrt(complex.real * complex.real + complex.imag * complex.imag)
    );
    
    // Define frequency bands (in Hz) - Reduced to 8 bands for better mobile performance
    const bands = [
      { name: 'subBass', min: 20, max: 100 },       // Sub Bass & Bass combined
      { name: 'lowMid', min: 100, max: 400 },       // Low Mid combined
      { name: 'mid', min: 400, max: 1000 },         // Mid range
      { name: 'highMid', min: 1000, max: 2500 },    // High Mid combined  
      { name: 'presence', min: 2500, max: 5000 },   // Presence
      { name: 'treble', min: 5000, max: 10000 },    // Treble combined
      { name: 'air', min: 10000, max: 16000 },      // Air frequencies
      { name: 'ultra', min: 16000, max: 20000 }     // Ultra high combined
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
      
      // Improved scaling approach for better visualization range
      let scaledAmplitude = amplitude;
      
      // Apply simple power scaling for better dynamic range (reverted to original)
      if (scaledAmplitude > 0) {
        scaledAmplitude = Math.pow(scaledAmplitude, 0.7); // Power scaling only
      }
      
      return {
        name: band.name,
        frequency: { min: band.min, max: band.max },
        amplitude: Math.min(0.9, scaledAmplitude), // Reverted to original 90% clamp
        binRange: { start: startBin, end: endBin }
      };
    });
  }

  // Beat detection using energy-based onset detection
  #detectBeats(buffer) {
    if (buffer.length < this.#fftSize) return;
    
    // Calculate current energy (sum of squares in frequency domain)
    const fftData = this.#fft(buffer);
    let currentEnergy = 0;
    
    // Focus on lower frequencies for beat detection (bass/kick drums)
    const bassEndBin = Math.floor(250 * this.#fftSize / sampleRate); // Up to 250Hz
    for (let i = 1; i < Math.min(bassEndBin, fftData.length / 2); i++) {
      const complex = fftData[i] || { real: 0, imag: 0 };
      currentEnergy += complex.real * complex.real + complex.imag * complex.imag;
    }
    
    // Normalize energy
    currentEnergy = Math.sqrt(currentEnergy / bassEndBin);
    
    // Add to energy history
    this.#energyHistory.push(currentEnergy);
    if (this.#energyHistory.length > this.#energyHistorySize) {
      this.#energyHistory.shift();
    }
    
    // Clear expired beat flag (beat lasts 50ms to ensure it's captured by main thread)
    if (this.#currentBeat && currentTime - this.#lastBeatTime > 0.05) {
      this.#currentBeat = false;
      this.#beatStrength = 0;
    }
    
    // Need enough history for comparison
    if (this.#energyHistory.length < this.#energyHistorySize) return;
    
    // Calculate average energy over recent history
    const avgEnergy = this.#energyHistory.reduce((sum, energy) => sum + energy, 0) / this.#energyHistory.length;
    
    // Calculate energy variance for adaptive sensitivity
    const variance = this.#energyHistory.reduce((sum, energy) => sum + Math.pow(energy - avgEnergy, 2), 0) / this.#energyHistory.length;
    this.#energyVariance = Math.sqrt(variance);
    
    // Track recent energy peaks for adaptive threshold
    if (currentEnergy > avgEnergy) {
      this.#recentEnergyPeaks.push(currentEnergy);
      if (this.#recentEnergyPeaks.length > 20) { // Keep last 20 peaks
        this.#recentEnergyPeaks.shift();
      }
    }
    
    // Adaptive threshold based on recent activity and variance
    let adaptiveMultiplier = 1.0;
    if (this.#energyVariance > 0 && avgEnergy > 0) {
      // Smart adaptive logic:
      // High variance = dynamic music = be more sensitive to relative changes
      // High energy + high variance = loud dynamic music = much more sensitive
      
      const normalizedVariance = Math.min(this.#energyVariance / 50, 1.0); // Cap variance impact
      const energyLevel = Math.min(avgEnergy / 30, 1.0); // Normalize energy level
      
      if (avgEnergy > 20) {
        // Loud music: be much more sensitive, focus on relative changes
        adaptiveMultiplier = Math.max(0.4, 0.8 - normalizedVariance * 0.3);
      } else if (normalizedVariance > 0.3) {
        // Dynamic music: moderately more sensitive
        adaptiveMultiplier = Math.max(0.7, 1.1 - normalizedVariance * 0.4);
      } else {
        // Quiet/steady music: standard sensitivity
        adaptiveMultiplier = 1.0 + normalizedVariance * 0.2;
      }
    }
    
    // Update adaptive threshold
    this.#adaptiveThreshold = this.#beatSensitivity * adaptiveMultiplier;
    
    // Also consider local peaks - if we haven't had a beat in a while, be more sensitive
    const timeSinceLastBeat = currentTime - this.#lastBeatTime;
    let timeBasedSensitivity = 1.0;
    if (timeSinceLastBeat > 0.3) { // If no beat for 300ms, get more sensitive
      timeBasedSensitivity = 1.0 + Math.min(0.4, (timeSinceLastBeat - 0.3) * 0.8); // Increase sensitivity up to 40%
    }
    
    // Final threshold: lower values = easier to trigger beats
    const finalThreshold = this.#adaptiveThreshold / timeBasedSensitivity;
    
    // Check if current energy is significantly higher than average
    const energyRatio = avgEnergy > 0 ? currentEnergy / avgEnergy : 0;
    
    // Debug logging much less frequently to reduce performance impact
    if (Math.floor(currentTime * 1) % 8 === 0 && this.#energyHistory.length >= this.#energyHistorySize) {
      // console.log(`🔍 Smart Adaptive Beat Detection:
      //   Current energy: ${currentEnergy.toFixed(4)}
      //   Average energy: ${avgEnergy.toFixed(4)}
      //   Energy variance: ${this.#energyVariance.toFixed(4)}
      //   Energy ratio: ${energyRatio.toFixed(4)}
      //   Base threshold: ${this.#beatSensitivity}
      //   Adaptive multiplier: ${adaptiveMultiplier.toFixed(3)}
      //   Time sensitivity boost: ${timeBasedSensitivity.toFixed(3)}
      //   Final threshold: ${finalThreshold.toFixed(3)}
      //   Time since last beat: ${timeSinceLastBeat.toFixed(4)}s
      //   Would trigger: ${energyRatio > finalThreshold && timeSinceLastBeat > this.#beatCooldown}`);
    }
    
    if (energyRatio > finalThreshold && timeSinceLastBeat > this.#beatCooldown) {
      this.#currentBeat = true;
      this.#beatStrength = Math.min(1.0, (energyRatio - finalThreshold) / 2.0); // 0-1 range
      this.#lastBeatTime = currentTime;
      // console.log(`🥁 SMART BEAT! Strength: ${this.#beatStrength.toFixed(4)}, Ratio: ${energyRatio.toFixed(4)}, Threshold: ${finalThreshold.toFixed(3)}`);
    }
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
