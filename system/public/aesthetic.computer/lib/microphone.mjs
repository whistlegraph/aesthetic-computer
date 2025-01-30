// 🎤 Microphone

// Note: This currently assumes a stereo input using only the first channel, then
//       outputs to all other channels.
//
//       💡 It's preconfigured for a Focusrite with 1/2 active channels.

// TODO: Add effects that can be toggled or controlled live from a disk.
//       Bitcrusher: https://github.com/jaz303/bitcrusher

const { abs } = Math;
let pitch;

class Microphone extends AudioWorkletProcessor {
  currentAmplitude = 0;
  currentWaveform = [];
  currentRecording; // Store an active sample to record.
  recording = false;

  constructor(options) {
    if (options.debug) console.log("🔊 Sound Synthesis Worklet Started");

    super();

    this.port.onmessage = (e) => {
      const msg = e.data;

      if (msg.type === "record:start") {
        this.recording = true; // Start keeping track of waveform data...
        this.currentRecording = [];
      }

      if (msg.type === "record:stop") {
        this.recording = false;
        
        // Find the maximum absolute value for normalization
        const max = Math.max(...this.currentRecording.map(Math.abs)) || 1;
        const waveform = this.currentRecording.map(sample => sample / max);
      
        // Define a threshold for what counts as "dead" silence (adjust as needed)
        const threshold = 0.01;
      
        // Find the first non-silent sample
        let start = 0;
        while (start < waveform.length && Math.abs(waveform[start]) < threshold) {
          start++;
        }
      
        // Find the last non-silent sample
        let end = waveform.length - 1;
        while (end > start && Math.abs(waveform[end]) < threshold) {
          end--;
        }
      
        // Trim the silent parts
        const trimmedRecording = waveform.slice(start, end + 1);
        
        this.port.postMessage({
          type: "recording:complete",
          content: { recording: trimmedRecording },
        });
        this.currentRecording = null;
      }

      if (msg.type === "get-amplitude") {
        this.port.postMessage({
          type: "amplitude",
          content: this.currentAmplitude,
        });
      }
      
      
      if (msg.type === "get-waveform") {
        const waveform = this.currentWaveform;
        const max = Math.max(...waveform.map(Math.abs)) || 1; // Prevent division by zero
    
        const normalizedWaveform = waveform.map(sample => sample / max);
  
        this.port.postMessage({
          type: "waveform",
          content: normalizedWaveform,
        });
      }

      if (msg.type === "get-pitch") {
        this.port.postMessage({
          type: "pitch",
          content: this.currentPitch,
        });
      }
    };
  }

  process(inputs, outputs) {
    let amp = 0;
    let waveform = [];
    const mic = inputs[0][0] || [];
    for (let s = 0; s < mic.length; s += 1) {
      outputs[0][0][s] = mic[s];
      outputs[0][1][s] = mic[s];
      if (this.recording) this.currentRecording.push(mic[s]);
      amp = abs(mic[s]) > amp ? abs(mic[s]) : amp; // Store maximum amplitude.
      if (s % 8 === 0) waveform.push(mic[s]); // Only capture every 8th value. (Usually 16)
    }
    // this.currentAmplitude = amp / mic.length;
    this.currentAmplitude = amp;
    this.currentWaveform = waveform.slice(0); // Capture a quantized sample.
    this.currentPitch = pitch(mic, sampleRate); // TODO: Make this conditional?
    return true;
  }
}

// 🐦
// Pitch recognition via: https://github.com/bojan88/WASM-vs-JS-Pitch-detector
{
  const LOWER_PITCH_CUTOFF = 10.0;
  const SMALL_CUTOFF = 0.5;
  const CUTOFF = 0.93;

  function getPitch(buffer, sampleRate) {
    const nsdf = normalizedSquareDifference(buffer);
    const maxPositions = peakPicking(nsdf);
    const estimates = [];

    let highestAmplitude = Number.MIN_SAFE_INTEGER;

    for (const i of maxPositions) {
      highestAmplitude = Math.max(highestAmplitude, nsdf[i]);
      if (nsdf[i] > SMALL_CUTOFF) {
        let est = parabolicInterpolation(nsdf, i);
        estimates.push(est);
        highestAmplitude = Math.max(highestAmplitude, est[1]);
      }
    }

    if (estimates.length === 0) {
      return null;
    }

    const actualCutoff = CUTOFF * highestAmplitude;
    let period = 0.0;

    for (const est of estimates) {
      if (est[1] >= actualCutoff) {
        period = est[0];
        break;
      }
    }

    const pitchEst = sampleRate / period;

    return pitchEst > LOWER_PITCH_CUTOFF ? pitchEst : -1;
  }

  pitch = getPitch; // Set for the outer context.

  function peakPicking(nsdf) {
    const maxPositions = [];
    let pos = 0;
    let curMaxPos = 0;
    const len = nsdf.length;

    while (pos < (len - 1) / 3 && nsdf[pos] > 0.0) {
      pos++;
    }
    while (pos < len - 1 && nsdf <= 0.0) {
      pos++;
    }

    if (pos === 0) {
      pos = 1;
    }

    while (pos < len - 1) {
      if (nsdf[pos] < nsdf[pos - 1] && nsdf[pos] >= nsdf[pos + 1]) {
        if (curMaxPos === 0) {
          curMaxPos = pos;
        } else if (nsdf[pos] > nsdf[curMaxPos]) {
          curMaxPos = pos;
        }
      }

      pos++;

      if (pos < len - 1 && nsdf[pos] <= 0.0) {
        if (curMaxPos > 0) {
          maxPositions.push(curMaxPos);
          curMaxPos = 0;
        }
        while (pos < len - 1 && nsdf <= 0.0) {
          pos++;
        }
      }
    }

    if (curMaxPos > 0) {
      maxPositions.push(curMaxPos);
    }

    return maxPositions;
  }

  function normalizedSquareDifference(buffer) {
    const len = buffer.length;
    const nsdf = new Array(len).fill(0.0);

    for (let tau = 0; tau < len; tau++) {
      let acf = 0.0;
      let divisorM = 0.0;

      for (let i = 0; i < len - tau; i++) {
        acf += buffer[i] * buffer[i + tau];
        let el1 = buffer[i];
        let p1 = Math.pow(el1, 2);
        let el2 = buffer[i + tau];
        let p2 = Math.pow(el2, 2);
        divisorM += p1 + p2;
      }

      nsdf[tau] = (2.0 * acf) / divisorM;
    }

    return nsdf;
  }

  function parabolicInterpolation(nsdf, tau) {
    const nsdfa = nsdf[tau - 1];
    const nsdfb = nsdf[tau];
    const nsdfc = nsdf[tau + 1];
    const bottom = nsdfc + nsdfa - 2.0 * nsdfb;

    if (bottom === 0.0) {
      return [tau, nsdfb];
    } else {
      let delta = nsdfa - nsdfc;
      return [
        tau + delta / (2.0 * bottom),
        nsdfb - (delta * delta) / (8.0 * bottom),
      ];
    }
  }
}

registerProcessor("microphone-processor", Microphone);
