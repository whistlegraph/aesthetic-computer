// 🎤 Microphone

// Note: This currently assumes a stereo input using only the first channel, then
//       outputs to all other channels.
//
//       💡 It's preconfigured for a Focusrite with 1/2 active channels.

// TODO: Add effects that can be toggled or controlled live from a disk.
//       Bitcrusher: https://github.com/jaz303/bitcrusher

const { abs } = Math;

class Microphone extends AudioWorkletProcessor {
  currentAmplitude = 0;
  currentWaveform = [];

  constructor(options) {
    if (options.debug) console.log("🔊 Sound Synthesis Worklet Started");

    super();

    this.port.onmessage = (e) => {
      const msg = e.data;

      if (msg.type === "get-amplitude") {
        this.port.postMessage({
          type: "amplitude",
          content: this.currentAmplitude,
        });
      }

      if (msg.type === "get-waveform") {
        this.port.postMessage({
          type: "waveform",
          content: this.currentWaveform,
        });
      }
    };
  }

  process(inputs, outputs) {
    let amp = 0;
    let waveform = [];
    const micChannel = inputs[0][0] || [];
    for (let s = 0; s < micChannel.length; s += 1) {
      outputs[0][0][s] = micChannel[s];
      outputs[0][1][s] = micChannel[s];
      amp += abs(micChannel[s]);
      if (s % 8 === 0) waveform.push(micChannel[s]); // Only capture every 8th value. (Usually 16)
    }
    this.currentAmplitude = amp / micChannel.length;
    this.currentWaveform = waveform.slice(0); // Capture a quantized sample.
    return true;
  }
}

registerProcessor("microphone-processor", Microphone);