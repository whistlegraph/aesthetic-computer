// A bounded stereo queue. Pausing clears queued sound before the next card plays.
class NESAudio extends AudioWorkletProcessor {
  constructor() {
    super(); this.samples = new Float32Array(16384); this.read = 0; this.write = 0;
    this.port.onmessage = ({ data }) => {
      if (data === 'clear') { this.read = this.write = 0; return; }
      for (const sample of data) { this.samples[this.write++ % this.samples.length] = sample; }
      if (this.write - this.read > this.samples.length) this.read = this.write - this.samples.length;
    };
  }
  process(inputs, outputs) {
    const [left, right] = outputs[0];
    for (let i = 0; i < left.length; i++) {
      const available = this.write - this.read >= 2;
      left[i] = available ? this.samples[this.read++ % this.samples.length] : 0;
      right[i] = available ? this.samples[this.read++ % this.samples.length] : 0;
    }
    return true;
  }
}
registerProcessor('mime-nes-audio', NESAudio);
