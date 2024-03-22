class SpiceProcessor extends AudioWorkletProcessor {
  process(inputs, outputs, parameters) {
    const input = inputs[0];
    const samples = [];

    for (let channel = 0; channel < input.length; channel++) {
      for (let sample = 0; sample < input[channel].length; sample++) {
        samples.push(input[channel][sample]);
      }
    }

    this.port.postMessage(samples); // Send the samples back.
    return true; // Return true to keep the audio worklet running
  }
}

registerProcessor("spice-processor", SpiceProcessor);