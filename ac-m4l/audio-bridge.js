// AC Audio Bridge for Max for Live
// Receives audio samples from jweb and outputs them as signals
// 
// This script processes Float32 audio samples sent from aesthetic.computer's
// speaker worklet via the jweb message bridge.

const Max = require("max-api");

// Audio buffer settings
const BUFFER_SIZE = 2048; // Ring buffer size
const CHUNK_SIZE = 128;   // Output chunk size (matches Max signal vector)

// Ring buffers for left and right channels
let leftBuffer = new Float32Array(BUFFER_SIZE);
let rightBuffer = new Float32Array(BUFFER_SIZE);
let writeIndex = 0;
let readIndex = 0;
let samplesAvailable = 0;

// Sample rate conversion (browser might be 48kHz, Ableton might be 44.1kHz)
let sourceSampleRate = 48000;
let targetSampleRate = 44100;

// Statistics
let totalSamplesReceived = 0;
let underruns = 0;

Max.post("🎛️ AC Audio Bridge loaded");

// Handle incoming samples from jweb
// Format: [numSamples, ...leftSamples, ...rightSamples]
Max.addHandler("samples", (...args) => {
  if (args.length < 1) return;
  
  const numSamples = args[0];
  if (args.length < 1 + numSamples * 2) {
    Max.post("⚠️ Invalid sample data received");
    return;
  }
  
  // Extract left and right samples
  const leftSamples = args.slice(1, 1 + numSamples);
  const rightSamples = args.slice(1 + numSamples, 1 + numSamples * 2);
  
  // Write samples to ring buffer
  for (let i = 0; i < numSamples; i++) {
    leftBuffer[writeIndex] = leftSamples[i];
    rightBuffer[writeIndex] = rightSamples[i];
    writeIndex = (writeIndex + 1) % BUFFER_SIZE;
    
    // Track available samples (don't exceed buffer size)
    if (samplesAvailable < BUFFER_SIZE) {
      samplesAvailable++;
    }
  }
  
  totalSamplesReceived += numSamples;
});

// Handle sample rate info from browser
Max.addHandler("samplerate", (rate) => {
  sourceSampleRate = rate;
  Max.post(`🎛️ Source sample rate: ${rate}Hz`);
});

// Output audio samples to Max signal outlets
// This is called by a metro or qmetro in Max
Max.addHandler("tick", () => {
  // Read samples from ring buffer
  const leftOut = [];
  const rightOut = [];
  
  for (let i = 0; i < CHUNK_SIZE; i++) {
    if (samplesAvailable > 0) {
      leftOut.push(leftBuffer[readIndex]);
      rightOut.push(rightBuffer[readIndex]);
      readIndex = (readIndex + 1) % BUFFER_SIZE;
      samplesAvailable--;
    } else {
      // Underrun - output silence
      leftOut.push(0);
      rightOut.push(0);
      underruns++;
    }
  }
  
  // Output as lists to Max
  Max.outlet("left", leftOut);
  Max.outlet("right", rightOut);
});

// Get buffer status
Max.addHandler("status", () => {
  Max.post(`📊 Buffer: ${samplesAvailable}/${BUFFER_SIZE} samples`);
  Max.post(`📊 Total received: ${totalSamplesReceived}`);
  Max.post(`📊 Underruns: ${underruns}`);
});

// Clear buffer
Max.addHandler("clear", () => {
  leftBuffer.fill(0);
  rightBuffer.fill(0);
  writeIndex = 0;
  readIndex = 0;
  samplesAvailable = 0;
  underruns = 0;
  Max.post("🧹 Buffer cleared");
});

// Report ready
Max.post("🎛️ AC Audio Bridge ready - waiting for samples from jweb");
