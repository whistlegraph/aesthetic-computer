// 🎵 Audio Analyzer - Shared module for consistent audio analysis
// This module provides the exact same algorithms used in speaker.mjs (AudioWorklet)
// so that both the AC runtime and kidlisp.com editor produce identical values.
//
// Usage in main thread (kidlisp.com editor):
//   const analyzer = new AudioAnalyzer(audioContext.sampleRate);
//   const audioData = analyzer.analyzeAudioData(float32Array);
//   // audioData = { amp, leftAmp, rightAmp, beat, kick, frequencies }
//
// The speaker.mjs AudioWorklet uses these same algorithms internally.

export const AUDIO_ANALYZER_VERSION = "1.0.0";

// Configuration constants (same as speaker.mjs)
export const FFT_SIZE = 512;
export const ENERGY_HISTORY_SIZE = 20;
export const BEAT_SENSITIVITY = 1.15;
export const BEAT_COOLDOWN = 0.08; // seconds

// Frequency band definitions (same as speaker.mjs)
export const FREQUENCY_BANDS = [
  { name: 'subBass', min: 20, max: 100 },
  { name: 'lowMid', min: 100, max: 400 },
  { name: 'mid', min: 400, max: 1000 },
  { name: 'highMid', min: 1000, max: 2500 },
  { name: 'presence', min: 2500, max: 5000 },
  { name: 'treble', min: 5000, max: 10000 },
  { name: 'air', min: 10000, max: 16000 },
  { name: 'ultra', min: 16000, max: 20000 }
];

/**
 * Iterative FFT implementation - same as speaker.mjs
 * @param {Float32Array|number[]} buffer - Audio samples
 * @returns {Array<{real: number, imag: number}>} Complex FFT result
 */
export function fft(buffer) {
  const N = buffer.length;
  if (N <= 1) return Array.from(buffer).map(x => ({ real: x, imag: 0 }));
  
  // Ensure power of 2 and use smaller size for consistency
  const powerOf2 = Math.min(FFT_SIZE, Math.pow(2, Math.floor(Math.log2(N))));
  const input = Array.from(buffer).slice(0, powerOf2);
  
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

/**
 * Analyze frequencies and return structured frequency bands - same as speaker.mjs
 * @param {Float32Array|number[]} buffer - Audio samples (should be FFT_SIZE length)
 * @param {number} sampleRate - Audio sample rate
 * @returns {Array<{name: string, frequency: {min: number, max: number}, amplitude: number}>}
 */
export function analyzeFrequencies(buffer, sampleRate) {
  if (buffer.length < FFT_SIZE) return [];
  
  // Simplified windowing - use rectangular window for consistency
  const windowedBuffer = Array.from(buffer).slice(0, FFT_SIZE);
  
  // Perform FFT
  const fftResult = fft(windowedBuffer);
  
  // Calculate magnitude spectrum
  const magnitudes = fftResult.map(complex => 
    Math.sqrt(complex.real * complex.real + complex.imag * complex.imag)
  );
  
  // Calculate bin frequency resolution
  const binFreq = sampleRate / FFT_SIZE;
  
  // Analyze each frequency band
  return FREQUENCY_BANDS.map(band => {
    const startBin = Math.floor(band.min / binFreq);
    const endBin = Math.min(Math.floor(band.max / binFreq), magnitudes.length / 2);
    
    let sum = 0;
    let count = 0;
    for (let i = startBin; i < endBin; i++) {
      sum += magnitudes[i];
      count++;
    }
    
    const amplitude = count > 0 ? sum / count : 0;
    
    // Apply power scaling for better dynamic range (same as speaker.mjs)
    let scaledAmplitude = amplitude;
    if (scaledAmplitude > 0) {
      scaledAmplitude = Math.pow(scaledAmplitude, 0.7);
    }
    
    return {
      name: band.name,
      frequency: { min: band.min, max: band.max },
      amplitude: Math.min(0.9, scaledAmplitude), // 90% clamp
      binRange: { start: startBin, end: endBin }
    };
  });
}

/**
 * Calculate peak amplitude from audio samples
 * @param {Float32Array|number[]} samples - Audio samples
 * @returns {number} Peak amplitude (0-1)
 */
export function calculateAmplitude(samples) {
  let peak = 0;
  for (let i = 0; i < samples.length; i++) {
    const abs = Math.abs(samples[i]);
    if (abs > peak) peak = abs;
  }
  return peak;
}

/**
 * Audio Analyzer class - maintains state for beat detection
 * Provides the same analysis as speaker.mjs AudioWorkletProcessor
 */
export class AudioAnalyzer {
  #sampleRate;
  #fftBufferLeft = [];
  #fftBufferRight = [];
  #energyHistory = [];
  #energyHistorySize = ENERGY_HISTORY_SIZE;
  #beatSensitivity = BEAT_SENSITIVITY;
  #beatCooldown = BEAT_COOLDOWN;
  #lastBeatTime = 0;
  #currentBeat = false;
  #beatStrength = 0;
  #adaptiveThreshold = BEAT_SENSITIVITY;
  #energyVariance = 0;
  #recentEnergyPeaks = [];
  #lastAnalysisTime = 0;
  
  constructor(sampleRate = 44100) {
    this.#sampleRate = sampleRate;
  }
  
  /**
   * Get the current time in seconds (for beat detection timing)
   * Override this if you need custom timing
   */
  getCurrentTime() {
    return performance.now() / 1000;
  }
  
  /**
   * Analyze stereo audio data and return all audio parameters
   * @param {Float32Array|number[]} leftChannel - Left channel samples
   * @param {Float32Array|number[]|null} rightChannel - Right channel samples (optional, defaults to left)
   * @returns {{
   *   amp: number,
   *   leftAmp: number,
   *   rightAmp: number,
   *   beat: number,
   *   kick: number,
   *   frequencies: {left: Array, right: Array},
   *   beatStrength: number
   * }}
   */
  analyze(leftChannel, rightChannel = null) {
    const currentTime = this.getCurrentTime();
    rightChannel = rightChannel || leftChannel;
    
    // Calculate amplitudes (peak detection, same as speaker.mjs)
    const leftAmp = calculateAmplitude(leftChannel);
    const rightAmp = calculateAmplitude(rightChannel);
    const amp = (leftAmp + rightAmp) / 2;
    
    // Add samples to FFT buffers
    this.#fftBufferLeft.push(...leftChannel);
    this.#fftBufferRight.push(...rightChannel);
    
    // Keep buffer size manageable
    if (this.#fftBufferLeft.length > FFT_SIZE) {
      this.#fftBufferLeft = this.#fftBufferLeft.slice(-FFT_SIZE);
      this.#fftBufferRight = this.#fftBufferRight.slice(-FFT_SIZE);
    }
    
    // Analyze frequencies
    let frequencyBandsLeft = [];
    let frequencyBandsRight = [];
    if (this.#fftBufferLeft.length >= FFT_SIZE) {
      frequencyBandsLeft = analyzeFrequencies(this.#fftBufferLeft, this.#sampleRate);
      frequencyBandsRight = analyzeFrequencies(this.#fftBufferRight, this.#sampleRate);
    }
    
    // Beat detection (same algorithm as speaker.mjs)
    this.#detectBeats(this.#fftBufferLeft, currentTime);
    
    // Scale amplitudes to 0-10 range for KidLisp (same as AC runtime)
    const scaledAmp = amp * 10;
    const scaledLeftAmp = leftAmp * 10;
    const scaledRightAmp = rightAmp * 10;
    
    return {
      amp: scaledAmp,
      leftAmp: scaledLeftAmp,
      rightAmp: scaledRightAmp,
      beat: this.#currentBeat ? 1 : 0,
      kick: this.#currentBeat ? 1 : 0, // alias for beat
      frequencies: {
        left: frequencyBandsLeft,
        right: frequencyBandsRight
      },
      beatStrength: this.#beatStrength
    };
  }
  
  /**
   * Beat detection using energy-based onset detection - same as speaker.mjs
   * @private
   */
  #detectBeats(buffer, currentTime) {
    if (buffer.length < FFT_SIZE) return;
    
    // Calculate current energy (sum of squares in frequency domain)
    const fftData = fft(buffer);
    let currentEnergy = 0;
    
    // Focus on lower frequencies for beat detection (bass/kick drums)
    const bassEndBin = Math.floor(250 * FFT_SIZE / this.#sampleRate);
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
    
    // Clear expired beat flag (beat lasts 50ms)
    if (this.#currentBeat && currentTime - this.#lastBeatTime > 0.05) {
      this.#currentBeat = false;
      this.#beatStrength = 0;
    }
    
    // Need enough history for comparison
    if (this.#energyHistory.length < this.#energyHistorySize) return;
    
    // Calculate average energy over recent history
    const avgEnergy = this.#energyHistory.reduce((sum, e) => sum + e, 0) / this.#energyHistory.length;
    
    // Calculate energy variance for adaptive sensitivity
    const variance = this.#energyHistory.reduce((sum, e) => sum + Math.pow(e - avgEnergy, 2), 0) / this.#energyHistory.length;
    this.#energyVariance = Math.sqrt(variance);
    
    // Track recent energy peaks for adaptive threshold
    if (currentEnergy > avgEnergy) {
      this.#recentEnergyPeaks.push(currentEnergy);
      if (this.#recentEnergyPeaks.length > 20) {
        this.#recentEnergyPeaks.shift();
      }
    }
    
    // Adaptive threshold based on recent activity and variance
    let adaptiveMultiplier = 1.0;
    if (this.#energyVariance > 0 && avgEnergy > 0) {
      const normalizedVariance = Math.min(this.#energyVariance / 50, 1.0);
      
      if (avgEnergy > 20) {
        // Loud music: be much more sensitive
        adaptiveMultiplier = Math.max(0.4, 0.8 - normalizedVariance * 0.3);
      } else if (normalizedVariance > 0.3) {
        // Dynamic music: moderately more sensitive
        adaptiveMultiplier = Math.max(0.7, 1.1 - normalizedVariance * 0.4);
      } else {
        // Quiet/steady music: standard sensitivity
        adaptiveMultiplier = 1.0 + normalizedVariance * 0.2;
      }
    }
    
    this.#adaptiveThreshold = this.#beatSensitivity * adaptiveMultiplier;
    
    // Time-based sensitivity boost
    const timeSinceLastBeat = currentTime - this.#lastBeatTime;
    let timeBasedSensitivity = 1.0;
    if (timeSinceLastBeat > 0.3) {
      timeBasedSensitivity = 1.0 + Math.min(0.4, (timeSinceLastBeat - 0.3) * 0.8);
    }
    
    const finalThreshold = this.#adaptiveThreshold / timeBasedSensitivity;
    const energyRatio = avgEnergy > 0 ? currentEnergy / avgEnergy : 0;
    
    // Detect beat
    if (energyRatio > finalThreshold && timeSinceLastBeat > this.#beatCooldown) {
      this.#currentBeat = true;
      this.#beatStrength = Math.min(1.0, (energyRatio - finalThreshold) / 2.0);
      this.#lastBeatTime = currentTime;
    }
  }
  
  /**
   * Reset the analyzer state
   */
  reset() {
    this.#fftBufferLeft = [];
    this.#fftBufferRight = [];
    this.#energyHistory = [];
    this.#lastBeatTime = 0;
    this.#currentBeat = false;
    this.#beatStrength = 0;
    this.#recentEnergyPeaks = [];
  }
}

export default AudioAnalyzer;
