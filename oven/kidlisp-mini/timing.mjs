// KidLisp Mini Timing Engine
// Handles Ns... (repeating) and Ns (one-shot) interpolation forms
// ~80 lines, produces ~1-2 KB minified

class TimingEngine {
  constructor(seed = null) {
    this.frame = 0;
    this.startTime = Date.now();
    this.seed = seed;
    this.durationMs = {};
  }

  tick() {
    this.frame++;
  }

  getElapsedMs() {
    return Date.now() - this.startTime;
  }

  // Evaluate a repeating timing form: (Ns... startVal endVal)
  // Returns interpolated value from 0-1 within the cycle
  evalRepeatingTiming(durationMs, startVal, endVal) {
    const elapsed = this.getElapsedMs();
    const cycle = elapsed % durationMs;
    const progress = cycle / durationMs;

    if (typeof startVal === 'number' && typeof endVal === 'number') {
      return startVal + (endVal - startVal) * progress;
    }

    // If start/end are colors or other types, return start on first half, end on second
    return progress < 0.5 ? startVal : endVal;
  }

  // Evaluate a one-shot timing form: (Ns startVal endVal)
  // Returns interpolated value from 0-1, clamped at end
  evalOnceTiming(durationMs, startVal, endVal) {
    const elapsed = this.getElapsedMs();
    const progress = Math.min(1, elapsed / durationMs);

    if (typeof startVal === 'number' && typeof endVal === 'number') {
      return startVal + (endVal - startVal) * progress;
    }

    return progress < 1 ? startVal : endVal;
  }

  // Random walk / wiggle: oscillate around a center value
  wiggle(center, amplitude, frequency = 1) {
    const phase = (this.frame * frequency / 60) % (Math.PI * 2);
    return center + Math.sin(phase) * amplitude;
  }

  // Deterministic random based on seed + frame
  seededRandom(seed = this.seed, max = 1) {
    if (seed === null) return Math.random() * max;

    // Simple seeded PRNG
    const x = Math.sin(seed + this.frame) * 10000;
    const random = x - Math.floor(x);
    return random * max;
  }
}

export { TimingEngine };
