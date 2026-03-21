// ⚙️ Gizmos
// These are designed to work well with 'sim' and run off of ticks.

import { Ticker } from "./ticker.mjs";

// A repeatable timer with callbacks.
export class Hourglass {
  ticks = 0;
  max = 1;
  progress = 0;
  complete = false;
  flips = 0n;
  #completed;
  #flipped;
  autoFlip = false;
  #every; // Callback that fires every frame.

  constructor(
    max = 1,
    { completed, flipped, every, autoFlip = false } = {},
    startingTicks = 0,
  ) {
    this.max = max;
    this.ticks = startingTicks;
    this.autoFlip = autoFlip;
    this.#completed = completed;
    this.#flipped = flipped;
    this.#every = every;
  }

  step() {
    if (this.complete === true) return console.log("⌛ Already complete.");

    this.ticks += 1;

    this.#every?.();

    this.progress = this.ticks / this.max;

    if (this.ticks >= this.max) {
      this.complete = true;
      this.progress = 1;
      this.#completed?.(this.flips);
      if (this.autoFlip) this.flip();
    }
  }

  get progress() {
    return this.ticks / this.max;
  }

  flip() {
    this.flips += 1n;
    this.ticks = 0;
    this.complete = false;
    this.#flipped?.(this.flips, ...arguments);
  }
}

// An animated "..." string.
export class EllipsisTicker {
  #ellipsisDots = 0;
  #lastUpdateTime = 0;
  #updateInterval = 500; // 500ms = 0.5 seconds, locked to 30fps equivalent

  constructor(options = {}) {
    this.#updateInterval = options.interval || 500; // Allow customization
  }

  // Get the current ellipses with a padded end.
  text(repeat, opts) {
    let ellipsis = "";
    repeat(this.#ellipsisDots, () => (ellipsis += "."));
    return opts?.pad === false ? ellipsis : ellipsis.padEnd(3, " ");
  }

  // Update the ticker using clock time instead of frame counting
  update(clockTime) {
    if (!clockTime) return; // No clock available
    
    const currentTime = clockTime.getTime();
    
    if (this.#lastUpdateTime === 0) {
      this.#lastUpdateTime = currentTime;
      return;
    }
    
    if (currentTime - this.#lastUpdateTime >= this.#updateInterval) {
      this.#ellipsisDots = (this.#ellipsisDots + 1) % 4;
      this.#lastUpdateTime = currentTime;
    }
  }

  // Keep the old sim() method for backward compatibility, but it won't do anything
  sim() {
    // This method is deprecated - use update(clockTime) instead
    // Keeping it for backward compatibility to avoid breaking existing code
  }
}

// Re-export Ticker for convenience
export { Ticker };
