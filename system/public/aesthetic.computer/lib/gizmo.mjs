// ⚙️ Gizmos
// These are designed to work well with 'sim' and run off of ticks.

// TODO: Why does this need both completed AND flipped callbacks?  23.01.29.13.08
export class Hourglass {
  ticks = 0;
  max = 1;
  complete = false;
  flips = 0n;
  #completed;
  #flipped;
  #autoFlip = false;
  #every; // Callback that fires every frame.

  constructor(
    max = 1,
    { completed, flipped, every, autoFlip = false } = {},
    startingTicks = 0,
  ) {
    this.max = max;
    this.ticks = startingTicks;
    this.#autoFlip = autoFlip;
    this.#completed = completed;
    this.#flipped = flipped;
    this.#every = every;
  }

  step() {
    if (this.complete === true) return console.log("⌛ Already complete.");

    this.ticks += 1;

    this.#every?.();

    if (this.ticks >= this.max) {
      this.complete = true;
      this.#completed?.(this.flips);
      if (this.#autoFlip) this.flip();
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
