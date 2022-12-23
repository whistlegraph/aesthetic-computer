// ⚙️ Gizmos
// These are designed to work well with 'sim' and run off of ticks.

export class Hourglass {
  ticks = 0;
  max = 1;
  complete = false;
  #completedCb;
  #flippedCb;
  #autoFlip = false;

  constructor(
    max = 1,
    { completed, flipped, autoFlip = false } = {},
    startingTicks = 0
  ) {
    this.max = max;
    this.ticks = startingTicks;
    this.#autoFlip = autoFlip;
    this.#completedCb = completed;
    this.#flippedCb = flipped;
  }

  step() {
    if (this.complete === true) return console.log("⌛ Already complete.");

    this.ticks += 1;
    if (this.ticks === this.max) {
      this.complete = true;
      this.#completedCb?.(this);
      if (this.#autoFlip) this.flip();
    }
  }

  get progress() {
    return this.ticks / this.max;
  }

  flip() {
    this.ticks = 0;
    this.complete = false;
    this.#flippedCb?.(...arguments);
  }
}
