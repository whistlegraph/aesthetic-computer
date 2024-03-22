// ⚙️ Gizmos
// These are designed to work well with 'sim' and run off of ticks.

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
  #hourglass;
  #ellipsisDots = 0;

  constructor() {
    this.#hourglass = new Hourglass(30, {
      completed: () => {
        this.#ellipsisDots = (this.#ellipsisDots + 1) % 4;
      },
      autoFlip: true,
    });
  }

  // Get the current ellipses with a padded end.
  text(repeat, opts) {
    let ellipsis = "";
    repeat(this.#ellipsisDots, () => (ellipsis += "."));
    return opts?.pad === false ? ellipsis : ellipsis.padEnd(3, " ");
  }

  sim() {
    this.#hourglass.step();
  }
}
