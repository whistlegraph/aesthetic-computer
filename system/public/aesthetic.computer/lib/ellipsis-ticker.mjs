export class EllipsisTicker {
  #hourglass;
  #ellipsisDots = 0;

  constructor(Hourglass) {
    this.#hourglass = new Hourglass(30, {
      completed: () => {
        this.#ellipsisDots = (this.#ellipsisDots + 1) % 4;
      },
      autoFlip: true,
    });
  }

  // Get the current ellipses with a padded end.
  text(repeat) {
    let ellipsis = "";
    repeat(this.#ellipsisDots, () => (ellipsis += "."));
    return ellipsis.padEnd(3, " ");
  }

  sim() {
    this.#hourglass.step();
  }
}
