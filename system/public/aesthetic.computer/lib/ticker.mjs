// Ticker, 2025.6.26.xx.xx
// A reusable scrolling text ticker class for Aesthetic Computer pieces.

export class Ticker {
  #offset = 0;
  #speed = 1;
  #separator = " - ";
  #text = "";
  #textWidth = 0;
  #separatorWidth = 0;
  #cycleWidth = 0;
  #api = null;

  constructor(text = "", options = {}) {
    this.#text = text;
    this.#speed = options.speed || 1;
    this.#separator = options.separator || " - ";
  }

  // Set the text content for the ticker
  setText(text) {
    this.#text = text;
    this.#updateMeasurements();
    return this;
  }

  // Set the scrolling speed (pixels per frame)
  setSpeed(speed) {
    this.#speed = speed;
    return this;
  }

  // Set the separator between text repetitions
  setSeparator(separator) {
    this.#separator = separator;
    this.#updateMeasurements();
    return this;
  }

  // Update text measurements when text or separator changes
  #updateMeasurements() {
    if (!this.#api || !this.#text) return;
    
    const textMeasurement = this.#api.text.box(this.#text);
    const separatorMeasurement = this.#api.text.box(this.#separator);
    this.#textWidth = textMeasurement.box.width;
    this.#separatorWidth = separatorMeasurement.box.width;
    this.#cycleWidth = this.#textWidth + this.#separatorWidth;
  }

  // Update the ticker animation (call this in your paint function)
  update(api) {
    this.#api = api;
    
    // Update measurements if this is the first update or API changed
    if (this.#cycleWidth === 0) {
      this.#updateMeasurements();
    }

    // Advance the ticker offset
    this.#offset += this.#speed;

    // Reset when one complete cycle has passed
    if (this.#offset >= this.#cycleWidth) {
      this.#offset = 0;
    }
  }

  // Render the ticker at the specified position
  paint(api, x, y, options = {}) {
    if (!this.#text) return;

    this.#api = api;
    
    // Update measurements if needed
    if (this.#cycleWidth === 0) {
      this.#updateMeasurements();
    }

    const displayWidth = options.width || api.screen.width;

    // Calculate how many complete cycles we need to fill the display area plus buffer
    const numCycles = Math.ceil((displayWidth + this.#cycleWidth) / this.#cycleWidth) + 1;

    // Render multiple copies with precise positioning
    for (let i = 0; i < numCycles; i++) {
      const baseX = x + i * this.#cycleWidth - this.#offset;

      // Only render if this cycle might be visible
      if (baseX > -this.#cycleWidth && baseX < x + displayWidth + this.#cycleWidth) {
        // Apply ink settings if provided in options
        if (options.color) api.ink(options.color, options.alpha || 255);
        
        api.write(this.#text, { x: baseX, y });
        api.write(this.#separator, { x: baseX + this.#textWidth, y });
      }
    }
  }

  // Get current offset (useful for debugging or synchronization)
  getOffset() {
    return this.#offset;
  }

  // Set the offset directly (useful for manual control)
  setOffset(offset) {
    this.#offset = offset; // Allow negative values for bidirectional scrubbing
    return this;
  }

  // Reset the ticker to the beginning
  reset() {
    this.#offset = 0;
    return this;
  }

  // Get the cycle width (useful for calculations)
  getCycleWidth() {
    return this.#cycleWidth;
  }
}

// Factory function for easier creation
export function ticker(text, options) {
  return new Ticker(text, options);
}
