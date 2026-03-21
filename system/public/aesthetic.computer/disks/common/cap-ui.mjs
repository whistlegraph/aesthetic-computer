// Cap UI - Shared utilities for snap.mjs and cap.mjs camera pieces
// Common UI components for photo/video capture workflow

const { floor, min, max, sin, PI } = Math;

/**
 * Sound effects for cap/snap interactions
 */
export const sounds = {
  down(sound) {
    sound.synth({
      type: "sine",
      tone: 600,
      attack: 0.1,
      decay: 0.99,
      volume: 0.75,
      duration: 0.001,
    });
  },

  push(sound) {
    sound.synth({
      type: "sine",
      tone: 800,
      attack: 0.1,
      decay: 0.99,
      volume: 0.75,
      duration: 0.005,
    });
  },

  shutter(sound) {
    // Camera shutter sound for snaps
    sound.synth({
      type: "noise",
      attack: 0.001,
      decay: 0.15,
      volume: 0.4,
      duration: 0.08,
    });
    sound.synth({
      type: "sine",
      tone: 2000,
      attack: 0.001,
      decay: 0.3,
      volume: 0.2,
      duration: 0.03,
    });
  },

  recordStart(sound) {
    // Beep to indicate recording started
    sound.synth({
      type: "sine",
      tone: 1200,
      attack: 0.01,
      decay: 0.3,
      volume: 0.4,
      duration: 0.1,
    });
  },

  recordStop(sound) {
    // Double beep to indicate recording stopped
    sound.synth({
      type: "sine",
      tone: 800,
      attack: 0.01,
      decay: 0.5,
      volume: 0.3,
      duration: 0.08,
    });
  },
};

/**
 * Circular record/capture button with optional pulsing animation
 */
export class CaptureButton {
  constructor({ x, y, radius = 24, type = "snap" }) {
    this.x = x;
    this.y = y;
    this.radius = radius;
    this.type = type; // "snap" for photo, "cap" for video
    this.down = false;
    this.recording = false;
    this.pulsePhase = 0;
    this.disabled = false;
  }

  reposition({ x, y, radius, screen }) {
    if (x !== undefined) this.x = x;
    if (y !== undefined) this.y = y;
    if (radius !== undefined) this.radius = radius;

    // Support anchor positioning
    if (screen) {
      if (x === "center") this.x = floor(screen.width / 2);
      if (y === "center") this.y = floor(screen.height / 2);
    }
  }

  contains(px, py) {
    const dx = px - this.x;
    const dy = py - this.y;
    return dx * dx + dy * dy <= this.radius * this.radius;
  }

  paint({ ink, screen }, frameCount = 0) {
    const { x, y, radius, type, down, recording, disabled } = this;

    // Outer ring
    const ringColor = disabled ? [60, 60, 60] : [255, 255, 255];
    ink(...ringColor).circle(x, y, radius + 4, false, 2);

    if (type === "snap") {
      // Snap button - white circle with camera icon feel
      const fillColor = down ? [180, 180, 180] : [255, 255, 255];
      ink(...fillColor).circle(x, y, radius - 2, true);
      // Inner detail
      ink(200, 200, 200).circle(x, y, radius - 8, false, 1);
    } else {
      // Cap button - red circle that becomes square when recording
      if (recording) {
        // Pulsing red square when recording (properly centered)
        this.pulsePhase += 0.1;
        const pulse = 0.7 + 0.3 * sin(this.pulsePhase);
        const r = floor(255 * pulse);
        const squareSize = floor(radius * 0.6); // Square is ~60% of radius
        const halfSquare = floor(squareSize / 2);
        ink(r, 40, 40).box(
          x - halfSquare,
          y - halfSquare,
          squareSize,
          squareSize,
        );
        // Recording indicator ring
        ink(255, 60, 60).circle(x, y, radius, false, 2);
      } else {
        // Red circle when not recording
        const fillColor = down ? [180, 40, 40] : [255, 60, 60];
        ink(...fillColor).circle(x, y, radius - 4, true);
      }
    }

    return this;
  }

  act(e, { down, push, hold } = {}) {
    if (this.disabled) return false;

    if (e.is("touch") && this.contains(e.x, e.y)) {
      this.down = true;
      down?.();
      return true;
    }

    if (e.is("lift") && this.down) {
      this.down = false;
      if (this.contains(e.x, e.y)) {
        push?.();
      }
      return true;
    }

    if (e.is("draw") && this.down) {
      hold?.(e);
    }

    return false;
  }
}

/**
 * Camera swap button for switching between front/back cameras
 */
export class SwapButton {
  constructor({ x, y, width = 48, height = 24 }) {
    this.x = x;
    this.y = y;
    this.width = width;
    this.height = height;
    this.down = false;
    this.disabled = false;
  }

  reposition({ right, bottom, screen }) {
    if (right !== undefined && screen) {
      this.x = screen.width - this.width - right;
    }
    if (bottom !== undefined && screen) {
      this.y = screen.height - this.height - bottom;
    }
  }

  contains(px, py) {
    return (
      px >= this.x &&
      px <= this.x + this.width &&
      py >= this.y &&
      py <= this.y + this.height
    );
  }

  paint({ ink, write }) {
    const { x, y, width, height, down, disabled } = this;

    const bg = disabled ? [40, 40, 40] : down ? [60, 60, 80] : [80, 80, 100];
    const fg = disabled ? [80, 80, 80] : [255, 255, 255];

    ink(...bg).box(x, y, width, height);
    ink(...fg).write("Swap", { x: x + 6, y: y + 6 });

    return this;
  }

  act(e, { down, push } = {}) {
    if (this.disabled) return false;

    if (e.is("touch") && this.contains(e.x, e.y)) {
      this.down = true;
      down?.();
      return true;
    }

    if (e.is("lift") && this.down) {
      this.down = false;
      if (this.contains(e.x, e.y)) {
        push?.();
      }
      return true;
    }

    return false;
  }
}

/**
 * Recording timer display
 */
export class RecordingTimer {
  constructor() {
    this.startTime = null;
    this.duration = 0;
    this.maxDuration = null; // Optional max duration in seconds
  }

  start(maxDuration = null) {
    this.startTime = Date.now();
    this.maxDuration = maxDuration;
  }

  stop() {
    if (this.startTime) {
      this.duration = (Date.now() - this.startTime) / 1000;
    }
    this.startTime = null;
    return this.duration;
  }

  get elapsed() {
    if (!this.startTime) return 0;
    return (Date.now() - this.startTime) / 1000;
  }

  get progress() {
    if (!this.maxDuration) return 0;
    return min(1, this.elapsed / this.maxDuration);
  }

  get isExpired() {
    return this.maxDuration && this.elapsed >= this.maxDuration;
  }

  formatTime(seconds = this.elapsed) {
    const mins = floor(seconds / 60);
    const secs = floor(seconds % 60);
    return `${mins}:${secs.toString().padStart(2, "0")}`;
  }

  paint({ ink, write, screen }, { x, y } = {}) {
    if (!this.startTime) return;

    const elapsed = this.elapsed;
    const display = this.formatTime(elapsed);

    // Position at top center by default
    const px = x ?? floor(screen.width / 2);
    const py = y ?? 12;

    // Pulsing red dot
    const pulse = 0.5 + 0.5 * sin(elapsed * 4);
    ink(255, floor(60 * pulse), floor(60 * pulse)).circle(px - 24, py, 4, true);

    // Time display
    ink(255, 255, 255).write(display, { x: px - 12, y: py - 4 });

    // Progress bar if max duration set
    if (this.maxDuration) {
      const barWidth = 60;
      const barX = px - barWidth / 2;
      const barY = py + 10;
      ink(60, 60, 60).box(barX, barY, barWidth, 3);
      ink(255, 80, 80).box(barX, barY, floor(barWidth * this.progress), 3);
    }
  }
}

/**
 * Flash effect overlay for photo capture
 */
export class FlashEffect {
  constructor() {
    this.active = false;
    this.alpha = 0;
    this.decay = 0.15;
  }

  trigger() {
    this.active = true;
    this.alpha = 255;
  }

  update() {
    if (!this.active) return;
    this.alpha = max(0, this.alpha - this.alpha * this.decay);
    if (this.alpha < 5) {
      this.active = false;
      this.alpha = 0;
    }
  }

  paint({ ink, screen }) {
    if (!this.active || this.alpha <= 0) return;
    ink(255, 255, 255, floor(this.alpha)).box(0, 0, screen.width, screen.height);
  }
}

/**
 * Microphone level indicator with visual boost for weak signals
 */
export class MicLevel {
  constructor() {
    this.level = 0;
    this.smoothedLevel = 0; // Smoothed for visual display
    this.peak = 0;
    this.peakDecay = 0.02;
    this.smoothing = 0.3; // Lower = smoother transitions
  }

  update(amplitude) {
    // Apply logarithmic scaling to make quiet sounds more visible
    // This compresses the dynamic range for better visualization
    const rawLevel = amplitude || 0;
    // Log scale: boost low levels, compress high levels
    // sqrt provides a gentler curve than log that still boosts lows
    const boostedLevel = Math.sqrt(rawLevel);
    
    // Smooth the level to avoid jittery display
    this.smoothedLevel += (boostedLevel - this.smoothedLevel) * this.smoothing;
    this.level = this.smoothedLevel;
    
    // Peak hold with decay
    if (this.level > this.peak) {
      this.peak = this.level;
    } else {
      this.peak = max(0, this.peak - this.peakDecay);
    }
  }

  paint({ ink, screen }, { x, y, width = 60, height = 6 } = {}) {
    const px = x ?? screen.width - width - 8;
    const py = y ?? 8;

    // Background
    ink(40, 40, 50).box(px, py, width, height);

    // Level bar with gradient based on level
    const levelWidth = floor(width * min(1, this.level));
    const color =
      this.level > 0.8 ? [255, 80, 80] : this.level > 0.5 ? [255, 200, 80] : [80, 255, 120];
    ink(...color).box(px, py, levelWidth, height);

    // Peak marker
    const peakX = px + floor(width * min(1, this.peak));
    ink(255, 255, 255).line(peakX, py, peakX, py + height);
  }
}
