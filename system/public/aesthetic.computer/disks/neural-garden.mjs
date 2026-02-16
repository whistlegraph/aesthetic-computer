// Neural Garden - Learns gestures as parametric curves
// "Surfing" through the learned wave encoded in network weights
// Learns: t (position 0→1 along curve) → (dx, dy) movement

class TinyBrain {
  constructor(size = 64) {
    this.size = size;
    // Input: Fourier features of t (multiple frequency encodings)
    // [sin(2πt), cos(2πt), sin(4πt), cos(4πt), ...]
    this.numFreqs = 6;
    this.inputSize = this.numFreqs * 2; // sin and cos for each frequency
    // Output: (dx, dy) movement = 2 outputs
    this.outputSize = 2;

    // Near-zero initialization - start from simple function, let training build up
    this.weightsIH = Array(this.inputSize).fill(0).map(() =>
      Array(size).fill(0).map(() => (Math.random() - 0.5) * 0.05) // Much smaller: ±0.025
    );
    this.weightsHO = Array(size).fill(0).map(() =>
      Array(this.outputSize).fill(0).map(() => (Math.random() - 0.5) * 0.05)
    );
    this.biasH = Array(size).fill(0).map(() => (Math.random() - 0.5) * 0.02);
    this.biasO = Array(this.outputSize).fill(0); // Start at zero, will be set by training

    this.health = 1.0;
    this.lastUsed = 0;
    this.predictionHistory = [];
    this.lr = 0.3; // Higher for near-zero start and complex patterns
  }

  // Encode t using Fourier features for better positional learning
  fourierFeatures(t) {
    const features = [];
    for (let i = 0; i < this.numFreqs; i++) {
      const freq = Math.pow(2, i + 1); // 2, 4, 8, 16, 32, 64
      features.push(Math.sin(freq * Math.PI * t));
      features.push(Math.cos(freq * Math.PI * t));
    }
    return features;
  }

  sigmoid(x) { return 1 / (1 + Math.exp(-x)); }
  sigmoidDeriv(x) { return x * (1 - x); }
  tanh(x) { return Math.tanh(x); }
  tanhDeriv(x) { return 1 - x * x; }

  forward(input) {
    const hidden = [];
    for (let i = 0; i < this.size; i++) {
      let sum = this.biasH[i];
      for (let j = 0; j < this.inputSize; j++) sum += input[j] * this.weightsIH[j][i];
      hidden[i] = this.sigmoid(sum);
    }
    const output = [];
    for (let i = 0; i < this.outputSize; i++) {
      let sum = this.biasO[i];
      for (let j = 0; j < this.size; j++) sum += hidden[j] * this.weightsHO[j][i];
      output[i] = this.tanh(sum); // tanh for outputs that can be negative (dx, dy)
    }
    return { hidden, output };
  }

  predict(t, shouldLog = false) {
    // Input: Fourier features of t for better positional encoding
    const input = this.fourierFeatures(t);
    const { output } = this.forward(input);

    // Output is (dx, dy) scaled by max movement size
    const maxMove = 30; // Increased to allow larger movements for spirals
    const dx = output[0] * maxMove;
    const dy = output[1] * maxMove;

    if (shouldLog) {
      console.log(`  t=${t.toFixed(2)} → dx=${dx.toFixed(1)} dy=${dy.toFixed(1)} dist=${Math.sqrt(dx*dx + dy*dy).toFixed(1)}`);
    }

    return { dx, dy };
  }

  train(t, targetDx, targetDy, frameCount) {
    // Input: Fourier features of t
    const input = this.fourierFeatures(t);

    // Forward pass
    const { hidden, output } = this.forward(input);

    // Target: normalized (dx, dy)
    const maxMove = 30; // Match predict maxMove
    const target = [targetDx / maxMove, targetDy / maxMove];

    // Calculate error and backprop
    const outputError = [], outputDelta = [];
    let totalError = 0;
    for (let i = 0; i < this.outputSize; i++) {
      outputError[i] = target[i] - output[i];
      totalError += Math.abs(outputError[i]);
      outputDelta[i] = outputError[i] * this.tanhDeriv(output[i]);
    }

    const quality = 1.0 - Math.min(1.0, totalError);
    const lr = this.lr * this.health;

    const hiddenError = [], hiddenDelta = [];
    for (let i = 0; i < this.size; i++) {
      let error = 0;
      for (let j = 0; j < this.outputSize; j++) error += outputDelta[j] * this.weightsHO[i][j];
      hiddenError[i] = error;
      hiddenDelta[i] = error * this.sigmoidDeriv(hidden[i]);
    }

    // Update weights
    for (let i = 0; i < this.size; i++)
      for (let j = 0; j < this.outputSize; j++)
        this.weightsHO[i][j] += lr * outputDelta[j] * hidden[i];
    for (let i = 0; i < this.outputSize; i++) this.biasO[i] += lr * outputDelta[i];
    for (let i = 0; i < this.inputSize; i++)
      for (let j = 0; j < this.size; j++)
        this.weightsIH[i][j] += lr * hiddenDelta[j] * input[i];
    for (let i = 0; i < this.size; i++) this.biasH[i] += lr * hiddenDelta[i];

    // Health updates
    if (quality > 0.7) this.health *= 1.005;
    else if (quality < 0.3) this.health *= 0.995;
    if (frameCount - this.lastUsed > 180) this.health *= 0.992;

    this.lastUsed = frameCount;
    this.predictionHistory.push({ error: totalError, quality, health: this.health });
    if (this.predictionHistory.length > 100) this.predictionHistory.shift();
  }

  reinitialize() {
    // Reinitialize with near-zero weights for clean learning
    this.weightsIH = Array(this.inputSize).fill(0).map(() =>
      Array(this.size).fill(0).map(() => (Math.random() - 0.5) * 0.05)
    );
    this.weightsHO = Array(this.size).fill(0).map(() =>
      Array(this.outputSize).fill(0).map(() => (Math.random() - 0.5) * 0.05)
    );
    this.biasH = Array(this.size).fill(0).map(() => (Math.random() - 0.5) * 0.02);
    this.biasO = Array(this.outputSize).fill(0);
    this.health = 1.0;
    this.predictionHistory = [];
    console.log("🔄 Network reinitialized - ready to learn a new wave");
  }

  get weights() { return this.weightsIH; }
  getPhase() {
    if (this.health < 0.3) return "dying";
    if (this.health < 0.7) return "sickly";
    if (this.health < 1.0) return "healthy";
    if (this.health < 1.3) return "mature";
    return "flourishing";
  }
}

let brain, trail = [], frameCount = 0, playbackMode = false, playbackTimer = 0, drawing = false;
let modeButton, resetButton;
let playbackFrames = 0, surfT = 0; // Track playback time and surf parameter
let learnedTrail = []; // Store original learned trail for comparison
let learnedTempo = 0.015; // Speed to advance t (learned from drawing speed)
let trailBuffer = null; // Painting buffer for accumulating trail
let drawStartTime = 0; // Track when drawing started
let drawingFrames = 0; // Count frames during drawing

function boot({ hud, ui, screen }) {
  brain = new TinyBrain(64); // 64 hidden neurons to encode complex curves
  hud.label("neural-garden");
  // Position buttons bottom-right
  const btnW = 40, btnH = 12;
  const unlearnW = 50;
  modeButton = new ui.TextButton("WATCH", {
    x: screen.width - btnW - 4,
    y: screen.height - btnH - 10
  });
  resetButton = new ui.TextButton("UNLEARN", {
    x: screen.width - btnW - unlearnW - 12,
    y: screen.height - btnH - 10
  });
}

function act({ event: e, pen, screen, num }) {
  if (e.is("draw") && pen) {
    if (!drawing && !playbackMode) {
      trail = []; // Clear old trail when starting new stroke
      drawStartTime = Date.now(); // Start timing
      drawingFrames = 0;
    }
    drawing = true;
    playbackMode = false; // Stop playback when drawing

    // Only add point if it's far enough from the last point (subsample!)
    if (trail.length === 0 || num.dist(pen.x, pen.y, trail[trail.length - 1].x, trail[trail.length - 1].y) > 8) {
      trail.push({ x: pen.x, y: pen.y });
      if (trail.length > 150) trail.shift();
      // DON'T train yet - capture the whole gesture first
    }
  }

  // Check mode button click
  if (modeButton && !modeButton.btn.disabled) {
    modeButton.btn.act(e, {
      push: () => {
        playbackMode = !playbackMode;
        modeButton.replaceLabel(playbackMode ? "DO" : "WATCH");
        if (playbackMode && learnedTrail.length >= 2) {
          playbackFrames = 0;
          surfT = 0.0;
          trail = [learnedTrail[0]]; // Start from learned point
          console.log(`\n🌊 DO mode: Continuous generation with wrapping (press WATCH to stop)`);
        } else {
          console.log("⏸ Switched to WATCH mode");
          // Restore learned trail
          if (learnedTrail.length > 0) trail = [...learnedTrail];
        }
      }
    });
  }

  // Check reset button click
  if (resetButton && !resetButton.btn.disabled) {
    resetButton.btn.act(e, {
      push: () => {
        brain.reinitialize();
        trail = [];
        learnedTrail = [];
        playbackMode = false;
        modeButton.replaceLabel("WATCH");
        trailBuffer = null; // Clear accumulated buffer
      }
    });
  }

  if (e.is("lift")) {
    if (drawing) {
      // Gesture complete - now train on the ENTIRE gesture
      if (trail.length >= 2) {
        const epochs = 200; // Many epochs needed for complex patterns
        console.log(`\n🌊 Encoding ${trail.length} points into wave (${epochs} epochs)`);

        // Save learned trail for comparison
        learnedTrail = [...trail];

        // Collect training data and calculate tempo
        const trainingData = [];
        let totalDist = 0;
        for (let i = 1; i < trail.length; i++) {
          const t = i / (trail.length - 1);
          const dx = trail[i].x - trail[i-1].x;
          const dy = trail[i].y - trail[i-1].y;
          const dist = Math.sqrt(dx * dx + dy * dy);
          totalDist += dist;
          trainingData.push({ t, dx, dy, dist });
        }

        // Calculate tempo: how fast to advance t to match original drawing speed
        // We want to traverse t: 0→1 over the same number of points as the original
        learnedTempo = 1.0 / (trail.length * 32); // *32 to slow down to match gesture speed

        console.log(`📏 Gesture stats: ${trail.length} points, ${totalDist.toFixed(0)}px total, tempo=${learnedTempo.toFixed(4)}`);

        // Train: map t values to movements
        // Shuffle training data each epoch to avoid local minima
        for (let epoch = 0; epoch < epochs; epoch++) {
          const shuffled = [...trainingData].sort(() => Math.random() - 0.5);
          for (const { t, dx, dy } of shuffled) {
            brain.train(t, dx, dy, frameCount);
          }
        }

        console.log(`✅ Wave encoded. Health: ${brain.health.toFixed(2)}`);

        // Test network predictions at sample points
        console.log("\n📊 LEARNED DATA vs NETWORK PREDICTIONS:");
        console.log("t     | Learned dx,dy      | Network dx,dy      | Error");
        console.log("------|--------------------|--------------------|-------");

        const testPoints = [0, 0.25, 0.5, 0.75, 1.0];
        for (const t of testPoints) {
          // Find closest learned point
          const idx = Math.round(t * (trainingData.length - 1));
          const learned = trainingData[idx] || trainingData[trainingData.length - 1];
          const predicted = brain.predict(t, false);
          const error = Math.sqrt(
            Math.pow(predicted.dx - learned.dx, 2) +
            Math.pow(predicted.dy - learned.dy, 2)
          );
          console.log(
            `${t.toFixed(2)}  | (${learned.dx.toFixed(1)}, ${learned.dy.toFixed(1)})`.padEnd(20) +
            `| (${predicted.dx.toFixed(1)}, ${predicted.dy.toFixed(1)})`.padEnd(20) +
            `| ${error.toFixed(1)}`
          );
        }
        console.log("\n");
      }
    }
    drawing = false;
  }
}

function sim({ screen }) {
  frameCount++;

  // Track frames while drawing for tempo calculation
  if (drawing) {
    drawingFrames++;
  }

  if (playbackMode && learnedTrail.length >= 2) {
    playbackFrames++;

    // Continuous generation at learned tempo
    surfT += learnedTempo;

    // Log periodically
    const shouldLog = playbackFrames % 60 === 0;
    if (shouldLog) console.log(`🌊 Generating at t=${surfT.toFixed(2)} (tempo=${learnedTempo.toFixed(4)})`);

    // Get movement from network (extrapolates beyond training)
    const { dx, dy } = brain.predict(surfT, shouldLog);

    // Add to trail with screen wrapping
    const last = trail[trail.length - 1];
    let newX = last.x + dx;
    let newY = last.y + dy;

    // Wrap around screen edges (turtle graphics style)
    while (newX < 0) newX += screen.width;
    while (newX >= screen.width) newX -= screen.width;
    while (newY < 0) newY += screen.height;
    while (newY >= screen.height) newY -= screen.height;

    trail.push({ x: newX, y: newY });
    if (trail.length > 300) trail.shift(); // Keep more history

    // Continuous generation - never stops automatically!
    // Press WATCH button to stop and draw a new pattern
  }
}

function paint({ wipe, ink, line, box, circle, write, screen, pen, num, painting, paste }) {
  // Wipe main canvas
  wipe(10, 10, 25);

  const health = brain.health;
  const phase = brain.getPhase();
  const recent = brain.predictionHistory[brain.predictionHistory.length - 1];
  const phaseColors = { dying: [150, 0, 0], sickly: [200, 100, 0], healthy: [0, 200, 0], mature: [0, 150, 100], flourishing: [0, 255, 150] };
  const phaseColor = phaseColors[phase];

  // Helper: Draw line with turtle graphics wrapping
  function wrappedLine($, x1, y1, x2, y2, w, h) {
    let dx = x2 - x1;
    let dy = y2 - y1;

    // Check if line wraps - if coordinate jump is > half screen, it wrapped
    const wrapsX = Math.abs(dx) > w / 2;
    const wrapsY = Math.abs(dy) > h / 2;

    if (!wrapsX && !wrapsY) {
      // No wrapping - draw normal line
      $.line(x1, y1, x2, y2);
      return;
    }

    // Calculate REAL movement (unwrapped)
    // If dx is large negative, we actually moved right (wrapping forward)
    // If dx is large positive, we actually moved left (wrapping backward)
    let realDx = dx;
    let realDy = dy;

    if (dx < -w / 2) realDx = dx + w;  // Wrapped right edge to left
    else if (dx > w / 2) realDx = dx - w;  // Wrapped left edge to right

    if (dy < -h / 2) realDy = dy + h;  // Wrapped bottom to top
    else if (dy > h / 2) realDy = dy - h;  // Wrapped top to bottom

    // Now draw the wrapped segments
    if (wrapsX) {
      // Horizontal wrap - which edge did we cross?
      if (realDx > 0) {
        // Moving right, crossing right edge at x=w
        const t = (w - x1) / realDx;  // Fraction of movement to reach edge
        const crossY = y1 + realDy * t;
        $.line(x1, y1, w, crossY);      // Draw to right edge
        $.line(0, crossY, x2, y2);      // Continue from left edge
      } else {
        // Moving left, crossing left edge at x=0
        const t = (0 - x1) / realDx;
        const crossY = y1 + realDy * t;
        $.line(x1, y1, 0, crossY);      // Draw to left edge
        $.line(w, crossY, x2, y2);      // Continue from right edge
      }
    } else if (wrapsY) {
      // Vertical wrap
      if (realDy > 0) {
        // Moving down, crossing bottom edge at y=h
        const t = (h - y1) / realDy;
        const crossX = x1 + realDx * t;
        $.line(x1, y1, crossX, h);      // Draw to bottom edge
        $.line(crossX, 0, x2, y2);      // Continue from top edge
      } else {
        // Moving up, crossing top edge at y=0
        const t = (0 - y1) / realDy;
        const crossX = x1 + realDx * t;
        $.line(x1, y1, crossX, 0);      // Draw to top edge
        $.line(crossX, h, x2, y2);      // Continue from bottom edge
      }
    }
  }

  // Create trail buffer (accumulates when in playback/DO mode)
  if (trail.length > 0) {
    trailBuffer = painting(screen.width, screen.height, ($) => {
      // Accumulate previous buffer when in playback mode (continuous generation)
      if (trailBuffer && playbackMode) {
        $.paste(trailBuffer, 0, 0);
      }

      // Draw trail with phase color
      $.ink(...phaseColor);

      // Draw only the latest segment(s) for performance
      const startIdx = playbackMode && trail.length > 50 ? trail.length - 5 : 1;
      for (let i = startIdx; i < trail.length; i++) {
        // Draw with turtle graphics wrapping
        wrappedLine($, trail[i - 1].x, trail[i - 1].y, trail[i].x, trail[i].y, screen.width, screen.height);
      }

      // Show cursor during drawing (WATCH mode only)
      if (pen && drawing && !playbackMode) {
        $.ink(255);
        $.circle(pen.x, pen.y, 4);
      }
    });

    // Paste trail buffer onto main canvas
    paste(trailBuffer, 0, 0);
  }

  // DRAW UI ON TOP
  // Update and render buttons (bottom-right)
  const btnW = 40, btnH = 12, unlearnW = 50;
  modeButton.reposition({
    x: screen.width - btnW - 4,
    y: screen.height - btnH - 10
  });
  modeButton.paint({ ink, write, box, screen });

  resetButton.reposition({
    x: screen.width - btnW - unlearnW - 12,
    y: screen.height - btnH - 10
  });
  resetButton.paint({ ink, write, box, screen });

  // Compact stats box (top-right)
  const boxW = 50, boxH = 80, boxX = screen.width - boxW - 4, boxY = 4, pad = 2;

  ink(20, 20, 30);
  box(boxX, boxY, boxW, boxH);
  ink(...phaseColor);
  box(boxX, boxY, boxW, boxH, "out");

  // Stats
  ink(...phaseColor);
  write(phase.slice(0, 3), { x: boxX + pad, y: boxY + pad }, undefined, undefined, false, "MatrixChunky8");
  if (playbackMode) ink(0, 255, 0);
  else ink(200);
  write(playbackMode ? "play" : `${(health * 100).toFixed(0)}%`, { x: boxX + pad, y: boxY + pad + 8 }, undefined, undefined, false, "MatrixChunky8");

  // Health bar
  ink(40);
  box(boxX + pad, boxY + pad + 16, boxW - pad * 2, 1);
  ink(...phaseColor);
  box(boxX + pad, boxY + pad + 16, Math.min(boxW - pad * 2, health * (boxW - pad * 2)), 1);

  // Quality
  if (recent) {
    const q = recent.quality;
    if (q > 0.7) ink(0, 200, 0);
    else if (q < 0.3) ink(200, 0, 0);
    else ink(150, 150, 0);
    write(q > 0.7 ? "coh" : (q < 0.3 ? "cha" : "neu"), { x: boxX + pad, y: boxY + pad + 20 }, undefined, undefined, false, "MatrixChunky8");
  }

  // Tiny weights visualization (12 Fourier inputs × 16 hidden shown, then 16×2 output)
  const cellSize = 2;
  let yPos = boxY + pad + 28;

  // Input → Hidden (show 12 Fourier inputs × first 16 hidden neurons)
  // Compress to show average weight strength for each hidden neuron
  for (let j = 0; j < 16; j++) {
    let totalW = 0;
    for (let i = 0; i < Math.min(12, brain.inputSize); i++) {
      totalW += Math.abs(brain.weightsIH[i][j]);
    }
    const avgW = totalW / 12;
    const brightness = Math.min(255, avgW * 200);
    ink(brightness, brightness * 0.5, 0);
    box(boxX + pad + (j % 8) * (cellSize + 1), yPos + Math.floor(j / 8) * (cellSize + 1), cellSize, cellSize);
  }

  // Hidden → Output (show first 16 neurons → 2 outputs)
  yPos += 2 * (cellSize + 1) + 1;
  for (let i = 0; i < 16; i++) {
    for (let j = 0; j < 2; j++) {
      const w = brain.weightsHO[i][j];
      const brightness = Math.min(255, Math.abs(w) * 500);
      ink(w > 0 ? 0 : brightness, w > 0 ? brightness : 0, 0);
      box(boxX + pad + i * (cellSize + 1), yPos + j * (cellSize + 1), cellSize, cellSize);
    }
  }

  // Tiny graph
  const history = brain.predictionHistory;
  if (history.length > 1) {
    const graphW = boxW - pad * 2, graphH = 10, graphX = boxX + pad, graphY = boxY + boxH - graphH - pad;
    ink(40, 40, 50);
    box(graphX, graphY, graphW, graphH);

    const maxH = Math.max(...history.map(h => Math.min(2, h.health)));
    const minH = Math.min(...history.map(h => h.health));
    const range = maxH - minH || 1;

    ink(0, 255, 0);
    for (let i = 1; i < history.length; i++) {
      const x1 = graphX + ((i - 1) / (history.length - 1)) * graphW;
      const x2 = graphX + (i / (history.length - 1)) * graphW;
      const h1 = (Math.min(2, history[i - 1].health) - minH) / range;
      const h2 = (Math.min(2, history[i].health) - minH) / range;
      line(x1, graphY + graphH - h1 * graphH, x2, graphY + graphH - h2 * graphH);
    }
  }

  // Instructions (bottom-left, before buttons)
  const instrY = screen.height - 10;
  if (trail.length === 0) {
    if (playbackMode) {
      ink(0, 255, 100);
      write("draw a pattern first", { x: 4, y: instrY });
    } else {
      ink(255, 200, 0);
      write("draw to learn style", { x: 4, y: instrY });
    }
  } else if (playbackMode) {
    ink(0, 255, 150);
    write(`generating... t=${surfT.toFixed(1)}`, { x: 4, y: instrY });
  }
}

export { boot, act, sim, paint };
