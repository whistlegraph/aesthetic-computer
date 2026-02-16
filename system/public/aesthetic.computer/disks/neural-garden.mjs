// Neural Garden - Minimal living neural network demo
// Learns pen positions, shows health based on prediction quality

// Tiny Hebbian network (pure JS, ~50 lines)
class TinyBrain {
  constructor(size = 8) {
    this.size = size;
    this.weights = Array(size).fill(0).map(() =>
      Array(size).fill(0).map(() => (Math.random() - 0.5) * 0.1)
    );
    this.health = 1.0;
    this.lastUsed = 0;
    this.predictionHistory = [];
  }

  activate(x) {
    return 1 / (1 + Math.exp(-x)); // Sigmoid
  }

  predict(input) {
    const padded = [...input, ...Array(this.size - input.length).fill(0)].slice(0, this.size);
    const output = Array(this.size).fill(0);

    for (let i = 0; i < this.size; i++) {
      let sum = 0;
      for (let j = 0; j < this.size; j++) {
        sum += this.weights[i][j] * padded[j];
      }
      output[i] = this.activate(sum);
    }

    return output.slice(0, input.length); // Return same size as input
  }

  train(input, actual, frameCount) {
    // Predict
    const predicted = this.predict(input);

    // Calculate error (prediction quality)
    let error = 0;
    for (let i = 0; i < predicted.length; i++) {
      error += Math.abs(predicted[i] - actual[i]);
    }
    error /= predicted.length;

    // Map error to quality (0 = bad, 1 = good)
    const quality = 1.0 - Math.min(1.0, error);

    // Update health based on quality
    if (quality > 0.7) {
      this.health *= 1.002; // Coherent use → flourish
    } else if (quality < 0.3) {
      this.health *= 0.998; // Chaotic use → sicken
    }

    // Neglect decay
    const timeSinceUse = frameCount - this.lastUsed;
    if (timeSinceUse > 300) {
      this.health *= 0.995; // Atrophy
    }

    this.lastUsed = frameCount;
    this.predictionHistory.push({ error, quality, health: this.health });
    if (this.predictionHistory.length > 200) {
      this.predictionHistory.shift();
    }

    // Hebbian learning: strengthen connections
    const lr = 0.01;
    const paddedInput = [...input, ...Array(this.size - input.length).fill(0)].slice(0, this.size);
    const paddedActual = [...actual, ...Array(this.size - actual.length).fill(0)].slice(0, this.size);

    for (let i = 0; i < this.size; i++) {
      for (let j = 0; j < this.size; j++) {
        this.weights[i][j] += lr * paddedInput[i] * paddedActual[j] * this.health;
      }
    }

    return { predicted, error, quality };
  }

  getPhase() {
    if (this.health < 0.3) return "dying";
    if (this.health < 0.7) return "sickly";
    if (this.health < 1.0) return "healthy";
    if (this.health < 1.3) return "mature";
    return "flourishing";
  }
}

let brain;
let lastPos = { x: 0, y: 0 };
let mode = "draw"; // draw, weights, graph
let frameCount = 0;

function boot({ wipe }) {
  brain = new TinyBrain(8);
  wipe(0);
}

function act({ event: e, screen }) {
  // Mode switching
  if (e.is("keyboard:down:w")) mode = "weights";
  if (e.is("keyboard:down:d")) mode = "draw";
  if (e.is("keyboard:down:g")) mode = "graph";

  // Drawing = training
  if (e.is("draw")) {
    const input = [e.x / screen.width, e.y / screen.height];
    const actual = [lastPos.x / screen.width, lastPos.y / screen.height];

    brain.train(input, actual, frameCount);

    lastPos = { x: e.x, y: e.y };
  }
}

function sim() {
  frameCount++;
}

function paint({ wipe, ink, line, box, write, screen }) {
  wipe(0, 0, 20);

  // Health bar (always visible)
  const health = brain.health;
  const phase = brain.getPhase();

  // Phase color
  const phaseColors = {
    dying: [150, 0, 0],
    sickly: [200, 100, 0],
    healthy: [0, 200, 0],
    mature: [0, 150, 100],
    flourishing: [0, 255, 150],
  };

  ink(...phaseColors[phase]);
  write(`${phase.toUpperCase()} ${(health * 100).toFixed(1)}%`, {
    x: 10, y: 10
  });

  // Health bar
  const barWidth = Math.min(200, health * 200);
  box(10, 30, barWidth, 10);

  // Instructions
  ink(150);
  write("[D]raw [W]eights [G]raph", { x: 10, y: screen.height - 30 });
  write("Draw with mouse to train", { x: 10, y: screen.height - 15 });

  // Mode-specific visualization
  if (mode === "weights") {
    paintWeights({ ink, box, write, screen });
  } else if (mode === "graph") {
    paintGraph({ ink, line, write, screen });
  } else {
    // Draw mode - show last prediction
    const recent = brain.predictionHistory[brain.predictionHistory.length - 1];
    if (recent) {
      ink(200);
      write(`Prediction quality: ${(recent.quality * 100).toFixed(1)}%`, {
        x: 10, y: 50
      });

      // Quality indicator
      if (recent.quality > 0.7) {
        ink(0, 255, 0);
        write("✓ Coherent use (healthy)", { x: 10, y: 70 });
      } else if (recent.quality < 0.3) {
        ink(255, 0, 0);
        write("✗ Chaotic use (unhealthy)", { x: 10, y: 70 });
      } else {
        ink(200, 200, 0);
        write("~ Mediocre use (neutral)", { x: 10, y: 70 });
      }
    }
  }
}

function paintWeights({ ink, box, write, screen }) {
  write("Weight Heatmap", { x: 10, y: 60 });

  const size = brain.size;
  const cellSize = Math.min(40, (screen.width - 20) / size);
  const startY = 90;

  for (let i = 0; i < size; i++) {
    for (let j = 0; j < size; j++) {
      const w = brain.weights[i][j];

      // Map weight to color
      if (w > 0) {
        const brightness = Math.min(255, Math.abs(w) * 500);
        ink(0, brightness, 0); // Green for positive
      } else {
        const brightness = Math.min(255, Math.abs(w) * 500);
        ink(brightness, 0, 0); // Red for negative
      }

      box(10 + j * cellSize, startY + i * cellSize, cellSize - 2, cellSize - 2);
    }
  }

  // Legend
  ink(150);
  write("Green = positive weights", { x: 10, y: startY + size * cellSize + 10 });
  write("Red = negative weights", { x: 10, y: startY + size * cellSize + 25 });
  write("Brightness = strength", { x: 10, y: startY + size * cellSize + 40 });
}

function paintGraph({ ink, line, write, screen }) {
  write("Health Evolution (last 200 frames)", { x: 10, y: 60 });

  const history = brain.predictionHistory;
  if (history.length < 2) return;

  const graphHeight = 150;
  const graphY = 90;
  const graphWidth = screen.width - 20;

  // Background
  ink(40, 40, 50);
  box(10, graphY, graphWidth, graphHeight);

  // Draw health line (green)
  ink(0, 255, 0);
  for (let i = 1; i < history.length; i++) {
    const x1 = 10 + ((i - 1) / history.length) * graphWidth;
    const x2 = 10 + (i / history.length) * graphWidth;
    const y1 = graphY + graphHeight - (history[i - 1].health * graphHeight);
    const y2 = graphY + graphHeight - (history[i].health * graphHeight);
    line(x1, y1, x2, y2);
  }

  // Draw quality line (yellow)
  ink(255, 255, 0);
  for (let i = 1; i < history.length; i++) {
    const x1 = 10 + ((i - 1) / history.length) * graphWidth;
    const x2 = 10 + (i / history.length) * graphWidth;
    const y1 = graphY + graphHeight - (history[i - 1].quality * graphHeight);
    const y2 = graphY + graphHeight - (history[i].quality * graphHeight);
    line(x1, y1, x2, y2);
  }

  // Legend
  ink(0, 255, 0);
  write("— Health", { x: 10, y: graphY + graphHeight + 10 });
  ink(255, 255, 0);
  write("— Prediction Quality", { x: 10, y: graphY + graphHeight + 25 });
}

export { boot, act, sim, paint };
