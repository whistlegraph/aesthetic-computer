// Neural Garden - Learns drawing gestures with a tiny GPT
// Transformer-based autoregressive model

class TinyGPT {
  constructor(dModel = 32, nHeads = 2, seqLen = 16) {
    this.dModel = dModel; // Model dimension
    this.nHeads = nHeads; // Attention heads
    this.dHead = Math.floor(dModel / nHeads);
    this.seqLen = seqLen; // Sequence length
    this.inputSize = 2; // (dx, dy)

    const scale = Math.sqrt(2.0 / dModel);

    // Input embedding: (dx, dy) → dModel
    this.Wembed = this.initWeights(this.inputSize, dModel, scale);
    this.bembed = Array(dModel).fill(0);

    // Positional encoding (learnable)
    this.posEmbed = Array(seqLen).fill(0).map(() =>
      Array(dModel).fill(0).map(() => (Math.random() - 0.5) * scale)
    );

    // Single attention layer (simplified multi-head)
    this.Wq = this.initWeights(dModel, dModel, scale);
    this.Wk = this.initWeights(dModel, dModel, scale);
    this.Wv = this.initWeights(dModel, dModel, scale);
    this.Wo = this.initWeights(dModel, dModel, scale);

    // FFN layer
    this.W1 = this.initWeights(dModel, dModel * 2, scale);
    this.b1 = Array(dModel * 2).fill(0);
    this.W2 = this.initWeights(dModel * 2, dModel, scale);
    this.b2 = Array(dModel).fill(0);

    // Output projection: dModel → (dx, dy)
    this.Wout = this.initWeights(dModel, this.inputSize, scale);
    this.bout = Array(this.inputSize).fill(0);

    this.lr = 0.01;
    this.maxMove = 30;
    this.health = 1.0;
    this.predictionHistory = [];
  }

  initWeights(rows, cols, scale) {
    return Array(rows).fill(0).map(() =>
      Array(cols).fill(0).map(() => (Math.random() - 0.5) * 2 * scale)
    );
  }

  matmul(A, B) {
    // A: [m, n], B: [n, p] → [m, p]
    const m = A.length, n = B.length, p = B[0].length;
    const C = Array(m).fill(0).map(() => Array(p).fill(0));
    for (let i = 0; i < m; i++) {
      for (let j = 0; j < p; j++) {
        for (let k = 0; k < n; k++) {
          C[i][j] += A[i][k] * B[k][j];
        }
      }
    }
    return C;
  }

  softmax(arr) {
    const max = Math.max(...arr);
    const exps = arr.map(x => Math.exp(x - max));
    const sum = exps.reduce((a, b) => a + b, 0);
    return exps.map(x => x / sum);
  }

  gelu(x) {
    return 0.5 * x * (1 + Math.tanh(Math.sqrt(2 / Math.PI) * (x + 0.044715 * Math.pow(x, 3))));
  }

  // Embed sequence with positional encoding
  embed(sequence) {
    const embedded = [];
    for (let i = 0; i < sequence.length; i++) {
      const [dx, dy] = sequence[i];
      const normalized = [dx / this.maxMove, dy / this.maxMove];

      // Linear projection
      const emb = Array(this.dModel).fill(0);
      for (let j = 0; j < this.dModel; j++) {
        emb[j] = this.bembed[j];
        for (let k = 0; k < this.inputSize; k++) {
          emb[j] += normalized[k] * this.Wembed[k][j];
        }
      }

      // Add positional encoding
      const pos = Math.min(i, this.seqLen - 1);
      for (let j = 0; j < this.dModel; j++) {
        emb[j] += this.posEmbed[pos][j];
      }

      embedded.push(emb);
    }
    return embedded;
  }

  // Simplified self-attention (single head for speed)
  attention(Q, K, V) {
    const seqLen = Q.length;
    const scores = Array(seqLen).fill(0).map(() => Array(seqLen).fill(0));

    // Compute attention scores: Q @ K^T / sqrt(d_head)
    const scale = Math.sqrt(this.dHead);
    for (let i = 0; i < seqLen; i++) {
      for (let j = 0; j <= i; j++) { // Causal mask: only attend to past
        let score = 0;
        for (let k = 0; k < this.dModel; k++) {
          score += Q[i][k] * K[j][k];
        }
        scores[i][j] = score / scale;
      }
    }

    // Softmax over each row
    const attnWeights = scores.map((row, i) => {
      const validScores = row.slice(0, i + 1);
      const softmaxed = this.softmax(validScores);
      return [...softmaxed, ...Array(seqLen - i - 1).fill(0)];
    });

    // Apply attention to values
    const output = Array(seqLen).fill(0).map(() => Array(this.dModel).fill(0));
    for (let i = 0; i < seqLen; i++) {
      for (let j = 0; j <= i; j++) {
        for (let k = 0; k < this.dModel; k++) {
          output[i][k] += attnWeights[i][j] * V[j][k];
        }
      }
    }

    return output;
  }

  // Forward pass
  forward(sequence) {
    // Embed
    let x = this.embed(sequence);

    // Attention block
    const Q = this.matmul(x, this.Wq);
    const K = this.matmul(x, this.Wk);
    const V = this.matmul(x, this.Wv);
    const attnOut = this.attention(Q, K, V);
    const attnProj = this.matmul(attnOut, this.Wo);

    // Residual + FFN
    x = x.map((vec, i) => vec.map((v, j) => v + attnProj[i][j]));

    // FFN
    const h1 = Array(x.length).fill(0).map((_, i) =>
      Array(this.dModel * 2).fill(0).map((_, j) => {
        let sum = this.b1[j];
        for (let k = 0; k < this.dModel; k++) {
          sum += x[i][k] * this.W1[k][j];
        }
        return this.gelu(sum);
      })
    );

    const h2 = Array(x.length).fill(0).map((_, i) =>
      Array(this.dModel).fill(0).map((_, j) => {
        let sum = this.b2[j];
        for (let k = 0; k < this.dModel * 2; k++) {
          sum += h1[i][k] * this.W2[k][j];
        }
        return sum;
      })
    );

    // Residual
    x = x.map((vec, i) => vec.map((v, j) => v + h2[i][j]));

    // Project to output
    const lastHidden = x[x.length - 1];
    const output = Array(this.inputSize).fill(0).map((_, i) => {
      let sum = this.bout[i];
      for (let j = 0; j < this.dModel; j++) {
        sum += lastHidden[j] * this.Wout[j][i];
      }
      return Math.tanh(sum); // Bound to [-1, 1]
    });

    return { output, hidden: x };
  }

  predict(sequence, shouldLog = false) {
    const { output } = this.forward(sequence);
    const dx = output[0] * this.maxMove;
    const dy = output[1] * this.maxMove;

    if (shouldLog) {
      console.log(`  Predicted: dx=${dx.toFixed(1)} dy=${dy.toFixed(1)}`);
    }

    return { dx, dy };
  }

  // Simple training: gradient descent on output only
  trainSequence(movements) {
    if (movements.length < 2) return 0;

    let totalLoss = 0;
    let count = 0;

    // Train on subsequences
    for (let i = 1; i < movements.length; i++) {
      const start = Math.max(0, i - this.seqLen);
      const seq = movements.slice(start, i);
      const target = movements[i];

      // Forward
      const { output, hidden } = this.forward(seq);
      const lastHidden = hidden[hidden.length - 1];

      // Loss
      const normalized = [target[0] / this.maxMove, target[1] / this.maxMove];
      const loss = normalized.reduce((sum, t, j) => sum + Math.pow(t - output[j], 2), 0);
      totalLoss += loss;
      count++;

      // Gradient
      const grad = output.map((o, j) => 2 * (o - normalized[j]));

      // Update output layer
      for (let j = 0; j < this.dModel; j++) {
        for (let k = 0; k < this.inputSize; k++) {
          this.Wout[j][k] -= this.lr * grad[k] * lastHidden[j];
        }
      }
      for (let k = 0; k < this.inputSize; k++) {
        this.bout[k] -= this.lr * grad[k];
      }
    }

    const avgLoss = totalLoss / count;
    this.health = Math.max(0.3, Math.min(2.0, 1.0 / (1.0 + avgLoss)));
    this.predictionHistory.push({ loss: avgLoss, health: this.health });
    if (this.predictionHistory.length > 100) this.predictionHistory.shift();

    return avgLoss;
  }

  reinitialize() {
    const scale = Math.sqrt(2.0 / this.dModel);
    this.Wembed = this.initWeights(this.inputSize, this.dModel, scale);
    this.Wq = this.initWeights(this.dModel, this.dModel, scale);
    this.Wk = this.initWeights(this.dModel, this.dModel, scale);
    this.Wv = this.initWeights(this.dModel, this.dModel, scale);
    this.Wo = this.initWeights(this.dModel, this.dModel, scale);
    this.W1 = this.initWeights(this.dModel, this.dModel * 2, scale);
    this.W2 = this.initWeights(this.dModel * 2, this.dModel, scale);
    this.Wout = this.initWeights(this.dModel, this.inputSize, scale);
    this.bembed = Array(this.dModel).fill(0);
    this.b1 = Array(this.dModel * 2).fill(0);
    this.b2 = Array(this.dModel).fill(0);
    this.bout = Array(this.inputSize).fill(0);
    this.health = 1.0;
    this.maxMove = 30;
    this.predictionHistory = [];
    console.log("🔄 Tiny GPT reinitialized");
  }

  getPhase() {
    if (this.health < 0.4) return "dying";
    if (this.health < 0.7) return "sickly";
    if (this.health < 1.0) return "healthy";
    if (this.health < 1.3) return "mature";
    return "flourishing";
  }
}

let brain, trail = [], frameCount = 0, playbackMode = false, drawing = false;
let modeButton, resetButton;
let playbackFrames = 0;
let learnedTrail = [];
let generatedHistory = [];
let trailBuffer = null;

function boot({ hud, ui, screen }) {
  brain = new TinyGPT(32, 2, 16); // 32-dim, 2 heads, 16 seq len
  hud.label("neural-garden");

  const btnW = 40, btnH = 12, unlearnW = 50;
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
      trail = [];
      drawing = true;
    }
    playbackMode = false;

    if (trail.length === 0 || num.dist(pen.x, pen.y, trail[trail.length - 1].x, trail[trail.length - 1].y) > 5) {
      trail.push({ x: pen.x, y: pen.y });
      if (trail.length > 150) trail.shift();
    }
  }

  if (modeButton && !modeButton.btn.disabled) {
    modeButton.btn.act(e, {
      push: () => {
        playbackMode = !playbackMode;
        modeButton.replaceLabel(playbackMode ? "DO" : "WATCH");
        if (playbackMode && learnedTrail.length >= 2) {
          playbackFrames = 0;
          trail = [learnedTrail[0]];
          generatedHistory = [[0, 0]];
          console.log(`\n🌊 Generating with attention`);
        } else {
          console.log("⏸ WATCH mode");
          if (learnedTrail.length > 0) trail = [...learnedTrail];
        }
      }
    });
  }

  if (resetButton && !resetButton.btn.disabled) {
    resetButton.btn.act(e, {
      push: () => {
        brain.reinitialize();
        trail = [];
        learnedTrail = [];
        generatedHistory = [];
        playbackMode = false;
        modeButton.replaceLabel("WATCH");
        trailBuffer = null;
      }
    });
  }

  if (e.is("lift")) {
    if (drawing) {
      if (trail.length >= 3) {
        console.log(`\n🌊 Training transformer on ${trail.length} points`);

        const movements = [];
        let maxDist = 0;
        for (let i = 1; i < trail.length; i++) {
          const dx = trail[i].x - trail[i-1].x;
          const dy = trail[i].y - trail[i-1].y;
          maxDist = Math.max(maxDist, Math.sqrt(dx * dx + dy * dy));
          movements.push([dx, dy]);
        }

        brain.maxMove = Math.max(20, maxDist * 1.2);
        learnedTrail = [...trail];

        console.log(`📏 ${trail.length} pts, maxMove=${brain.maxMove.toFixed(1)}`);

        const epochs = 50;
        for (let e = 0; e < epochs; e++) {
          brain.trainSequence(movements);
        }

        const finalLoss = brain.trainSequence(movements);
        console.log(`✅ Loss: ${finalLoss.toFixed(4)}, Health: ${brain.health.toFixed(2)}`);

        // Test
        console.log("\n📊 Predictions:");
        for (let i = 0; i < Math.min(5, movements.length); i++) {
          const start = Math.max(0, i + 1 - brain.seqLen);
          const seq = movements.slice(start, i + 1);
          const pred = brain.predict(seq, false);
          const actual = i + 1 < movements.length ? movements[i + 1] : movements[i];
          const err = Math.sqrt(Math.pow(pred.dx - actual[0], 2) + Math.pow(pred.dy - actual[1], 2));
          console.log(`  ${i}: actual=(${actual[0].toFixed(1)},${actual[1].toFixed(1)}) pred=(${pred.dx.toFixed(1)},${pred.dy.toFixed(1)}) err=${err.toFixed(1)}`);
        }
        console.log("\n");
      }
    }
    drawing = false;
  }
}

function sim({ screen }) {
  frameCount++;

  if (playbackMode && learnedTrail.length >= 2) {
    playbackFrames++;

    if (playbackFrames % 2 === 0) {
      const shouldLog = playbackFrames % 60 === 0;
      const seq = generatedHistory.slice(-brain.seqLen);
      const { dx, dy } = brain.predict(seq, shouldLog);

      const last = trail[trail.length - 1];
      let newX = last.x + dx;
      let newY = last.y + dy;

      while (newX < 0) newX += screen.width;
      while (newX >= screen.width) newX -= screen.width;
      while (newY < 0) newY += screen.height;
      while (newY >= screen.height) newY -= screen.height;

      trail.push({ x: newX, y: newY });
      if (trail.length > 300) trail.shift();

      generatedHistory.push([dx, dy]);
      if (generatedHistory.length > brain.seqLen * 2) generatedHistory.shift();
    }
  }
}

function paint({ wipe, ink, line, box, write, screen, pen, painting, paste }) {
  wipe(10, 10, 25);

  const health = brain.health;
  const phase = brain.getPhase();
  const phaseColors = { dying: [150, 0, 0], sickly: [200, 100, 0], healthy: [0, 200, 0], mature: [0, 150, 100], flourishing: [0, 255, 150] };
  const phaseColor = phaseColors[phase];

  if (trail.length > 0) {
    trailBuffer = painting(screen.width, screen.height, ($) => {
      if (trailBuffer && playbackMode) $.paste(trailBuffer, 0, 0);

      $.ink(...phaseColor);
      const startIdx = playbackMode && trail.length > 50 ? trail.length - 5 : 1;
      for (let i = startIdx; i < trail.length; i++) {
        $.line(trail[i - 1].x, trail[i - 1].y, trail[i].x, trail[i].y);
      }

      if (pen && drawing && !playbackMode) {
        $.ink(255);
        $.circle(pen.x, pen.y, 4);
      }
    });
    paste(trailBuffer, 0, 0);
  }

  const btnW = 40, btnH = 12, unlearnW = 50;
  modeButton.reposition({ x: screen.width - btnW - 4, y: screen.height - btnH - 10 });
  modeButton.paint({ ink, write, box, screen });
  resetButton.reposition({ x: screen.width - btnW - unlearnW - 12, y: screen.height - btnH - 10 });
  resetButton.paint({ ink, write, box, screen });

  const boxW = 50, boxH = 60, boxX = screen.width - boxW - 4, boxY = 4, pad = 2;
  ink(20, 20, 30);
  box(boxX, boxY, boxW, boxH);
  ink(...phaseColor);
  box(boxX, boxY, boxW, boxH, "out");

  ink(...phaseColor);
  write(phase.slice(0, 3), { x: boxX + pad, y: boxY + pad }, undefined, undefined, false, "MatrixChunky8");
  if (playbackMode) ink(0, 255, 0);
  else ink(200);
  write(playbackMode ? "attn" : `${(health * 100).toFixed(0)}%`, { x: boxX + pad, y: boxY + pad + 8 }, undefined, undefined, false, "MatrixChunky8");

  ink(40);
  box(boxX + pad, boxY + pad + 16, boxW - pad * 2, 1);
  ink(...phaseColor);
  box(boxX + pad, boxY + pad + 16, Math.min(boxW - pad * 2, health * (boxW - pad * 2)), 1);

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

  const instrY = screen.height - 10;
  if (trail.length === 0) {
    ink(255, 200, 0);
    write("draw gesture to learn", { x: 4, y: instrY });
  } else if (playbackMode) {
    ink(0, 255, 150);
    write(`generating with attention`, { x: 4, y: instrY });
  }
}

export { boot, act, sim, paint };
