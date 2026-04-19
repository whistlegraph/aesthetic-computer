// Carry, 26.04.19.00.00
// Learn base-10 arithmetic by feel. Tap columns (1s, 10s, 100s) to add
// beads. Ten beads in a column collapse into one bead in the next column —
// that is place value made physical. Hit the target number exactly to score.

const { floor, min, max, sin, PI, random } = Math;

const CAP = 10;
const VALUE = [1, 10, 100];
const LABELS = ["1", "10", "100"];
const COLUMN_COLORS = [
  [80, 200, 255],
  [255, 180, 60],
  [230, 100, 220],
];

let target = 0;
let fillLevel = [0, 0, 0];
let flash = [0, 0, 0];
let carry = null;
let won = 0;
let overshot = 0;
let score = 0;
let best = 0;
let frame = 0;

let columnBtns = [null, null, null];
let columnBoxes = [null, null, null];
let resetBtn = null;

function total() {
  return fillLevel[0] + fillLevel[1] * 10 + fillLevel[2] * 100;
}

function pickTarget(num) {
  const ceiling = min(999, 10 + score * 11);
  target = num.randIntRange(1, ceiling);
  fillLevel = [0, 0, 0];
  carry = null;
  won = 0;
  overshot = 0;
}

function layout(screen) {
  const topZone = 72;
  const bottomZone = 28;
  const gap = 6;
  const colW = floor((screen.width - gap * 4) / 3);
  const colH = screen.height - topZone - bottomZone;
  for (let i = 0; i < 3; i++) {
    const visIdx = 2 - i;
    const x = gap + visIdx * (colW + gap);
    columnBoxes[i] = { x, y: topZone, w: colW, h: colH };
  }
}

function setupButtons(ui) {
  for (let i = 0; i < 3; i++) {
    const b = columnBoxes[i];
    columnBtns[i] = new ui.Button(b.x, b.y, b.w, b.h);
  }
  resetBtn = new ui.Button(0, 0, 40, 16);
}

function boot({ screen, ui, num, hud }) {
  hud.labelBack?.();
  layout(screen);
  setupButtons(ui);
  pickTarget(num);
}

function addBead(col, sound) {
  if (carry || won > 0 || overshot > 0) return;
  if (col === 2 && fillLevel[2] >= CAP - 1) {
    triggerOvershoot(sound);
    return;
  }
  fillLevel[col] += 1;
  flash[col] = 1;
  sound?.synth({
    type: "sine",
    tone: 420 + col * 140,
    duration: 0.05,
    volume: 0.35,
  });
  if (fillLevel[col] >= CAP) {
    carry = { col, t: 0 };
  } else {
    checkState(sound);
  }
}

function checkState(sound) {
  const t = total();
  if (t === target) {
    won = 70;
    score += 1;
    best = max(best, score);
    sound?.synth({ type: "sine", tone: 523, duration: 0.25, volume: 0.3 });
    sound?.synth({ type: "sine", tone: 659, duration: 0.25, volume: 0.3 });
    sound?.synth({ type: "sine", tone: 784, duration: 0.35, volume: 0.3 });
  } else if (t > target) {
    triggerOvershoot(sound);
  }
}

function triggerOvershoot(sound) {
  overshot = 45;
  score = 0;
  sound?.synth({
    type: "sawtooth",
    tone: 110,
    duration: 0.35,
    volume: 0.22,
  });
}

function sim({ sound, num }) {
  frame += 1;
  if (won > 0) {
    won -= 1;
    if (won === 0) pickTarget(num);
  }
  if (overshot > 0) {
    overshot -= 1;
    if (overshot === 0) {
      fillLevel = [0, 0, 0];
      carry = null;
    }
  }
  for (let i = 0; i < 3; i++) {
    if (flash[i] > 0) flash[i] *= 0.86;
  }
  if (carry) {
    carry.t += 1;
    if (carry.t >= 45) {
      const c = carry.col;
      fillLevel[c] = 0;
      if (c < 2) {
        fillLevel[c + 1] += 1;
        flash[c + 1] = 1;
        const note = [0, 659, 784][c + 1] || 784;
        sound?.synth({
          type: "triangle",
          tone: note,
          duration: 0.22,
          volume: 0.4,
        });
      } else {
        sound?.synth({
          type: "sawtooth",
          tone: 90,
          duration: 0.4,
          volume: 0.2,
        });
      }
      carry = null;
      if (fillLevel[2] >= CAP) {
        fillLevel[2] = CAP - 1;
        triggerOvershoot(sound);
      } else if (c < 2 && fillLevel[c + 1] >= CAP) {
        carry = { col: c + 1, t: 0 };
      } else {
        checkState(sound);
      }
    }
  }
}

function drawColumn($, i, shakeX, shakeY) {
  const { ink } = $;
  const b = columnBoxes[i];
  const color = COLUMN_COLORS[i];
  const isActive = columnBtns[i]?.down;
  const isDark = $.dark;

  const bg = isDark
    ? [floor(color[0] * 0.12), floor(color[1] * 0.12), floor(color[2] * 0.12)]
    : [
        floor(255 - (255 - color[0]) * 0.15),
        floor(255 - (255 - color[1]) * 0.15),
        floor(255 - (255 - color[2]) * 0.15),
      ];
  ink(...bg).box(b.x + shakeX, b.y + shakeY, b.w, b.h, "fill");

  const outlineBoost = isActive ? 80 : 0;
  ink(
    min(255, color[0] + outlineBoost),
    min(255, color[1] + outlineBoost),
    min(255, color[2] + outlineBoost),
  ).box(b.x + shakeX, b.y + shakeY, b.w, b.h, "outline");

  ink(color[0], color[1], color[2]).write(LABELS[i], {
    x: b.x + shakeX + 6,
    y: b.y + shakeY + 6,
    size: 2,
  });

  const count = fillLevel[i];
  const beadSlot = floor((b.h - 28) / CAP);
  const beadSize = min(beadSlot - 2, b.w - 16);
  const beadX = b.x + shakeX + floor((b.w - beadSize) / 2);

  const isCarrying = carry && carry.col === i;
  const carryProg = isCarrying ? carry.t / 45 : 0;

  for (let k = 0; k < count; k++) {
    let bx = beadX;
    let by = b.y + shakeY + b.h - 8 - (k + 1) * beadSlot;
    let alpha = 255;
    let pulse = 1;

    if (isCarrying) {
      if (carryProg < 0.5) {
        pulse = 1 + sin(carryProg * PI * 6 + k * 0.3) * 0.18;
      } else {
        const p = (carryProg - 0.5) / 0.5;
        if (i < 2) {
          const nextB = columnBoxes[i + 1];
          const nextCount = fillLevel[i + 1];
          const nextY = nextB.y + nextB.h - 8 - (nextCount + 1) * beadSlot;
          const targetX =
            nextB.x + shakeX + floor((nextB.w - beadSize) / 2);
          bx = floor(bx + (targetX - bx) * p);
          by = floor(by + (nextY - by) * p);
        } else {
          by = floor(by - p * 30);
          alpha = floor((1 - p) * 255);
        }
        pulse = 1 - p * 0.55;
      }
    }

    const size = max(2, floor(beadSize * pulse));
    const cx = bx + floor(beadSize / 2);
    const cy = by + floor(beadSlot / 2);
    ink(color[0], color[1], color[2], alpha).box(
      cx - floor(size / 2),
      cy - floor(size / 2),
      size,
      size,
      "fill",
    );
    ink(255, 255, 255, floor(alpha * 0.45)).box(
      cx - floor(size / 2),
      cy - floor(size / 2),
      size,
      size,
      "outline",
    );
  }

  if (flash[i] > 0.03) {
    ink(255, 255, 255, floor(flash[i] * 80)).box(
      b.x + shakeX,
      b.y + shakeY,
      b.w,
      b.h,
      "fill",
    );
  }
}

function paint($) {
  const { wipe, ink, screen } = $;
  const isDark = $.dark;
  const bg = isDark ? [8, 12, 20] : [242, 240, 250];
  wipe(...bg);

  let sx = 0;
  let sy = 0;
  if (overshot > 0) {
    sx = floor((random() - 0.5) * 6);
    sy = floor((random() - 0.5) * 6);
  }

  const cur = total();
  const tgtColor =
    cur === target
      ? [60, 220, 90]
      : cur > target
        ? [240, 80, 80]
        : isDark
          ? [235, 235, 245]
          : [30, 30, 50];
  ink(...tgtColor).write(`${target}`, {
    center: "x",
    y: 10,
    size: 4,
    screen,
  });

  const curColor =
    cur === target
      ? [60, 220, 90]
      : cur > target
        ? [240, 80, 80]
        : isDark
          ? [140, 140, 170]
          : [140, 140, 170];
  ink(...curColor).write(`= ${cur}`, {
    center: "x",
    y: 44,
    size: 2,
    screen,
  });

  for (let i = 2; i >= 0; i--) drawColumn($, i, sx, sy);

  const footColor = isDark ? [160, 160, 180] : [90, 90, 110];
  ink(...footColor).write(`score ${score}   best ${best}`, {
    x: 4,
    y: screen.height - 14,
  });
  ink(...footColor).write(`tap a column`, {
    x: screen.width - 72,
    y: screen.height - 14,
  });

  if (won > 0) {
    const a = floor((won / 70) * 230);
    ink(60, 220, 90, a).box(
      0,
      floor(screen.height / 2) - 18,
      screen.width,
      36,
    );
    ink(10, 40, 20, a).write("NICE!", {
      center: "xy",
      size: 3,
      screen,
    });
  }

  if (overshot > 0 && overshot > 20) {
    const a = floor((overshot / 45) * 180);
    ink(240, 80, 80, a).write("OVER", {
      center: "xy",
      size: 3,
      screen,
    });
  }
}

function act({ event: e, screen, ui, sound }) {
  if (e.is("reframed")) {
    layout(screen);
    setupButtons(ui);
  }
  for (let i = 0; i < 3; i++) {
    columnBtns[i]?.act(e, {
      push: () => addBead(i, sound),
    });
  }
}

function meta() {
  return {
    title: "Carry",
    desc: "Learn base 10 by feel — tap columns, watch ten become one.",
  };
}

export { boot, sim, paint, act, meta };
