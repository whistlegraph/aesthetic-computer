// SO SOFT + Pals side identity for Social Software reels.
// Extends the shared AC stamp language with the two-box mark prototyped in
// prompt.mjs: SO and SOFT remain separate outlined objects, joined by a live
// line that changes posture rather than becoming a static sponsor bug.

import { createCanvas, registerFont } from "canvas";
import { resolve } from "node:path";
import { makeSideStamps } from "./side-stamps.mjs";

export async function makeSosoftSideIdentity({ w, h, fps, frames, assetsDir } = {}) {
  try {
    registerFont(resolve("slab/menuband/Sources/MenuBand/Resources/ywft-processing-bold.ttf"), {
      family: "YWFT Processing",
      weight: "bold",
    });
  } catch {}

  const pals = await makeSideStamps({
    w, h, fps, frames, assetsDir,
    title: "SCORES FOR SOCIAL SOFTWARE",
    edgeX: 78,
    stampSize: 128,
    titlePx: 58,
    charScale: 0.48,
    leftCy: h * 0.78,
    rightCy: h * 0.22,
    speed: 0.14,
  });

  const mark = createCanvas(360, 150);
  const m = mark.getContext("2d");
  let spring = 0, velocity = 0;

  function renderMark(t, env) {
    const dt = 1 / fps;
    velocity += (28 * (env - spring) - 8 * velocity) * dt;
    spring += velocity * dt;
    m.clearRect(0, 0, mark.width, mark.height);
    m.font = "bold 42px YWFT Processing, Arial";
    m.textBaseline = "middle";
    m.textAlign = "center";
    m.lineJoin = "round";

    const phase = Math.floor(t / 4.5) % 3;
    const drift = Math.sin(t * 0.75) * 5;
    const boxes = phase === 0
      ? [{ text: "SO", x: 38, y: 54 + drift, w: 76 }, { text: "SOFT", x: 188, y: 54 - drift, w: 134 }]
      : phase === 1
        ? [{ text: "SO", x: 106 + drift, y: 20, w: 76 }, { text: "SOFT", x: 106 - drift, y: 88, w: 134 }]
        : [{ text: "SO", x: 32, y: 18 + drift, w: 76 }, { text: "SOFT", x: 188, y: 82 - drift, w: 134 }];
    const a = boxes[0], b = boxes[1];
    const cyA = a.y + 27, cyB = b.y + 27;

    // Cyan/magenta ghosts bind the SO SOFT mark to the Pals reel chrome.
    for (const [dx, color, alpha] of [[-3, "#ff3c78", .34], [3, "#46aaff", .34], [0, "#fffdf3", 1]]) {
      m.globalAlpha = alpha;
      m.strokeStyle = color;
      m.fillStyle = color;
      m.lineWidth = dx === 0 ? 3 : 5;
      m.beginPath();
      m.moveTo(a.x + a.w + dx, cyA);
      const midX = (a.x + a.w + b.x) / 2 + dx;
      m.bezierCurveTo(midX, cyA + Math.sin(t * 2) * 11, midX, cyB - Math.sin(t * 2) * 11, b.x + dx, cyB);
      m.stroke();
      for (const box of boxes) {
        m.strokeRect(box.x + dx, box.y, box.w, 54);
        m.fillText(box.text, box.x + box.w / 2 + dx, box.y + 29);
      }
    }
    m.globalAlpha = 1;
  }

  function draw(ctx, t, env = 0, tint = null) {
    pals.draw(ctx, t, env, tint);
    renderMark(t, env);
    const pulse = 1 + spring * 0.12;
    const y = h * 0.51 + Math.sin(t * 0.55) * 22;
    for (const side of [-1, 1]) {
      ctx.save();
      ctx.translate(side < 0 ? 26 : w - 26, y);
      ctx.rotate(side < 0 ? Math.PI / 2 : -Math.PI / 2);
      ctx.scale(pulse, pulse);
      ctx.globalAlpha = 0.92;
      ctx.drawImage(mark, -180, -75);
      ctx.restore();
    }
    ctx.globalAlpha = 1;
  }

  return { draw };
}
