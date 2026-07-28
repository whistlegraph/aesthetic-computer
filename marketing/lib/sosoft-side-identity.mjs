// SO SOFT + Pals side identity for Social Software reels.
// Extends the shared AC stamp language with the two-box mark prototyped in
// prompt.mjs: SO and SOFT remain separate outlined objects, joined by a live
// line that changes posture rather than becoming a static sponsor bug.

import { registerFont } from "canvas";
import { resolve } from "node:path";
import { makeSideStamps } from "./side-stamps.mjs";

export async function makeSosoftSideIdentity({
  w, h, fps, frames, assetsDir, showPals = true,
} = {}) {
  try {
    registerFont(resolve("slab/menuband/Sources/MenuBand/Resources/ywft-processing-bold.ttf"), {
      family: "YWFT Processing",
      weight: "bold",
    });
  } catch {}

  const pals = showPals
    ? await makeSideStamps({
      w, h, fps, frames, assetsDir,
      title: "SCORES FOR SOCIAL SOFTWARE",
      edgeX: 78,
      stampSize: 128,
      titlePx: 58,
      charScale: 0.48,
      leftCy: h * 0.78,
      rightCy: h * 0.22,
      speed: 0.14,
    })
    : null;

  let spring = 0, velocity = 0;

  const rgb = (hex) => [1, 3, 5].map((at) => parseInt(hex.slice(at, at + 2), 16));
  const mix = (from, to, amount) => {
    const a = rgb(from), b = rgb(to);
    return `#${a.map((value, i) => Math.round(value + (b[i] - value) * amount).toString(16).padStart(2, "0")).join("")}`;
  };
  const softPinkBlink = (t, response) => response
    * (0.64 + 0.36 * ((Math.sin(t * Math.PI * 2.2) + 1) / 2));

  function advanceSpring(env) {
    const dt = 1 / fps;
    velocity += (28 * (env - spring) - 8 * velocity) * dt;
    spring += velocity * dt;
  }

  function drawMarkVector(m, t, opacity, pinkResponse = 0) {
    // The Processing display face is symbol-mapped on some hosts and renders
    // SO / SOFT as question-mark glyphs. Keep the box geometry expressive but
    // force these identity letters through a dependable Latin face.
    m.font = "bold 42px Arial";
    m.textBaseline = "middle";
    m.textAlign = "center";
    m.lineJoin = "round";

    // One continuous side-by-side posture. The boxes never jump layouts; the
    // changing gap lengthens and shortens their connecting rope while opposite
    // vertical arcs give the pair a gentle suspended swing.
    const reach = 20 + (Math.sin(t * 0.72) + 1) * 18;
    const swing = Math.sin(t * 0.92) * 10;
    const boxes = [
      { text: "SO", x: 32, y: 52 + swing, w: 76 },
      { text: "SOFT", x: 174 + reach, y: 52 - swing, w: 134 },
    ];
    const a = boxes[0], b = boxes[1];
    const cyA = a.y + 27, cyB = b.y + 27;

    // The stamp slowly moves between CSS salmon/pink and the publication's
    // darker blues. Smoothstep holds each end of the cycle long enough to read
    // as a color state rather than a continuously flickering rainbow.
    const rawBlue = (Math.sin(t * 0.48 - 0.9) + 1) / 2;
    const blue = rawBlue * rawBlue * (3 - 2 * rawBlue);
    const flicker = softPinkBlink(t, pinkResponse);
    const color = mix(mix("#ff9bae", "#287ea6", blue), "#ffadc0", flicker);
    const shadowX = 1.5 + Math.sin(t * 0.71) * 0.35;
    const shadowY = 1.5 + Math.cos(t * 0.63) * 0.3;
    const layers = [
      { dx: shadowX, dy: shadowY, color: pinkResponse > 0.01 ? "#ff5e86" : "#071f35", alpha: .12 + flicker * .12, width: 6.75 + flicker * 1.1 },
      { dx: 0, dy: 0, color, alpha: 1, width: 6.25 },
    ];
    for (const { dx, dy, color, alpha, width } of layers) {
      m.globalAlpha = alpha * opacity;
      m.strokeStyle = color;
      m.fillStyle = color;
      // Match the box/rope core to the visible stroke weight of the letters.
      m.lineWidth = width;
      m.beginPath();
      m.moveTo(a.x + a.w + dx, cyA + dy);
      const midX = (a.x + a.w + b.x) / 2 + dx;
      m.bezierCurveTo(midX, cyA + dy + Math.sin(t * 2) * 11, midX, cyB + dy - Math.sin(t * 2) * 11, b.x + dx, cyB + dy);
      m.stroke();
      for (const box of boxes) {
        m.strokeRect(box.x + dx, box.y + dy, box.w, 54);
        m.fillText(box.text, box.x + box.w / 2 + dx, box.y + dy + 29);
      }
    }
    m.globalAlpha = 1;
  }

  function draw(ctx, t, env = 0, pinkResponse = 0) {
    pals?.draw(ctx, t, env, null);
    advanceSpring(env);
    const flicker = softPinkBlink(t, pinkResponse);
    const pulse = (1 + spring * 0.12 + flicker * 0.04) * 0.88;
    for (const side of [-1, 1]) {
      ctx.save();
      // The mark's vertical footprint becomes horizontal after rotation. Keep
      // generous edge room for its pulse, glow, and wobble at full response.
      const baseY = side < 0 ? h * 0.87 : h * 0.13;
      const smoothBob = Math.sin(t * 0.55 + (side < 0 ? 0 : Math.PI)) * 12;
      const wobbleX = pinkResponse * Math.sin(t * 2.1 + side) * 2.2;
      const wobbleY = pinkResponse * Math.sin(t * 1.7 - side) * 3;
      const wobbleAngle = pinkResponse * Math.sin(t * 2.4 + side) * 0.025;
      ctx.translate((side < 0 ? 126 : w - 126) + wobbleX, baseY + smoothBob + wobbleY);
      ctx.rotate((side < 0 ? Math.PI / 2 : -Math.PI / 2) + wobbleAngle);
      ctx.scale(pulse, pulse);
      ctx.translate(-200, -75);
      // Draw directly into the final frame. Avoiding a rotated/scaled bitmap
      // keeps the Latin counters, square corners, and rope edge genuinely crisp.
      if (pinkResponse > 0.01) {
        ctx.shadowColor = `rgba(255,94,134,${0.18 * flicker})`;
        ctx.shadowBlur = 7 + 5 * flicker;
      }
      drawMarkVector(ctx, t, 0.82 + 0.16 * flicker, pinkResponse);
      ctx.restore();
    }
    ctx.globalAlpha = 1;
  }

  return { draw };
}
