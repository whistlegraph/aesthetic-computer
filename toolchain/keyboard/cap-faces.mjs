// How each keycap is actually printed on a MacBook Neo.
//
// Kept separate from macbook-neo-layout.mjs on purpose. That file owns the
// canonical label of every cap — the thing validate-keyboard.mjs counts and
// the thing SEMITONE_CAPS is keyed by. This file owns only how a cap's face
// is *drawn*, which is a rendering concern and must never be able to change
// the inventory. A cap with no entry here simply draws its label centred.
//
// Faces are read off the top-down product photograph:
//   • number row, brackets and punctuation print the shifted symbol ABOVE
//   • the big modifiers print a glyph in a bottom CORNER, not a spelled word
//   • control / option / command print a glyph ABOVE the word
//   • every F-key prints its system icon above a small F-number
//   • the Touch ID cap is a circle and prints nothing

// Apple prints its legends in a light, open, geometric sans — noticeably
// thinner and wider than the default Helvetica node-canvas would otherwise
// pick, and in a medium grey rather than black. Matching the weight matters
// more than matching the exact typeface: a 400-weight legend reads as a
// cheap keyboard even when every glyph is correct.
const LEGEND_FONT = '"Helvetica Neue", Helvetica, Arial, sans-serif';
const LEGEND_WEIGHT = 300;

/// Face types: dual (upper/lower), corner, stack, fkey, blank.
export const FACES = {
  // Number row — shifted symbol above the unshifted one.
  "`": { type: "dual", upper: "~", lower: "`" },
  "1": { type: "dual", upper: "!", lower: "1" },
  "2": { type: "dual", upper: "@", lower: "2" },
  "3": { type: "dual", upper: "#", lower: "3" },
  "4": { type: "dual", upper: "$", lower: "4" },
  "5": { type: "dual", upper: "%", lower: "5" },
  "6": { type: "dual", upper: "^", lower: "6" },
  "7": { type: "dual", upper: "&", lower: "7" },
  "8": { type: "dual", upper: "*", lower: "8" },
  "9": { type: "dual", upper: "(", lower: "9" },
  "0": { type: "dual", upper: ")", lower: "0" },
  "-": { type: "dual", upper: "_", lower: "-" },
  "=": { type: "dual", upper: "+", lower: "=" },

  // Brackets and punctuation carry their shifted faces too.
  "[": { type: "dual", upper: "{", lower: "[" },
  "]": { type: "dual", upper: "}", lower: "]" },
  "\\": { type: "dual", upper: "|", lower: "\\" },
  ";": { type: "dual", upper: ":", lower: ";" },
  "'": { type: "dual", upper: '"', lower: "'" },
  ",": { type: "dual", upper: "<", lower: "," },
  ".": { type: "dual", upper: ">", lower: "." },
  "/": { type: "dual", upper: "?", lower: "/" },

  // Big modifiers print a glyph in a bottom corner.
  "tab": { type: "corner", glyph: "⇥", side: "left" },
  "caps lock": { type: "corner", glyph: "⇪", side: "left", dot: true },
  "shift": { type: "corner", glyph: "⇧", side: "auto" },
  "delete": { type: "corner", glyph: "⌫", side: "right" },
  "return": { type: "corner", glyph: "↵", side: "right" },

  // Glyph above a small word.
  "control": { type: "stack", glyph: "⌃", word: "control" },
  "option": { type: "stack", glyph: "⌥", word: "option" },
  "command": { type: "stack", glyph: "⌘", word: "command" },
  "fn": { type: "stack", glyph: "✵", word: "fn", globe: true },

  // esc is one of the few caps that really is a word.
  "esc": { type: "word", word: "esc", side: "left" },

  // Touch ID: a circle with no legend.
  "power": { type: "blank", circle: true },

  // The spacebar is bare.
  "space": { type: "blank" },
};

for (let i = 1; i <= 12; i++) FACES[`F${i}`] = { type: "fkey", n: i };

/// The twelve function-row icons, drawn as primitives rather than a font so
/// the render has no external type dependency. Each receives a unit box.
function fkeyIcon(ctx, n, cx, cy, s) {
  // Thin. At s*0.09 an outlined triangle or speaker closes up and paints
  // itself solid, which is why the transport keys still read as filled after
  // the shapes were switched to stroke. The interior has to survive.
  const line = Math.max(1, s * 0.058);
  ctx.lineWidth = line;
  ctx.lineCap = "round";
  ctx.lineJoin = "round";

  // Every mark in this row is an OUTLINE on the real machine — SF Symbols
  // style, thin stroke, nothing solid. Filling them was a systematic error:
  // at cap size a filled crescent, triangle or speaker reads as a blob and
  // stops matching the row it sits in.
  const sun = (r, rays) => {
    ctx.beginPath();
    ctx.arc(cx, cy, r, 0, Math.PI * 2);
    ctx.stroke();
    for (let i = 0; i < 8; i++) {
      const a = (i * Math.PI) / 4;
      ctx.beginPath();
      ctx.moveTo(cx + Math.cos(a) * (r + rays * 0.45), cy + Math.sin(a) * (r + rays * 0.45));
      ctx.lineTo(cx + Math.cos(a) * (r + rays), cy + Math.sin(a) * (r + rays));
      ctx.stroke();
    }
  };
  const speaker = () => {
    ctx.beginPath();
    ctx.moveTo(cx - s * 0.40, cy - s * 0.14);
    ctx.lineTo(cx - s * 0.22, cy - s * 0.14);
    ctx.lineTo(cx - s * 0.02, cy - s * 0.36);
    ctx.lineTo(cx - s * 0.02, cy + s * 0.36);
    ctx.lineTo(cx - s * 0.22, cy + s * 0.14);
    ctx.lineTo(cx - s * 0.40, cy + s * 0.14);
    ctx.closePath();
    ctx.stroke();
  };
  const arcs = (count) => {
    for (let i = 0; i < count; i++) {
      ctx.beginPath();
      ctx.arc(cx - s * 0.02, cy, s * (0.16 + i * 0.15), -0.85, 0.85);
      ctx.stroke();
    }
  };
  const tri = (ox, dir) => {
    ctx.beginPath();
    ctx.moveTo(cx + ox - dir * s * 0.15, cy - s * 0.24);
    ctx.lineTo(cx + ox + dir * s * 0.17, cy);
    ctx.lineTo(cx + ox - dir * s * 0.15, cy + s * 0.24);
    ctx.closePath();
    ctx.stroke();
  };

  switch (n) {
    case 1: sun(s * 0.13, s * 0.16); break;              // brightness down
    case 2: sun(s * 0.2, s * 0.22); break;               // brightness up
    case 3: {                                            // mission control
      // Three ROUNDED rects in the real arrangement: a wide one top-left, a
      // smaller one beneath it, and a tall one down the right spanning both.
      // It was drawn as two-on-top-of-one-wide, which is a different mark.
      const rr = s * 0.07;
      const box = (bx, by, bw, bh) => {
        const x0 = cx + s * bx, y0 = cy + s * by, w0 = s * bw, h0 = s * bh;
        const r0 = Math.min(rr, w0 / 2, h0 / 2);
        ctx.beginPath();
        ctx.moveTo(x0 + r0, y0);
        ctx.arcTo(x0 + w0, y0, x0 + w0, y0 + h0, r0);
        ctx.arcTo(x0 + w0, y0 + h0, x0, y0 + h0, r0);
        ctx.arcTo(x0, y0 + h0, x0, y0, r0);
        ctx.arcTo(x0, y0, x0 + w0, y0, r0);
        ctx.closePath();
        ctx.stroke();
      };
      box(-0.40, -0.34, 0.42, 0.28);   // wide, top-left
      box(-0.40, 0.06, 0.30, 0.28);    // smaller, below it
      box(0.10, -0.34, 0.30, 0.68);    // tall, right
      break;
    }
    case 4:                                              // spotlight
      ctx.beginPath();
      ctx.arc(cx - s * 0.06, cy - s * 0.06, s * 0.24, 0, Math.PI * 2);
      ctx.stroke();
      ctx.beginPath();
      ctx.moveTo(cx + s * 0.12, cy + s * 0.12);
      ctx.lineTo(cx + s * 0.34, cy + s * 0.34);
      ctx.stroke();
      break;
    case 5:                                              // dictation
      ctx.beginPath();
      if (ctx.roundRect) ctx.roundRect(cx - s * 0.11, cy - s * 0.36, s * 0.22, s * 0.42, s * 0.11);
      else ctx.rect(cx - s * 0.11, cy - s * 0.36, s * 0.22, s * 0.42);
      ctx.stroke();
      ctx.beginPath();
      ctx.arc(cx, cy + s * 0.04, s * 0.24, 0, Math.PI);
      ctx.stroke();
      ctx.beginPath();
      ctx.moveTo(cx, cy + s * 0.28);
      ctx.lineTo(cx, cy + s * 0.4);
      ctx.stroke();
      break;
    case 6: {                                            // do not disturb
      // An OUTLINED crescent, like every other icon in this row. Filling it
      // turns the moon into a solid blob at cap size — the real mark is a
      // thin stroked crescent opening to the upper right.
      const R0 = s * 0.34, R1 = s * 0.30;
      const ox = cx + s * 0.20, oy = cy - s * 0.14;
      ctx.beginPath();
      ctx.arc(cx, cy, R0, Math.PI * 0.36, Math.PI * 1.60, false);
      ctx.arc(ox, oy, R1, Math.PI * 1.32, Math.PI * 0.58, true);
      ctx.closePath();
      ctx.stroke();
      break;
    }
    case 7: tri(-s * 0.16, -1); tri(s * 0.16, -1); break; // rewind
    case 8:                                               // play / pause
      tri(-s * 0.22, 1);
      for (const bx of [s * 0.16, s * 0.32]) {
        ctx.beginPath();
        ctx.moveTo(cx + bx, cy - s * 0.24);
        ctx.lineTo(cx + bx, cy + s * 0.24);
        ctx.stroke();
      }
      break;
    case 9: tri(-s * 0.16, 1); tri(s * 0.16, 1); break;   // fast forward
    case 10:                                              // mute
      speaker();
      // A single slash, running up to the right — not a cross.
      ctx.beginPath();
      ctx.moveTo(cx - s * 0.30, cy + s * 0.34);
      ctx.lineTo(cx + s * 0.26, cy - s * 0.34);
      ctx.stroke();
      break;
    case 11: speaker(); arcs(1); break;                   // volume down
    case 12: speaker(); arcs(2); break;                   // volume up
  }
}

/// Draw a cap's printed face. `ink` is the already-chosen legend colour.
export function drawCapFace(ctx, { x, y, w, h, label, unit, ink, boxes = false }) {
  const face = FACES[label] ?? { type: "plain" };
  ctx.fillStyle = ink;
  ctx.strokeStyle = ink;
  ctx.textAlign = "center";
  ctx.textBaseline = "middle";
  const font = (px, weight = LEGEND_WEIGHT) => {
    ctx.font = `${weight} ${Math.max(4, Math.round(px))}px ${LEGEND_FONT}`;
  };
  const inset = unit * 0.17;

  /// Place a legend so its measured INK box bottom lands exactly on `bottom`.
  /// Setting a baseline instead lets descenders and tall glyphs hang past the
  /// cap edge — which is invisible at render size and is exactly what the
  /// box overlay caught on esc, tab, caps lock and every F-number.
  function putInkBottom(str, tx, bottom) {
    const prev = ctx.textBaseline;
    ctx.textBaseline = "alphabetic";
    const m = ctx.measureText(str);
    put(str, tx, bottom - (m.actualBoundingBoxDescent ?? 0));
    ctx.textBaseline = prev;
  }

  /// Same, anchoring the ink box TOP.
  function putInkTop(str, tx, top) {
    const prev = ctx.textBaseline;
    ctx.textBaseline = "alphabetic";
    const m = ctx.measureText(str);
    put(str, tx, top + (m.actualBoundingBoxAscent ?? 0));
    ctx.textBaseline = prev;
  }

  /// Draw a legend and, in debug mode, its measured ink box. Overflow past
  /// the cap's safe inset and collisions between an upper and lower legend
  /// are the two failures that are invisible at render size and obvious here.
  function put(str, tx, ty) {
    ctx.fillText(str, tx, ty);
    if (!boxes) return;
    const m = ctx.measureText(str);
    const a = m.actualBoundingBoxAscent ?? 0, d = m.actualBoundingBoxDescent ?? 0;
    const left = ctx.textAlign === "center" ? tx - m.width / 2
      : ctx.textAlign === "right" ? tx - m.width : tx;
    ctx.save();
    ctx.strokeStyle = "rgba(255,0,170,0.95)";
    ctx.lineWidth = Math.max(0.5, unit * 0.012);
    ctx.strokeRect(left, ty - a, m.width, a + d);
    ctx.restore();
  }

  switch (face.type) {
    case "blank":
      if (face.circle) {
        ctx.lineWidth = Math.max(1, unit * 0.04);
        ctx.beginPath();
        ctx.arc(x + w / 2, y + h / 2, Math.min(w, h) * 0.3, 0, Math.PI * 2);
        ctx.stroke();
      }
      return;

    case "dual":
      font(unit * 0.26);
      putInkTop(face.upper, x + w / 2, y + inset * 0.85);
      putInkBottom(face.lower, x + w / 2, y + h - inset * 0.85);
      return;

    case "corner": {
      font(unit * 0.3);
      // A right shift prints its glyph on the right; a left shift on the left.
      const right = face.side === "right"
        || (face.side === "auto" && x > ctx.canvas.width / 2);
      ctx.textAlign = right ? "right" : "left";
      const gx = right ? x + w - inset : x + inset;
      putInkBottom(face.glyph, gx, y + h - inset * 0.8);
      if (face.dot) {
        ctx.beginPath();
        ctx.arc(x + inset * 0.75, y + inset, unit * 0.045, 0, Math.PI * 2);
        ctx.fill();
      }
      return;
    }

    case "stack": {
      // fn is the exception: the real cap prints the globe and the word
      // SIDE BY SIDE on one baseline near the bottom, not stacked.
      if (face.globe) {
        const r = unit * 0.11;
        const gy = y + h - inset * 1.05;
        const gx = x + inset + r;
        ctx.lineWidth = Math.max(1, unit * 0.026);
        ctx.beginPath(); ctx.arc(gx, gy, r, 0, Math.PI * 2); ctx.stroke();
        ctx.beginPath(); ctx.moveTo(gx - r, gy); ctx.lineTo(gx + r, gy); ctx.stroke();
        ctx.beginPath();
        ctx.ellipse(gx, gy, r * 0.45, r, 0, 0, Math.PI * 2);
        ctx.stroke();
        font(unit * 0.19);
        ctx.textAlign = "right";
        putInkBottom(face.word, x + w - inset, gy + unit * 0.09);
        return;
      }
      font(unit * 0.26);
      put(face.glyph, x + w / 2, y + h * 0.34);
      font(unit * 0.19);
      putInkBottom(face.word, x + w / 2, y + h - inset * 0.8);
      return;
    }

    case "word":
      font(unit * 0.21);
      ctx.textAlign = "left";
      putInkBottom(face.word, x + inset, y + h - inset * 0.8);
      return;

    case "fkey": {
      const s = unit * 0.40;
      fkeyIcon(ctx, face.n, x + w / 2, y + h * 0.36, s);
      ctx.fillStyle = ink;
      font(unit * 0.17);
      ctx.textAlign = "center";
      putInkBottom(`F${face.n}`, x + w / 2, y + h - inset * 0.7);
      return;
    }

    default:
      // Letter caps print UPPERCASE on the real machine, centred.
      font(label.length > 1 ? unit * 0.21 : unit * 0.34);
      put(label.length === 1 ? label.toUpperCase() : label,
        x + w / 2, y + h / 2);
  }
}
