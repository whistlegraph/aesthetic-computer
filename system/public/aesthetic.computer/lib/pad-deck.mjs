// 🎮 Pad Deck
// What a pad's hand sees on the prompt: a line of hints, each button as the
// coloured badge rax gives it (rax/highlight.mjs), and, while the prompt is
// typing, the rings of lib/pad-ring.mjs: the words that may come next round
// the outside, the letter wheel inside. lib/pad-cursor.mjs says what to show.
//
// It is drawn in the piece's own pixels on a canvas laid over the frame
// (window.acFrameGrid), in the prompt's 6×10 letters (lib/pixel-font-data.mjs).
// The canvas takes no pointer events: a mouse goes through it to the piece.

import { glyphRows } from "./pixel-font-data.mjs";

const { floor, max, min, sin, cos, PI } = Math;

// Xbox layout, as rax draws it; a badge lights white while its button is held.
const GREY = ["#5a5f70", "#ffffff"];
const BUTTONS = { A: ["#3fb950", "#ffffff"], B: ["#e5534b", "#ffffff"], X: ["#4a90e2", "#ffffff"], Y: ["#f5c518", "#181818"] };
const LIT = ["#ffffff", "#181818"];
const PLATE = "rgba(21, 23, 30, 0.86)", TEXT = "#f2f2f2", KEY = "#2c3040", DIM = "#8a90a4", PICK = "#ffe066";

// The hints of each state: [badge, what it does]. `D` is the d-pad's cross.
const HINTS = {
  curtain: [["A", "press"], ["X", "type"], ["RS", "mimes"], ["START", "rax"]],
  type: [["LS", "word"], ["A", "write"], ["LB", "more"], ["RS", "letters"], ["X", "delete"], ["Y", "space"], ["RT", "enter"], ["LT", "123"], ["B", "close"]],
  mimes: [["RS", "scroll"], ["B", "prompt"]],
};

// A petal's four signs sit where their buttons do: Y, B, A, X.
const PLACES = [["Y", 0, -8], ["B", 6, 0], ["A", 0, 8], ["X", -6, 0]];
const LABEL = 7; // Letters of a word shown on the ring.

export function padDeck() {
  const face = document.createElement("canvas");
  face.id = "pad-deck";
  Object.assign(face.style, { position: "fixed", left: "0", top: "0", display: "none", pointerEvents: "none", zIndex: "2147483645", imageRendering: "pixelated" });
  document.body.append(face);
  const g = face.getContext("2d");
  let was = ""; // What is drawn.

  const rect = (x, y, w, h, color) => { g.fillStyle = color; g.fillRect(x, y, w, h); };
  function write(text, x, y, color) {
    g.fillStyle = color;
    for (const char of text) {
      const rows = glyphRows[char] || glyphRows["?"];
      for (let r = 0; r < 10; r++) for (let c = 0; c < 6; c++) if (rows[r] & (1 << c)) g.fillRect(x + c, y + r, 1, 1);
      x += 6;
    }
  }

  const badgeWidth = (b) => (b === "D" ? 7 : b.length * 6 + 3);
  const itemWidth = ([b, word]) => badgeWidth(b) + 3 + word.length * 6;
  function badge(b, x, y, held) {
    if (b === "D") { // A small cross, seven square.
      const c = held ? LIT[0] : GREY[0];
      rect(x + 2, y + 1, 3, 7, c);
      rect(x, y + 3, 7, 3, c);
      rect(x + 3, y + 4, 1, 1, KEY);
      return;
    }
    const [fill, ink] = held ? LIT : BUTTONS[b] || GREY;
    rect(x, y, badgeWidth(b), 10, fill);
    write(b, x + 2, y, ink);
  }

  // Hint lines, filled greedily to the width, the last one resting on `bottom`.
  function hints(items, held, width, bottom, atTop) {
    const lines = [[]], GAP = 7, room = width - 10;
    let w = 0;
    for (const item of items) {
      const iw = itemWidth(item);
      if (w && w + GAP + iw > room) { lines.push([]); w = 0; }
      lines.at(-1).push(item);
      w += (w ? GAP : 0) + iw;
    }
    let y = atTop ? 4 : bottom - lines.length * 14;
    const top = y;
    for (const line of lines) {
      const lw = line.reduce((a, i) => a + itemWidth(i), 0) + GAP * (line.length - 1);
      let x = floor((width - lw) / 2);
      rect(x - 3, y, lw + 6, 14, PLATE);
      for (const item of line) {
        badge(item[0], x, y + 2, held.has(item[0]));
        write(item[1], x + badgeWidth(item[0]) + 3, y + 2, TEXT);
        x += itemWidth(item) + GAP;
      }
      y += 14;
    }
    return top;
  }

  const plate = (text, x, y, color, fill = PLATE) => {
    const w = text.length * 6;
    rect(x - floor(w / 2) - 2, y - 6, w + 4, 12, fill);
    write(text, x - floor(w / 2), y - 5, color);
  };

  // The word ring round the letter wheel, resting on `bottom`.
  function rings({ words, word, partial, page, pages, loading, petals, petal }, width, bottom) {
    const half = LABEL * 3 + 3;
    const R = max(34, floor(min(width - half * 2 - 8, bottom - 16) / 2));
    const r = min(R - 22, max(36, floor(R * 0.56))); // Petals 26 tall clear each other from 34 out.
    const cx = floor(width / 2), cy = bottom - R - 8;
    words.forEach((w, i) => {
      const a = (i / words.length) * 2 * PI;
      const text = w.length > LABEL ? w.slice(0, LABEL - 1) + "~" : w;
      const lit = i === word;
      plate(text, cx + floor(sin(a) * R), cy - floor(cos(a) * R), lit ? "#181818" : TEXT, lit ? PICK : PLATE);
    });
    petals.forEach((signs, i) => {
      const a = (i / petals.length) * 2 * PI;
      const px = cx + floor(sin(a) * r), py = cy - floor(cos(a) * r), lit = i === petal;
      rect(px - 9, py - 13, 19, 26, lit ? "#ffffff" : PLATE);
      PLACES.forEach(([b, dx, dy], k) => write(signs[k], px + dx - 2, py + dy - 5, lit ? BUTTONS[b][0] : DIM));
    });
    // The middle: the word pointed at, or what of it is written, or the page.
    const middle = word >= 0 ? words[word] : loading ? "..." : !words.length ? (partial ? partial : "abc") : pages > 1 ? `${page + 1}/${pages}` : partial;
    if (middle) plate(middle.length > 9 ? middle.slice(0, 8) + "~" : middle, cx, cy, word >= 0 ? PICK : TEXT);
  }

  return {
    // mode: "curtain" | "type" | "mimes"; held: the badges held now; rings:
    // what the typing rings show (lib/pad-cursor.mjs).
    draw({ mode, held, rings: shown = null }) {
      const grid = window.acFrameGrid;
      if (!grid?.pixel) return this.hide();
      const { pixel, left, top, width, height } = grid;
      const sign = JSON.stringify([mode, [...held].sort(), shown, pixel, left, top, width, height, !!window.acRaxSheet]);
      if (sign === was) return;
      was = sign;
      if (face.width !== width || face.height !== height) { face.width = width; face.height = height; }
      Object.assign(face.style, { display: "block", left: `${left}px`, top: `${top}px`, width: `${width * pixel}px`, height: `${height * pixel}px` });
      g.clearRect(0, 0, width, height);
      // Leave the curtain's MIMEs wordmark and Rax crosshair visible.
      const above = hints(HINTS[mode].filter(([b]) => b !== "START" || window.acRaxSheet), held, width, height - 4, mode === "curtain");
      if (mode === "type" && shown) rings(shown, width, above - 2);
    },
    hide() {
      if (!was) return;
      was = "";
      face.style.display = "none";
    },
  };
}
