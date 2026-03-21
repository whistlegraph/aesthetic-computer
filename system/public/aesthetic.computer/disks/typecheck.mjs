// Typecheck, 2026.02.19
// International type specimen for AC fonts.

/* #region 📚 README
  Auto-scrolls and loops seamlessly. Space to pause/resume.
  Drag or arrow keys for manual scroll.
#endregion */

let scrollY = 0;
let dragStart = null;
let dragScrollStart = 0;
let contentHeight = 800;
let autoScroll = true;

const P = 12;
const AUTO_SPEED = 0.4;

// 🥾 Boot
function boot({ cursor }) {
  cursor("native");
}

// 🎨 Paint
function paint({ wipe, ink, screen, write, text, line, needsPaint }) {
  wipe(8, 8, 12);
  const w = screen.width;
  const h = screen.height;

  // wr: write and return exact rendered height via text.box
  const wr = (str, x, y, maxW, ww, font) => {
    const tb = text.box(str, { x, y }, maxW, 1, ww ?? true, font);
    if (!tb) return 0;
    write(str, { x, y }, undefined, maxW, ww ?? true, font);
    return tb.box.height;
  };

  // Draw content twice for seamless loop
  const h1 = drawContent(scrollY, w, ink, line, wr);
  drawContent(scrollY + h1, w, ink, line, wr);
  contentHeight = h1;

  // Scrollbar
  if (contentHeight > h) {
    const barH = Math.max(12, (h / contentHeight) * h);
    const maxScroll = contentHeight - h;
    const barY = (-scrollY / maxScroll) * (h - barH);
    ink(...(autoScroll ? [100, 200, 140] : [40, 42, 58])).box(w - 3, barY, 2, barH);
  }

  if (autoScroll) needsPaint();
}

// 🧮 Sim
function sim() {
  if (!autoScroll) return;
  scrollY -= AUTO_SPEED;
  if (scrollY <= -contentHeight) scrollY += contentHeight; // seamless loop
}

// 🎪 Act
function act({ event: e, screen }) {
  const step = 20;
  const maxUp = -Math.max(0, contentHeight - screen.height);

  if (e.is("keyboard:down: ")) { autoScroll = !autoScroll; return; }

  if (e.is("keyboard:down:arrowup"))   { autoScroll = false; scrollY = Math.min(0, scrollY + step); }
  if (e.is("keyboard:down:arrowdown")) { autoScroll = false; scrollY = Math.max(maxUp, scrollY - step); }
  if (e.is("keyboard:down:pageup"))    { autoScroll = false; scrollY = Math.min(0, scrollY + screen.height); }
  if (e.is("keyboard:down:pagedown"))  { autoScroll = false; scrollY = Math.max(maxUp, scrollY - screen.height); }
  if (e.is("keyboard:down:home"))      { autoScroll = false; scrollY = 0; }
  if (e.is("keyboard:down:end"))       { autoScroll = false; scrollY = maxUp; }

  if (e.is("touch")) {
    autoScroll = false;
    dragStart = e.y;
    dragScrollStart = scrollY;
  }
  if (e.is("draw") && dragStart !== null) {
    scrollY = Math.min(0, dragScrollStart + (e.y - dragStart));
  }
  if (e.is("lift")) dragStart = null;
}

// 📰 Meta
function meta() {
  return {
    title: "typecheck",
    desc: "International type specimen for AC fonts.",
  };
}

export { boot, sim, paint, act, meta };
export const nohud = true;

// 📚 Library

function drawContent(startY, w, ink, line, wr) {
  let y = startY;

  // ── font_1 ─────────────────────────────────────────────────────
  ink(35, 37, 54).line(P, y + 3, w - P, y + 3);
  y += 10;
  ink(255, 210, 55);   y += wr("font_1", P, y) + 2;
  ink(55, 57, 80);     y += wr("6x10  monospace  BDF:6x10 fallback", P, y) + 14;

  ink(210, 210, 228);  y += wr("ABCDEFGHIJKLMNOPQRSTUVWXYZ", P, y) + 2;
  ink(190, 190, 210);  y += wr("abcdefghijklmnopqrstuvwxyz", P, y) + 2;
  ink(170, 170, 192);  y += wr("0123456789", P, y) + 2;
  ink(145, 145, 170);  y += wr("! @ # $ % ^ & * ( ) _ + - = { } [ ] \\ | ; : ' \" , . < > / ? ~ …", P, y, w - P * 2) + 14;

  ink(50, 52, 74);     y += wr("BDF:6x10 fallback ─────────────", P, y, w - P * 2) + 6;
  const f1 = [
    ["Cyrillic", "Съешь же ещё этих мягких французских булок, да выпей чаю."],
    ["Greek",    "Ξεσκεπάζω την ψυχοφθόρα βδελυγμία."],
    ["Accented", "café résumé naïve Zürich Ångström søren Ø ø Ñ ñ í"],
  ];
  for (const [label, sample] of f1) {
    ink(65, 67, 90);    y += wr(label, P, y) + 2;
    ink(195, 195, 215); y += wr(sample, P, y, w - P * 2) + 8;
  }

  y += 22;

  // ── MatrixChunky8 ──────────────────────────────────────────────
  ink(35, 37, 54).line(P, y + 3, w - P, y + 3);
  y += 10;
  ink(100, 220, 160);  y += wr("MatrixChunky8", P, y, undefined, false, "MatrixChunky8") + 2;
  ink(55, 57, 80);     y += wr("8px  proportional  BDF", P, y) + 14;

  ink(210, 210, 228);  y += wr("ABCDEFGHIJKLMNOPQRSTUVWXYZ", P, y, w - P * 2, false, "MatrixChunky8") + 2;
  ink(190, 190, 210);  y += wr("abcdefghijklmnopqrstuvwxyz", P, y, w - P * 2, false, "MatrixChunky8") + 2;
  ink(170, 170, 192);  y += wr("0123456789", P, y, w - P * 2, false, "MatrixChunky8") + 2;
  ink(145, 145, 170);  y += wr("! ? @ # $ % & * ( ) _ + - = [ ] { } \\ | ; : ' \" , . < > / ~ ^ `", P, y, w - P * 2, true, "MatrixChunky8") + 14;

  ink(175, 232, 195);  y += wr("The quick brown fox jumps over the lazy dog.", P, y, w - P * 2, true, "MatrixChunky8");

  y += 22;

  // ── unifont ────────────────────────────────────────────────────
  ink(35, 37, 54).line(P, y + 3, w - P, y + 3);
  y += 10;
  ink(180, 140, 255);  y += wr("unifont", P, y, undefined, false, "unifont") + 2;
  ink(55, 57, 80);     y += wr("8x16  monospace  Unicode 16.0.03", P, y) + 14;

  const uni = [
    ["Latin",    "The quick brown fox jumps over the lazy dog."],
    ["Cyrillic", "Съешь же ещё этих мягких французских булок."],
    ["Greek",    "Ξεσκεπάζω την ψυχοφθόρα βδελυγμία."],
    ["Arabic",   "أبجد هوز حطي كلمن سعفص قرشت ثخذ ضظغ"],
    ["Hebrew",   "שלום עולם כל אנשי טעם"],
    ["Japanese", "いろはにほへとちりぬるをわかよたれそつねならむ"],
    ["Chinese",  "天地玄黄 宇宙洪荒 日月盈昃 辰宿列张"],
    ["Korean",   "다람쥐 헌 쳇바퀴에 타고파"],
  ];
  for (const [label, sample] of uni) {
    ink(65, 67, 90);    y += wr(label, P, y) + 2;
    ink(195, 195, 215); y += wr(sample, P, y, w - P * 2, false, "unifont") + 8;
  }

  y += 24; // bottom gap matches top gap for seamless loop

  return y - startY;
}
