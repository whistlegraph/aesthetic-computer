// Easel, 2026.09.11
// What the terminal editor is, what it needs, and the one line that installs it.
//
// A command-line tool has nowhere to be sent to, so this page is the
// destination: it has to say the thing rather than link to it. Three facts, in
// the order someone needs them —
//
//   1. what Easel is
//   2. the line that installs it
//   3. that it opens as a standalone desktop app
//
// The download line is tappable and copies itself for the terminal-friendly
// path. The normal Easel experience uses AC's hosted backend; Claude and Codex
// are optional bring-your-own-provider modes.

const INSTALL = "curl -fsSL prompt.ac/easel.sh | sh";
const MANIFEST = "https://aesthetic.computer/easel.json";

// The prompt's own dark scheme, so the page and the thing it installs agree
// about what purple is.
const GROUND = [40, 28, 58];
const INK = [255, 255, 255];
const PINK = [200, 30, 100];
const PURPLE = [150, 110, 220];
const SOFT = [190, 160, 225];
const MUTED = [130, 110, 165];
const WARN = [255, 160, 60];

function donkey({ ink, x, y, scale = 1 }) {
  // A tiny pixel Fonkey: ears up, looking toward the workbench.
  const p = (color, dx, dy, w, h) => ink(color).box({ x: x + dx * scale, y: y + dy * scale, w: w * scale, h: h * scale }, "fill");
  const body = [124, 76, 116], shadow = [48, 32, 68], light = [218, 154, 170], dark = [35, 25, 48], nose = [244, 198, 174];
  p(shadow, 5, 35, 44, 4); p(body, 15, 16, 27, 18); p(body, 10, 22, 34, 12);
  p(body, 8, 9, 12, 18); p(body, 12, 2, 7, 12); p(body, 23, 4, 8, 9);
  p(light, 13, 3, 4, 8); p(light, 25, 5, 4, 7); p(light, 13, 19, 9, 7);
  p(nose, 34, 13, 10, 9); p(dark, 38, 15, 2, 2); p(dark, 25, 12, 3, 3);
  p(dark, 14, 31, 5, 7); p(dark, 34, 30, 5, 8); p(light, 18, 27, 7, 4);
  p(light, 5, 19, 7, 3); p(dark, 3, 17, 4, 3);
}

let version = null;
let copyBtn, copied = 0;

async function fetchVersion() {
  try {
    const response = await fetch(MANIFEST, { cache: "no-cache" });
    if (!response.ok) return;
    version = (await response.json()).version || null;
  } catch {
    // The page is about what Easel is, not what version it is on. A failed
    // lookup drops one line rather than showing an error.
  }
}

function boot({ ui, screen, typeface }) {
  fetchVersion();
  copyBtn = new ui.Button();
}

function paint({ wipe, ink, screen, text }) {
  wipe(GROUND);

  const cx = screen.width / 2;
  const narrow = screen.width < 320;

  // Centre the block rather than starting from the top: this page is short and
  // most screens are not, so anchoring it high leaves a void underneath that
  // reads as something failing to load. The floor keeps it clear of the corner
  // label the system paints at (6, 6), which is how anyone gets back.
  const blockHeight = narrow ? 220 : 280;
  let y = Math.max(narrow ? 16 : 26, Math.round((screen.height - blockHeight) / 2));

  // Framed hero: the same donkey that accompanies the native desktop app.
  const hero = { x: Math.round(cx - (narrow ? 132 : 190)), y: y - 8, w: narrow ? 264 : 380, h: narrow ? 58 : 72 };
  ink([58, 42, 82]).box(hero, "fill");
  ink(PURPLE).box(hero, "outline");
  donkey({ ink, x: hero.x + hero.w - (narrow ? 58 : 68), y: hero.y + (narrow ? 10 : 12), scale: narrow ? 1 : 1.25 });
  ink(PURPLE).write("EASEL", { x: cx - (narrow ? 8 : 12), y: y + 8, center: "x", size: narrow ? 2 : 3 });
  ink(SOFT).write("with fonkey", { x: cx - (narrow ? 8 : 12), y: y + (narrow ? 28 : 36), center: "x", size: narrow ? 0.8 : 1 });
  y += narrow ? 68 : 82;

  ink(SOFT).write("the Aesthetic Computer editor,", { x: cx, y, center: "x" });
  y += 12;
  ink(SOFT).write("in your terminal", { x: cx, y, center: "x" });
  y += narrow ? 18 : 24;

  // The install line, in a box you can tap. Measured rather than estimated: a
  // guess at six pixels per character was wrong on a phone, and the command ran
  // out past both edges of its own border with the first and last letters
  // clipped — on the one line the whole page exists to hand over. Shrink it
  // until it fits instead, because a command that is hard to read is still a
  // command, and a truncated one is a broken install.
  let installScale = 1;
  let installWidth = text.box(INSTALL, { x: 0, y: 0 }, undefined, 1).box.width;
  const room = screen.width - 16;
  while (installWidth + 12 > room && installScale > 0.4) {
    installScale -= 0.1;
    installWidth = text.box(INSTALL, { x: 0, y: 0 }, undefined, installScale).box.width;
  }

  const boxWidth = Math.min(room, Math.round(installWidth) + 12);
  const boxHeight = Math.round(14 * installScale) + 8;
  copyBtn.box = { x: Math.round(cx - boxWidth / 2), y, w: boxWidth, h: boxHeight };

  ink(copyBtn.down ? PINK : [58, 42, 82]).box(copyBtn.box, "fill");
  ink(copyBtn.down ? INK : PURPLE).box(copyBtn.box, "outline");
  ink(INK).write(INSTALL, { x: cx, y: y + 5, center: "x", size: installScale });
  y += boxHeight + 6;

  // Say what tapping did, rather than leaving the tap unacknowledged.
  if (copied > 0) {
    ink(PINK).write("copied", { x: cx, y, center: "x" });
  } else {
    ink(MUTED).write("tap to copy", { x: cx, y, center: "x" });
  }
  y += narrow ? 16 : 22;

  ink(PURPLE).write("a standalone studio for making pieces", { x: cx, y, center: "x" });
  y += 11;
  ink(SOFT).write("pictures · sounds · papers · games", { x: cx, y, center: "x" });
  y += narrow ? 16 : 22;

  ink(WARN).write("AC hosted inference is ready to use", { x: cx, y, center: "x" });
  y += 12;
  ink(MUTED).write("Claude and Codex are optional provider modes", { x: cx, y, center: "x" });
  y += 11;
  ink(MUTED).write("every save goes live at a scannable URL", { x: cx, y, center: "x" });

  if (version) {
    ink(MUTED).write(`v${version}`, { x: cx, y: screen.height - 14, center: "x" });
  }
}

function sim() {
  if (copied > 0) copied -= 1;
}

function act({ event: e, send, needsPaint }) {
  copyBtn?.act(e, {
    push: () => {
      send({ type: "copy", content: INSTALL });
      // Held long enough to read, short enough not to linger over a second tap.
      copied = 120;
      needsPaint();
    },
  });
}

export { boot, paint, sim, act };
export const nohud = false;
