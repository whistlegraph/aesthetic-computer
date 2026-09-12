// Easel, 2026.09.11
// What the terminal editor is, what it needs, and the one line that installs it.
//
// A command-line tool has nowhere to be sent to, so this page is the
// destination: it has to say the thing rather than link to it. Three facts, in
// the order someone needs them —
//
//   1. what Easel is
//   2. the line that installs it
//   3. that it drives a vendor CLI you supply
//
// The third is not a footnote. Easel spawns `claude` or `codex` and signs in
// with that tool's own credentials, so a visitor holding neither installs a
// correct, well-tested interface attached to nothing. Burying that would trade
// a minute of honesty for a support thread, so it sits above the fold in its
// own colour.
//
// The install line is tappable and copies itself, because it is going to be
// typed into a terminal and a page that shows a command you cannot take with
// you is a screenshot.

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
  const blockHeight = narrow ? 176 : 218;
  let y = Math.max(narrow ? 16 : 26, Math.round((screen.height - blockHeight) / 2));

  ink(PURPLE).write("EASEL", { x: cx, y, center: "x", size: narrow ? 2 : 3 });
  y += narrow ? 20 : 30;

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

  // The requirement, in its own colour, above everything else it can do.
  ink(WARN).write("needs the claude or codex CLI", { x: cx, y, center: "x" });
  y += 11;
  ink(MUTED).write("and a subscription to one", { x: cx, y, center: "x" });
  y += narrow ? 16 : 22;

  ink(SOFT).write("then run  ac", { x: cx, y, center: "x" });
  y += 12;
  ink(MUTED).write("every save goes live at a URL", { x: cx, y, center: "x" });
  y += 11;
  ink(MUTED).write("you can scan from the screen", { x: cx, y, center: "x" });

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
