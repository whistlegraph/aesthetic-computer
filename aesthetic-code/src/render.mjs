// render.mjs — one frame of the Aesthetic Code interface.
//
// The palette is the Aesthetic Computer prompt's dark scheme (disks/prompt.mjs
// `scheme.dark`): purple ground, pink prompt block, orange highlight, magenta
// handle, light-purple secondary text.
import { existsSync } from "node:fs";
import { homedir } from "node:os";
import { join } from "node:path";
import { MASCOT_HEIGHT, mascotAt, mascotRow } from "./mascot.mjs";

const ESCAPE = /\x1b(?:\[[0-?]*[ -/]*[@-~]|\][^\x07]*(?:\x07|\x1b\\))/g;
const CONTROLS = /[\x00-\x08\x0b\x0c\x0e-\x1f\x7f]/g;

export const palette = {
  background: [70, 50, 100],
  text: [255, 255, 255],
  prompt: [200, 30, 100],
  block: [200, 30, 100],
  highlight: [255, 100, 0],
  handle: [255, 100, 255],
  soft: [220, 180, 255],
  muted: [170, 150, 205],
  status: [0, 255, 0],
  error: [255, 90, 90],
  you: [255, 90, 160],
  run: [255, 160, 60],
  edit: [130, 255, 130],
};

const truecolor = /truecolor|24bit/i.test(process.env.COLORTERM || "");

// xterm-256 approximation for terminals without 24-bit color (Terminal.app).
// The dark purple ground has no faithful cube entry, so it is pinned to a
// slate-purple index by hand instead of collapsing into gray.
const LEVELS = [0, 95, 135, 175, 215, 255];
const PINNED = new Map([[palette.background.join(","), 60]]);
function cube(rgb) {
  const pinned = PINNED.get(rgb.join(","));
  if (pinned !== undefined) return pinned;
  const nearest = (value) =>
    LEVELS.reduce((best, level, index) => (Math.abs(level - value) < Math.abs(LEVELS[best] - value) ? index : best), 0);
  const [r, g, b] = rgb.map(nearest);
  return 16 + 36 * r + 6 * g + b;
}
const fg = (rgb) => (truecolor ? `\x1b[38;2;${rgb.join(";")}m` : `\x1b[38;5;${cube(rgb)}m`);
const bg = (rgb) => (truecolor ? `\x1b[48;2;${rgb.join(";")}m` : `\x1b[48;5;${cube(rgb)}m`);

// Slab tints the whole Terminal window by session status — lifted while the
// machine works, pulled toward the prompt's pink when it wants you, settled
// deeper when it is done. Painting our own fixed ground on top of that leaves
// the interface one purple and the rest of the window another, so a hard
// rectangle appears around the text and moves every time the status changes.
//
// Where Slab is managing the window, inherit its colour and let the whole
// window carry the signal together. Everywhere else — a plain Terminal, iTerm,
// an ssh session — keep painting, because this palette's light text needs a
// dark ground under it and there is nobody else to supply one.
const slabState = join(
  process.env.SLAB_HOME || join(homedir(), ".local", "share", "slab"),
  "state",
);
const groundMode = (process.env.AESTHETIC_CODE_GROUND || "").toLowerCase();
const slabManagesWindow =
  process.env.TERM_PROGRAM === "Apple_Terminal" && existsSync(slabState);
export const paintsGround =
  groundMode === "paint" || (groundMode !== "inherit" && !slabManagesWindow);

export const color = {
  reset: "\x1b[0m",
  bold: "\x1b[1m",
  inverse: "\x1b[7m",
  ground: (paintsGround ? bg(palette.background) : "") + fg(palette.text),
  text: fg(palette.text),
  prompt: fg(palette.prompt),
  highlight: fg(palette.highlight),
  handle: fg(palette.handle),
  soft: fg(palette.soft),
  muted: fg(palette.muted),
  status: fg(palette.status),
  error: fg(palette.error),
  you: fg(palette.you),
  run: fg(palette.run),
  edit: fg(palette.edit),
  block: bg(palette.block) + fg(palette.text),
};

export function cleanText(value) {
  return String(value ?? "")
    .replace(ESCAPE, "")
    .replace(CONTROLS, "")
    .replace(/\r/g, "")
    .replace(/\t/g, "  ");
}

// Cells wide, not characters long. A terminal gives an emoji or a CJK glyph
// two columns and a combining mark none, so counting characters measures a row
// short — and a row measured short overflows the window, wraps, and scrolls
// the whole frame up. The QR code lives on the bottom rows, so it is the first
// thing that goes. Erring wide is therefore the safe direction: an over-counted
// row is merely a space short, an under-counted one destroys the frame.
const ZERO = [
  [0x0300, 0x036f], [0x200b, 0x200f], [0x20d0, 0x20f0], [0xfe00, 0xfe0f],
];
const WIDE = [
  [0x1100, 0x115f], [0x231a, 0x231b], [0x2329, 0x232a], [0x23e9, 0x23ec],
  [0x23f0, 0x23f0], [0x23f3, 0x23f3], [0x25fd, 0x25fe], [0x2614, 0x2615],
  [0x2648, 0x2653], [0x267f, 0x267f], [0x2693, 0x2693], [0x26a1, 0x26a1],
  [0x26aa, 0x26ab], [0x26bd, 0x26be], [0x26c4, 0x26c5], [0x26ce, 0x26ce],
  [0x26d4, 0x26d4], [0x26ea, 0x26ea], [0x26f2, 0x26f3], [0x26f5, 0x26f5],
  [0x26fa, 0x26fa], [0x26fd, 0x26fd], [0x2705, 0x2705], [0x270a, 0x270b],
  [0x2728, 0x2728], [0x274c, 0x274c], [0x274e, 0x274e], [0x2753, 0x2755],
  [0x2757, 0x2757], [0x2795, 0x2797], [0x27b0, 0x27b0], [0x27bf, 0x27bf],
  [0x2b1b, 0x2b1c], [0x2b50, 0x2b50], [0x2b55, 0x2b55], [0x2e80, 0x303e],
  [0x3041, 0x33ff], [0x3400, 0x4dbf], [0x4e00, 0x9fff], [0xa000, 0xa4cf],
  [0xa960, 0xa97f], [0xac00, 0xd7a3], [0xf900, 0xfaff], [0xfe10, 0xfe19],
  [0xfe30, 0xfe6f], [0xff00, 0xff60], [0xffe0, 0xffe6], [0x1f000, 0x1faff],
  [0x20000, 0x3fffd],
];
const within = (code, ranges) => ranges.some(([low, high]) => code >= low && code <= high);

function charWidth(character) {
  const code = character.codePointAt(0);
  if (within(code, ZERO)) return 0;
  return within(code, WIDE) ? 2 : 1;
}

export function textWidth(value) {
  let width = 0;
  for (const character of cleanText(value)) width += charWidth(character);
  return width;
}

export function clipText(value, width) {
  const characters = Array.from(cleanText(value));
  if (textWidth(characters.join("")) <= width) return characters.join("");
  if (width <= 1) return "…".slice(0, width);
  let used = 0;
  const kept = [];
  for (const character of characters) {
    const columns = charWidth(character);
    if (used + columns > width - 1) break;
    kept.push(character);
    used += columns;
  }
  return `${kept.join("")}…`;
}

export function wrapText(value, width) {
  const safeWidth = Math.max(1, width);
  const output = [];
  const paragraphs = cleanText(value).split("\n");

  for (const paragraph of paragraphs) {
    if (!paragraph) {
      output.push("");
      continue;
    }
    let remaining = paragraph;
    while (textWidth(remaining) > safeWidth) {
      const characters = Array.from(remaining);
      // How many characters fill one row, and the last space inside them — a
      // word break is taken only if it leaves the row better than half full.
      // At least one character always goes, or a glyph wider than the column
      // would spin here forever.
      let used = 0;
      let count = 0;
      let breakAt = -1;
      while (count < characters.length) {
        const columns = charWidth(characters[count]);
        if (used + columns > safeWidth) break;
        if (characters[count] === " ") breakAt = count;
        used += columns;
        count += 1;
      }
      if (count === 0) count = 1;
      const at = breakAt > Math.floor(safeWidth * 0.45) ? breakAt : count;
      output.push(characters.slice(0, at).join("").trimEnd());
      remaining = characters.slice(at).join("").trimStart();
    }
    output.push(remaining);
  }
  return output;
}

// Paint a span, then fall back to the purple ground so the row stays filled.
function paint(enabled, tone, value) {
  if (!enabled) return value;
  const tones = tone.split(" ").map((name) => color[name] || "").join("");
  return `${tones}${value}${color.reset}${color.ground}`;
}

// Make a painted row exactly `width` columns: pad it short, clip it long, and
// carry its colour either way. Every row of every frame goes through this, so
// no row can reach the terminal wide enough to wrap — and since one wrapped
// row scrolls the entire frame, this is what keeps the QR code on screen.
const SPANS = /(\x1b(?:\[[0-?]*[ -/]*[@-~]|\][^\x07]*(?:\x07|\x1b\\)))/;

function fit(value, width) {
  const parts = String(value ?? "").split(SPANS);
  let out = "";
  let used = 0;
  let full = false;
  for (let index = 0; index < parts.length; index += 1) {
    if (!parts[index]) continue;
    if (index % 2 === 1) {
      out += parts[index]; // An escape sequence costs no columns.
      continue;
    }
    if (full) continue;
    for (const character of parts[index]) {
      const columns = charWidth(character);
      // A double-width glyph on the last column is dropped rather than let
      // through: the terminal would wrap it whole onto the next row.
      if (used + columns > width) {
        full = true;
        break;
      }
      out += character;
      used += columns;
    }
  }
  return out + " ".repeat(Math.max(0, width - used));
}

const STYLES = {
  user: ["YOU", "you"],
  assistant: ["AC", "soft"],
  command: ["RUN", "run"],
  change: ["EDIT", "edit"],
  publish: ["PUB", "handle"],
  notice: ["·", "muted"],
  error: ["!", "error"],
};

function entryLines(entry, width, useColor) {
  const [label, tone] = STYLES[entry.kind] || STYLES.notice;
  const prefix = `${label.padEnd(4)} `;
  const continuation = " ".repeat(5);
  const bodyTone = entry.kind === "notice" ? "muted" : entry.kind === "error" ? "error" : "text";
  return wrapText(entry.text, Math.max(1, width - 5)).map(
    (line, index) => `${paint(useColor, tone, index === 0 ? prefix : continuation)}${paint(useColor, bodyTone, line)}`,
  );
}

function statusTone(status) {
  if (status === "ready") return "status";
  if (status === "working" || status === "interrupting" || status === "starting") return "prompt";
  if (status === "approval") return "highlight";
  if (status === "failed" || status === "offline") return "error";
  return "soft";
}

// The entrance. Opening a session means waiting on the engine bridge — the
// handshake is most of a second — so the little guy walks in across that wait
// rather than adding one of his own. When the bridge answers, the interface
// replaces this frame mid-stride, which is the right time for him to stop.
export function renderBoot(elapsed = 0, columns = 80, rows = 24, useColor = true) {
  const width = Math.max(32, columns);
  const height = Math.max(10, rows);
  const ground = useColor ? color.ground : "";
  const reset = useColor ? color.reset : "";

  const { lines: sprite, x } = mascotAt(elapsed);
  const title = "AESTHETIC CODE";
  // He walks along a baseline under the title, indented to the same margin the
  // interface uses so the two frames agree about where the left edge is.
  const floor = Math.floor(height / 2);
  const top = floor - MASCOT_HEIGHT;

  const rows_ = [];
  for (let row = 0; row < height; row += 1) {
    if (row === top - 2) {
      const pad = Math.max(1, Math.floor((width - textWidth(title)) / 2));
      rows_.push(`${" ".repeat(pad)}${paint(useColor, "bold text", title)}`);
      continue;
    }
    if (row === top - 1) {
      const label = "connecting";
      const pad = Math.max(1, Math.floor((width - textWidth(label)) / 2));
      rows_.push(`${" ".repeat(pad)}${paint(useColor, "muted", label)}`);
      continue;
    }
    const band = row - top;
    if (band >= 0 && band < MASCOT_HEIGHT) {
      // Clip on the left: he starts outside the frame and walks in, so early
      // frames show only his trailing edge.
      const glyphs = Array.from(sprite[band]);
      let line = "";
      for (let index = 0; index < glyphs.length; index += 1) {
        const column = x + index;
        if (column < 0) continue;
        if (line === "") line = " ".repeat(column + 1);
        line += glyphs[index];
      }
      rows_.push(paint(useColor, band === 0 ? "handle" : "soft", line));
      continue;
    }
    rows_.push("");
  }

  return rows_
    .slice(0, height)
    .map((line) => `${ground}${fit(line, width)}${reset}`)
    .join("\n");
}

export function renderFrame(state, columns = 80, rows = 24, useColor = true) {
  const width = Math.max(32, columns);
  const height = Math.max(10, rows);
  const ground = useColor ? color.ground : "";
  const reset = useColor ? color.reset : "";

  const mode = state.mode === "local" ? "LOCAL" : "REMOTE";
  const status = String(state.status || "ready").toUpperCase();
  const right = `${paint(useColor, state.mode === "local" ? "status" : "highlight", mode)} · ${paint(useColor, statusTone(state.status), status)}`;
  const rightWidth = textWidth(`${mode} · ${status}`);

  // The prompt rock parks itself over the top-right corner of the terminal
  // window, so the header stops short of it. The rock is a fixed 80 points
  // wide however this window is sized, which lands between eleven and sixteen
  // columns across the font sizes anyone reads code in; sixteen clears it, and
  // the header has the slack to give. A narrow window keeps its status and
  // spends the gutter instead — nothing is worth hiding the state behind a
  // rock that might not be there.
  const rockGutter = width >= 64 ? 16 : 0;
  const room = Math.max(0, width - 3 - rightWidth - rockGutter);
  const title = "AESTHETIC CODE";
  let account = state.account || "not signed in";
  let piece = state.piece ? clipText(state.piece, 24) : "";
  if (textWidth(`${title}  ${account}  ${piece}`) > room) piece = "";
  if (textWidth(`${title}  ${account}`) > room) account = "";
  const leftPlain = clipText(
    `${title}${account ? `  ${account}` : ""}${piece ? `  ${piece}` : ""}`,
    room,
  );
  const left =
    leftPlain === title || !account
      ? paint(useColor, "bold text", leftPlain)
      : `${paint(useColor, "bold text", title)}  ` +
        `${paint(useColor, account.startsWith("@") ? "handle" : "muted", account)}` +
        `${piece ? `  ${paint(useColor, "soft", piece)}` : ""}`;
  const gap = " ".repeat(
    Math.max(1, width - 2 - textWidth(leftPlain) - rightWidth - rockGutter),
  );
  const header = ` ${left}${gap}${right}${" ".repeat(rockGutter)} `;
  const workspace = clipText(state.workspace || "workspace", Math.max(8, width - 2));
  const pathLine = paint(useColor, "muted", ` ${workspace}`);

  const transcriptRows = height - 5;
  // The QR code keeps its own column on the right, so the transcript is
  // narrowed rather than overdrawn. A code is an image, not text: it needs its
  // own black on white to be scannable, so a window with colour switched off or
  // too little room shows the scan URL instead and drops the code.
  // The code needs the rows it occupies and not one more. An earlier `+ 2`
  // asked for breathing room it never used, which put the cliff at 24 rows and
  // hid the code from a 23-row window for no reason a reader could see.
  const qr =
    useColor && state.qr && width >= state.qr.width + 24 && transcriptRows >= state.qr.height
      ? state.qr
      : null;
  const contentWidth = qr ? width - qr.width - 2 : width - 2;
  const transcript = state.entries.flatMap((entry) => entryLines(entry, contentWidth, useColor));
  const visible = transcript.slice(Math.max(0, transcript.length - transcriptRows));
  while (visible.length < transcriptRows) visible.unshift("");

  const body = visible.map((line, index) => {
    const row = ` ${fit(line, contentWidth)}`;
    if (!qr) return row;
    const band = index - (transcriptRows - qr.height);
    return band >= 0 ? `${row} ${qr.lines[band]}` : row;
  });

  let prompt;
  if (state.approval) {
    // The subject gets whatever the label and the three answers leave, measured
    // rather than guessed: a hand-counted margin was two columns short, and the
    // row it overflowed wrapped every approval into a scroll.
    const choices = "  y once  a session  n deny";
    const room = Math.max(4, width - textWidth("ALLOW ") - textWidth(choices));
    const subject = clipText(state.approval.subject || "requested action", room);
    prompt = `${paint(useColor, "highlight bold", "ALLOW")} ${subject}  ${paint(useColor, "bold", "y")} once  ${paint(useColor, "bold", "a")} session  ${paint(useColor, "bold", "n")} deny`;
  } else {
    const input = Array.from(cleanText(state.input || ""));
    const cursor = Math.max(0, Math.min(state.cursor ?? input.length, input.length));
    const room = Math.max(1, width - 3);
    const start = Math.max(0, cursor - room + 1);
    const visibleInput = input.slice(start, start + room);
    const visibleCursor = cursor - start;
    const before = visibleInput.slice(0, visibleCursor).join("");
    const underCursor = visibleInput[visibleCursor] || " ";
    const after = visibleInput.slice(visibleCursor + 1).join("");
    const cursorCell = useColor ? paint(true, "block", underCursor) : underCursor;
    prompt = `${paint(useColor, "prompt bold", "›")} ${start > 0 ? "‹" : ""}${before}${cursorCell}${after}`;
  }

  const rule = paint(useColor, "muted", "─".repeat(width));
  // The little guy keeps the far corner from the QR code. He is one row and he
  // does not move: an animated footer costs a full repaint every few seconds
  // for the rest of the session, and the entrance already showed he is alive.
  // He dances while the machine has the floor and stands while it is yours —
  // the one fact the interface would otherwise need a row and a word to say.
  const pose = Array.from(mascotRow(state.mascotMs ?? 0, Boolean(state.busy)));
  const guy =
    `${paint(useColor, "soft", pose[0])}` +
    `${paint(useColor, "handle", pose[1])}` +
    `${paint(useColor, "soft", pose[2])}`;
  const helpText = state.busy
    ? " ctrl-c interrupt"
    : " /help \u00b7 /login \u00b7 /publish \u00b7 /open \u00b7 /qr \u00b7 ctrl-c quit";
  const help =
    width >= 23
      ? ` ${guy}${paint(useColor, "muted", clipText(helpText, width - 5))}`
      : paint(useColor, "muted", clipText(helpText, width));
  const lines = [header, pathLine, ...body, rule, prompt, help];
  return lines
    .slice(0, height)
    .map((line) => `${ground}${fit(line, width)}${reset}`)
    .join("\n");
}
