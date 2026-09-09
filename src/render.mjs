// render.mjs — one frame of the Aesthetic Code interface.
//
// The palette is the Aesthetic Computer prompt's dark scheme (disks/prompt.mjs
// `scheme.dark`): purple ground, pink prompt block, orange highlight, magenta
// handle, light-purple secondary text.
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

export const color = {
  reset: "\x1b[0m",
  bold: "\x1b[1m",
  inverse: "\x1b[7m",
  ground: bg(palette.background) + fg(palette.text),
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

export function textWidth(value) {
  return Array.from(cleanText(value)).length;
}

export function clipText(value, width) {
  const characters = Array.from(cleanText(value));
  if (characters.length <= width) return characters.join("");
  if (width <= 1) return "…".slice(0, width);
  return `${characters.slice(0, width - 1).join("")}…`;
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
      const window = characters.slice(0, safeWidth + 1).join("");
      const breakAt = window.lastIndexOf(" ");
      const count = breakAt > Math.floor(safeWidth * 0.45) ? breakAt : safeWidth;
      output.push(characters.slice(0, count).join("").trimEnd());
      remaining = characters.slice(count).join("").trimStart();
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

function pad(value, width) {
  return value + " ".repeat(Math.max(0, width - textWidth(value)));
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

export function renderFrame(state, columns = 80, rows = 24, useColor = true) {
  const width = Math.max(32, columns);
  const height = Math.max(10, rows);
  const ground = useColor ? color.ground : "";
  const reset = useColor ? color.reset : "";

  const mode = state.mode === "local" ? "LOCAL" : "REMOTE";
  const status = String(state.status || "ready").toUpperCase();
  const right = `${paint(useColor, state.mode === "local" ? "status" : "highlight", mode)} · ${paint(useColor, statusTone(state.status), status)}`;
  const rightWidth = textWidth(`${mode} · ${status}`);
  const account = state.account || "not signed in";
  const piece = state.piece ? clipText(state.piece, 24) : "";
  const leftPlain = `AESTHETIC CODE  ${account}${piece ? `  ${piece}` : ""}`;
  const left =
    `${paint(useColor, "bold text", "AESTHETIC CODE")}  ` +
    `${paint(useColor, account.startsWith("@") ? "handle" : "muted", account)}` +
    `${piece ? `  ${paint(useColor, "soft", piece)}` : ""}`;
  const gap = " ".repeat(Math.max(1, width - 2 - textWidth(leftPlain) - rightWidth));
  const header = ` ${left}${gap}${right} `;
  const workspace = clipText(state.workspace || "workspace", Math.max(8, width - 2));
  const pathLine = paint(useColor, "muted", ` ${workspace}`);

  const transcriptRows = height - 5;
  const transcript = state.entries.flatMap((entry) => entryLines(entry, width - 2, useColor));
  const visible = transcript.slice(Math.max(0, transcript.length - transcriptRows));
  while (visible.length < transcriptRows) visible.unshift("");

  let prompt;
  if (state.approval) {
    const subject = clipText(state.approval.subject || "requested action", Math.max(4, width - 31));
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
  const help = paint(
    useColor,
    "muted",
    state.busy ? " ctrl-c interrupt" : " /help · /login · /publish · ctrl-c quit",
  );
  const lines = [header, pathLine, ...visible.map((line) => ` ${line}`), rule, prompt, help];
  return lines
    .slice(0, height)
    .map((line) => `${ground}${pad(line, width)}${reset}`)
    .join("\n");
}
