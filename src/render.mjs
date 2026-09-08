const ESCAPE = /\x1b(?:\[[0-?]*[ -/]*[@-~]|\][^\x07]*(?:\x07|\x1b\\))/g;
const CONTROLS = /[\x00-\x08\x0b\x0c\x0e-\x1f\x7f]/g;

export const color = {
  reset: "\x1b[0m",
  bold: "\x1b[1m",
  dim: "\x1b[2m",
  pink: "\x1b[38;5;205m",
  cyan: "\x1b[38;5;80m",
  green: "\x1b[38;5;84m",
  yellow: "\x1b[38;5;221m",
  red: "\x1b[38;5;203m",
  inverse: "\x1b[7m",
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

function paint(enabled, tone, value) {
  return enabled ? `${color[tone]}${value}${color.reset}` : value;
}

function pad(value, width) {
  return value + " ".repeat(Math.max(0, width - textWidth(value)));
}

function entryLines(entry, width, useColor) {
  const styles = {
    user: ["YOU", "pink"],
    assistant: ["AC", "cyan"],
    command: ["RUN", "yellow"],
    change: ["EDIT", "green"],
    notice: ["·", "dim"],
    error: ["!", "red"],
  };
  const [label, tone] = styles[entry.kind] || styles.notice;
  const prefix = `${label.padEnd(4)} `;
  const continuation = " ".repeat(5);
  return wrapText(entry.text, Math.max(1, width - 5)).map((line, index) =>
    `${paint(useColor, tone, index === 0 ? prefix : continuation)}${line}`,
  );
}

export function renderFrame(state, columns = 80, rows = 24, useColor = true) {
  const width = Math.max(32, columns);
  const height = Math.max(10, rows);
  const workspace = clipText(state.workspace || "workspace", Math.max(8, width - 34));
  const mode = state.mode === "local" ? "LOCAL" : "REMOTE";
  const right = `${mode} · ${String(state.status || "ready").toUpperCase()}`;
  const gap = " ".repeat(Math.max(1, width - 2 - textWidth("AESTHETIC CODE") - textWidth(right)));
  const header = ` ${paint(useColor, "bold", "AESTHETIC CODE")}${gap}${paint(useColor, state.mode === "local" ? "green" : "yellow", right)} `;
  const pathLine = paint(useColor, "dim", ` ${clipText(workspace, width - 2)}`);

  const transcriptRows = height - 5;
  const transcript = state.entries.flatMap((entry) => entryLines(entry, width - 2, useColor));
  const visible = transcript.slice(Math.max(0, transcript.length - transcriptRows));
  while (visible.length < transcriptRows) visible.unshift("");

  let prompt;
  if (state.approval) {
    const subject = clipText(state.approval.subject || "requested action", Math.max(4, width - 31));
    prompt = `${paint(useColor, "yellow", "ALLOW")} ${subject}  ${paint(useColor, "bold", "y")} once  ${paint(useColor, "bold", "a")} session  ${paint(useColor, "bold", "n")} deny`;
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
    const cursorCell = useColor ? paint(true, "inverse", underCursor) : underCursor;
    prompt = `${paint(useColor, "pink", "›")} ${start > 0 ? "‹" : ""}${before}${cursorCell}${after}`;
  }

  const rule = paint(useColor, "dim", "─".repeat(width));
  const help = paint(useColor, "dim", state.busy ? " ctrl-c interrupt" : " /help · ctrl-c quit");
  const lines = [header, pathLine, ...visible.map((line) => ` ${line}`), rule, prompt, pad(help, width)];
  return lines.slice(0, height).map((line) => pad(line, width)).join("\n");
}
