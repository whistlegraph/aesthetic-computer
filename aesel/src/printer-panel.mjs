// printer-panel — the front panel of a printer that is switched on.
//
// One frame of the printer's window as rows of text, plus where its buttons
// landed so a click can find them. Nothing here reads the terminal or the
// network: `panel(state, size)` is the whole of it.
//
//   ╭──────────────────────────────────────────────╮
//   │  donkey and printer    THE NAME, LARGE        │
//   │                        whose it is · its code │
//   │                        ink ███████░░░         │
//   ╰──────────────────────────────────────────────╯
//    prints (click one to open)
//    now printing…
//    [ Prints folder ]  [ Edit prompt ]  [ Switch off ]
import { TILE_INKS } from "./mascot.mjs";

// The letterhead donkey from the top of every Aesel thread (mascot.mjs TILE),
// with a printer where his easel stands. Each text cell is 2x2 pixels, and
// every edge falls on a cell boundary. X is his fur, E his eye, K the printer
// in the easel's pink, P paper: a sheet waiting in the top, and one coming out
// while a job goes through.
const TILE = [
  "..............X.X.......PPPPPP....",
  "..............X.X.......PPPPPP....",
  "..............XXXXXX..KKKKKKKKKK..",
  "..............XEXXXX..KKKKKKKKKK..",
  "...XXXXXXXXXXXXXX.....KKKKKKKKKK..",
  "...XXXXXXXXXXXXXX.....KKKKKKKKKK..",
  "..X.XXXXXXXXXXXXX.................",
  "....XXXXXXXXXXXXX.................",
  "....X.X.......X.X.................",
  "..................................",
];
const SHEET = "PPPPPP", SHEET_X = 24;
const INKS = { ...TILE_INKS, P: [238, 236, 226] };
const QUADRANTS = " ▘▝▀▖▌▞▛▗▚▐▜▄▙▟█";
const fg = ([r, g, b]) => `\x1b[38;2;${r};${g};${b}m`, bg = ([r, g, b]) => `\x1b[48;2;${r};${g};${b}m`;
function cell(pixels) {
  const inks = [...new Set(pixels.filter((p) => p !== "."))];
  const ink = inks.includes("E") ? "E" : inks[0];
  if (!ink) return "\x1b[49m ";
  const glyph = QUADRANTS[pixels.reduce((mask, p, i) => mask | (p === ink ? 1 << i : 0), 0)];
  const other = !pixels.includes(".") && inks.find((p) => p !== ink);
  return `${other ? bg(INKS[other]) : glyph === "█" ? bg(INKS[ink]) : "\x1b[49m"}${fg(INKS[ink])}${glyph}`;
}
export const ART_WIDTH = TILE[0].length / 2;
/** The donkey and his printer as rows of text. Busy, a sheet feeds out below. */
export function art(busy = false, tick = 0) {
  const tile = [...TILE];
  if (busy) {
    const put = (row) => { tile[row] = tile[row].slice(0, SHEET_X) + SHEET + tile[row].slice(SHEET_X + SHEET.length); };
    put(6); put(7);
    if (tick % 2) { put(8); put(9); tile[0] = tile[1] = TILE[9]; }
  }
  const rows = [];
  for (let y = 0; y < tile.length; y += 2) {
    let row = "";
    for (let x = 0; x < tile[y].length; x += 2) row += cell([tile[y][x], tile[y][x + 1], tile[y + 1][x], tile[y + 1][x + 1]]);
    rows.push(row + "\x1b[0m");
  }
  return rows;
}

/** The name as a nameplate: spaced capitals over a rule. Plain letters, since
 *  block letters band wherever a terminal leaves air between its rows. */
export function large(word) {
  const spaced = [...String(word).toUpperCase()].join(" ");
  return ["", `\x1b[1m${spaced}\x1b[22m`, "━".repeat(spaced.length)];
}

const width = (text) => [...text.replace(/\x1b\[[0-9;]*m/g, "")].reduce((sum, ch) => sum + (/[\u{1F300}-\u{1FAFF}]/u.test(ch) ? 2 : 1), 0);
const clip = (text, room) => (width(text) <= room ? text : [...text].slice(0, Math.max(0, room - 1)).join("") + "…");
const pad = (text, room) => text + " ".repeat(Math.max(0, room - width(text)));
const bold = (text) => `\x1b[1m${text}\x1b[22m`, dim = (text) => `\x1b[2m${text}\x1b[22m`;
const button = (label) => `\x1b[7m ${label} \x1b[27m`;

/** The ink meter: ten cells, filled to the level. */
export const meter = (level) => "█".repeat(Math.round(level / 10)) + "░".repeat(10 - Math.round(level / 10));

/**
 * One frame. `state` is what the printer knows; the answer is the rows to
 * write and `zones`, the buttons: { row, from, to, action } in screen cells
 * counted from zero.
 */
export function panel(state, { columns = 80, rows = 24 } = {}) {
  const inner = Math.max(40, columns - 6); // the box's own edges and a margin take six
  const out = [], zones = [];
  const place = (row, from, label, action) => zones.push({ row, from, to: from + width(label) - 1, action });

  // The box: the donkey on the left, the printer's name and facts on the right.
  const picture = art(Boolean(state.job), state.tick || 0);
  const room = inner - ART_WIDTH - 3;
  const name = large(state.name);
  const fits = width(name[1]) <= room;
  const facts = [
    ...(fits ? name : [bold(clip(state.name.toUpperCase(), room))]),
    "",
    clip(`${state.owner ? `${state.owner}’s ` : ""}printer${state.code ? ` · ${bold(state.code)}` : ""} · a page in → ${state.goal}`, room + 8),
    state.ink == null ? dim("ink: reading…") : `ink ${meter(state.level)} ${state.ink.toLocaleString("en-US")} braincells`,
  ];
  out.push(` ╭${"─".repeat(inner + 2)}╮`);
  for (let i = 0; i < Math.max(picture.length, facts.length); i++)
    out.push(` │ ${pad(`${pad(picture[i] || "", ART_WIDTH)}   ${facts[i] || ""}`, inner)} │`);
  out.push(` ╰${"─".repeat(inner + 2)}╯`);
  out.push(dim(`  ${clip(state.on || "", inner)}`));
  out.push("");

  // What is going through now.
  if (state.job) {
    out.push(`  ${bold(`▸ ${clip(state.job.name, 40)}`)}${state.job.from ? `  from ${state.job.from}` : ""}  ${dim(`${state.job.seconds || 0} s`)}`);
    for (const line of [state.job.read, state.job.knobs, state.job.step && `… ${state.job.step}`].filter(Boolean)) out.push(`    ${clip(line, inner - 4)}`);
  } else out.push(dim(state.error ? `  \x1b[31m✗ ${clip(state.error, inner - 4)}\x1b[39m` : "  waiting for a page…"));
  out.push("");

  // The footer is laid out first so the list knows how many rows it has.
  const footer = [["Prints folder", "folder"], ["Edit prompt", "prompt"], ["Switch off", "off"]];
  const listRoom = Math.max(1, rows - out.length - 4);
  const prints = state.prints || [];
  out.push(prints.length ? `  ${bold("prints")} ${dim("— click one to open, or press its number")}` : dim("  no prints yet"));
  const shown = prints.slice(-listRoom).reverse();
  for (const print of shown) {
    const label = `[${String(print.n).padStart(2)}]`;
    const row = `  ${label}  ${print.at}  ${pad(clip(print.name, 26), 26)}  ${pad(clip(print.from || "", 12), 12)}  ${dim(clip(print.file, Math.max(8, inner - 62)))}`;
    place(out.length, 0, row, { open: print.n });
    out.push(row);
  }
  while (out.length < rows - 2) out.push("");
  let x = 2, line = "  ";
  for (const [label, action] of footer) {
    const drawn = button(label);
    place(out.length, x, ` ${label} `, action);
    line += drawn + "  "; x += width(drawn) + 2;
  }
  out.push(line);
  return { rows: out.slice(0, rows), zones };
}

/** The action under a click, or undefined. */
export const hit = (zones, x, y) => zones.find((zone) => zone.row === y && x >= zone.from && x <= zone.to)?.action;
