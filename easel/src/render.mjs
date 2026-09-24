import {requestFeedback} from "./request-feedback.mjs";
import {syntaxSpans,syntaxLine} from "./syntax.mjs";
import {drawerOptions,drawerIndex} from "./provider-picker.mjs";
// render.mjs — one frame of the aesel interface.
//
// The palette is the Aesthetic Computer prompt's dark scheme (disks/prompt.mjs
// `scheme.dark`): purple ground, pink prompt block, orange highlight, magenta
// handle, light-purple secondary text.
import { existsSync } from "node:fs";
import { homedir } from "node:os";
import { join } from "node:path";
import { MASCOT_HEIGHT, MASCOT_ROW_WIDTH, mascotAt, mascotRow } from "./mascot.mjs";
import { rowSpan } from "./selection.mjs";
import { handleCharacterColors } from "./handle-colors.mjs";
import { aboutMap } from "./about.mjs";
import { formatJoules } from "./energy.mjs";

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
  // A line from another session. Its own colour, because it has to be
  // unmistakable for what it is not: something the user typed.
  inbox: [120, 200, 255],
  // The typing bar in pro: one shade up from the ground, so the line being
  // written is a place and not just a row.
  bar: [95, 70, 135],
  // A reply's cloud: near-white, so the machine's prose reads as a message
  // in a bubble, dark ink on it, with a deeper purple for what is soft.
  cloud: [246, 242, 252],
  cloudInk: [42, 28, 66],
  cloudSoft: [112, 72, 166],
  cloudHighlight: [196, 74, 22],
  // Your own bubble: a pink tint of the cloud, dark pink ink, so the two
  // voices read as two voices.
  typedCloud: [248, 228, 240],
  typedInk: [132, 22, 84],
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
const themeSlots = new Map(Object.entries({text:7,prompt:13,highlight:3,handle:5,soft:6,muted:8,status:2,error:1,you:9,run:11,edit:10}).map(([role,index]) => [palette[role].join(","),index]));
export function coloredHandle(account,colors,useColor=true,hover=false){
  if(!useColor)return account;
  const rgb=Array.isArray(colors)&&colors.length===Array.from(account).length?colors:handleCharacterColors(account);
  // A dark cell behind every letter: the handle's colours are light, and
  // whatever the window's ground is, they need something to stand on.
  return '\x1b[48;5;233m'+(hover?'\x1b[4m':'')+Array.from(account).map((ch,i)=>`${truecolor?'\x1b[38;2;'+rgb[i].join(';')+'m':'\x1b[38;5;'+cube(rgb[i])+'m'}${ch}`).join('')+'\x1b[0m'+color.ground;
}
const fg = (rgb) => followsSlab ? `\x1b[38;5;${themeSlots.get(rgb.join(",")) ?? 7}m` : (truecolor ? `\x1b[38;2;${rgb.join(";")}m` : `\x1b[38;5;${cube(rgb)}m`);
const bg = (rgb) => followsSlab ? (rgb === palette.background ? "\x1b[49m" : "\x1b[48;5;13m") : (truecolor ? `\x1b[48;2;${rgb.join(";")}m` : `\x1b[48;5;${cube(rgb)}m`);

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
const groundMode = (process.env.EASEL_GROUND || "").toLowerCase();
const slabManagesWindow =
  process.env.TERM_PROGRAM === "Apple_Terminal" && existsSync(slabState);
// In a window Slab dresses, the ink follows Slab's theme slots too, so the
// words sit in the palette the window was given rather than in 24-bit tones
// Terminal.app renders pale. EASEL_THEME=own keeps the interface's own ink.
const followsSlab = process.env.EASEL_THEME === "slab" || (process.env.EASEL_THEME !== "own" && slabManagesWindow);
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
  inbox: fg(palette.inbox),
  block: bg(palette.block) + fg(palette.text),
};

// The easel splash paints the piece's name a hue per character. These are the
// palette's saturated entries in the order they read best left to right — warm
// through pink into green — skipping `error` and the two purples, which are the
// interface's own vocabulary and would make a name look like a status.
export const aeselInk = {
  frame: fg(palette.soft),
  name: [
    fg(palette.highlight),
    fg(palette.handle),
    fg(palette.you),
    fg(palette.edit),
    fg(palette.status),
    fg(palette.run),
  ],
  address: fg(palette.muted),
  cursor: fg(palette.prompt),
  legs: fg(palette.muted),
  reset: color.reset + color.ground,
};

// Static, low-contrast grain. SGR styling and selected control backgrounds survive.
export function woodgrain(line,row=0) {
  let x=0,explicit=false,last='',out='';
  for(const token of line.match(/\x1b\[[0-?]*[ -/]*[@-~]|[^\x1b]/gu)||[]) {
    if(token.startsWith('\x1b')) {
      if(token.endsWith('m')) { const codes=token.slice(2,-1).split(';').map(Number);if(codes[0]===0||codes[0]===49)explicit=false;if(codes[0]===48||(codes[0]>=40&&codes[0]<=47))explicit=true;last=''; }
      out+=token;continue;
    }
    if(!explicit) {
      const tone=(Math.floor(x/5)+row*3+Math.floor(x/17))%9;
      const rgb=tone===0?[67,45,30]:tone<3?[58,39,28]:[51,35,27];
      const escape=truecolor?'\x1b[48;2;'+rgb.join(';')+'m':'\x1b[48;5;'+(tone===0?237:tone<3?236:235)+'m';
      if(last!==escape){out+=escape;last=escape;}
    }
    out+=token;x+=charWidth(token);
  }
  return out;
}

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
  inbox: ["↓", "inbox"],
  // A line you typed, in pro: the prompt glyph and the words, no badge.
  typed: ["›", "prompt"],
};

const BODY_TONES = { notice: "muted", error: "error", inbox: "inbox", typed: "prompt" };

// The little markdown a reply is written in, read rather than shown: the
// markers come out and what they marked keeps its meaning as a tone. Bold is
// bold, inline code is soft, a heading is the highlight, a bullet is a bullet.
// Offsets are on the cleaned text, which is what gets wrapped and painted.
export function markdown(source) {
  const spans = [];
  const lines = String(source ?? "").split("\n");
  let offset = 0;
  const out = lines.map((line) => {
    let text = line;
    let lineTone = "";
    const heading = /^\s{0,3}(#{1,6})\s+(.*)$/.exec(text);
    if (heading) { text = heading[2]; lineTone = "highlight bold"; }
    text = text.replace(/^(\s*)[-*+]\s+/, "$1• ");
    // Inline marks, innermost first: code keeps its insides verbatim.
    const inline = [];
    let cleaned = "";
    let i = 0;
    while (i < text.length) {
      let m;
      if ((m = /^`([^`\n]+)`/.exec(text.slice(i)))) { inline.push({ start: cleaned.length, end: cleaned.length + m[1].length, tone: "soft" }); cleaned += m[1]; i += m[0].length; continue; }
      if ((m = /^\*\*([^*\n]+)\*\*/.exec(text.slice(i)))) { inline.push({ start: cleaned.length, end: cleaned.length + m[1].length, tone: "bold" }); cleaned += m[1]; i += m[0].length; continue; }
      if ((m = /^\[([^\]\n]+)\]\((https?:[^)\s]+)\)/.exec(text.slice(i)))) { inline.push({ start: cleaned.length, end: cleaned.length + m[1].length, tone: "soft", url: m[2] }); cleaned += m[1]; i += m[0].length; continue; }
      cleaned += text[i]; i += 1;
    }
    if (lineTone) spans.push({ start: offset, end: offset + cleaned.length, tone: lineTone });
    else for (const span of inline) spans.push({ ...span, start: span.start + offset, end: span.end + offset });
    offset += cleaned.length + 1;
    return cleaned;
  });
  return { text: out.join("\n"), spans };
}

function outputRows(source,width,useColor,tone,code=false,prose=false,ink=null){
  const read = prose && !code ? markdown(source) : { text: source, spans: [] };
  const text = read.text;
  const links=Array.from(text.matchAll(/https?:\/\/[^\s<>"'`]+/g),m=>{const url=m[0].replace(/[.,;!?)\]}]+$/g,'');return {start:m.index,end:m.index+url.length,tone:'soft',url};});
  let sourceOffset=0;
  const codeSpans=code?text.split('\n').flatMap(line=>{const tokens=syntaxSpans(line).map(s=>({...s,start:s.start+sourceOffset,end:s.end+sourceOffset}));sourceOffset+=line.length+1;return tokens;}):[];
  const marked=read.spans.filter(s=>!links.some(l=>s.start<l.end&&s.end>l.start));
  const spans=[...codeSpans.filter(s=>!links.some(l=>s.start<l.end&&s.end>l.start)),...marked,...links].sort((a,b)=>a.start-b.start);
  let cursor=0;
  return wrapText(text,width).map(line=>{
    const start=Math.max(cursor,text.indexOf(line,cursor));cursor=start+line.length;
    return syntaxLine(text,spans,start,cursor,(role,value,span)=>{
      const painted=ink?ink(role==='text'?tone:role,value):paint(useColor,role==='text'?tone:role,value);
      return useColor&&span?.url?`\x1b]8;;${span.url}\x07\x1b[4m${painted}\x1b[24m\x1b]8;;\x07`:painted;
    });
  });
}

function entryLines(entry, width, useColor, gutter = "wide") {
  const [label, tone] = STYLES[entry.kind] || STYLES.notice;
  // Pro reads like a page: your lines and the replies flush left, and only
  // a notice, an error or an inbox line wears a two-cell mark.
  const flush = gutter === "narrow" && (entry.kind === "typed" || entry.kind === "assistant");
  // In pro a reply is a cloud: its prose in dark ink on a near-white ground
  // with a cell of air around it and rounded ends, so what the machine says
  // reads as a message and not as more of the page.
  const cloud = gutter === "narrow" && entry.kind === "assistant" && useColor;
  const typed = gutter === "narrow" && entry.kind === "typed" && useColor;
  const prefix = flush ? "" : gutter === "narrow" ? `${label.slice(0, 1)} ` : `${label.padEnd(4)} `;
  const continuation = " ".repeat(prefix.length);
  // An inbox line leads with who sent it — `↓ host:name · text` — so the
  // sender is read before the request, the way the model reads the stamp.
  const bodyTone = BODY_TONES[entry.kind] || "text";
  if (entry.kind === "typed" && entry.wave !== undefined && useColor) {
    // The wave: each letter takes the next hue along, the whole run shifted
    // one step per frame, the splash's own colours in the splash's own order.
    const hues = aeselInk.name;
    let at = entry.wave;
    const tint = TYPED_TINT;
    return cloudRows(wrapText(cleanText(entry.text), Math.max(1, width - prefix.length - 2)).map((line) => Array.from(line).map((ch) => `${bg(tint.bg)}${hues[(at++) % hues.length]}${ch}`).join("")), width - prefix.length, tint);
  }
  const rows=[],text=cleanText(entry.kind === "inbox" && entry.from ? `${entry.from} · ${entry.text}` : entry.text),parts=text.split(/(^[ \t]*```[^\n]*$)/m);
  let fenced=false;
  for(const part of parts){
    if(/^[ \t]*```/.test(part)){
      fenced=!fenced;
      rows.push(paint(useColor,"muted",part.trim()));continue;
    }
    // Code, and what a command printed, stand on their own ground — a darker
    // cell behind every row, the syntax colours on top — so a block reads as
    // a block and not as more prose.
    const codeGround=useColor&&(fenced||entry.kind==='command'||entry.kind==='change');
    const onGround=(line)=>codeGround?`\x1b[48;5;234m${fit(line,Math.max(1,width-prefix.length))}\x1b[49m`:line;
    if(!fenced){rows.push(...outputRows(part.replace(/^\n|\n$/g,''),Math.max(1,width-prefix.length-(cloud||typed?2:0)),useColor,bodyTone,entry.kind==='command'||entry.kind==='change',entry.kind==='assistant',cloud?(r,v)=>cloudInk(r,v,CLOUD_TINT):typed?(r,v)=>cloudInk(r,v,TYPED_TINT):null).map(onGround));continue;}
    const code=part.replace(/^\n|\n$/g,''),spans=syntaxSpans(code);
    let offset=0;
    for(const line of code.split("\n")){
      let start=offset,used=0;
      for(const ch of line){
        const w=charWidth(ch);
        if(used+w>Math.max(1,width-5)&&used){rows.push(onGround(syntaxLine(code,spans,start,offset,(tone,value)=>paint(useColor,tone,value))));start=offset;used=0;}
        offset+=ch.length;used+=w;
      }
      rows.push(onGround(syntaxLine(code,spans,start,offset,(tone,value)=>paint(useColor,tone,value))));offset++;
    }
  }
  if (cloud) return cloudRows(rows, width, CLOUD_TINT);
  // A typed line in pro is a bubble of its own kind: the cloud's shape in a
  // pink tint, dark pink ink — two voices, two bubbles.
  if (typed) return cloudRows(rows, width, TYPED_TINT);
  if(entry.kind === "notice" && /^Desktop (thread restored|restart|update)/.test(text)) return rows.map(line=>" ".repeat(Math.max(0,Math.floor((width-textWidth(line))/2)))+line);
  return rows.map((line,index)=>`${paint(useColor,tone,index===0?prefix:continuation)}${line}`);
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
  const title = "aesel";
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

// The only gate before the interface. There are exactly two starts, passed by
// the genre catalog, and each gets one visible label. The active route is
// readable from its arrow and colour rather than a second explanatory line.
export function renderGenrePicker(choices, selected = 0, columns = 80, rows = 24, useColor = true) {
  const width = Math.max(32, columns);
  const height = Math.max(10, rows);
  const ground = useColor ? color.ground : "";
  const reset = useColor ? color.reset : "";
  const labels = Array.from(choices || [], (choice) => cleanText(choice));
  const start = Math.max(3, Math.floor(height / 2) - 3);
  const lines = Array.from({ length: height }, () => "");

  const centred = (text, tone) => {
    const clipped = clipText(text, Math.max(1, width - 4));
    const pad = Math.max(1, Math.floor((width - textWidth(clipped)) / 2));
    return `${" ".repeat(pad)}${paint(useColor, tone, clipped)}`;
  };

  lines[start - 2] = centred("AESTHETIC CODE", "bold text");
  lines[start] = centred("CHOOSE A GENRE", "muted");
  labels.forEach((label, index) => {
    const active = index === selected;
    lines[start + 2 + index] = centred(`${active ? "›" : " "} ${label}`, active ? "prompt bold" : "soft");
  });
  lines[height - 2] = centred("↑↓ choose · enter", "muted");

  return lines
    .map((line) => `${ground}${fit(line, width)}${reset}`)
    .join("\n");
}

// The gauge row: everything happening on the far side of the QR code — how many
// people are at the piece, and what their browsers are painting — and, last, the
// running electricity estimate for the session. Parts fall off
// the right as the window narrows, worst news first — a blank frame outranks a
// viewer count, because it is the one thing here that means something is wrong.
//
// Nothing is printed for a fact that has not been established. `here` is null
// until the session server answers, and a null is "not known", not zero — see
// `audience.mjs`. The same goes for the frame.
export function audienceReadout(state, room = 80, useColor = true) {
  const here = state?.here;
  const frame = state?.frame;
  const parts = [];

  // A flat frame is what a piece looks like when it fails without throwing,
  // which is exactly the failure nothing else in the interface can see.
  if (frame?.blank)
    parts.push({
      text: frame.color ? `blank ${frame.color.join(",")}` : "blank",
      tone: "error",
    });

  if (Number.isFinite(here))
    // Green only when somebody is actually there, so the eye can find it
    // without reading it.
    parts.push({ text: `${here} here`, tone: here > 0 ? "status" : "muted" });

  if (Number.isFinite(here) && state.peak > here)
    parts.push({ text: `${state.peak} peak`, tone: "muted" });
  if (frame && !frame.blank && frame.colors > 0)
    parts.push({ text: `${frame.colors} colors`, tone: "muted" });
  if (Number.isFinite(state?.online))
    parts.push({ text: `${state.online} on AC`, tone: "muted" });
  // Last, so it is the first thing the row gives up when the window narrows: a
  // running estimate is the least urgent number here. The tilde is load-bearing
  // — see energy.mjs on why this is an estimate and can only be one.
  if (state?.energy > 0)
    parts.push({ text: `~${formatJoules(state.energy)}`, tone: "muted" });

  if (parts.length === 0) return { plain: "", painted: "" };

  // Drop from the right until it fits, rather than clipping mid-number.
  while (parts.length > 1 && textWidth(parts.map((p) => p.text).join(" \u00b7 ")) > room)
    parts.pop();
  const plain = parts.map((p) => p.text).join(" \u00b7 ");
  if (textWidth(plain) > room) return { plain: "", painted: "" };

  const separator = paint(useColor, "muted", " \u00b7 ");
  return {
    plain,
    painted: parts.map((p) => paint(useColor, p.tone, p.text)).join(separator),
  };
}

// Shared geometry for painting and mouse hit testing, in terminal cells.
export function modelControls(state, columns) {
  const p=state.settings||state.providerSettings;
  if(!p)return [];
  const apply=state.settings?8:0;
  const size=Math.max(6,Math.floor((columns-2-apply)/3));
  const values=[p.backend==='ac'?'AC hosted':`BYO ${p.backend}`,p.model||'CLI default',p.effort||'default'];
  return ['Provider','Model','Effort',...(state.settings?['Apply']:[])].map((label,i)=>({
    action:`settings:${i}`,x:2+i*size,width:i===3?apply:size,
    text:clipText(i===3?'Apply ✓':`${label} ${values[i]} ▾`,i===3?apply-1:size-1),
    tone:['handle','soft','highlight','status'][i],selected:state.settings?.row===i,
  }));
}
function drawerRows(state,columns,rows,useColor) {
  const p=state.settings;if(!p)return [];
  const options=drawerOptions(p),index=p.index??drawerIndex(p);
  const count=Math.min(6,Math.max(1,rows-8),options.length);
  const start=Math.max(0,Math.min(index-Math.floor(count/2),options.length-count));
  const width=columns-2;
  const title=['Provider','Model','Effort','Apply settings'][p.row];
  const heading=paint(useColor,'muted','─ ')+paint(useColor,'handle bold',title)+paint(useColor,'muted',' '+ '─'.repeat(Math.max(0,width-title.length-3)));
  return [heading,...options.slice(start,start+count).map((option,i)=>{
    const selected=start+i===index;
    const value=[p.backend,p.model,p.effort,'apply'][p.row];
    const text=clipText(`${selected?'›':' '} ${option.id===value?'●':'○'} ${option.label}`,width);
    return paint(useColor,selected?'block bold':['handle','soft','highlight','status'][p.row],text.padEnd(width));
  })];
}

export function frameLayout(state, rows = 24) {
  const height = Math.max(10, rows);
  const trayRows = 4;
  return {rows:height, trayStartRow:height-trayRows, trayRows};
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

  // No gutter any more. Slab's corner overlays — the prompt rock, the piece
  // preview — all park along the top of the window, and the header used to
  // reserve sixteen columns so it could sit beside the rock. It no longer sits
  // up there to be sat beside: everything the interface says about itself has
  // moved to the bottom, and the top is scrollback, which is the one thing that
  // can be covered without costing anything. Old lines are already read.
  const rockGutter = 0;
  const room = Math.max(0, width - 3 - rightWidth - rockGutter);
  const title = "Aesel";
  let account = state.account || "not signed in";
  let piece = state.piece ? `${clipText(state.piece, 24)}${state.pieceVersion ? ` v${state.pieceVersion}` : ""}` : "";
  if (textWidth(`${title}  ${account}  ${piece}`) > room) piece = "";
  if (textWidth(`${title}  ${account}`) > room) account = "";
  const leftPlain = clipText(
    `${title}${account ? `  ${account}` : ""}${piece ? `  ${piece}` : ""}`,
    room,
  );
  const titleInk=text=>!useColor?text:Array.from(text).map((ch,i)=>paint(true, (state.hover==='about'?'block ':'')+['highlight','handle','status','soft','prompt'][i%5],ch)).join('');
  const left =
    leftPlain === title || !account
      ? titleInk(leftPlain)
      : `${titleInk(title)}  ` +
        `${account.startsWith("@") ? coloredHandle(account,state.handleColors,useColor,state.hover === "profile") : paint(useColor,"muted",account)}` +
        `${piece ? `  ${paint(useColor, "soft", piece)}` : ""}`;
  const gap = " ".repeat(
    Math.max(1, width - 2 - textWidth(leftPlain) - rightWidth - rockGutter),
  );
  const header = ` ${left}${gap}${right}${" ".repeat(rockGutter)} `;
  // The workspace path and the audience share a row: the path is reference the
  // eye skips after the first second, and the count is the one number in the
  // interface that changes because of somebody else.
  const audience = audienceReadout(
    { ...state.audience, frame: state.health?.frame, energy: state.energy?.joules },
    Math.max(0, width - 4 - textWidth(state.workspace || "workspace")),
    useColor,
  );
  const workspace = clipText(
    state.workspace || "workspace",
    Math.max(8, width - 2 - (audience.plain ? textWidth(audience.plain) + 2 : 0)),
  );
  const pathLine = audience.plain
    ? ` ${paint(useColor, "muted", workspace)}` +
      " ".repeat(Math.max(1, width - 2 - textWidth(workspace) - textWidth(audience.plain))) +
      `${audience.painted} `
    : paint(useColor, "muted", ` ${workspace}`);

  // Pro has no header band: four rows at the bottom — air, the bar, air, the
  // status line — and the rest is the conversation.
  const pro = state.profile?.name === "pro" && !(state.desktop || state.desktopProsePrompt);
  // The shape is data — see layout.mjs — so the rows under the transcript are
  // whatever the layout says, in the order it says them.
  const shape = { bottom: ["gap", "bar", "gap", "status"], status: ["handle", "workspace", "model", "activity"], bar: [95, 70, 135], prompt: "", separator: " · ", ...(state.layout || {}) };
  const transcriptRows = pro ? height - shape.bottom.length : height - 5;
  // The QR code keeps its own column on the right, so the transcript is
  // narrowed rather than overdrawn. A code is an image, not text: it needs its
  // own black on white to be scannable, so a window with colour switched off or
  // too little room shows the scan URL instead and drops the code.
  // The code needs the rows it occupies and not one more. An earlier `+ 2`
  // asked for breathing room it never used, which put the cliff at 24 rows and
  // hid the code from a 23-row window for no reason a reader could see.
  const qr =
    !state.about && useColor && state.qr && width >= state.qr.width + 24 && transcriptRows >= state.qr.height
      ? state.qr
      : null;
  const contentWidth = qr ? width - qr.width - 2 : width - 2;
  const unruled = new Set();
  const transcript = state.about
    ? aboutMap().flatMap((line) => wrapText(line, contentWidth))
    : (() => {
        // In pro the line being worked on is the animation: while the machine
        // has the floor, the last thing typed runs a colour wave through its
        // letters, frame by frame on the dance clock, and settles when the
        // answer lands.
        // A run of typed lines is one bubble: what you said before the machine
        // answered, together, rather than a stack of pills.
        const grouped = pro ? state.entries.reduce((list, entry) => {
          const last = list[list.length - 1];
          if (entry.kind === "user" && last?.kind === "user") { last.text = `${last.text}\n${entry.text}`; return list; }
          list.push(entry.kind === "user" ? { ...entry } : entry);
          return list;
        }, []) : state.entries;
        const working = pro && state.busy ? grouped.reduce((found, entry, index) => (entry.kind === "user" ? index : found), -1) : -1;
        return grouped.flatMap((entry, index) => {
          const lines = entryLines(pro && entry.kind === "user" ? { ...entry, kind: "typed", ...(index === working ? { wave: Math.floor((state.mascotMs || 0) / 120) } : {}) } : entry, contentWidth, useColor, pro ? "narrow" : "wide");
          // A cloud's rows are not ruled; the flag rides along with the row.
          if (pro && (entry.kind === "assistant" || entry.kind === "user")) for (const line of lines) unruled.add(line);
          return lines;
        });
      })();
  // Provisional rows: the tools and subtasks running right now, one line each
  // at the foot of the transcript, gone the moment each one finishes. They
  // are not entries; nothing of them is kept on the page.
  const provisional = pro && state.busy && state.toolsNow?.size
    ? [...state.toolsNow.values()].map((text) => paint(useColor, "muted", clipText(`⋯ ${String(text).replace(/\s+/g, " ")}`, contentWidth)))
    : [];
  for (const row of provisional) { transcript.push(row); unruled.add(row); }
  const drawer=drawerRows(state,width,height,useColor);
  const availableRows=transcriptRows-drawer.length;
  const start = state.about ? Math.min(state.aboutScroll || 0, Math.max(0, transcript.length - transcriptRows))
    : Math.max(0, transcript.length - availableRows - (state.scrollOffset || 0));
  const visible = transcript.slice(start, start + availableRows);
  while (visible.length < availableRows) state.about ? visible.push("") : visible.unshift("");

  visible.push(...drawer);
  const body = visible.map((line, index) => {
    const row = ` ${fit(line, contentWidth)}`;
    if (!qr) return row;
    const band = index - (transcriptRows - qr.height);
    return band >= 0 ? `${row} ${qr.lines[band]}` : row;
  });

  const controlsWidth = process.env.EASEL_DESKTOP ? Math.max(22,width-10) : width;
  let prompt;
  if (state.approval) {
    // The subject gets whatever the label and the three answers leave, measured
    // rather than guessed: a hand-counted margin was two columns short, and the
    // row it overflowed wrapped every approval into a scroll.
    const choices = "  y once  a session  n deny";
    const room = Math.max(4, controlsWidth - textWidth("ALLOW ") - textWidth(choices));
    const subject = clipText(state.approval.subject || "requested action", room);
    prompt = `${paint(useColor, "highlight bold", "ALLOW")} ${subject}  ${paint(useColor, "bold", cleanText(state.approval.choicesText || "y once  a session  n deny"))}`;
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

  if (state.desktopProsePrompt) prompt = "";

  if (pro) {
    // Codex's shape: the transcript, a bar to type in with a blank line on
    // either side, and one muted line under it saying who, where, which model.
    // Nothing sits between the words and the typing.
    const barBg = useColor ? bg(state.tray === "light" ? [232, 232, 236] : state.tray === "dark" ? [28, 28, 32] : shape.bar) : "";
    const ink = (rgb) => (useColor ? fg(rgb) : "");
    let inner;
    state.cursorCell = null;
    if (state.approval) {
      const choices = cleanText(state.approval.choicesText || "y once  a session  n deny");
      const room = Math.max(4, width - textWidth("ALLOW ") - textWidth(choices) - 4);
      inner = `${ink(palette.highlight)}ALLOW${ink(palette.text)} ${clipText(state.approval.subject || "requested action", room)}  ${choices}`;
    } else {
      const input = Array.from(cleanText(state.input || ""));
      const cursor = Math.max(0, Math.min(state.cursor ?? input.length, input.length));
      const room = Math.max(1, width - 2);
      const start = Math.max(0, cursor - room + 1);
      const shown = input.slice(start, start + room);
      const at = cursor - start;
      const under = shown[at] || " ";
      // The cursor is painted by hand: `paint` would hand the row back to the
      // ground after it, and the bar has to run to the edge.
      // No painted block: the terminal's own cursor stands here and blinks
      // the way it does everywhere else. The frame says where it goes.
      const lead = `${shape.prompt ? `${shape.prompt} ` : ""}${start > 0 ? "‹" : ""}`;
      inner = `${shape.prompt ? `${ink(palette.prompt)}${shape.prompt}${ink(palette.text)} ` : ""}${start > 0 ? "‹" : ""}${shown.slice(0, at).join("")}${under === " " && at >= shown.length ? "" : under}${shown.slice(at + 1).join("")}`;
      state.cursorCell = { row: height - shape.bottom.length + shape.bottom.indexOf("bar") + 1, col: 1 + textWidth(lead) + textWidth(shown.slice(0, at).join("")) };
    }
    // The words start on the bar's first cell: the purple is the margin.
    const bar = `${barBg}${ink(palette.text)}${fit(inner, width)}${reset}`;
    const account = state.account || "";
    const model = state.model || state.providerSettings?.model || "";
    const engine = state.providerSettings?.backend || "";
    const statusLine = proStatus(state, width, useColor, shape).line;
    const rows = {
      gap: () => "",
      bar: () => bar,
      status: () => statusLine,
      rule: () => paint(useColor, "muted", "─".repeat(width)),
      help: () => ` ${paint(useColor, "muted", clipText(state.busy ? requestFeedback(state) : "/help · /layout · /inbox · /mode · /ask · ctrl-c quit", width - 2))}`,
      header: () => header,
      path: () => pathLine,
    };
    // Notebook lines: a faint rule under the rest of every transcript row,
    // the way the GUI's page is ruled. Only the blank part of a row is
    // underlined, so the words sit on the line rather than under a bar.
    const ruled = shape.lines === true && useColor ? body.map((row, index) => (unruled.has(visible[index]) ? row : ruleRow(row, width))) : body;
    // The transcript as plain rows, for the selection to read and copy from.
    state.pageRows = body.map((row) => plainRow(row));
    // A question takes the page: what is asked, and three answers, nothing
    // else on screen until one is given.
    if (state.approval) {
      const modal = approvalModal(state, width, transcriptRows, useColor);
      return [...modal, ...shape.bottom.map((name) => (name === "bar" ? `${useColor ? bg(shape.bar) : ""}${fit("", width)}${reset}` : rows[name]?.() ?? ""))].slice(0, height).map((line) => `${ground}${fit(line, width)}${reset}`).join("\n");
    }
    // A selection in progress or just made: those cells in reverse video, on
    // top of whatever the row was painting.
    const shown = state.selection
      ? ruled.map((row, index) => {
          const span = rowSpan(index + 1, state.selection, width);
          return span ? selectRow(state.pageRows[index], span, width, useColor) : row;
        })
      : ruled;
    // Under the hood: the tray — the air, the bar, the status line — sits on
    // black, or on white in light mode, whatever ground the window has. The
    // transcript above is the hood; this is the machinery, and it always
    // looks like itself.
    const light = state.tray === "light";
    const trayGround = useColor ? `${bg(light ? [255, 255, 255] : [0, 0, 0])}${fg(light ? [40, 40, 40] : [230, 230, 230])}` : "";
    const trayMuted = useColor ? fg(light ? [110, 110, 110] : [150, 150, 150]) : "";
    const onTray = (row) => (useColor ? `${trayGround}${String(row).replace(/\x1b\[0m/g, `\x1b[0m${trayGround}`).replace(new RegExp(color.ground.replace(/[[\]\\]/g, "\\$&"), "g"), trayGround).replace(new RegExp(color.muted.replace(/[[\]\\]/g, "\\$&"), "g"), trayMuted)}${trayGround}` : row);
    const trayRows = shape.bottom.map((name) => onTray(name === "bar" ? bar : name === "gap" ? fit("", width) : rows[name]?.() ?? ""));
    return paintDropdown([...shown, ...trayRows].slice(0, height), state, width, height, useColor, shape)
      .map((line) => `${ground}${fit(line, width)}${reset}`)
      .join("\n");
  }

  const rule = paint(useColor, "muted", "─".repeat(width));
  // The little guy keeps the far corner from the QR code. He is one row and he
  // does not move: an animated footer costs a full repaint every few seconds
  // for the rest of the session, and the entrance already showed he is alive.
  // He dances while the machine has the floor and stands while it is yours —
  // the one fact the interface would otherwise need a row and a word to say.
  const pose = Array.from(mascotRow(state.mascotMs ?? 0, Boolean(state.busy)));
  const guy = pose.map((ch,i)=>paint(useColor,i<2?'handle':'soft',ch)).join('');
  const helpText = state.settings ? " ↑ ↓ choose · Tab field · Enter select · Apply saves · Esc cancel" : state.about ? " Esc back · ↑/↓ scroll"
    : state.scrollOffset ? ` ${state.scrollOffset} lines above · End latest`
    : state.hover === "about" ? " aesel home · click"
    : state.hover === "profile" ? " Open profile in browser · click"
    : state.busy
    ? ` ${requestFeedback(state)}`
    : state.profile?.name === "pro" ? " /help \u00b7 /inbox \u00b7 /mode \u00b7 /ask \u00b7 ctrl-c quit"
    : process.env.EASEL_DESKTOP ? "" : " /settings \u00b7 /login \u00b7 /publish \u00b7 /open \u00b7 /qr \u00b7 ctrl-c quit";
  const footerRoom=width-MASCOT_ROW_WIDTH-3;
  const caption=clipText(helpText,Math.max(1,footerRoom));
  const help=state.desktopProsePrompt ? "" : ` ${paint(useColor,"muted",caption)}${" ".repeat(Math.max(1,width-textWidth(caption)-MASCOT_ROW_WIDTH-2))}${process.env.EASEL_DESKTOP==='1'?' '.repeat(MASCOT_ROW_WIDTH):guy} `;
  // Bottom-heavy, so the top of the frame is nothing but scrollback. A preview
  // window or a prompt rock landing over these rows covers lines that have
  // already been read, rather than the title, the handle, the piece, the
  // status, or the thing being typed.
  const controls=modelControls(state,width);
  const controlLine=controls.length?' '+controls.map(c=>paint(useColor,c.selected||state.hover===c.action?'block bold':c.tone+' bold',c.text.padEnd(c.width))).join(''):pathLine;
  const desktop = state.desktop || state.desktopProsePrompt;
  const lines = [...body, desktop ? "" : rule, desktop ? "" : header, desktop ? "" : controlLine, prompt, desktop ? "" : help];
  return lines
    .slice(0, height)
    .map((line,index) => {const fitted=fit(line,width);return `${ground}${useColor && !process.env.EASEL_DESKTOP && index>=height-4?woodgrain(fitted,index-(height-4)):fitted}${reset}`;})
    .join("\n");
}

// What the engine is called on the bottom line and in the title: the hosted
// bridge is Aesthetic Computer's own, so it says so.
export function providerLabel(engine) {
  return engine === "ac" ? "aesthetic" : engine || "";
}

// A path the way fish's prompt shows it: every directory on the way is its
// first letter, the one you are in is whole — `~/a/c/easel` — so the end,
// which is the part that tells you where you are, is never the part cut off.
export function fishPath(path, home = process.env.HOME || "") {
  if (!path) return "";
  let rest = path;
  let head = "";
  if (home && (path === home || path.startsWith(`${home}/`))) {
    head = "~";
    rest = path.slice(home.length);
  }
  const parts = rest.split("/").filter(Boolean);
  const short = parts.map((part, index) => (index === parts.length - 1 ? part : part.startsWith(".") ? part.slice(0, 2) : part[0]));
  return `${head}${short.length ? `/${short.join("/")}` : head ? "" : "/"}`;
}

// The window's title, in the menubar's own vocabulary — a mark, the name, the
// place, the provider, and what the machine is doing when it is doing
// something — so an Easel tab reads like a Claude or Codex tab from across
// the room.
export function windowTitle(state) {
  const provider = providerLabel(state.providerSettings?.backend);
  const doing = state.approval ? "◉ approval" : state.busy ? "● working" : state.status === "connecting" ? "◌ connecting" : state.status === "offline" ? "○ offline" : "";
  return ["🫏 aesel", fishPath(state.workspace), provider, doing].filter(Boolean).join(" · ");
}

// A drop-down: a short list standing on the status line's fact that opened
// it — provider or model — with the current row marked. Where it sits and
// which rows are showing are computed once, here, so the frame that paints it
// and the click that lands on it agree to the cell.
export function dropdownGeometry(state, width, height, shape = state.layout || {}) {
  const drop = state.dropdown;
  if (!drop) return null;
  const bottom = shape.bottom || ["gap", "bar", "gap", "status"];
  const statusRow = bottom.lastIndexOf("status");
  // The anchor row is the status line; without one, the bar.
  const anchor = height - bottom.length + (statusRow >= 0 ? statusRow : Math.max(0, bottom.lastIndexOf("bar")));
  const facts = proStatus(state, width, false, shape).spans;
  const span = facts.find((s) => s.name === "model") || facts.find((s) => s.name === "engine");
  const items = drop.loading ? [{ id: "", label: "loading…", detail: "" }] : drop.items.length ? drop.items : [{ id: "", label: "nothing to choose", detail: "" }];
  const count = Math.max(1, Math.min(items.length, 10, anchor - 2));
  const index = Math.max(0, Math.min(drop.index || 0, items.length - 1));
  const start = Math.max(0, Math.min(index - Math.floor(count / 2), items.length - count));
  const labelWidth = Math.max(...items.map((item) => textWidth(item.label)));
  const detailWidth = Math.max(...items.map((item) => textWidth(item.detail || "")));
  const boxWidth = Math.min(width - 2, 4 + labelWidth + (detailWidth ? 2 + detailWidth : 0) + 1);
  const x = Math.max(0, Math.min(span ? span.x : 1, width - boxWidth - 1));
  // Row 0 is the title; rows 1..count are the choices; all sit above the anchor.
  const top = anchor - count - 1;
  return { x, top, count, start, index, items, labelWidth, detailWidth, boxWidth, title: "provider · model" };
}

function paintDropdown(rows, state, width, height, useColor, shape) {
  const g = dropdownGeometry(state, width, height, shape);
  if (!g || g.top < 0) return rows;
  const bar = useColor ? bg(shape.bar || [95, 70, 135]) : "";
  const reset = useColor ? color.reset : "";
  const cell = (text, tone) => `${useColor ? (tone === "block" ? bg(palette.block) + fg(palette.text) : bar + fg(palette[tone] || palette.text)) : ""}${fit(text, g.boxWidth)}${reset}${useColor ? color.ground : ""}`;
  const place = (y, painted) => { if (y >= 0 && y < rows.length) rows[y] = `${fit(rows[y], g.x)}${painted}`; };
  place(g.top, cell(` ▾ ${g.title}`, "soft"));
  for (let j = 0; j < g.count; j += 1) {
    const item = g.items[g.start + j];
    const selected = g.start + j === g.index;
    const label = item.label.padEnd(g.labelWidth);
    const text = item.header ? ` ${label}${g.detailWidth ? `  ${item.detail || ""}` : ""}` : ` ${selected ? "›" : " "} ${label}${g.detailWidth ? `  ${item.detail || ""}` : ""}`;
    place(g.top + 1 + j, cell(text, selected ? "block" : item.header ? "highlight" : item.muted ? "muted" : "text"));
  }
  return rows;
}

let CORNERS = "slant";
export function setCorners(style) { CORNERS = style === "block" ? "block" : "slant"; }
const CLOUD_TINT = { bg: palette.cloud, ink: palette.cloudInk, soft: palette.cloudSoft, highlight: palette.cloudHighlight };
const TYPED_TINT = { bg: palette.typedCloud, ink: palette.typedInk, soft: [170, 80, 130], highlight: palette.cloudHighlight };
function cloudInk(role, value, tint = CLOUD_TINT) {
  const inks = { text: tint.ink, soft: tint.soft, muted: tint.soft, highlight: tint.highlight, error: palette.error, inbox: tint.soft, prompt: tint.ink };
  const bold = /\bbold\b/.test(role);
  const base = role.replace(/\bbold\b/, "").trim() || "text";
  return `${bg(tint.bg)}${fg(inks[base] || tint.ink)}${bold ? "\x1b[1m" : ""}${value}${bold ? "\x1b[22m" : ""}`;
}

// Rows of a reply wrapped in their cloud: a rounded top edge drawn with the
// lower half-blocks, the rows on the cloud's ground with a cell of air at
// either side, a rounded bottom edge with the upper half-blocks. Code inside
// keeps its own dark ground, the way a code block sits in any bubble.
function cloudRows(rows, width, tint = CLOUD_TINT) {
  // Air is the cloud's own; blank rows at either end are not kept.
  while (rows.length > 1 && !paintedWidth(rows[rows.length - 1])) rows.pop();
  while (rows.length > 1 && !paintedWidth(rows[0])) rows.shift();
  const inner = Math.min(width - 2, Math.max(8, ...rows.map((row) => paintedWidth(row))));
  const edge = fg(tint.bg);
  const ground = `${color.reset}${color.ground}`;
  // Slanted corners by default — the triangles cut the corner on the
  // diagonal — or stepped quadrants when the layout says `corners: block`.
  const slant = CORNERS !== "block";
  const top = `${edge}${slant ? "◢" : "▗"}${"▄".repeat(inner)}${slant ? "◣" : "▖"}${ground}`;
  const bottom = `${edge}${slant ? "◥" : "▝"}${"▀".repeat(inner)}${slant ? "◤" : "▘"}${ground}`;
  const body = rows.map((row) => `${bg(tint.bg)}${fg(tint.ink)} ${fit(row.replace(/\x1b\[49m/g, bg(tint.bg)), inner)}${bg(tint.bg)} ${ground}`);
  return [top, ...body, bottom];
}

// A painted row without its escapes: the words as they stand on the screen.
function plainRow(row) {
  return String(row ?? "").replace(/\x1b(?:\[[0-?]*[ -/]*[@-~]|\][^\x07]*(?:\x07|\x1b\\))/g, "");
}

// A row with part of it selected: the plain words, the chosen cells in
// reverse video. Selected rows give up their colours for the moment, which is
// what a selection looks like everywhere else too.
function selectRow(plain, [from, to], width, useColor) {
  const chars = Array.from(fit(plain, width));
  const before = chars.slice(0, from - 1).join("");
  const chosen = chars.slice(from - 1, to).join("");
  const after = chars.slice(to).join("");
  if (!useColor) return `${before}${chosen}${after}`;
  return `${paint(true, "text", before)}\x1b[7m${paint(true, "text", chosen)}\x1b[27m${paint(true, "text", after)}`;
}

// The question, centred, with its three answers under it. The row in hand
// wears the block; y, a and n are written after each answer for the hand
// that knows them.
export function approvalModal(state, width, rows, useColor) {
  const inner = Math.min(width - 4, 76);
  const subject = wrapText(cleanText(state.approval.subject || "requested action"), inner);
  const choices = [["Allow once", "y"], ["Allow every time this session", "a"], ["Deny", "n"]];
  const index = state.approvalIndex || 0;
  const lines = [
    paint(useColor, "highlight bold", "allow?"),
    "",
    ...subject.map((line) => paint(useColor, "text", line)),
    "",
    ...choices.map(([label, key], i) => (i === index ? paint(useColor, "block bold", ` ${label} `) : paint(useColor, "soft", ` ${label} `)) + paint(useColor, "muted", `  ${key}`)),
  ];
  const top = Math.max(0, Math.floor((rows - lines.length) / 2));
  const left = Math.max(0, Math.floor((width - inner) / 2));
  const out = Array.from({ length: rows }, () => "");
  lines.forEach((line, i) => { if (top + i < rows) out[top + i] = `${" ".repeat(left)}${line}`; });
  return out;
}

// The columns a painted row occupies, escapes not counted.
function paintedWidth(row) {
  return textWidth(String(row ?? "").replace(/\x1b(?:\[[0-?]*[ -/]*[@-~]|\][^\x07]*(?:\x07|\x1b\\))/g, ""));
}

// A transcript row on ruled paper: the words as they were, then the rest of
// the line underlined in the muted ink, faint enough to read as paper.
function ruleRow(row, width) {
  // The underline runs the whole row, words included, in the muted ink where
  // the terminal can colour an underline (SGR 58) and in the text's own ink
  // where it cannot. The ruled paper is the point; the words sit on it.
  const ink = truecolor ? `\x1b[58;2;${palette.muted.join(";")}m` : `\x1b[58;5;${cube(palette.muted)}m`;
  return `\x1b[4m${ink}${fit(row, width).replace(/\x1b\[0m/g, `\x1b[0m\x1b[4m${ink}`)}\x1b[59m\x1b[24m`;
}

// While the machine works the handle breathes: its colours slide toward the
// muted ink and back on a slow sine, a few seconds a cycle, so the pulse
// reads as breathing rather than blinking. Idle, the colours stand still.
// `28s…`, and `· quiet 20s` once nothing has arrived for a while.
export function workingTimer(state, now = Date.now()) {
  const started = state.requestStartedAt || now;
  const seconds = Math.max(0, Math.floor((now - started) / 1000));
  const quiet = Math.max(0, Math.floor((now - (state.lastRequestEventAt || started)) / 1000));
  return `${seconds}s…${quiet >= 15 && state.status !== "approval" ? ` · quiet ${quiet}s` : ""}`;
}
export const BREATH_MS = 3200;
export function breathingHandle(account, state) {
  const base = Array.isArray(state.handleColors) && state.handleColors.length === Array.from(account).length ? state.handleColors : handleCharacterColors(account);
  if (!state.busy) return base;
  const phase = 0.5 + 0.5 * Math.sin(((state.mascotMs || 0) / BREATH_MS) * Math.PI * 2);
  const k = 0.35 + 0.65 * phase;
  return base.map(([r, g, b]) => [r, g, b].map((v, i) => Math.round(palette.muted[i] + (v - palette.muted[i]) * k)));
}

// The status line under the bar, and where each fact on it starts, so the
// frame can paint it and a click can find the model on it.
export function proStatus(state, width, useColor, shape = state.layout || {}) {
  const status = shape.status || ["handle", "workspace", "model", "activity"];
  const separator = shape.separator ?? " · ";
  const account = state.account || "";
  const model = state.modelLabel || state.model || state.providerSettings?.model || "";
  const engine = state.providerSettings?.backend || "";
  const queuedInbox = (state.queued || []).filter((line) => line?.inbox).length;
  const plain = {
    handle: account,
    workspace: clipText(fishPath(state.workspace || ""), Math.max(8, Math.floor(width / 3))),
    model,
    engine: providerLabel(engine),
    mode: state.mode === "local" ? "local" : "remote",
    // The little guy dances on the line while the machine has the floor, and
    // beside him the seconds, the way Claude Code counts them.
    activity: state.busy ? `${mascotRow(state.mascotMs ?? 0, true)} ${workingTimer(state)}${state.toolNow ? ` · ${clipText(state.toolNow.replace(/\s+/g, " "), Math.max(12, Math.floor(width / 3)))}` : ""}` : state.selection && !state.selection.active ? "selected · Enter copies · Esc clears" : state.flash && state.flash.until > Date.now() ? state.flash.text : state.status === "connecting" ? "connecting…" : state.status === "offline" ? "offline" : state.scrollOffset ? `${state.scrollOffset} lines above · End latest` : "",
    inbox: queuedInbox ? `${queuedInbox} inbox queued` : "",
  };
  const muted = (text) => paint(useColor, "muted", text);
  // When the line is short of room, the facts that matter least go first —
  // the engine, then the place, then the model, then the handle — so what
  // the machine is doing is the last thing to be cut.
  const keep = new Set(status);
  const measure = () => [...keep].reduce((n, name) => (plain[name] ? n + textWidth(plain[name]) + (n ? textWidth(separator) : 0) : n), 1);
  for (const name of ["engine", "workspace", "model", "handle", "mode", "inbox"]) {
    if (measure() <= width - 1) break;
    keep.delete(name);
  }
  const spans = [];
  let line = " ";
  let x = 1;
  for (const name of status) {
    if (!keep.has(name)) continue;
    const text = plain[name] || "";
    if (!text) continue;
    if (spans.length) {
      line += muted(separator);
      x += textWidth(separator);
    }
    spans.push({ name, x, width: textWidth(text) });
    // The model underlines under the mouse: it is the one fact that is a control.
    const hovered = (name === "model" && state.hover === "model") || (name === "engine" && state.hover === "provider");
    // While the machine works the handle breathes: bright, then muted, on the
    // dance clock — a pulse you can see from across the room.
    line += name === "handle" && text.startsWith("@") ? coloredHandle(text, breathingHandle(text, state), useColor) : hovered && useColor ? paint(useColor, "muted", `\x1b[4m${text}\x1b[24m`) : muted(text);
    x += textWidth(text);
  }
  return { line, spans };
}

export function transcriptLineCount(state, columns = 80, rows = 24, useColor = true) {
  const width = Math.max(32, columns), height = Math.max(10, rows);
  const qr = useColor && state.qr && width >= state.qr.width + 24 && height - 5 >= state.qr.height ? state.qr : null;
  return state.entries.reduce((count, entry) => count + entryLines(entry, qr ? width - qr.width - 2 : width - 2, false).length, 0);
}

// Terminal mouse coordinates are one-based, like the displayed header row.
export function headerAction(state, columns, rows, x, y) {
  if (columns < 32 || rows < 10) return "";
  // Pro draws no header and no model controls. The one thing to click is the
  // model on the status line, which opens the settings drawer.
  if (state.profile?.name === "pro") {
    const shape = { bottom: ["gap", "bar", "gap", "status"], ...(state.layout || {}) };
    // An open drop-down owns the mouse: a row of it picks, anywhere else closes.
    const g = dropdownGeometry(state, Math.max(32, columns), Math.max(10, rows), shape);
    if (g) {
      const row = y - 1 - g.top - 1;
      if (row >= 0 && row < g.count && x >= g.x + 1 && x <= g.x + g.boxWidth) return `pick:${g.start + row}`;
      return "dismiss";
    }
    const row = shape.bottom.lastIndexOf("status");
    if (row < 0 || y !== rows - (shape.bottom.length - 1 - row)) return "";
    const hit = proStatus(state, Math.max(32, columns), false, shape).spans.find((span) => (span.name === "model" || span.name === "engine") && x >= span.x + 1 && x <= span.x + span.width);
    return hit ? (hit.name === "engine" ? "provider" : "model") : "";
  }
  if(y===rows-2){const hit=modelControls(state,columns).find(c=>x>=c.x&&x<c.x+c.width);return hit?.action||"";}
  if(state.settings){
    const p=state.settings,options=drawerOptions(p),index=p.index??drawerIndex(p);
    const count=Math.min(6,Math.max(1,rows-8),options.length);
    const start=Math.max(0,Math.min(index-Math.floor(count/2),options.length-count));
    const first=rows-5-count+1;
    if(y>=first&&y<first+count&&x>=2&&x<columns)return `choice:${start+y-first}`;
  }
  if(y!==rows-3)return "";
  const mode = state.mode === "local" ? "LOCAL" : "REMOTE";
  const rightWidth = textWidth(`${mode} · ${String(state.status || "ready").toUpperCase()}`);
  const room = Math.max(0, columns - 3 - rightWidth);
  if (room >= 5 && x >= 2 && x <= 6) return "about";
  const account = state.account || "";
  if (account.startsWith("@") && textWidth(`aesel  ${account}`) <= room
      && x >= 9 && x < 9 + textWidth(account)) return "profile";
  return "";
}
