#!/usr/bin/env node
// marimba/bin/gen-comfy-board.mjs — render the marimbaba pipeline as a
// ComfyUI-style node graph (SVG). It's a picture of the real lane:
// canonical refs + plain-english prompt -> gpt-image-2 -> wizard approval
// -> Seedance motion -> assemble over the AC-synth track -> 35mm print.
//
// The point of the artifact: you don't have to live in a node editor to
// get the node-editor VIEW. An LLM emits this board from the pipeline in
// one pass. Convert to PNG with:
//   node gen-comfy-board.mjs > out/marimbaba-comfy-board.svg
//   rsvg-convert -w 2000 out/marimbaba-comfy-board.svg > out/marimbaba-comfy-board.png

const W = 2000, H = 920;

// type -> wire/slot color (ComfyUI-ish)
const C = {
  image:  "#d9822b", // refs / frames
  text:   "#a7c957", // prompts
  video:  "#6a8cff", // clips
  audio:  "#c77dff", // the AC synth lane
  final:  "#e0574e", // the render
};

// each node: header color, title, subtitle, body rows (slots)
const nodes = {
  ref: {
    x: 40, y: 90, w: 250, h: 118, hc: "#2f6d5b", title: "Load Reference Imagery",
    sub: "canonical input · not the model's imagination",
    outs: [{ id: "refs", label: "reference set", t: "image" }],
    note: "jeffrey portrait corpus + Gates ref",
  },
  prompt: {
    x: 40, y: 268, w: 250, h: 232, hc: "#5b6b2f", title: "Descriptive Prompt",
    sub: "plain english · marimbaba.illy.txt",
    outs: [{ id: "cond", label: "still prompt", t: "text" }],
    note: "“yellow Sailor Pro Gear pen, clip\nhooked over the pocket's top hem…”\nthe read of the real thing, in words",
  },
  gpt: {
    x: 360, y: 150, w: 258, h: 176, hc: "#7a4a2f", title: "gpt-image-2  ·  OpenAI",
    sub: "images / edits — refs + prompt → frames",
    ins: [{ id: "refs", label: "reference set", t: "image" }, { id: "cond", label: "still prompt", t: "text" }],
    outs: [{ id: "frames", label: "10 section frames", t: "image" }],
    note: "one call per section, refs on every call",
  },
  wizard: {
    x: 688, y: 168, w: 236, h: 140, hc: "#6a2f6b", title: "Wizard · Director Approval",
    sub: "keep / reject · re-roll off-model",
    ins: [{ id: "frames", label: "frames", t: "image" }],
    outs: [{ id: "ok", label: "approved", t: "image" }],
    note: "my own tool — human in the seat",
  },
  seed: {
    x: 994, y: 120, w: 262, h: 214, hc: "#2f3f7a", title: "Seedance 2.0  ·  fal.ai",
    sub: "image → video · per-section motion",
    ins: [{ id: "ok", label: "approved frames", t: "image" }],
    outs: [{ id: "clips", label: "motion clips", t: "video" }],
    note: "“slow push-in along the garden\npath… the type ball spins…”\none motion prompt per shot",
  },
  synth: {
    x: 688, y: 560, w: 262, h: 132, hc: "#4a2f6b", title: "AC Marimba Synth",
    sub: "no AI · modal synthesis, no samples",
    outs: [{ id: "audio", label: "marimbaba.mp3", t: "audio" }],
    note: "the song is aesthetic.computer's own",
  },
  asm: {
    x: 1330, y: 240, w: 258, h: 168, hc: "#2f5b6b", title: "Assemble · Motion Score",
    sub: "clips on the audio timeline",
    ins: [{ id: "clips", label: "clips", t: "video" }, { id: "audio", label: "audio", t: "audio" }],
    outs: [{ id: "cut", label: "cut", t: "video" }],
    note: "nine sections, one subject",
  },
  print: {
    x: 1664, y: 170, w: 236, h: 120, hc: "#6b3f2f", title: "35mm Print Pass",
    sub: "grain · halation · gate",
    ins: [{ id: "cut", label: "cut", t: "video" }],
    outs: [{ id: "printed", label: "printed", t: "final" }],
  },
  out: {
    x: 1664, y: 350, w: 236, h: 128, hc: "#7a2f2f", title: "Output",
    sub: "marimbaba-motion-score-yt.mp4",
    ins: [{ id: "printed", label: "printed", t: "final" }],
    note: "1920×1080 · 1:23 · released lullaby",
  },
};

// links: [fromNode, fromSlotId, toNode, toSlotId]
const links = [
  ["ref", "refs", "gpt", "refs"],
  ["prompt", "cond", "gpt", "cond"],
  ["gpt", "frames", "wizard", "frames"],
  ["wizard", "ok", "seed", "ok"],
  ["seed", "clips", "asm", "clips"],
  ["synth", "audio", "asm", "audio"],
  ["asm", "cut", "print", "cut"],
  ["print", "printed", "out", "printed"],
];

const HEADER = 30, ROW = 26, PAD = 14, SLOT_R = 5.5;

function slotY(n, side, id) {
  const rows = (side === "in" ? n.ins : n.outs) || [];
  const idx = rows.findIndex((r) => r.id === id);
  return n.y + HEADER + PAD + 12 + idx * ROW;
}
function outXY(n, id) { return [n.x + n.w, slotY(n, "out", id)]; }
function inXY(n, id) { return [n.x, slotY(n, "in", id)]; }
function esc(s) { return s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;"); }

let s = "";
s += `<svg xmlns="http://www.w3.org/2000/svg" width="${W}" height="${H}" viewBox="0 0 ${W} ${H}" font-family="Inter, Helvetica, Arial, sans-serif">`;
// background + dot grid (ComfyUI canvas)
s += `<rect width="${W}" height="${H}" fill="#1b1b1f"/>`;
s += `<defs><pattern id="dots" width="26" height="26" patternUnits="userSpaceOnUse"><circle cx="1.5" cy="1.5" r="1.5" fill="#2a2a30"/></pattern></defs>`;
s += `<rect width="${W}" height="${H}" fill="url(#dots)"/>`;
// title strip
s += `<text x="40" y="52" fill="#e8e8ee" font-size="26" font-weight="700">marimbaba — pipeline</text>`;
s += `<text x="330" y="52" fill="#8a8a95" font-size="16">a ComfyUI-style board an LLM emitted straight from the lane · plain english + hosted services, no node editor required</text>`;

// links first (behind nodes)
for (const [fn, fs, tn, ts] of links) {
  const [x1, y1] = outXY(nodes[fn], fs);
  const [x2, y2] = inXY(nodes[tn], ts);
  const dx = Math.max(48, (x2 - x1) * 0.5);
  const col = C[(nodes[fn].outs.find((o) => o.id === fs) || {}).t] || "#888";
  s += `<path d="M ${x1} ${y1} C ${x1 + dx} ${y1}, ${x2 - dx} ${y2}, ${x2} ${y2}" fill="none" stroke="${col}" stroke-width="3" opacity="0.9"/>`;
}

function drawNode(n) {
  const r = 9;
  // body
  s += `<rect x="${n.x}" y="${n.y}" width="${n.w}" height="${n.h}" rx="${r}" fill="#26262c" stroke="#0e0e12" stroke-width="1.5"/>`;
  // header
  s += `<path d="M ${n.x} ${n.y + HEADER} L ${n.x} ${n.y + r} Q ${n.x} ${n.y} ${n.x + r} ${n.y} L ${n.x + n.w - r} ${n.y} Q ${n.x + n.w} ${n.y} ${n.x + n.w} ${n.y + r} L ${n.x + n.w} ${n.y + HEADER} Z" fill="${n.hc}"/>`;
  s += `<text x="${n.x + 12}" y="${n.y + 20}" fill="#f2f2f6" font-size="14.5" font-weight="700">${esc(n.title)}</text>`;
  s += `<text x="${n.x + 12}" y="${n.y + HEADER + 16}" fill="#9aa0a8" font-size="11.5">${esc(n.sub)}</text>`;
  // slots
  for (const side of ["in", "out"]) {
    const rows = (side === "in" ? n.ins : n.outs) || [];
    for (const row of rows) {
      const [cx, cy] = side === "in" ? inXY(n, row.id) : outXY(n, row.id);
      s += `<circle cx="${cx}" cy="${cy}" r="${SLOT_R}" fill="${C[row.t]}" stroke="#111" stroke-width="1"/>`;
      if (side === "in")
        s += `<text x="${cx + 11}" y="${cy + 4}" fill="#cdd2d8" font-size="11.5">${esc(row.label)}</text>`;
      else
        s += `<text x="${cx - 11}" y="${cy + 4}" fill="#cdd2d8" font-size="11.5" text-anchor="end">${esc(row.label)}</text>`;
    }
  }
  // note (footer text, may be multiline via \n)
  if (n.note) {
    const lines = n.note.split("\n");
    const baseY = n.y + n.h - 12 - (lines.length - 1) * 15;
    lines.forEach((ln, i) => {
      s += `<text x="${n.x + 12}" y="${baseY + i * 15}" fill="#7d838b" font-size="11" font-style="italic">${esc(ln)}</text>`;
    });
  }
}
for (const k of Object.keys(nodes)) drawNode(nodes[k]);

// legend
const leg = [["reference / frames", C.image], ["prompt (text)", C.text], ["motion / video", C.video], ["audio", C.audio], ["render", C.final]];
let lx = 40;
const ly = H - 34;
for (const [lab, col] of leg) {
  s += `<circle cx="${lx}" cy="${ly}" r="5.5" fill="${col}"/>`;
  s += `<text x="${lx + 12}" y="${ly + 4}" fill="#9aa0a8" font-size="12.5">${esc(lab)}</text>`;
  lx += 40 + lab.length * 7.4;
}

s += `</svg>`;
process.stdout.write(s);
