#!/usr/bin/env node
// object-cards.mjs — an object's .lisp as a deck of phone-sized PNG cards:
// one idea a card, the source syntax-highlighted in KidLisp's colours, its
// own comments as the captions. The export path from the engine for "how
// is this thing defined": send the cards, swipe through them.
//
//   node xbox/tools/object-cards.mjs objects/hound.lisp [--out dir]
//     [--frame still.png] [--title "hound"] [--sub "a four-legged walker"]
//     [--size 1170x2532] [--scale 1]
//
// A section is a run of `;` comment lines and the code until the next run.
// The comment is the caption, the code is the card. The title card carries
// the frame (a still of the thing drawn) and the first comment. Rendered by
// headless Chrome through playwright, so the type is real type.
import { readFileSync, writeFileSync, mkdirSync, existsSync } from "node:fs";
import { basename, resolve, join } from "node:path";
import { pathToFileURL } from "node:url";

const args = process.argv.slice(2);
const option = (name, fallback) => { const at = args.indexOf(name); return at >= 0 ? args.splice(at, 2)[1] : fallback; };
const out = resolve(option("--out", "cards"));
const frame = option("--frame", null);
const size = option("--size", "1170x2532").split("x").map(Number);
const scale = Number(option("--scale", "1"));
const file = args[0];
if (!file) { console.error("usage: object-cards.mjs <object.lisp> [--out dir] [--frame still.png] [--title t] [--sub s]"); process.exit(2); }
const source = readFileSync(file, "utf8");
const name = option("--title", basename(file, ".lisp"));
const sub = option("--sub", "");

// ——— sections: comment runs caption the code that follows ———
export function sections(source) {
  const lines = source.replace(/\s+$/, "").split("\n");
  const out = [];
  let cur = null;
  for (const raw of lines) {
    const line = raw.replace(/\s+$/, "");
    const comment = /^\s*;/.test(line);
    if (comment) {
      const text = line.replace(/^\s*;\s?/, "");
      if (!cur || cur.code.length || text.trim() === "") { cur = { caption: [], code: [] }; out.push(cur); }
      if (text.trim() !== "") cur.caption.push(text);
    } else if (line.trim() === "") {
      if (cur && cur.code.length) cur.code.push("");
    } else {
      if (!cur) { cur = { caption: [], code: [] }; out.push(cur); }
      cur.code.push(line);
    }
  }
  for (const s of out) {
    while (s.code.length && s.code[s.code.length - 1] === "") s.code.pop();
    const indent = Math.min(...s.code.filter((l) => l.trim()).map((l) => l.match(/^ */)[0].length), 0);
    const least = Math.min(...s.code.filter((l) => l.trim()).map((l) => l.match(/^ */)[0].length));
    if (Number.isFinite(least) && least > 0) s.code = s.code.map((l) => l.slice(least));
    void indent;
  }
  return out.filter((s) => s.caption.length || s.code.length);
}

// ——— KidLisp's colours, as the prompt paints them: comments gray, numbers
// pink (an r g b triple in its own channels), strings yellow, parens by
// nesting, the form's name after a paren, inputs and bound names lime ———
const forms = new Set(["def", "let", "if", "repeat", "later", "use", "ink", "glow", "move", "rotate", "scale",
  "radial", "mirror", "outline", "nudge", "toward", "on", "surface", "grain", "mottle", "rough", "revolve"]);
const shapes = new Set(["tri", "quad", "disc", "hoop", "band", "capsule", "line", "tube", "patch", "orb", "bend",
  "ball", "limb", "ring", "drum", "stroke", "plate", "slab", "bone", "skin"]);
const inputs = new Set(["time", "distance", "speed", "lean", "heading", "pitch", "turbo", "hit", "land", "blink",
  "hurt", "skirt", "glasses", "headless", "cylinder", "spent", "cock", "crowd", "detail", "owner", "pi", "tau"]);
const math = new Set(["+", "-", "*", "/", "%", "=", "<", ">", "<=", ">=", "and", "or", "not", "min", "max", "abs", "sign",
  "sin", "cos", "tan", "atan", "sqrt", "pow", "floor", "round", "clamp", "mix"]);
// The prompt's rainbow, lifted a little for a dark card.
const parens = ["#ff6b6b", "#ffa94d", "#ffd43b", "#8ce99a", "#74c0fc", "#b197fc", "#f783ac"];
const esc = (s) => s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
export function highlight(line, state = { depth: 0 }) {
  const tokens = line.match(/\(|\)|"[^"]*"|;.*$|[^\s()]+|\s+/g) || [];
  let html = "", afterParen = false, inkArgs = 0, inkRgb = [];
  for (let i = 0; i < tokens.length; i++) {
    const t = tokens[i];
    if (/^\s+$/.test(t)) { html += t; continue; }
    if (t.startsWith(";")) { html += `<span class="c">${esc(t)}</span>`; continue; }
    if (t === "(") { html += `<span style="color:${parens[state.depth % parens.length]}">(</span>`; state.depth++; afterParen = true; continue; }
    if (t === ")") { state.depth = Math.max(0, state.depth - 1); html += `<span style="color:${parens[state.depth % parens.length]}">)</span>`; afterParen = false; continue; }
    if (afterParen) {
      afterParen = false;
      const cls = math.has(t) ? "o" : forms.has(t) || shapes.has(t) ? "f" : "w";
      html += `<span class="${cls}">${esc(t)}</span>`;
      if (t === "ink") { inkArgs = 3; inkRgb = []; }
      continue;
    }
    if (/^-?\d*\.?\d+$/.test(t)) {
      if (inkArgs > 0) {
        const v = Math.max(0, Math.min(255, +t)), k = 3 - inkArgs;
        // the channel's hue, never darker than readable on the panel
        const lift = (x) => Math.round(150 + x * 105 / 255);
        const col = k === 0 ? `rgb(${lift(v)},90,90)` : k === 1 ? `rgb(90,${lift(v)},90)` : `rgb(110,${Math.round(110 + v * .5)},${lift(v)})`;
        inkRgb.push(v); inkArgs--;
        html += `<span style="color:${col}">${t}</span>`;
        if (inkArgs === 0) html += ` <span class="sw" style="background:rgb(${inkRgb.join(",")})"></span>`;
        continue;
      }
      html += `<span class="n">${t}</span>`; continue;
    }
    if (t.startsWith('"')) { html += `<span class="q">${esc(t)}</span>`; continue; }
    if (inputs.has(t)) { html += `<span class="i">${esc(t)}</span>`; continue; }
    html += `<span class="w">${esc(t)}</span>`;
  }
  return html;
}

function page(cards, [w, h]) {
  const css = `
  :root { --bg:#17151c; --panel:#211e27; --ink:#ece7f2; --dim:#9a93a6; --line:#36313f; --accent:#ff7cb0; }
  * { box-sizing:border-box; } html,body { margin:0; background:var(--bg); color:var(--ink); }
  body { font: 42px/1.35 ui-monospace, "SF Mono", Menlo, monospace; }
  .card { width:${w}px; height:${h}px; padding:96px 72px; display:flex; flex-direction:column; gap:40px; page-break-after:always; overflow:hidden; }
  .kicker { color:var(--accent); font-size:40px; letter-spacing:.02em; }
  h1 { font-size:88px; line-height:1.05; margin:0; font-weight:600; }
  h2 { font-size:64px; line-height:1.1; margin:0; font-weight:600; }
  .cap { color:var(--ink); font-size:50px; line-height:1.35; font-family: -apple-system, "Helvetica Neue", Helvetica, Arial, sans-serif; }
  .cap b { color: var(--accent); font-weight: 600; }
  .cap.big { font-size:56px; line-height:1.3; margin-top:40px; }
  pre { margin:0; padding:40px 44px; background:var(--panel); border:2px solid var(--line); border-radius:28px; font-size:35px; line-height:1.42; white-space:pre-wrap; word-break:break-word; overflow:hidden; }
  /* KidLisp's prompt colours: functions cyan, math lime, names orange, numbers pink, strings yellow, comments gray */
  .c { color:var(--dim); } .f { color:#5fe1ec; } .o { color:#9cff57; } .n { color:#ff9ac8; } .q { color:#ffe066; } .i { color:#ffb15c; } .w { color:#ffb15c; }
  .sw { display:inline-block; width:.9em; height:.9em; border-radius:.2em; vertical-align:-.12em; border:2px solid #0008; }
  .frame { border-radius:28px; overflow:hidden; border:2px solid var(--line); background:#000; }
  .frame img { display:block; width:100%; }
  .foot { margin-top:auto; color:var(--dim); font-size:34px; display:flex; justify-content:space-between; }
  .n-of { color:var(--dim); }`;
  const body = cards.map((c) => `<section class="card">${c}</section>`).join("\n");
  return `<!doctype html><html><head><meta charset="utf-8"><style>${css}</style></head><body>${body}</body></html>`;
}

const secs = sections(source);
const cards = [];
const frameTag = frame ? `<div class="frame"><img src="${pathToFileURL(resolve(frame)).href}"></div>` : "";
const firstCaption = secs[0]?.caption.length && !secs[0].code.length ? secs.shift().caption.join(" ") : "";
const total = secs.length + 1;
cards.push(`<div class="kicker">${esc(name)}.lisp · an oskiewar object</div><h1>${esc(name)}</h1>${sub ? `<h2 style="color:var(--dim);font-weight:400">${esc(sub)}</h2>` : ""}${frameTag}<div class="cap">${esc(firstCaption)}</div><div class="foot"><span>no keyframes · no mesh files</span><span class="n-of">1 / ${total}</span></div>`);
secs.forEach((s) => {
  const state = { depth: 0 };
  const code = s.code.map((l) => highlight(l, state)).join("\n");
  const caption = s.caption.join(" ");
  const body = s.code.length ? `<div class="cap">${esc(caption)}</div><pre>${code}</pre>` : `<div class="cap big">${esc(caption)}</div>`;
  cards.push(`<div class="kicker">${esc(name)}.lisp</div>${body}<div class="foot"><span>oskiewar object dialect</span><span class="n-of">${cards.length + 1} / ${total}</span></div>`);
});

mkdirSync(out, { recursive: true });
const html = join(out, "cards.html");
writeFileSync(html, page(cards, size));
const { chromium } = await import(pathToFileURL(resolve(process.cwd(), "node_modules/playwright/index.mjs")).href);
const browser = await chromium.launch({ channel: "chrome", headless: true });
const pageView = await browser.newPage({ viewport: { width: size[0], height: size[1] }, deviceScaleFactor: scale });
await pageView.goto(pathToFileURL(html).href);
await pageView.waitForTimeout(300);
const n = await pageView.locator("section.card").count();
for (let i = 0; i < n; i++) {
  const path = join(out, `${name}-${String(i + 1).padStart(2, "0")}.png`);
  await pageView.locator("section.card").nth(i).screenshot({ path });
  console.log(path);
}
await browser.close();
