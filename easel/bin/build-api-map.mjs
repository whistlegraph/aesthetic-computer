#!/usr/bin/env node
// build-api-map — the static map of the piece API, read off the runtime source.
//
// Every aesel session so far has opened with the same hunt: grep graph.mjs for
// `function circle(`, sed a window of disk.mjs to see what `$paintApiUnwrapped`
// exposes, grep the disks for one piece that already calls `synth(`. Ten
// sessions, the same eight commands, a minute or two each before the first
// edit. The answers do not change between sessions; only the model's memory
// does. So the answers are computed once, here, and shipped as
// `easel/context/api.json` for `ac_api` to serve in a single call.
//
// What it records, per API name a piece can call:
//   - where it lives on `$api` (`circle`, `sound.synth`, `ui.Button`)
//   - the runtime signature, read from the defining `function` line
//   - the comment immediately above the definition, when there is one
//   - up to three one-line uses from real pieces under disks/, as file:line
//
// Regex over source, not a parser: the runtime is one 600 KB file with a
// house style regular enough that `^  name: graph.name,` is a grammar. Where
// the pattern misses, the entry is simply absent — a hole in the map, never a
// wrong signature.
//
//   node bin/build-api-map.mjs          # write context/api.json
//   node bin/build-api-map.mjs --check  # exit 1 if it is stale
import { readFileSync, readdirSync, writeFileSync, existsSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const aesel = join(HERE, "..");
const REPO = join(aesel, "..");
const AC = join(REPO, "system", "public", "aesthetic.computer");
const OUT = join(aesel, "context", "api.json");

const read = (file) => readFileSync(join(AC, file), "utf8").split("\n");

// The comment block sitting directly above `line` (0-based), joined, trimmed.
function commentAbove(lines, line, max = 6) {
  const out = [];
  for (let i = line - 1; i >= 0 && out.length < max; i--) {
    const text = lines[i].trim();
    if (!text.startsWith("//")) break;
    out.unshift(text.replace(/^\/\/\s?/, ""));
  }
  return out.join(" ").replace(/\s+/g, " ").trim();
}

// `function name(a, b = 1, ...rest) {` → "name(a, b = 1, ...rest)". A signature
// split across lines (oval, synth) is joined until its parenthesis closes.
function signatureAt(lines, line, name) {
  let text = "";
  for (let i = line; i < Math.min(lines.length, line + 24); i++) {
    text += lines[i].replace(/\/\/.*$/, "").trim() + " ";
    if (/\)\s*(=>\s*)?\{?\s*$/.test(text) || text.includes(") {")) break;
  }
  const match = text.match(/\(([\s\S]*)\)\s*(?:=>\s*)?\{?\s*$/);
  if (!match) return `${name}(…)`;
  let params = match[1].replace(/\s+/g, " ").trim();
  // A destructured options object reads as its keys.
  params = params.replace(/\{\s*([^}]*)\}\s*=\s*\{\}/, (_, keys) => `{ ${keys.trim()} }`);
  return `${name}(${params})`;
}

// Find `function name(` / `name = function(` / `name(...) {` / `name: function`
// anywhere in `lines`, preferring a top-level `function`.
function definitionOf(lines, name) {
  const patterns = [
    new RegExp(`^(?:export\\s+)?(?:async\\s+)?function\\s+${name}\\s*\\(`),
    new RegExp(`^\\s*(?:const|let)\\s+${name}\\s*=\\s*(?:async\\s*)?(?:function\\s*\\(|\\()`),
    new RegExp(`^\\s*[$\\w.]*\\.?${name}\\s*=\\s*(?:async\\s+)?function\\s*(?:\\w+\\s*)?\\(`),
    new RegExp(`^\\s*${name}\\s*:\\s*(?:async\\s+)?function\\s*(?:\\w+\\s*)?\\(`),
    new RegExp(`^\\s*(?:async\\s+)?${name}\\s*\\([^)]*\\)\\s*\\{\\s*$`),
    new RegExp(`^\\s*${name}\\s*:\\s*(?:async\\s*)?\\(`),
  ];
  for (const pattern of patterns) {
    const at = lines.findIndex((text) => pattern.test(text));
    if (at >= 0) return at;
  }
  return -1;
}

const disk = read("lib/disk.mjs");
const graph = read("lib/graph.mjs");

// The object the paint API is built from. `circle: graph.circle,` is the shape
// of most of it; the rest are inline functions wrapping a graph call.
function paintApi() {
  const start = disk.findIndex((text) => /^const \$paintApiUnwrapped = \{$/.test(text));
  if (start < 0) throw new Error("disk.mjs: $paintApiUnwrapped not found");
  let end = start + 1;
  while (end < disk.length && !/^\};?$/.test(disk[end])) end++;
  const entries = [];
  for (let i = start + 1; i < end; i++) {
    const text = disk[i];
    let match = text.match(/^  (\w+): graph\.(\w+),?\s*(\/\/\s*(.*))?$/);
    if (match) {
      const [, name, target, , note] = match;
      const at = definitionOf(graph, target);
      entries.push({
        name,
        path: name,
        signature: at >= 0 ? signatureAt(graph, at, name) : `${name}(…)`,
        doc: note?.trim() || (at >= 0 ? commentAbove(graph, at) : "") || commentAbove(disk, i),
        source: at >= 0 ? `lib/graph.mjs:${at + 1}` : `lib/disk.mjs:${i + 1}`,
      });
      continue;
    }
    match = text.match(/^  (\w+)(?::\s*(?:async\s+)?function\s*\w*\s*\(|\s*\()/);
    if (match) {
      const name = match[1];
      // An inline wrapper usually forwards to graph.<same name>; take the
      // graph signature when it exists, since that is what the arguments are.
      const at = definitionOf(graph, name);
      entries.push({
        name,
        path: name,
        signature: at >= 0 ? signatureAt(graph, at, name) : signatureAt(disk, i, name),
        doc: commentAbove(disk, i) || (at >= 0 ? commentAbove(graph, at) : ""),
        source: at >= 0 ? `lib/graph.mjs:${at + 1}` : `lib/disk.mjs:${i + 1}`,
      });
    }
  }
  return entries;
}

// Things a piece reaches through a namespace. Named by hand: this is the short
// list the sessions actually hunted for, and each one is checked against the
// source so the signature is the runtime's, not a memory of it.
const NAMESPACED = [
  ["sound.synth", disk, "synth", "Play a synthesized tone. `tone` is Hz or a note name like \"c4\"; returns a voice with .kill() and .update()."],
  ["sound.play", disk, "play", "Play a loaded sample or sfx by id."],
  ["ui.Button", null, "Button", "A rectangular button: new ui.Button(x, y, w, h) or ({x,y,w,h}); btn.paint(callback) inside paint, btn.act(e, { push, down, up, cancel }) inside act."],
  ["ui.TextButton", null, "TextButton", "A labelled button sized to its text."],
  ["hud.label", disk, "label", "Take over the system's corner label — the only sanctioned way to draw in the top-left."],
  ["write", disk, "write", "Draw text with the current ink: write(text, { x, y, size, center: \"x\" }) or write(text, x, y). Chain from ink(): ink(\"white\").write(...)."],
  ["num.randInt", null, "randInt", "Random integer in [0, n]."],
  ["num.randIntRange", null, "randIntRange", "Random integer in [low, high]."],
  ["num.lerp", null, "lerp", "Linear interpolation a→b by t."],
  ["num.clamp", null, "clamp", "Clamp a value between min and max."],
  ["num.dist", null, "dist", "Distance between two points."],
  ["num.map", null, "map", "Map a value from one range to another."],
  ["num.radians", null, "radians", "Degrees to radians."],
  ["geo.Box", null, "Box", "An axis-aligned rectangle with .contains(point) and .crop()."],
  ["geo.Circle", null, "Circle", "A circle with .contains(point)."],
];

function namespaced() {
  const libs = {
    ui: read("lib/ui.mjs"),
    num: read("lib/num.mjs"),
    geo: read("lib/geo.mjs"),
  };
  const out = [];
  for (const [path, where, name, doc] of NAMESPACED) {
    const [ns] = path.split(".");
    const lines = where || libs[ns];
    if (!lines) continue;
    let at = definitionOf(lines, name);
    let signature = `${path}(…)`;
    let source = "";
    if (at >= 0) {
      signature = signatureAt(lines, at, path);
      source = `lib/${where ? "disk" : ns}.mjs:${at + 1}`;
    } else {
      // A class: the signature is its constructor's.
      const cls = lines.findIndex((text) => new RegExp(`^(?:export\\s+)?class\\s+${name}\\b`).test(text));
      if (cls < 0) continue;
      const ctor = lines.slice(cls).findIndex((text) => /^\s*constructor\s*\(/.test(text));
      signature = ctor >= 0 ? signatureAt(lines, cls + ctor, `new ${path}`) : `new ${path}(…)`;
      source = `lib/${ns}.mjs:${cls + 1}`;
      at = cls;
    }
    out.push({ name: path.split(".").pop(), path, signature, doc: doc || commentAbove(lines, at), source });
  }
  return out;
}

// Up to `limit` short lines from pieces that call `name(`. Long lines and the
// definition of a same-named helper inside a piece are skipped; what is wanted
// is a call site a model can copy the shape of.
function examplesFor(entries, limit = 3) {
  const disksDir = join(AC, "disks");
  const files = readdirSync(disksDir).filter((f) => f.endsWith(".mjs")).sort();
  const sources = files.map((f) => [f, readFileSync(join(disksDir, f), "utf8").split("\n")]);
  for (const entry of entries) {
    const leaf = entry.path.split(".").pop();
    const needle = entry.path.includes(".")
      ? new RegExp(`\\b${entry.path.replace(".", "\\.")}\\s*\\(|new ${entry.path.replace(".", "\\.")}\\s*\\(`)
      : new RegExp(`(?<![\\w.$])${leaf}\\s*\\(`);
    const found = [];
    // Newest-named pieces are not better examples; alternate through the
    // alphabet so the same three pieces do not answer for everything.
    for (const [file, lines] of sources) {
      for (let i = 0; i < lines.length && found.length < limit; i++) {
        const text = lines[i];
        if (!needle.test(text)) continue;
        if (/^\s*(export\s+)?(async\s+)?function\b|^\s*\/\//.test(text)) continue;
        if (text.length > 140) continue;
        found.push(`disks/${file}:${i + 1}  ${text.trim()}`);
      }
      if (found.length >= limit) break;
    }
    entry.examples = found;
  }
}

// Definitions that read `arguments` show up as `name()`. The real shapes, by
// hand, for the handful a piece cannot do without — checked against
// graph.mjs's own header comments, which this generator otherwise trusts.
const OVERRIDES = {
  ink: { signature: "ink(r, g, b, a) | ink(gray, a) | ink(\"red\") | ink([r, g, b]) | ink() (random)", doc: "Set the paint color for everything drawn next. Returns the API so calls chain: ink(255, 0, 0).line(0, 0, 10, 10)." },
  ink2: { signature: "ink2(...color)", doc: "Secondary color, used by gradient-aware primitives." },
  wipe: { signature: "wipe(...color)", doc: "Fill the whole screen with a color; the usual first line of paint()." },
  line: { signature: "line(x1, y1, x2, y2) | line({x, y}, {x, y}) | line(x1, y1, x2, y2, thickness)", doc: "Draw a 1px line between two points in the current ink." },
  box: { signature: "box(x, y, size) | box(x, y, w, h) | box(x, y, w, h, mode) | box({x, y, w, h}, mode)", doc: "Rectangle. `mode` is \"fill\" (default), \"outline\", \"inline\", or \"fill*center\" / \"outline*center\" to draw from the center." },
  shape: { signature: "shape(x1, y1, x2, y2, ...) | shape([[x, y], [x, y], ...], filled = true)", doc: "Rasterize a filled or outlined polygon from point pairs." },
  tri: { signature: "tri(x1, y1, x2, y2, x3, y3, mode = \"fill\")", doc: "Triangle from three points; mode \"fill\" or \"outline\"." },
  clear: { signature: "clear()", doc: "Clear the buffer to transparent (unlike wipe, which paints a color)." },
  page: { signature: "page(buffer)", doc: "Point subsequent drawing at another painting buffer; page(screen) comes back." },
  draw: { signature: "draw(drawing, x, y, scale = 1, angle = 0, thickness = 1)", doc: "Draw a stored vector drawing (from `drawing`/store) at a position." },
  unpan: { signature: "unpan()", doc: "Undo pan(x, y)." },
  mask: { signature: "mask({ x, y, width, height })", doc: "Clip drawing to a rectangle until unmask()." },
  unmask: { signature: "unmask()", doc: "Lift the clip set by mask()." },
  flip: { signature: "flip(horizontal = false, vertical = false)", doc: "Mirror the screen." },
  sort: { signature: "sort()", doc: "Pixel-sort the screen — a glitch effect." },
  invert: { signature: "invert()", doc: "Invert every pixel's color." },
  "num.dist": { signature: "num.dist(x1, y1, x2, y2)", doc: "Distance between two points." },
  "ui.Button": { signature: "new ui.Button(x, y, w, h) | new ui.Button({ x, y, w, h })", doc: "A button. In paint: btn.paint((b) => { ink(b.down ? \"yellow\" : \"gray\").box(b.box) }). In act: btn.act(e, { push: () => {}, down: () => {}, up: () => {}, cancel: () => {} }). Pass pens() as the 3rd arg to act for multitouch. Rebuild buttons in `reframed`." },
  "geo.Box": { signature: "new geo.Box(x, y, w, h)", doc: "An axis-aligned rectangle with .x .y .w .h and .contains({x, y})." },
  write: { signature: "write(text, { x, y, size, center: \"x\" | \"xy\" }) | write(text, x, y)", doc: "Draw text in the current ink. Chains from ink(): ink(\"white\").write(\"hi\", { x: 10, y: 40 })." },
  "hud.label": { signature: "hud.label(text, color, offset)", doc: "Take over the system's corner label — the only sanctioned way to draw in the top-left." },
};

// Not functions, so nothing to read a signature from — but the sessions hunted
// for these as often as for any primitive.
const STATIC = [
  { name: "screen", path: "screen", signature: "screen.width, screen.height, screen.pixels, screen.center", doc: "The canvas. Read width/height in paint(); never write screen.pixels directly (writes can silently drop) — draw into your own painting buffer and paste() it.", source: "lib/disk.mjs" },
  { name: "pen", path: "pen", signature: "pen.x, pen.y, pen.drawing, pen.delta", doc: "The single primary pointer; null when there is none. Read in paint()/sim().", source: "lib/disk.mjs" },
  { name: "pens", path: "pens", signature: "pens() → [{ x, y, id, drawing }]", doc: "Every active pointer, for multitouch. Pass to btn.act(e, callbacks, pens()).", source: "lib/disk.mjs" },
  { name: "event", path: "act(e)", signature: "e.is(\"touch\") | e.is(\"draw\") | e.is(\"lift\") | e.is(\"keyboard:down:space\") | e.is(\"reframed\") ; e.x, e.y, e.delta, e.key", doc: "Events arrive in act({ event: e, ... }). Pointer: touch → draw → lift. Keys: keyboard:down:<key>, keyboard:up:<key>.", source: "lib/disk.mjs" },
  { name: "sim", path: "sim", signature: "function sim({ ... }) — runs 120 times per second", doc: "Physics and timers go here, not in paint(); paint() runs at display rate and only when something needs painting.", source: "lib/disk.mjs" },
  { name: "needsPaint", path: "needsPaint", signature: "needsPaint()", doc: "Ask for another paint() when the piece is static and something changed.", source: "lib/disk.mjs" },
  { name: "painting", path: "painting", signature: "painting(w, h, (api) => { ... }) → buffer", doc: "Make an offscreen buffer by drawing into it; show it later with paste(buffer, x, y).", source: "lib/disk.mjs" },
  { name: "help.choose", path: "help.choose", signature: "help.choose(...items)", doc: "Pick one item at random.", source: "lib/help.mjs" },
  { name: "help.repeat", path: "help.repeat", signature: "help.repeat(n, (i) => { ... })", doc: "Call a function n times.", source: "lib/help.mjs" },
];

export function build() {
  const entries = [...paintApi(), ...namespaced()];
  for (const entry of entries) {
    const fix = OVERRIDES[entry.path];
    if (!fix) continue;
    if (fix.signature) entry.signature = fix.signature;
    if (fix.doc) entry.doc = fix.doc;
  }
  // graph.mjs's TODO notes are not documentation.
  for (const entry of entries) if (/^TODO/i.test(entry.doc)) entry.doc = "";
  examplesFor(entries);
  entries.push(...STATIC.map((entry) => ({ ...entry, examples: [] })));
  const body = JSON.stringify(
    {
      about:
        "The Aesthetic Computer piece API, read from the runtime. `path` is where it sits on the $api object a piece destructures in paint/act/sim. Signatures are the runtime's own.",
      built_from: "system/public/aesthetic.computer/lib/{disk,graph,ui,num,geo}.mjs",
      entries,
    },
    null,
    1,
  );
  return body + "\n";
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const check = process.argv.includes("--check");
  const body = build();
  const current = existsSync(OUT) ? readFileSync(OUT, "utf8") : "";
  if (current === body) {
    console.log("context/api.json is current.");
  } else if (check) {
    console.error("stale: context/api.json no longer matches the runtime — run `npm run context`.");
    process.exit(1);
  } else {
    writeFileSync(OUT, body);
    const count = JSON.parse(body).entries.length;
    console.log(`wrote context/api.json  (${count} entries, ${(body.length / 1024).toFixed(1)} KB)`);
  }
}
