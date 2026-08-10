// snapshot.mjs — stage one of the game's own objects offline, at any size.
//
// The game only ever speaks in draw commands — wipe, box, line, triangle — and
// it projects to screen space before it emits them. This tool listens to those
// commands instead of to a screen, so a frame can be re-framed around a single
// object and redrawn as vectors at whatever resolution the job asks for. That
// is what separates an avatar from a cropped screenshot: nothing is resampled,
// and the framing is computed from the object's real geometry rather than
// eyeballed against a pixel grid.
//
//   node xbox/live/snapshot.mjs --list
//   node xbox/live/snapshot.mjs --object=dummy-head --size=1024 --out=pfp.png
//   node xbox/live/snapshot.mjs --object=dummy --size=2048 --pad=.3 --svg
//
// Flags: --object (see --list) · --size px · --pad headroom as a fraction of
// the object · --out path · --svg keep the vector next to the png · --ticks how
// many 60Hz frames to settle before the shot · --bg override the wipe colour
// (`--bg=none` for transparency, which is what a favicon wants).

import { mkdir, readFile, writeFile } from "node:fs/promises";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../..");

const flags = new Map(process.argv.slice(2).map((entry) => {
  const [key, value = "true"] = entry.replace(/^--/, "").split("=");
  return [key, value];
}));

// Each object says where to stand and what to frame. `settle` is how many
// simulation ticks to run first — a fighter breathes, so a shot taken on tick
// zero catches a pose nobody ever sees.
const OBJECTS = {
  "dummy-head": { pad: 1, part: "head", settle: 24,
    note: "the training dummy's face — X eyes, one flat smile" },
  "dummy": { pad: 1, part: "body", settle: 24,
    note: "the whole training dummy, standing" },
  "player-head": { pad: 0, part: "head", settle: 24,
    note: "player one's head" },
  "player": { pad: 0, part: "body", settle: 24,
    note: "player one, whole" },
  "frame": { part: "frame", settle: 24,
    note: "the entire painted frame, unframed" },
};

if (flags.has("list")) {
  console.log("objects:");
  for (const [name, object] of Object.entries(OBJECTS))
    console.log(`  ${name.padEnd(12)} ${object.note}`);
  process.exit(0);
}

const objectName = flags.get("object") || "dummy-head";
const object = OBJECTS[objectName];
if (!object) {
  console.error(`unknown object "${objectName}" — try --list`);
  process.exit(1);
}

const size = Number(flags.get("size") || 1024);
const headroom = Number(flags.get("pad") ?? .18);
const ticks = Number(flags.get("ticks") ?? object.settle);
const viewport = { width: 1920, height: 1080 };

// ---------------------------------------------------------------- the game

const commands = [];
let background = [8, 10, 20];
let clock = 0;

const source = await readFile(join(HERE, "oskiewar.js"), "utf8");
const noOp = () => {};
const pads = [0, 1].map(() => ({ connected: true, down: [], leftX: 0, leftY: 0 }));

// `triangle` (flat 2D) is deliberately the only triangle host wired up. The
// 3D and batched paths hand back the same shapes with a z the screen has
// already discarded, and 2D is what an SVG wants.
const game = new Function(
  "runtime", "gamepad", "capabilities", "telemetry", "gameSignal", "saveReplay",
  "publishLive", "analytics", "drum", "wipe", "box", "line", "triangle",
  "triangle3d", "triangles3d", "write", "systemWrite", "gameView",
  `${source}\nreturn { boot, sim, paint, players, runnerWorldGeometry,` +
  ` projectPoint, disableBall: () => { ballEnabled = false;` +
  ` for (const item of balls) item.active = false; },` +
  ` setWind: (value) => { windAcceleration = value; },` +
  ` enterGame: () => enterGame(runtime().monotonicUs),` +
  ` startFightAgainst: (kind) => startFightAgainst(kind, runtime().monotonicUs) };`
)(
  () => ({ monotonicUs: clock, unixMs: 1785870000000 + Math.floor(clock / 1000),
    simCount: Math.floor(clock / 16667), paintCount: 0,
    clientErrorReportStatus: "" }),
  (index = 0) => ({ ...pads[index], down: pads[index].down.slice() }),
  () => ({ platform: "web", inputFamily: "keyboard" }),
  noOp,
  noOp,
  // Deliberately inert. The shell's saveReplay posts to the *production*
  // replay store, so a rendering tool that forwarded it would file staged
  // poses next to matches people actually played.
  () => Promise.resolve(true),
  noOp,
  noOp,
  noOp,
  (r, g, b) => { background = [r, g, b]; commands.length = 0; },
  (x, y, width, height, r, g, b) =>
    commands.push({ kind: "box", x, y, width, height, ink: [r, g, b] }),
  (x1, y1, x2, y2, width, r, g, b) =>
    commands.push({ kind: "line", x1, y1, x2, y2, width, ink: [r, g, b] }),
  (x1, y1, x2, y2, x3, y3, r, g, b) =>
    commands.push({ kind: "triangle", points: [[x1, y1], [x2, y2], [x3, y3]],
      ink: [r, g, b] }),
  undefined, undefined,
  noOp, noOp,
  () => viewport,
);

game.boot();
game.enterGame();
game.disableBall();
// No wind: a shot should be reproducible, and drift makes the same seed frame
// differently every run.
game.setWind(0);
game.startFightAgainst("dummy");
for (let step = 0; step < ticks; step++) { clock += 16667; game.sim(); }
game.paint();
console.log(`⛏  ${commands.length} draw commands captured at tick ${ticks}`);

// --------------------------------------------------------------- framing

function frameFor() {
  if (object.part === "frame")
    return { x: 0, y: 0, width: viewport.width, height: viewport.height };
  const player = game.players[object.pad];
  const geometry = game.runnerWorldGeometry(player, clock / 1e6);
  if (object.part === "head") {
    const head = geometry.head;
    if (!head) throw new Error("this build's geometry has no head");
    const centre = game.projectPoint(head.x, head.y, head.z);
    const edge = game.projectPoint(head.x + head.radius, head.y, head.z);
    const radius = Math.abs(edge.x - centre.x) * (1 + headroom);
    console.log(`⛏  head at world (${head.x.toFixed(0)}, ${head.y.toFixed(0)})` +
      ` → screen (${centre.x.toFixed(0)}, ${centre.y.toFixed(0)}) r${radius.toFixed(0)}`);
    return { x: centre.x - radius, y: centre.y - radius,
      width: radius * 2, height: radius * 2 };
  }
  // A whole body: take the union of every projected segment rather than the
  // capsule maths, so held items and detached parts stay inside the frame.
  let left = Infinity, top = Infinity, right = -Infinity, bottom = -Infinity;
  for (const segment of geometry.segments) {
    for (const point of [[segment.x1, segment.y1, segment.z1],
                         [segment.x2, segment.y2, segment.z2]]) {
      const screen = game.projectPoint(point[0], point[1], point[2]);
      const width = segment.width || 0;
      left = Math.min(left, screen.x - width);
      right = Math.max(right, screen.x + width);
      top = Math.min(top, screen.y - width);
      bottom = Math.max(bottom, screen.y + width);
    }
  }
  const head = geometry.head;
  if (head) {
    const centre = game.projectPoint(head.x, head.y, head.z);
    const edge = game.projectPoint(head.x + head.radius, head.y, head.z);
    const radius = Math.abs(edge.x - centre.x);
    left = Math.min(left, centre.x - radius);
    right = Math.max(right, centre.x + radius);
    top = Math.min(top, centre.y - radius);
    bottom = Math.max(bottom, centre.y + radius);
  }
  // Square it, so the caller always gets the aspect an avatar slot wants.
  const span = Math.max(right - left, bottom - top) * (1 + headroom);
  const midX = (left + right) / 2, midY = (top + bottom) / 2;
  return { x: midX - span / 2, y: midY - span / 2, width: span, height: span };
}

const frame = frameFor();

// ------------------------------------------------------------------ vector

const ink = ([r, g, b]) => `rgb(${Math.round(r)},${Math.round(g)},${Math.round(b)})`;
const round = (value) => Number(value.toFixed(2));

// Only what the frame can actually see. A full paint carries the whole stage,
// and an avatar that ships the scenery it cropped out is just a big file.
function visible(command) {
  const inside = (x, y) => x >= frame.x - frame.width && x <= frame.x + frame.width * 2 &&
    y >= frame.y - frame.height && y <= frame.y + frame.height * 2;
  if (command.kind === "triangle") return command.points.some(([x, y]) => inside(x, y));
  if (command.kind === "line") return inside(command.x1, command.y1) ||
    inside(command.x2, command.y2);
  return inside(command.x, command.y) ||
    inside(command.x + command.width, command.y + command.height);
}

const drawn = commands.filter(visible);
const body = drawn.map((command) => {
  if (command.kind === "triangle")
    return `<polygon points="${command.points.map(([x, y]) =>
      `${round(x)},${round(y)}`).join(" ")}" fill="${ink(command.ink)}"/>`;
  if (command.kind === "line")
    return `<line x1="${round(command.x1)}" y1="${round(command.y1)}"` +
      ` x2="${round(command.x2)}" y2="${round(command.y2)}"` +
      ` stroke="${ink(command.ink)}" stroke-width="${round(command.width)}"` +
      ` stroke-linecap="round"/>`;
  return `<rect x="${round(command.x)}" y="${round(command.y)}"` +
    ` width="${round(command.width)}" height="${round(command.height)}"` +
    ` fill="${ink(command.ink)}"/>`;
}).join("\n  ");

const backdrop = flags.get("bg") === "none" ? ""
  : `<rect x="${round(frame.x)}" y="${round(frame.y)}" width="${round(frame.width)}"` +
    ` height="${round(frame.height)}" fill="${flags.get("bg") || ink(background)}"/>\n  `;

const svg = `<svg xmlns="http://www.w3.org/2000/svg" width="${size}" height="${size}"` +
  ` viewBox="${round(frame.x)} ${round(frame.y)} ${round(frame.width)} ${round(frame.height)}"` +
  ` shape-rendering="geometricPrecision">\n  ${backdrop}${body}\n</svg>\n`;

console.log(`⛏  ${drawn.length} of ${commands.length} commands are inside the frame`);

// ----------------------------------------------------------------- output

const outPath = resolve(flags.get("out") ||
  join(REPO, "tmp", "oskiewar-snapshots", `${objectName}-${size}.png`));
await mkdir(dirname(outPath), { recursive: true });
const svgPath = outPath.replace(/\.png$/, ".svg");
if (flags.has("svg") || outPath.endsWith(".svg")) {
  await writeFile(svgPath, svg);
  console.log(`✅ ${svgPath}`);
}
if (outPath.endsWith(".png")) {
  const { default: puppeteer } = await import("puppeteer");
  const browser = await puppeteer.launch({ headless: true,
    executablePath: process.env.PUPPETEER_EXECUTABLE_PATH ||
      "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" });
  try {
    const page = await browser.newPage();
    await page.setViewport({ width: size, height: size, deviceScaleFactor: 1 });
    await page.setContent(
      `<style>html,body{margin:0;padding:0;background:transparent}</style>${svg}`,
      { waitUntil: "load" });
    await page.screenshot({ path: outPath, type: "png",
      omitBackground: flags.get("bg") === "none" });
  } finally { await browser.close(); }
  console.log(`✅ ${outPath} at ${size}×${size}`);
}
