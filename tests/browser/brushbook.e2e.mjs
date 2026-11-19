// brushbook.e2e, 2026.11.18
// Runs every brush of No Paint classic (the 2021 Construct build served at
// nopaint.art) in a real browser, twice: once answered "Paint" and once
// answered "No". It records what the painting looked like before and after,
// a screenshot of the proposal, and the painting itself as a PNG. The
// brushbook (papers/arxiv-nopaint-brushbook) prints these.
//
// What it checks, per brush:
//   opens    the prompt line took the game to the brush's layout
//   paints   after Paint the game came back to Index and the painting changed
//   refuses  after No the game came back to Index and the painting did not change
// Playlist is a sequence that paints by itself, so it is only watched.
// Brushes start on a blank painting. Filters start on a test card, because a
// filter over nothing shows nothing.
//
// It talks to the game through the bridge that toolchain/nopaint/patch-classic.mjs
// appends to scripts/c3runtime.js, and serves the build itself from a folder,
// so nothing has to be released first and nothing reaches the network.
//
// On a fleet machine with no checkout:
//   mkdir -p ~/scratch/brushes && cd ~/scratch/brushes && npm i puppeteer-core
//   rsync the build to ./site, copy book-kit.mjs, this file and brushes.json
//   node brushbook.e2e.mjs          # every brush
//   node brushbook.e2e.mjs Flip     # brushes whose name contains "Flip"
//   node brushbook.e2e.mjs "Flip,Dark Window"   # several
//
//   LINE="Softy: 128 128 S ?" node brushbook.e2e.mjs Softy   # one brush, with options
//
// Env: SITE (the build folder, default ./site or the checkout's),
//      SITE_URL (a served site to test in place of the folder),
//      BRUSHES (brushes.json), LOOK (ms to watch a proposal, default 2600),
//      plus book-kit's CHROME, OUT, VIDEO, W, H, DSF.

import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { fileURLToPath } from "node:url";
import { launch, openPage, shot, startClip, trimLog, saveResults, serveFolder, onlyLocal, viewport, wait, slug, OUT } from "./book-kit.mjs";

const here = (path) => fileURLToPath(new URL(path, import.meta.url));
const SITE = process.env.SITE || (existsSync(here("./site")) ? here("./site") : here("../../system/public/nopaint.art"));
const BRUSHES = process.env.BRUSHES || (existsSync(here("./brushes.json")) ? here("./brushes.json") : here("../../papers/arxiv-nopaint-brushbook/brushes.json"));
const LOOK = parseInt(process.env.LOOK || "2600", 10);
const only = process.argv[2] || null;
// The game is 256 × 352; twice that, one device pixel per CSS pixel.
const VIEW = viewport(512, 704, 1);

// Not run: Load with no code and Playlist with no name open the device's file
// picker. Load with a code fetches a saved painting from the save server.
const SKIP = { Load: "opens a file picker, or fetches a painting from the save server" };
// A line that differs from the bare brush name. With no options the game
// places a brush anywhere, and a mark at the edge of one still picture shows
// little, so a few brushes are asked for at the middle or at full size.
const LINES = {
  Playlist: "Playlist: Smiley", Softy: "Softy: 128 128 L ?", "Dark Window": "Dark Window: 128 128",
  Wafer: "Wafer: 128 128", Caterpillar: "Caterpillar: 32", Breathe: "Breathe: 128 128 5",
};
// Brushes that need something under them to show: they start on the test card.
const ON_CARD = new Set(["Breathe"]);
// Brushes that take a while to get anywhere: watch them longer (ms).
const LOOKS = { Walker: 9000, Bubbles: 6000, Caterpillar: 6000 };
// What the options a brush chose for itself must look like. Stamp, Softy and
// Build pick from weighted tables of their own; the tuned picker
// (toolchain/nopaint/classic-bridge.js) must leave those picks to the game.
const CHOSE = { Stamp: /@\w+ \d+/, Softy: / [SML] [0-9A-F]{6}$/ };
const startsOnCard = (brush) => brush.table === "Filters" || ON_CARD.has(brush.name);
// A playlist is a written sequence: some of its steps paint by themselves and
// some wait for you. It is watched, not answered: it passes when it has
// painted at least once on its own and the painting has changed.
const WATCHED = { Playlist: 40000 };

const { brushes } = JSON.parse(readFileSync(BRUSHES, "utf8"));

// The page's end of the bridge.
function client() {
  const channel = new BroadcastChannel("nopaint-classic");
  const waiting = new Map(), events = [];
  let count = 0;
  channel.onmessage = ({ data }) => {
    if (!data || data.to !== "page") return;
    if (data.id && waiting.has(data.id)) {
      const { resolve, reject } = waiting.get(data.id);
      waiting.delete(data.id);
      data.error ? reject(new Error(data.error)) : resolve(data.result);
    } else if (data.event) events.push({ ...data, at: Date.now() });
  };
  const ask = (op, args = {}, timeout = 8000) => new Promise((resolve, reject) => {
    const id = `q${++count}`;
    waiting.set(id, { resolve, reject });
    channel.postMessage({ to: "game", id, op, ...args });
    setTimeout(() => { if (waiting.delete(id)) reject(new Error(`the game did not answer "${op}"`)); }, timeout);
  });
  window.__np = {
    events, ask,
    // Hash and size of a canvas, without its bytes.
    async look(object = "Painting") { const { data, ...rest } = await ask("pixels", { object }); return rest; },
    // The top square of a canvas as a PNG data URL.
    async png(object = "Painting") {
      const image = await ask("pixels", { object, data: true });
      const side = Math.min(image.width, image.height);
      const canvas = document.createElement("canvas");
      canvas.width = side; canvas.height = side;
      canvas.getContext("2d").putImageData(new ImageData(new Uint8ClampedArray(image.data), image.width, image.height), 0, 0);
      return canvas.toDataURL("image/png");
    },
    // A test card for the filters: four unlike quarters, a ramp, a diagonal
    // and an off-centre disc, so no flip, turn or shift maps it onto itself.
    async card(object = "Painting") {
      const { width, height } = await ask("pixels", { object });
      const side = Math.min(width, height), data = new Uint8ClampedArray(width * height * 4);
      const quarters = [[232, 64, 56], [248, 208, 64], [56, 120, 224], [72, 176, 96]];
      for (let y = 0; y < side; y++) for (let x = 0; x < side; x++) {
        const u = x / side, v = y / side, i = (y * width + x) * 4;
        let [r, g, b] = quarters[(v < 0.5 ? 0 : 2) + (u < 0.5 ? 0 : 1)];
        if (v > 0.82) r = g = b = Math.round(u * 255);                       // grey ramp along the bottom
        if (Math.abs(u - v) < 0.02) r = g = b = 255;                         // one diagonal
        if (Math.hypot(u - 0.3, v - 0.34) < 0.13) { r = 24; g = 24; b = 32; } // a disc, off centre
        data[i] = r; data[i + 1] = g; data[i + 2] = b; data[i + 3] = 255;
      }
      return ask("load", { object, width, height, data });
    },
  };
}

const ask = (page, op, args) => page.evaluate((o, a) => window.__np.ask(o, a), op, args || {});
const look = (page) => page.evaluate(() => window.__np.look());
async function until(page, test, ms, label) {
  const end = Date.now() + ms;
  let state = null;
  while (Date.now() < end) {
    state = await ask(page, "state").catch(() => null);
    if (state && test(state)) return state;
    await wait(150);
  }
  throw new Error(`timed out waiting for ${label}; the game was at ${state ? `${state.layout}${state.painting ? " (painting)" : ""}` : "no answer"}`);
}
async function savePng(page, name) {
  const url = await page.evaluate(() => window.__np.png());
  writeFileSync(join(OUT, `${name}.png`), Buffer.from(url.split(",")[1], "base64"));
  return `${name}.png`;
}

// A fresh visitor on the Index layout with an empty painting, or a test card.
async function begin(browser, base, brush, blocked) {
  const context = await browser.createBrowserContext();
  const { page, log } = await openPage(browser, VIEW, { context, consoleTypes: ["error", "warning"] });
  // The page's own module is left out: it keeps guests from painting and
  // records steps, and this test is of the brushes alone.
  await onlyLocal(page, base, blocked, /\/classic-steps\.mjs/);
  await page.evaluateOnNewDocument(client);
  // The page's own stickers (the Notepat note, the teacher chip) sit over the
  // canvas; the shots are of the game.
  await page.evaluateOnNewDocument(() => addEventListener("DOMContentLoaded", () => {
    const style = document.createElement("style");
    style.textContent = "body > *:not(canvas):not(script):not(style) { display: none !important; }";
    document.head.append(style);
  }));
  await page.goto(base + "/", { waitUntil: "domcontentloaded", timeout: 45000 });
  await until(page, (s) => !!s.layout, 40000, "the game to start");
  await wait(600);
  await ask(page, "goto", { layout: "Index" });
  await until(page, (s) => s.layout === "Index", 10000, "the Index layout");
  await wait(700);
  if (startsOnCard(brush)) { await page.evaluate(() => window.__np.card()); await wait(250); }
  return { context, page, log };
}

// One brush, one decision.
async function decide(browser, base, brush, decision) {
  const line = process.env.LINE || LINES[brush.name] || brush.name;
  const id = `${brush.name}:${decision}`, file = slug(`${brush.name}-${decision}`);
  const r = { id, brush: brush.name, decision, line, start: startsOnCard(brush) ? "card" : "blank" };
  const blocked = [];
  const t0 = Date.now();
  let session = null, clip = null;
  try {
    session = await begin(browser, base, brush, blocked);
    const { page } = session;
    clip = decision !== "no" ? await startClip(page, brush.name) : null;
    r.before = await look(page);
    if (decision !== "no") r.beforePng = await savePng(page, slug(`${brush.name}-before`));
    const start = await page.evaluate(() => window.__np.events.length);
    await ask(page, "prompt", { text: line });
    const opened = await until(page, (s) => s.layout === brush.name, 8000, `the ${brush.name} layout`).catch((e) => ({ error: e.message }));
    r.opens = !opened.error;
    if (!r.opens) throw new Error(opened.error);
    await wait(LOOKS[brush.name] || LOOK);
    const during = await ask(page, "state");
    // The brush with the options it ran with: typed ones, and the game's
    // choices where none were typed (its parameter hint, tags removed).
    const hint = String(during.hint || "").replace(/\[\/?[a-z]+(?:=[^\]]*)?\]/gi, "").trim();
    r.said = hint ? `${brush.name} ${hint}` : during.prompt;
    if (decision !== "no") r.shot = await shot(page, slug(`${brush.name}-proposal`), { type: "png" });
    if (decision === "watch") {
      const painted = await page.waitForFunction((from) => window.__np.events.slice(from).some((e) => e.event === "painted"),
        { timeout: WATCHED[brush.name], polling: 100 }, start).then(() => true, () => false);
      await wait(1200);
      r.events = await page.evaluate((from) => window.__np.events.slice(from).map((e) => e.event === "layout" ? `layout ${e.layout}` : e.event), start);
      r.home = painted;
      if (!painted) throw new Error(`it painted nothing by itself in ${WATCHED[brush.name] / 1000} s; it reported: ${r.events.join(", ") || "nothing"}`);
      r.after = await look(page);
      r.changed = r.after.hash !== r.before.hash;
      r.png = await savePng(page, slug(`${brush.name}-painted`));
      r.pass = r.changed;
      throw null; // done; skip the two-button path below
    }
    // The game proposes the next brush as soon as it is back on Index, so
    // "home" is read from the bridge's events, not from where the game is now.
    const mark = await page.evaluate(() => window.__np.events.length);
    await ask(page, "call", { name: decision === "paint" ? "Paint" : "No" });
    const home = await page.waitForFunction((from, paint) => {
      const seen = window.__np.events.slice(from);
      return seen.some((e) => e.event === "layout" && e.layout === "Index") && (!paint || seen.some((e) => e.event === "painted"));
    }, { timeout: 20000, polling: 100 }, mark, decision === "paint").then(() => ({}), () => ({ error: true }));
    r.events = await page.evaluate((from) => window.__np.events.slice(from).map((e) => e.event === "layout" ? `layout ${e.layout}` : e.event), mark);
    r.home = !home.error;
    if (!r.home) throw new Error(`the game did not come back to Index after ${decision === "paint" ? "Paint" : "No"}; it reported: ${r.events.join(", ") || "nothing"}`);
    r.after = await look(page);
    r.changed = r.after.hash !== r.before.hash;
    if (decision === "paint") r.png = await savePng(page, slug(`${brush.name}-painted`));
    r.pass = decision === "paint" ? r.changed : !r.changed;
    if (CHOSE[brush.name] && !process.env.LINE && !CHOSE[brush.name].test(r.said || "")) { r.pass = false; r.error = `it chose "${r.said}", which is not what ${brush.name} chooses`; }
  } catch (e) {
    if (e !== null) { r.error = String(e?.message || e).slice(0, 240); r.pass = false; }
  }
  if (clip) Object.assign(r, await clip.stop());
  r.ms = Date.now() - t0;
  if (session) {
    r.log = trimLog(session.log, 8);
    r.blocked = [...new Set(blocked.map((url) => url.split("/").slice(0, 3).join("/")))];
    await session.context.close().catch(() => {});
  }
  return r;
}

// SITE_URL runs the same test against a served site (https://nopaint.art).
// Only that origin is reachable, so the visit tracker and the save server
// are still blocked.
const site = process.env.SITE_URL ? { url: process.env.SITE_URL.replace(/\/$/, ""), close() {} } : await serveFolder(SITE);
const browser = await launch(VIEW);
const results = [];
const wanted = only ? only.toLowerCase().split(",").map((name) => name.trim()).filter(Boolean) : null;
const list = brushes.filter((b) => !wanted || wanted.some((name) => b.name.toLowerCase().includes(name)));
for (const [i, brush] of list.entries()) {
  if (SKIP[brush.name]) {
    results.push({ id: `${brush.name}:skipped`, brush: brush.name, skipped: SKIP[brush.name] });
    console.log(`${String(i + 1).padStart(2)}/${list.length} ${brush.name.padEnd(12)} skipped: ${SKIP[brush.name]}`);
    continue;
  }
  for (const decision of WATCHED[brush.name] ? ["watch"] : ["paint", "no"]) {
    const r = await decide(browser, site.url, brush, decision);
    results.push(r);
    console.log(`${String(i + 1).padStart(2)}/${list.length} ${brush.name.padEnd(12)} ${decision.padEnd(5)} ${r.pass ? "ok  " : "FAIL"} ${(r.said || "").padEnd(34).slice(0, 34)} ${r.error || `${r.before?.hash} → ${r.after?.hash}`}`);
    saveResults(site.url, results, { build: process.env.SITE_URL ? "No Paint classic, as served" : "No Paint classic, served from a folder", view: VIEW, look: LOOK });
  }
}
saveResults(site.url, results, { build: process.env.SITE_URL ? "No Paint classic, as served" : "No Paint classic, served from a folder", view: VIEW, look: LOOK });
await browser.close();
site.close();
const failed = results.filter((r) => r.pass === false);
console.log(`done: ${results.filter((r) => r.pass).length} passed, ${failed.length} failed, ${results.filter((r) => r.skipped).length} skipped`);
process.exit(failed.length ? 1 : 0);
