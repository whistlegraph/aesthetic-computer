// Run a .oskie game headless in installed Chrome and write screenshots plus a
// report, so an agent can see what it built without a server or a login.
//   node system/tests/oskie/shot.mjs <game.oskie | game folder> [options]
//     --out <dir>        screenshots + report.json (default: <folder>/shots)
//     --seconds <n>      how long to play after the start gesture (default 7)
//     --keys a,b,c       keys pressed once after clicking the canvas (default Enter)
//     --hold Code:ms,…   keys held while playing, in order (e.g. ArrowUp:1500,Space:200)
//     --drag             drag the pointer across the canvas while playing
//     --width/--height   viewport (default 1280×720); --mobile = 390×844
// Exit code 1 when the game failed to load or threw. The engine comes from the
// local xbox/live checkout when present, else from aesthetic.computer (cached in
// ~/.cache/oskie-engine). Same in-memory store and API as browser-check.mjs.
// Uses puppeteer (present in node_modules on every AC host) and installed Chrome.
import { createServer } from "node:http";
import { readFile, writeFile, mkdir, stat, rm } from "node:fs/promises";
import { homedir } from "node:os";
import path from "node:path";
import puppeteer from "puppeteer";
import { memoryDb } from "./memory-db.mjs";
import { createGameStore } from "../../backend/oskie-store.mjs";
import { createGameAPI } from "../../netlify/functions/oskie.mjs";
import { validateGame } from "../../public/oskie/format.mjs";
import { packDir } from "./pack.mjs";

const args = process.argv.slice(2), opts = { seconds: 7, keys: "Enter", hold: "", width: 1280, height: 720 };
let input;
for (let i = 0; i < args.length; i++) {
  const a = args[i];
  if (a === "--mobile") { opts.width = 390; opts.height = 844; }
  else if (a === "--drag") opts.drag = true;
  else if (a.startsWith("--")) opts[a.slice(2)] = args[++i];
  else input = a;
}
if (!input) { console.error("usage: shot.mjs <game.oskie|dir> [--out dir] [--seconds n] [--keys Enter,Space] [--hold ArrowUp:1500]"); process.exit(2); }
input = path.resolve(input);
const isDir = (await stat(input)).isDirectory();
const game = validateGame(isDir ? await packDir(input) : JSON.parse(await readFile(input, "utf8")));
const out = path.resolve(opts.out || path.join(isDir ? input : path.dirname(input), "shots"));
await mkdir(out, { recursive: true });

const ENGINE = ["oskiewar.js", "scene3d.mjs", "scene3d-webgl.mjs", "oskiewar-sfx.mjs"];
async function engineFile(name) {
  const local = name.endsWith(".woff2")
    ? new URL("../../public/papers.aesthetic.computer/foundry/fonts/" + name, import.meta.url)
    : new URL("../../../xbox/live/" + name, import.meta.url);
  try { const b = await readFile(local); if (b.length > 100) return b; } catch {}
  const cache = path.join(homedir(), ".cache/oskie-engine", name);
  try { return await readFile(cache); } catch {}
  const r = await fetch("https://aesthetic.computer/oskie/engine/" + name);
  if (!r.ok) throw Error("engine asset " + name + " → HTTP " + r.status);
  const buf = Buffer.from(await r.arrayBuffer());
  await mkdir(path.dirname(cache), { recursive: true });
  await writeFile(cache, buf);
  return buf;
}

const owner = { sub: "shot", handle: "@shot" }, store = createGameStore(memoryDb());
const project = await store.create(owner, { slug: game.manifest.slug, title: game.manifest.title, bundle: game });
const api = createGameAPI({ store: async () => store });
// One request handler serves both transports: a loopback HTTP server, or, when a
// sandbox denies listening sockets (EPERM), puppeteer request interception on a
// fake origin, so the harness still runs fully inside an agent's sandbox.
async function handle(method, p, reqHeaders, body) {
  try {
    if (p.startsWith("/api/oskie/")) {
      const r = await api({ path: p, httpMethod: method, headers: reqHeaders, body });
      return { status: r.statusCode, headers: r.headers, body: r.body };
    }
    if (p.startsWith("/oskie/engine/")) {
      const name = p.slice("/oskie/engine/".length);
      if (name !== "ComicRelief-Regular.woff2" && !ENGINE.includes(name)) return { status: 404, headers: {}, body: "" };
      return { status: 200, headers: { "Content-Type": name.endsWith(".woff2") ? "font/woff2" : "text/javascript" }, body: await engineFile(name) };
    }
    const file = /^\/oskie\/(?:[a-f0-9]{32})?\/?$/.test(p) ? "index.html" : p.replace("/oskie/", "");
    if (!/^[a-z.-]+$/.test(file)) return { status: 404, headers: {}, body: "" };
    const type = file.endsWith(".mjs") ? "text/javascript" : file.endsWith(".css") ? "text/css" : "text/html";
    return { status: 200, headers: { "Content-Type": type }, body: await readFile(new URL("../../public/oskie/" + file, import.meta.url)) };
  } catch (e) { return { status: e.status || 500, headers: {}, body: e.message }; }
}
const server = createServer(async (req, res) => {
  let body = ""; for await (const part of req) body += part;
  const r = await handle(req.method, new URL(req.url, "http://localhost").pathname, req.headers, body);
  res.writeHead(r.status, r.headers); res.end(r.body);
});
const listenError = process.env.OSKIE_NO_SERVER ? "forced" : await new Promise((resolve) => {
  server.once("error", (e) => resolve(e.code || e.message));
  server.listen(0, "127.0.0.1", () => resolve(null));
});
const sandboxed = Boolean(listenError);
// https keeps the page a secure context (crypto.randomUUID) even though nothing hits the network.
const base = sandboxed ? "https://oskie.sandbox" : "http://127.0.0.1:" + server.address().port;
if (sandboxed) console.log(`shot: no local server (${listenError}); serving the page through request interception`);

const report = { game: game.manifest.slug, title: game.manifest.title, files: Object.keys(game.files).length, bytes: JSON.stringify(game).length, errors: [], console: [], shots: [], status: "", fps: null };
const CHROME = process.env.OSKIE_CHROME || "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
const chromeArgs = ["--autoplay-policy=no-user-gesture-required", "--mute-audio"];
if (sandboxed) chromeArgs.push("--no-sandbox", "--disable-gpu-sandbox", "--disable-dev-shm-usage");
if (process.env.OSKIE_CHROME_ARGS) chromeArgs.push(...process.env.OSKIE_CHROME_ARGS.split(" ").filter(Boolean));
const browser = await puppeteer.launch({
  executablePath: CHROME, headless: true, args: chromeArgs, dumpio: Boolean(process.env.OSKIE_DUMPIO),
  ...(sandboxed ? { pipe: true, userDataDir: path.join(out, ".chrome-profile") } : {}),
});
let code = 0;
try {
  const page = await browser.newPage();
  await page.setViewport({ width: +opts.width, height: +opts.height });
  if (sandboxed) {
    await page.setRequestInterception(true);
    page.on("request", async (req) => {
      const u = new URL(req.url());
      if (u.origin !== base) return req.abort().catch(() => {});
      const r = await handle(req.method(), u.pathname, req.headers(), req.postData() || "");
      req.respond({ status: r.status, headers: r.headers, body: r.body }).catch(() => {});
    });
  }
  page.on("pageerror", (e) => report.errors.push(String(e.message || e)));
  // The engine probes its fight sound effects, which this sandbox never serves; that noise is not the game's.
  const engineNoise = (t) => /\/sfx\/|Content Security Policy|\/cursors\/|\/type\/webfonts\//.test(t);
  page.on("console", (m) => { const where = m.location()?.url ? " @ " + m.location().url : ""; if (["error", "warn", "warning"].includes(m.type()) && !engineNoise(m.text() + where)) report.console.push(m.type() + ": " + m.text() + where); });
  const shot = async (name) => { const f = path.join(out, name + ".png"); await page.screenshot({ path: f }); report.shots.push(f); };
  const status = () => page.$eval("#status", (e) => e.textContent).catch(() => "");
  const preview = new URL(project.preview);
  await page.goto(base + preview.pathname + preview.hash);
  const loaded = await page.waitForFunction(() => document.querySelector("#status")?.textContent.includes("Revision"), { timeout: 20000 }).then(() => true).catch(() => false);
  report.status = await status();
  if (!loaded) {
    report.errors.push("game did not reach a running revision: " + (await page.$eval("body", (e) => e.innerText).catch(() => "")).slice(0, 400));
    await shot("failed");
    throw Error("load failed");
  }
  await new Promise((r) => setTimeout(r, 700));
  await shot("0-title");
  const frame = page.frames().find((f) => f.parentFrame());
  const canvas = await frame.waitForSelector("canvas", { timeout: 5000 });
  const box = await canvas.boundingBox();
  // Inputs must outlast a 60 Hz sim tick, or games polling keys()/pointer().down never see them.
  const wait = (ms) => new Promise((r) => setTimeout(r, ms));
  await page.mouse.move(box.x + box.width / 2, box.y + box.height / 2);
  await page.mouse.down(); await wait(120); await page.mouse.up(); await wait(200);
  for (const k of opts.keys.split(",").filter(Boolean)) { await page.keyboard.press(k, { delay: 150 }); await wait(300); }
  await wait(500);
  await shot("1-start");
  const holds = opts.hold.split(",").filter(Boolean).map((h) => { const [k, ms] = h.split(":"); return [k, +ms || 800]; });
  const end = Date.now() + opts.seconds * 1000;
  let n = 2, next = Date.now() + 1500, hi = 0;
  const fpsPromise = frame.evaluate(() => new Promise((done) => { let c = 0; const t0 = performance.now(); const tick = () => { c++; performance.now() - t0 < 1000 ? requestAnimationFrame(tick) : done(c); }; requestAnimationFrame(tick); })).catch(() => null);
  while (Date.now() < end) {
    if (hi < holds.length) {
      const [k, ms] = holds[hi++];
      await page.keyboard.down(k); await wait(ms); await page.keyboard.up(k);
    } else if (opts.drag) {
      await page.mouse.move(box.x + box.width * 0.3, box.y + box.height * 0.6);
      await page.mouse.down(); await page.mouse.move(box.x + box.width * 0.7, box.y + box.height * 0.4, { steps: 12 }); await page.mouse.up();
    } else await wait(400);
    if (Date.now() >= next) { await shot(`${n++}-play`); next = Date.now() + 2000; }
  }
  report.fps = await fpsPromise;
  await shot(`${n}-end`);
  report.status = await status();
} catch (e) {
  code = 1; if (!report.errors.length) report.errors.push(String(e.message || e));
} finally {
  await browser.close(); server.close();
  if (sandboxed) await rm(path.join(out, ".chrome-profile"), { recursive: true, force: true }).catch(() => {});
}
if (report.errors.length) code = 1;
await writeFile(path.join(out, "report.json"), JSON.stringify(report, null, 2));
console.log(`${report.game}: ${report.status} · ${report.files} files · ${(report.bytes / 1024).toFixed(0)} KB · fps≈${report.fps ?? "?"} · ${report.shots.length} shots in ${out}`);
for (const e of report.errors) console.log("ERROR " + e);
for (const c of report.console.slice(0, 10)) console.log("console " + c);
process.exit(code);
