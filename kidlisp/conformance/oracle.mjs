#!/usr/bin/env node
// oracle — renders the KidLisp corpus from a checkout and compares two renders,
// so a change to kidlisp.mjs can be judged by its pixels.
//
//   node kidlisp/conformance/oracle.mjs refresh [--top 40]
//   node kidlisp/conformance/oracle.mjs render --out DIR [--repo PATH | --base URL] [--only bop,pie] [--video]
//   node kidlisp/conformance/oracle.mjs compare BEFORE AFTER --out DIR
//   node kidlisp/conformance/oracle.mjs check [--against origin/main] [--out DIR] [--video]
//   node kidlisp/conformance/oracle.mjs check --worker-against HEAD [--only bop,pie]
//
// `check` is the PR gate: it renders `--against` twice in a scratch worktree
// (the second pass measures each piece's own noise) and the current checkout
// once, then compares. Exit 1 means a piece went blank or threw a new error;
// `changed` and `drift` pieces get before/after sheets for a human to judge.
//
// Frames are not pixel-exact yet (KidLisp seeds `?` from Date.now and runs in a
// worker we can't clock), so compare judges by colour histograms: it catches a
// piece going blank, erroring, or turning into something else, and leaves
// subtle drift to the eye via the side-by-side sheets.

import puppeteer from "puppeteer";
import sharp from "sharp";
import { spawn, execFileSync } from "node:child_process";
import { get as httpsGet } from "node:https";
import { existsSync, mkdirSync, mkdtempSync, readFileSync, writeFileSync, symlinkSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const repo = resolve(here, "../..");
const SIZE = 384;


const [cmd, ...rest] = process.argv.slice(2);
const { flags, pos } = parse(rest);
const corpusPath = resolve(flags.corpus || join(here,"corpus.json"));
const AT = String(flags.at || "1500,4000").split(",").map(Number); // frame times after boot.

// 🗂️ corpus

async function refresh() {
  const top = Number(flags.top || 40);
  const res = await fetch(`https://aesthetic.computer/api/store-kidlisp?recent=true&limit=${top}&sort=hits`);
  const { recent } = await res.json();
  const pieces = recent.map(({ code, hits, source, handle }) => ({
    code,
    hits,
    handle,
    source,
    embeds: /\$[a-z0-9]{3,}|#[a-z0-9]{3,}/i.test(source), // $code embeds + #painting stamps.
  }));
  const corpus = { refreshed: new Date().toISOString().slice(0, 10), pieces };
  writeFileSync(corpusPath, JSON.stringify(corpus, null, 2) + "\n");
  console.log(`📚 ${pieces.length} pieces → ${corpusPath}`);
}

function corpus() {
  const { pieces } = JSON.parse(readFileSync(corpusPath, "utf8"));
  if (!flags.only) return pieces;
  const only = String(flags.only).split(",").map((c) => c.replace(/^\$/, ""));
  return pieces.filter((p) => only.includes(p.code));
}

// 🎞️ render

async function renderCmd() {
  const out = resolve(need("out"));
  if (flags.base) return render(flags.base, out);
  const server = await serve(resolve(flags.repo || repo), 8870);
  try {
    await render(server.base, out);
  } finally {
    server.stop();
  }
}

async function render(base, out) {
  mkdirSync(out, { recursive: true });
  const pieces = corpus();
  const browser = await launch();
  await shoot(browser, base, pieces[0], mkdtempSync(join(tmpdir(), "kidlisp-warm-"))); // cold lith compiles on first hit.
  const report = {};
  const lanes = Number(flags.lanes || 3);
  let next = 0;
  await Promise.all(
    Array.from({ length: lanes }, async () => {
      while (next < pieces.length) {
        const p = pieces[next++];
        report[p.code] = await shoot(browser, base, p, out);
        if (report[p.code].errors.includes(NOBOOT)) report[p.code] = await shoot(browser, base, p, out); // boot flakes under load; one retry.
        const errs = report[p.code].errors.length;
        console.log(`  $${p.code}${errs ? `  ⚠️ ${errs} error(s)` : ""}`);
      }
    }),
  );
  await browser.close();
  writeFileSync(join(out, "render.json"), JSON.stringify({ base, at: AT, pieces: report }, null, 2));
  if (flags["worker-against"] && Object.values(report).some(p => !p.worker?.verified)) {
    throw new Error(`Required worker was not active for every piece; inspect ${join(out, "render.json")}`);
  }
  console.log(`🎞️ ${pieces.length} pieces → ${out}`);
}

async function shoot(browser, base, piece, out) {
  // Own context per piece: pages sharing an origin share AC's service worker
  // and module cache, and boot trips over each other.
  const context = await browser.createBrowserContext();
  const page = await context.newPage();
  const errors = [], inclusions = new Set();
  page.on("pageerror", (e) => errors.push(String(e.message || e).slice(0, 300)));
  page.on("console", (m) => m.type() === "error" && errors.push(m.text().slice(0, 300)));
  await page.setRequestInterception(true);
  page.on("request", (req) => {
    const url=new URL(req.url());
    if(url.pathname==="/api/store-kidlisp"&&url.searchParams.has("code"))inclusions.add(url.searchParams.get("code"));
    answer(req);
  });
  await page.setViewport({ width: SIZE, height: SIZE, deviceScaleFactor: 1 });
  // By $code, exactly as it runs live. Sources resolve from prod (lith falls
  // back when there's no local db), which is fine: a $code's source never changes.
  const url = `${base}/$${piece.code}?nogap=true&nolabel=true&density=1&noauth=true${flags["worker-against"] ? "&workerbundle=1" : ""}`;
  await page.goto(url, { waitUntil: "domcontentloaded", timeout: 30_000 });
  // The clock starts when boot hides its overlay, not at navigation; boot time
  // swings by seconds under load and would read as a changed piece.
  await page
    .waitForFunction(() => window.acBOOTED === true, { timeout: 30_000 })
    .catch(() => errors.push(NOBOOT));
  let worker;
  if (flags["worker-against"]) {
    const state = await page.evaluate(() => window.acWORKER_BUNDLE);
    const expected = workerOverride?.filename || JSON.parse(readFileSync(join(repo, "system/public/aesthetic.computer/lib/disk-worker-manifest.json"), "utf8")).filename;
    worker = { expected, state, verified: Boolean(state?.ready && state.active && state.filename === expected) };
    if (!worker.verified) errors.push(`Required worker ${expected} was not active: ${JSON.stringify(state)}`);
  }
  const t0 = Date.now();
  const video = flags.video ? await page.screencast({ path: join(out, `${piece.code}.webm`) }) : null;
  const frames = [];
  for (const [i, t] of AT.entries()) {
    await wait(t - (Date.now() - t0));
    const path = join(out, `${piece.code}-${i}.png`);
    await page.screenshot({ path, type: "png" });
    frames.push(path);
  }
  await video?.stop();
  await context.close();
  return { url, frames, errors, inclusions:[...inclusions], ...(worker && { worker }) };
}

const NOBOOT = "oracle: never booted";

// $code lookups are answered here, from prod, cached for the run. A local lith
// has no db, and its 503 → prod fallback costs seconds per embed; this also
// hands both sides of a check the exact same sources.
const sources = new Map();
if(flags.corpus) {
  const snapshot=JSON.parse(readFileSync(corpusPath,"utf8"));
  for(const piece of [...snapshot.pieces,...Object.values(snapshot.dependencies||{})]) {
    sources.set(piece.code,Promise.resolve({status:200,body:JSON.stringify({source:piece.source,handle:piece.handle,hits:piece.hits})}));
  }
}
let workerOverride = null;
let workerOverrideRequests = 0;

async function answer(req) {
  const url = new URL(req.url());
  if (workerOverride && url.pathname === "/aesthetic.computer/lib/disk-worker-manifest.json") {
    workerOverrideRequests++;
    return req.respond({ status: 200, contentType: "application/json", body: JSON.stringify(workerOverride) });
  }
  const code = url.pathname === "/api/store-kidlisp" && url.searchParams.get("code");
  if (!code) return req.continue();
  if(flags.corpus&&!sources.has(code))return req.respond({status:404,contentType:"application/json",body:JSON.stringify({error:`Unpinned inclusion $${code}`})});
  if (!sources.has(code)) sources.set(code, lookup(code));
  const { status, body } = await sources.get(code);
  req.respond({ status, contentType: "application/json", body });
}

async function lookup(code, tries = 3) {
  try {
    const r = await fetch(`https://aesthetic.computer/api/store-kidlisp?code=${code}`);
    return { status: r.status, body: await r.text() };
  } catch (e) {
    if (tries > 1) return (await wait(1000), lookup(code, tries - 1));
    console.error(`  ⚠️ couldn't reach prod for $${code}: ${e.cause?.code || e.message}`);
    return { status: 502, body: JSON.stringify({ error: "oracle: prod unreachable" }) };
  }
}

// ⚖️ compare

async function compareCmd() {
  const [a, b] = pos;
  if (!a || !b) throw new Error("compare needs BEFORE and AFTER dirs");
  const noise = flags.noise ? resolve(flags.noise) : null;
  const failed = await compare(resolve(a), resolve(b), resolve(need("out")), noise);
  process.exit(failed ? 1 : 0);
}

// Histogram distance 0..1 between two frames. Under DRIFT is the same piece;
// over CHANGED it's become something else. Both sit on top of the piece's own
// noise (base rendered twice), since `?` and timing make many pieces differ
// from themselves.
const DRIFT = 0.12;
const CHANGED = 0.4;
const GONE = 0.95; // a steady piece that became a different picture entirely.
const CHAOS = 0.3; // a piece this far from itself can't be judged by histogram; the sheet goes to the eye.

async function compare(beforeDir, afterDir, out, noiseDir) {
  mkdirSync(out, { recursive: true });
  const load = (dir) => JSON.parse(readFileSync(join(dir, "render.json"), "utf8")).pieces;
  const before = load(beforeDir);
  const after = load(afterDir);
  const again = noiseDir ? load(noiseDir) : {};
  const rows = [];
  for (const code of Object.keys(before)) {
    if (!after[code]) continue;
    const A = await Promise.all(before[code].frames.map(look));
    const B = await Promise.all(after[code].frames.map(look));
    const A2 = again[code] ? await Promise.all(again[code].frames.map(look)) : null;
    const gap = (X, Y) => Math.max(...X.map((f, i) => distance(f.hist, Y[i].hist)));
    const dist = gap(A, B);
    const noise = A2 ? gap(A, A2) : 0;
    // Flat and far from before: covers `(wipe blue)` losing its blue as well
    // as a busy piece going dark.
    const blank = dist > CHANGED && B.some((f, i) => f.colors <= 2 && distance(f.hist, A[i].hist) > CHANGED);
    const seen = new Set([...before[code].errors, ...(again[code]?.errors || [])].map(gist));
    const newErrors = [...new Set(after[code].errors.map(gist))].filter((e) => e && !seen.has(e));
    const verdict = blank
      ? "blank"
      : newErrors.length
        ? "error"
        : dist > GONE && noise < 0.1
          ? "gone"
          : dist > Math.max(CHANGED, noise + 0.25) && noise < CHAOS
          ? "changed"
          : dist > Math.max(DRIFT, noise + 0.1)
            ? "drift"
            : "same";
    rows.push({ code, verdict, dist: +dist.toFixed(3), noise: +noise.toFixed(3), newErrors });
    if (verdict !== "same") await sheet(code, before[code].frames, after[code].frames, join(out, `${code}.png`));
  }
  // Only the unambiguous verdicts fail. `changed` can still be a chaotic piece
  // being itself until KidLisp can render deterministically, so it goes to
  // the reviewer as a sheet instead.
  const failing = rows.filter((r) => ["blank", "error", "gone"].includes(r.verdict));
  writeFileSync(join(out, "compare.json"), JSON.stringify(rows, null, 2));
  writeFileSync(join(out, "compare.md"), markdown(rows));
  const tally = Object.groupBy(rows, (r) => r.verdict);
  console.log("⚖️ " + Object.entries(tally).map(([v, rs]) => `${v} ${rs.length}`).join(" · "));
  for (const r of rows.filter((r) => r.verdict !== "same")) console.log(`  $${r.code}  ${r.verdict}  ${r.dist}`);
  return failing.length > 0;
}

async function look(path) {
  // Downsample so noise and 1px shifts wash out; 4 levels per channel = 64 bins.
  const { data } = await sharp(path).resize(64, 64, { fit: "fill" }).removeAlpha().raw().toBuffer({ resolveWithObject: true });
  const hist = new Float64Array(64);
  const seen = new Set();
  for (let i = 0; i < data.length; i += 3) {
    const [r, g, b] = [data[i] >> 6, data[i + 1] >> 6, data[i + 2] >> 6];
    hist[(r << 4) | (g << 2) | b] += 1;
    seen.add((data[i] << 16) | (data[i + 1] << 8) | data[i + 2]);
  }
  const n = data.length / 3;
  return { hist: hist.map((v) => v / n), colors: seen.size };
}

// An error worth reporting, reduced to its gist; local-env network noise
// (no db, no session server) returns "" and is dropped.
const NOISE = /Failed to load resource|Failed to fetch|WebSocket|HTTP 50\d|Service Unavailable|ERR_(CONNECTION|SSL)/;
const gist = (e) => (NOISE.test(e) ? "" : e.replace(/\d+/g, "#"));

const distance = (h1, h2) => h1.reduce((s, v, i) => s + Math.abs(v - h2[i]), 0) / 2;

async function sheet(code, beforeFrames, afterFrames, path) {
  const pad = 8;
  const cell = SIZE / 2;
  const cols = beforeFrames.length;
  const composite = [];
  for (const [row, frames] of [beforeFrames, afterFrames].entries()) {
    for (const [col, f] of frames.entries()) {
      composite.push({
        input: await sharp(f).resize(cell, cell).png().toBuffer(),
        left: pad + col * (cell + pad),
        top: pad + row * (cell + pad),
      });
    }
    const tag = `<svg width="64" height="20" xmlns="http://www.w3.org/2000/svg"><rect width="64" height="20" fill="#000" opacity="0.7"/><text x="6" y="14" font-family="monospace" font-size="12" fill="#fff">${row ? "after" : "before"}</text></svg>`;
    composite.push({ input: Buffer.from(tag), left: pad, top: pad + row * (cell + pad) });
  }
  await sharp({
    create: { width: pad + cols * (cell + pad), height: pad + 2 * (cell + pad), channels: 3, background: "#111" },
  })
    .composite(composite)
    .png()
    .toFile(path);
}

function markdown(rows) {
  const mark = { same: "·", drift: "~", changed: "?", blank: "✗", error: "✗", gone: "✗" };
  const lines = ["| piece | verdict | distance | own noise |", "|---|---|---:|---:|"];
  for (const r of rows) lines.push(`| \`$${r.code}\` | ${mark[r.verdict]} ${r.verdict} | ${r.dist} | ${r.noise} |`);
  const errs = rows.filter((r) => r.newErrors.length);
  if (errs.length) {
    lines.push("", "New console errors:", "");
    for (const r of errs) for (const e of r.newErrors) lines.push(`- \`$${r.code}\`: ${e}`);
  }
  return lines.join("\n") + "\n";
}

// ✅ check — the PR gate

async function check() {
  if (flags["worker-against"]) return checkWorker();
  const against = flags.against || "origin/main";
  const out = resolve(flags.out || mkdtempSync(join(tmpdir(), "kidlisp-oracle-")));
  const tree = mkdtempSync(join(tmpdir(), "kidlisp-base-"));
  execFileSync("git", ["-C", repo, "worktree", "add", "--detach", tree, against], { stdio: "ignore" });
  for (const dir of ["", "system", "lith"]) {
    const nm = join(repo, dir, "node_modules");
    if (existsSync(nm)) symlinkSync(nm, join(tree, dir, "node_modules"));
  }
  const servers = [];
  let clean = false;
  const cleanup = () => {
    if (clean) return;
    clean = true;
    for (const s of servers) s.stop();
    execFileSync("git", ["-C", repo, "worktree", "remove", "--force", tree]);
  };
  process.on("exit", cleanup); // also covers a crash mid-render.
  servers.push(...(await Promise.all([serve(tree, 8871), serve(repo, 8872)])));
  const [a, b] = servers;
  await render(a.base, join(out, "before"));
  await render(a.base, join(out, "noise"));
  await render(b.base, join(out, "after"));
  cleanup();
  const failed = await compare(join(out, "before"), join(out, "after"), join(out, "diff"), join(out, "noise"));
  console.log(`📎 ${out}`);
  process.exit(failed ? 1 : 0);
}

// Compare the exact committed worker using one development host. No checkout:
// useful under a disk reserve, and explicitly narrower than a whole-tree check.
async function checkWorker() {
  const ref = String(flags["worker-against"]);
  if (!/^[a-zA-Z0-9][a-zA-Z0-9/._~^@{}-]*$/.test(ref)) throw new Error("Invalid worker revision");
  const out = resolve(flags.out || mkdtempSync(join(tmpdir(), "kidlisp-worker-oracle-")));
  const manifest = JSON.parse(execFileSync("git", ["-C", repo, "show", `${ref}:system/public/aesthetic.computer/lib/disk-worker-manifest.json`], { encoding: "utf8" }));
  if (!/^disk\.worker\.[a-f0-9]{12}\.mjs$/.test(manifest.filename)) throw new Error("Invalid committed worker manifest");
  const worker = execFileSync("git", ["-C", repo, "show", `${ref}:system/public/aesthetic.computer/lib/${manifest.filename}`], { maxBuffer: 32 * 1024 * 1024 });
  const { createHash } = await import("node:crypto");
  if (createHash("sha256").update(worker).digest("hex") !== manifest.sha256) throw new Error("Committed worker hash differs from its manifest");
  const servedWorker = readFileSync(join(repo, "system/public/aesthetic.computer/lib", manifest.filename));
  if (createHash("sha256").update(servedWorker).digest("hex") !== manifest.sha256) throw new Error("Committed worker artifact is absent or modified in this checkout");
  const server = await serve(repo, 8870);
  try {
    workerOverride = manifest;
    await render(server.base, join(out, "before"));
    const beforeRequests = workerOverrideRequests;
    await render(server.base, join(out, "noise"));
    if (!beforeRequests || workerOverrideRequests === beforeRequests) throw new Error("Baseline worker was not intercepted in both passes");
    workerOverride = null;
    await render(server.base, join(out, "after"));
    writeFileSync(join(out, "scope.json"), JSON.stringify({ mode: "worker-only", reference: ref, workerSha256: manifest.sha256, host: "current checkout in development mode", pieces: corpus().map(p => p.code) }, null, 2));
    const failed = await compare(join(out, "before"), join(out, "after"), join(out, "diff"), join(out, "noise"));
    console.log(`📎 ${out} (worker comparison; current host on both sides)`);
    process.exitCode = failed ? 1 : 0;
  } finally { workerOverride = null; server.stop(); }
}

// 🧰 plumbing

async function serve(root, port) {
  const child = spawn("node", ["server.mjs"], {
    cwd: join(root, "lith"),
    // Conformance must never start production maintenance jobs, even if the
    // invoking shell has NODE_ENV=production. Local certs are handled below.
    env: { ...process.env, NODE_ENV: "development", PORT: String(port) },
    stdio: "ignore",
  });
  const tls = existsSync(join(root, "ssl-dev/localhost.pem")) && existsSync(join(root, "ssl-dev/localhost-key.pem"));
  const base = `${tls ? "https" : "http"}://localhost:${port}`;
  const ready = () => tls ? new Promise(resolve => {
    // This exception is confined to the child we own on loopback. It does not
    // change global TLS validation or requests to any external service.
    const request = httpsGet(base, { rejectUnauthorized: false }, response => {
      response.resume();
      resolve(response.statusCode >= 200 && response.statusCode < 300);
    });
    request.on("error", () => resolve(false));
    request.setTimeout(1000, () => { request.destroy(); resolve(false); });
  }) : fetch(base).then(r => r.ok, () => false);
  for (let i = 0; i < 60; i++) {
    if (await ready()) return { base, stop: () => child.kill() };
    await wait(500);
  }
  child.kill();
  throw new Error(`lith didn't come up on ${port} from ${root}`);
}

async function launch() {
  const mac = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
  return puppeteer.launch({
    headless: "new",
    executablePath: flags.chrome || (existsSync(mac) ? mac : undefined),
    args: [
      "--no-sandbox",
      "--mute-audio",
      "--autoplay-policy=no-user-gesture-required",
      // Lanes render side by side; without these, every page but one is a
      // throttled background tab and paints nothing.
      "--disable-background-timer-throttling",
      "--disable-renderer-backgrounding",
      "--disable-backgrounding-occluded-windows",
      "--allow-insecure-localhost", // the repo's local development certificate
    ],
  });
}

const wait = (ms) => new Promise((r) => setTimeout(r, Math.max(0, ms)));

function need(key) {
  if (!flags[key]) throw new Error(`missing --${key}`);
  return flags[key];
}

function parse(args) {
  const flags = {};
  const pos = [];
  for (let i = 0; i < args.length; i++) {
    if (!args[i].startsWith("--")) pos.push(args[i]);
    else if (args[i + 1] === undefined || args[i + 1].startsWith("--")) flags[args[i].slice(2)] = true;
    else flags[args[i].slice(2)] = args[++i];
  }
  return { flags, pos };
}

// Dispatch last, so every const above is initialised.
const commands = { refresh, render: renderCmd, compare: compareCmd, check };
if (!commands[cmd]) {
  console.error("usage: oracle.mjs refresh | render | compare | check  (see header)");
  process.exit(1);
}
await commands[cmd]();
