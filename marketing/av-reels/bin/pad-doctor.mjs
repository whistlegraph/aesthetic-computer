#!/usr/bin/env node
// pad-doctor.mjs — client-side health check for a pad (or any AC piece): real
// render fps + settled adaptive quality, JS console errors (with the offline
// serve-local noise filtered out), and JS-heap growth over time (the signal that
// actually explains a "it crashed" — a leak or runaway allocation). Frame-less,
// so it's disk-safe (writes nothing).
//
//   node marketing/av-reels/bin/pad-doctor.mjs wispo --base http://localhost:8899
//   node .../pad-doctor.mjs 'merryo~^pads' --base http://localhost:8899 --secs 30
//
// Flags: --base URL  --secs N (watch window, default 15)  --density N (default 1)
//        --all-logs (don't filter offline noise)  --w --h
import { existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const REPO = resolve(dirname(fileURLToPath(import.meta.url)), "..", "..", "..");
const argv = process.argv.slice(2);
const flags = {};
let PIECE = null;
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) flags[a.slice(2)] = argv[i + 1]?.startsWith("--") || i + 1 >= argv.length ? true : argv[++i];
  else if (!PIECE) PIECE = a;
}
if (!PIECE) { console.error("usage: pad-doctor.mjs '<piece>' [--base URL] [--secs 15] [--density 1] [--all-logs]"); process.exit(1); }

const BASE = flags.base || "https://aesthetic.computer";
const SECS = parseFloat(flags.secs || "15");
const DENSITY = flags.density || "1";
const W = parseInt(flags.w || "432", 10);
const H = parseInt(flags.h || "768", 10);

// Known-benign lines when running against serve-local (no session/backend): not
// pad bugs. Hidden unless --all-logs.
const NOISE = [
  /localhost:8889/, /ERR_CONNECTION_REFUSED/, /Could not fetch version/,
  /Batch glyph fetch failed/, /Session connection error/, /\/api\/clock/,
  /Invalid time value/, /favicon/, /net::ERR_/, /udp|geckos/i,
];
const isNoise = (s) => !flags["all-logs"] && NOISE.some((re) => re.test(s));

const PUP = [`${REPO}/node_modules/puppeteer`, `${REPO}/oven/node_modules/puppeteer`, "/opt/oven/node_modules/puppeteer"].find((p) => existsSync(p));
if (!PUP) throw new Error("puppeteer not found");
const puppeteer = (await import(`${PUP}/lib/esm/puppeteer/puppeteer.js`)).default;
const CHROME = [process.env.PUPPETEER_EXECUTABLE_PATH, "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"].find((p) => p && existsSync(p));

const url = `${BASE}/${encodeURIComponent(PIECE)}?nolabel&nogap&density=${DENSITY}`;
const b = await puppeteer.launch({
  headless: "new", ...(CHROME ? { executablePath: CHROME } : {}),
  args: ["--no-sandbox", "--autoplay-policy=no-user-gesture-required", "--use-gl=angle", "--use-angle=metal",
    "--enable-gpu", "--ignore-gpu-blocklist", "--disable-background-timer-throttling",
    "--disable-backgrounding-occluded-windows", "--disable-renderer-backgrounding",
    "--disable-features=CalculateNativeWinOcclusion", `--window-size=${W},${H}`],
});
const p = await b.newPage();
await p.setViewport({ width: W, height: H, deviceScaleFactor: 1 });

const errors = [];
const fps = [], qual = [];
const FRE = /\[pads:fps\]\s+([\d.]+)\s+q=([\d.]+)\s+work=([\d.]+)/;
let lastWork = null;
const onLine = (type, s) => {
  const m = FRE.exec(s);
  if (m) { fps.push(+m[1]); qual.push(+m[2]); lastWork = +m[3]; return; }
  if ((type === "error" || type === "warning" || /error|cannot read|undefined is not|exception|❌/i.test(s)) && !isNoise(s))
    errors.push(`[${type}] ${s.slice(0, 150)}`);
};
p.on("console", (m) => onLine(m.type(), m.text()));
p.on("workercreated", (w) => w.on("console", (m) => onLine(m.type(), m.text())));
p.on("pageerror", (e) => { const s = (e.stack || e.message).split("\n").slice(0, 2).join(" | "); if (!isNoise(s)) errors.push("PAGEERR: " + s.slice(0, 200)); });

try { await p.goto(url, { waitUntil: "networkidle2", timeout: 45000 }); }
catch (e) { console.log(`⚠ goto: ${e.message.slice(0, 70)}`); }

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const heap = async () => { try { const m = await p.evaluate(() => performance?.memory?.usedJSHeapSize || 0); return m; } catch { return 0; } };

await sleep(2500); // warmup
const heapStart = await heap();
const ticks = Math.max(2, Math.round(SECS / 1.5));
let heapEnd = heapStart;
for (let i = 0; i < ticks; i++) { await sleep(1500); heapEnd = await heap(); }
await b.close();

const mb = (n) => (n / 1048576).toFixed(1);
const grew = heapEnd - heapStart;
fps.sort((a, b) => a - b);
const favg = fps.length ? Math.round(fps.reduce((a, b) => a + b, 0) / fps.length) : "?";
const fmin = fps.length ? Math.round(fps[0]) : "?";
const q = qual.length ? qual[qual.length - 1].toFixed(2) : "?";
const fpsV = fmin === "?" ? "no [pads:fps] (not a pads.mjs pad)" : `${fmin >= 55 ? "✅" : fmin >= 40 ? "⚠" : "❌"} fps min=${fmin} avg=${favg} q=${q} work=${lastWork ?? "?"}ms`;
const leakV = heapStart === 0 ? "heap: n/a (no performance.memory)" :
  `heap ${mb(heapStart)}→${mb(heapEnd)}MB (${grew >= 0 ? "+" : ""}${mb(grew)}MB/${SECS}s) ${grew > 40 * 1048576 ? "❌ LEAK?" : grew > 12 * 1048576 ? "⚠ growing" : "✅ stable"}`;

console.log(`\n🩺 ${PIECE}  · density=${DENSITY} · ${SECS}s`);
console.log(`   ${fpsV}`);
console.log(`   ${leakV}`);
console.log(`   JS errors (offline noise filtered): ${errors.length}`);
if (errors.length) console.log(errors.slice(0, 25).map((e) => "     " + e).join("\n"));
