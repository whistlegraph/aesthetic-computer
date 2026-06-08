// Boot Performance Test — real website boot, measured in a real browser.
//
// This measures the AESTHETIC.COMPUTER WEBSITE boot (the browser runtime that
// boot.mjs bootstraps) — NOT the AC Native OS / `ac-os` bare-metal boot. Those
// are unrelated; don't conflate them.
//
// It loads the live site in headless Chrome, waits for the real boot to finish,
// and reads boot.mjs's own telemetry (window._bootTimings, window.acAuthTiming)
// plus JS coverage. It replaces the previous simulated test, which slept on
// setTimeout and measured nothing.
//
//   node tests/performance/boot-performance.test.mjs               # prod
//   TEST_URL=https://localhost:8888 node .../boot-performance.test.mjs   # local dev
//
// Single browser instance on purpose (8 GB machine — no parallel Chromium).

import puppeteer from "puppeteer";
import { writeFileSync, mkdirSync, existsSync } from "fs";

// Prefer a system Chrome (avoids puppeteer's version-pinned download). Override
// with PUPPETEER_EXECUTABLE_PATH. Falls back to puppeteer's bundled binary.
function chromePath() {
  const env = process.env.PUPPETEER_EXECUTABLE_PATH;
  if (env && existsSync(env)) return env;
  const mac = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
  return existsSync(mac) ? mac : undefined;
}

const TEST_URL = process.env.TEST_URL || "https://aesthetic.computer";
const OUT_DIR = "./tests/performance/reports";
const CORE = ["boot.mjs", "bios.mjs", "disk.mjs", "kidlisp.mjs", "graph.mjs"];
const BOOT_BUDGET = 6000; // ms, anon boot target — warn (don't fail) over this

// boot.mjs's bootTelemetry POSTs {phase:"complete", data.elapsedTotal} to
// /api/boot-log exactly once when boot is fully ready (bios → acHIDE_BOOT_LOG).
// Hook fetch before navigation so we catch that authoritative completion stamp,
// rather than guessing from overlay/timeline state.
async function installBootProbe(page) {
  await page.evaluateOnNewDocument(() => {
    window.__bootComplete = null;
    const origFetch = window.fetch;
    window.fetch = function (url, opts) {
      try {
        const u = typeof url === "string" ? url : url?.url;
        if (u && u.includes("/api/boot-log") && opts?.body) {
          const b = JSON.parse(opts.body);
          if (b.phase === "complete" && !window.__bootComplete) {
            window.__bootComplete = { at: performance.now(), elapsedTotal: b.data?.elapsedTotal ?? null };
          }
        }
      } catch {}
      return origFetch.apply(this, arguments);
    };
  });
}

// Wait for the authoritative complete stamp. Fall back to a settled timeline +
// rendered canvas if the complete event never arrives (e.g. telemetry blocked).
async function waitForBoot(page, maxMs = 60000) {
  const t0 = Date.now();
  let last = -1, stable = 0;
  while (Date.now() - t0 < maxMs) {
    const s = await page.evaluate(() => {
      const c = document.querySelector("canvas");
      return {
        complete: window.__bootComplete,
        n: (window._bootTimings || []).length,
        canvas: !!c && c.width > 0,
      };
    });
    if (s.complete) return { done: true, reason: "complete-event", elapsedTotal: s.complete.elapsedTotal, n: s.n, canvas: s.canvas };
    if (s.n > 0 && s.n === last) stable++; else stable = 0;
    last = s.n;
    if (stable >= 10 && s.canvas) return { done: true, reason: "timeline-settled", n: s.n, canvas: s.canvas }; // ~3s quiet, no complete event
    await new Promise((r) => setTimeout(r, 300));
  }
  return { done: false, ms: Date.now() - t0 };
}

function coverageByFile(entries) {
  const byFile = {};
  let blobTotal = 0, blobUsed = 0;
  for (const e of entries) {
    const total = e.text.length;
    const used = e.ranges.reduce((s, r) => s + (r.end - r.start), 0);
    if (e.url.startsWith("blob:")) { blobTotal += total; blobUsed += used; continue; }
    const name = (e.url.split("/").pop() || e.url).split("?")[0];
    const f = (byFile[name] ||= { total: 0, used: 0 });
    f.total += total; f.used += used;
  }
  return { byFile, blob: { total: blobTotal, used: blobUsed } };
}

const kb = (n) => (n / 1024).toFixed(1) + "KB";
const pct = (used, total) => (total ? ((used / total) * 100).toFixed(0) + "%" : "—");

async function run() {
  console.log(`\n🚀 Boot Performance — website boot (not ac-native-os)`);
  console.log(`   target: ${TEST_URL}\n`);

  const browser = await puppeteer.launch({
    headless: true,
    executablePath: chromePath(),
    args: ["--no-sandbox", "--disable-setuid-sandbox", "--ignore-certificate-errors"],
  });

  try {
    const page = await browser.newPage();
    await installBootProbe(page);
    await page.coverage.startJSCoverage({ resetOnNavigation: false });

    const wallStart = Date.now();
    // domcontentloaded, not networkidle — AC holds live sockets + an animation
    // loop, so the network never goes idle and networkidle2 would hang.
    await page.goto(TEST_URL, { waitUntil: "domcontentloaded", timeout: 30000 });

    const booted = await waitForBoot(page);
    const wallMs = Date.now() - wallStart;

    const data = await page.evaluate(() => {
      let auth = null;
      try { auth = window.acAuthTiming?.computeDurations?.() || null; } catch {}
      const nav = performance.getEntriesByType("navigation")[0] || {};
      const paint = performance.getEntriesByType("paint");
      return {
        timings: window._bootTimings || [],
        auth,
        ttfb: (nav.responseStart || 0) - (nav.requestStart || 0),
        fcp: paint.find((p) => p.name === "first-contentful-paint")?.startTime || 0,
        domComplete: nav.domComplete || 0,
        hasCanvas: !!document.querySelector("canvas"),
      };
    });

    const resources = await page.evaluate(() =>
      performance.getEntriesByType("resource")
        .map((r) => ({ name: (r.name.split("/").pop() || r.name).split("?")[0], ms: r.duration, size: r.transferSize }))
        .sort((a, b) => b.ms - a.ms).slice(0, 12));

    const cov = coverageByFile(await page.coverage.stopJSCoverage());

    // boot timeline (elapsed ms is measured from bootStart inside boot.mjs).
    // Prefer the authoritative elapsedTotal from the "complete" telemetry event;
    // fall back to the last logged phase, then wall time.
    const timeline = data.timings;
    const bootMs = booted.elapsedTotal ?? (timeline.length ? Math.max(...timeline.map((t) => t.elapsed || 0)) : wallMs);

    console.log(`⏱️  boot: ${bootMs}ms (telemetry)   wall: ${wallMs}ms   ttfb: ${data.ttfb.toFixed(0)}ms   fcp: ${data.fcp.toFixed(0)}ms`);
    console.log(`   complete: ${booted.done ? booted.reason : "❌ NEVER (timeout)"}   canvas: ${data.hasCanvas ? "yes" : "NO"}\n`);

    console.log(`📋 boot timeline (${timeline.length} phases):`);
    let prev = 0;
    for (const t of timeline) {
      const d = (t.elapsed || 0) - prev; prev = t.elapsed || 0;
      console.log(`   +${String(t.elapsed).padStart(5)}ms  (${d >= 0 ? "+" : ""}${d}ms)  ${t.message}`);
    }
    console.log("");

    if (data.auth && Object.keys(data.auth).length) {
      console.log("🔐 auth0 durations:");
      for (const [k, v] of Object.entries(data.auth)) console.log(`   ${k}: ${Math.round(v)}ms`);
      console.log("");
    }

    console.log("📦 slowest resources:");
    resources.forEach((r, i) => console.log(`   ${i + 1}. ${r.name} — ${r.ms.toFixed(0)}ms ${r.size ? "(" + kb(r.size) + ")" : ""}`));
    console.log("");

    console.log("📊 JS coverage — core files (used / total):");
    for (const f of CORE) {
      const s = cov.byFile[f];
      if (s) console.log(`   ${f.padEnd(12)} ${pct(s.used, s.total).padStart(4)}  (${kb(s.used)} / ${kb(s.total)})`);
      else console.log(`   ${f.padEnd(12)}   —   (loaded via WS blob — see note)`);
    }
    if (cov.blob.total) {
      console.log(`   blob (WS)    ${pct(cov.blob.used, cov.blob.total).padStart(4)}  (${kb(cov.blob.used)} / ${kb(cov.blob.total)})`);
      console.log(`   ⚠️  modules loaded as blob: URLs can't be mapped to filenames — for clean per-file`);
      console.log(`      coverage, run against local dev or with the WS loader disabled.`);
    }
    console.log("");

    // assertions: it must actually boot and show a surface. speed only warns.
    const ok = booted.done && data.hasCanvas && timeline.length > 0;
    console.log(ok ? "✅ booted with a render surface" : "❌ boot did not complete / no canvas");
    if (bootMs > BOOT_BUDGET) console.log(`⚠️  boot ${bootMs}ms over ${BOOT_BUDGET}ms budget (baseline note, not a failure)`);

    // persist a dated baseline
    try {
      mkdirSync(OUT_DIR, { recursive: true });
      const stamp = new Date().toISOString().replace(/[:.]/g, "-");
      const path = `${OUT_DIR}/boot-${stamp}.json`;
      writeFileSync(path, JSON.stringify({ url: TEST_URL, bootMs, wallMs, ttfb: data.ttfb, fcp: data.fcp, complete: booted, timeline, auth: data.auth, resources, coverage: cov }, null, 2));
      console.log(`\n📄 baseline → ${path}`);
    } catch (e) {
      console.log(`\n⚠️  could not write baseline: ${e.message}`);
    }

    if (!ok) process.exit(1);
  } finally {
    await browser.close();
  }
}

if (import.meta.url === `file://${process.argv[1]}`) {
  run().catch((e) => { console.error("❌ test failed:", e.message); process.exit(1); });
}

export { run };
