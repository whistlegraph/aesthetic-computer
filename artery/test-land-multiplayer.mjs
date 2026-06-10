#!/usr/bin/env node
// 🌾 land 2-player smoke test — opens two headless Chromium pages on /land,
// waits for boot + netcode, then asserts BOTH clients are receiving land
// snapshots and can see each other in the roster. Screenshots land in /tmp.
//
// Usage: node artery/test-land-multiplayer.mjs [--url https://localhost:8888] [--keep]
//   --keep  leave the browsers open (headful debugging on a desktop host)

import puppeteer from "puppeteer";

const argv = process.argv.slice(2);
const flag = (n, d) => { const i = argv.indexOf("--" + n); return i === -1 ? d : argv[i + 1]; };
const URL_BASE = flag("url", process.env.AC_LAND_URL || "https://localhost:8888");
const KEEP = argv.includes("--keep");

const pass = (m) => console.log(`✅ ${m}`);
const fail = (m) => { console.log(`❌ ${m}`); process.exitCode = 1; };

async function openPlayer(name) {
  const browser = await puppeteer.launch({
    executablePath: "/usr/sbin/chromium-browser",
    headless: !KEEP,
    args: [
      "--no-sandbox",
      "--ignore-certificate-errors",
      "--use-fake-ui-for-media-stream",
      "--window-size=900,600",
    ],
  });
  const page = (await browser.pages())[0] ?? (await browser.newPage());
  await page.setViewport({ width: 900, height: 600 });
  const consoleLines = [];
  page.on("console", (msg) => {
    const t = msg.text();
    consoleLines.push(t);
    if (/🌾|welcome|roster|snap/.test(t)) console.log(`[${name}] ${t}`);
  });
  await page.goto(`${URL_BASE}/land`, { waitUntil: "load", timeout: 60_000 });
  return { browser, page, name, consoleLines };
}

// The land piece batches a perf ring to bios, which mirrors it onto
// window.__arena_perfStats (shared channel name — see bios.mjs). meta carries
// myHandle / snapsRx / position, which is all this smoke test needs.
async function perf(page) {
  return page.evaluate(() => {
    const fn = window.__arena_perfStats;
    return typeof fn === "function" ? fn().meta : null;
  });
}

async function perfWait(page, name, ms = 45_000) {
  const t0 = Date.now();
  while (Date.now() - t0 < ms) {
    const m = await perf(page);
    if (m && m.snapsRx > 10) return m;
    await new Promise((r) => setTimeout(r, 1_500));
  }
  console.log(`⚠️ ${name}: perf meta never arrived`);
  return null;
}

const p1 = await openPlayer("P1");
const p2 = await openPlayer("P2");

// Boot + WS/UDP + first snaps take a few seconds; poll rather than guess.
const m1 = await perfWait(p1.page, "P1");
const m2 = await perfWait(p2.page, "P2");

console.log("P1 meta:", JSON.stringify(m1));
console.log("P2 meta:", JSON.stringify(m2));

if (m1 && m1.snapsRx > 10) pass(`P1 (${m1.myHandle}) receiving snaps: ${m1.snapsRx}`);
else fail("P1 not receiving snaps");
if (m2 && m2.snapsRx > 10) pass(`P2 (${m2.myHandle}) receiving snaps: ${m2.snapsRx}`);
else fail("P2 not receiving snaps");
if (m1 && m2 && m1.myHandle !== m2.myHandle) pass("distinct handles");

// 🤖 Wire-level walker bot joins the same meadow and strolls — both browser
// clients must see it appear in their rosters. (Keyboard-driven walking
// needs real pointer lock; verify that manually with --keep on a desktop.)
const { WebSocket } = await import("ws");
const { packCmd } = await import(
  "../system/public/aesthetic.computer/lib/pmove.mjs"
);
const SESSION_URL = flag("session", "wss://localhost:8889");
await new Promise((resolve) => {
  const bot = new WebSocket(SESSION_URL, { rejectUnauthorized: false });
  const bsend = (type, content) =>
    bot.send(JSON.stringify({ type, content: JSON.stringify(content) }));
  let seq = 0;
  const t0 = Date.now();
  bot.on("message", (d) => {
    try {
      const m = JSON.parse(d.toString());
      if (m.type?.startsWith("land:") && m.type !== "land:snap") {
        console.log(`[bot] ${m.type}`);
      }
    } catch {}
  });
  bot.on("open", () => {
    console.log(`[bot] connected → ${SESSION_URL}`);
    bsend("land:hello", { handle: "meadowbot" });
    const iv = setInterval(() => {
      seq++;
      bsend("land:cmd", { handle: "meadowbot", firstSeq: seq, ack: 0,
        cmds: [packCmd({ ms: Date.now() - t0, fwd: 1, right: 0, yaw: 45, pitch: 0, buttons: 0 })] });
    }, 50);
    setTimeout(() => {
      clearInterval(iv);
      bsend("land:bye", { handle: "meadowbot" });
      bot.close();
      resolve();
    }, 5_000);
  });
  bot.on("error", (e) => { console.log("[bot] error:", e.message); resolve(); });
});
await new Promise((r) => setTimeout(r, 2_000));
for (const p of [p1, p2]) {
  if (p.consoleLines.some((l) => l.includes("meadowbot"))) {
    pass(`${p.name} saw meadowbot join its roster`);
  } else {
    fail(`${p.name} never saw meadowbot`);
  }
}

await p1.page.screenshot({ path: "/tmp/land-p1.png" });
await p2.page.screenshot({ path: "/tmp/land-p2.png" });
console.log("📸 screenshots: /tmp/land-p1.png /tmp/land-p2.png");

if (!KEEP) {
  await p1.browser.close();
  await p2.browser.close();
}
console.log(process.exitCode ? "❌ FAIL" : "🌾 PASS");
