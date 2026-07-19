#!/usr/bin/env node
// scrub-jam.mjs — "prove the groove": open AC synthtapes in separate, AUDIBLE
// Chrome windows and freestyle-perform them forever (scrub, scratch, beat-jump,
// chop, flick, steady-dial), periodically releasing so each tape drifts back
// into net-time sync. Prints a live sync/rate readout per window so unison is
// provable numerically; runs until Ctrl+C (or --secs N).
//
// Why real Chrome + puppeteer input (not window.dispatchEvent): only TRUSTED
// input events unlock a suspended AudioContext, so synthetic dispatch stays
// silent. puppeteer's mouse/keyboard/wheel go through CDP's Input domain →
// trusted → audible. Each window is its own puppeteer launch = its own OS
// window with its own audio.
//
//   node toolchain/tapes/scrub-jam.mjs                       # local, sine+house
//   node toolchain/tapes/scrub-jam.mjs --prod                # aesthetic.computer
//   node toolchain/tapes/scrub-jam.mjs --tapes break,house,dub,sine
//   node toolchain/tapes/scrub-jam.mjs --secs 60 --shots     # timed + screenshots

import puppeteer from "puppeteer-core";
import { mkdir, writeFile } from "node:fs/promises";
import { join, dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const __dir = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(__dir, "..", "..");
const CHROME =
  process.env.CHROME_PATH ||
  "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";

const args = process.argv.slice(2);
const has = (f) => args.includes(f);
const val = (f, d) => {
  const i = args.indexOf(f);
  return i >= 0 && args[i + 1] ? args[i + 1] : d;
};

const PROD = has("--prod");
const BASE = PROD
  ? "https://aesthetic.computer"
  : val("--base", "http://localhost:8899");
const TAPES = val("--tapes", "sine,house").split(",").map((s) => s.trim());
const SECS = parseFloat(val("--secs", "0")) || 0; // 0 = until Ctrl+C
const SHOTS = has("--shots");
const W = 720;
const H = 480;

const rnd = (a, b) => a + Math.random() * (b - a);
const pick = (arr) => arr[Math.floor(Math.random() * arr.length)];
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

// One performer per window. Holds a puppeteer page and drives it forever.
class Performer {
  constructor(page, tape, idx) {
    this.page = page;
    this.tape = tape;
    this.idx = idx;
    this.cx = W / 2;
    this.cy = H / 2;
    this.stopped = false;
    this.last = { rate: 0, syncMs: 0, locked: false, act: "boot" };
  }

  async telemetry() {
    try {
      return await this.page.evaluate(() => window.__tapeTelemetry || null);
    } catch {
      return null;
    }
  }

  // Unlock audio with a trusted click, then let the loop play a beat.
  async wake() {
    const m = this.page.mouse;
    await m.move(this.cx, this.cy);
    await m.click(this.cx, this.cy); // Trusted → unlocks AudioContext
    await sleep(400);
  }

  async wheelBurst(deltaX, count, gap) {
    const m = this.page.mouse;
    await m.move(this.cx, this.cy);
    for (let i = 0; i < count && !this.stopped; i++) {
      await m.wheel({ deltaX });
      await sleep(gap);
    }
  }

  // A rubber-band drag: grab, displace horizontally (rate), release.
  async dragScratch(dir, reach, hold) {
    const m = this.page.mouse;
    await m.move(this.cx, this.cy);
    await m.down();
    const steps = 8;
    for (let i = 1; i <= steps && !this.stopped; i++) {
      await m.move(this.cx + dir * reach * (i / steps), this.cy);
      await sleep(hold / steps);
    }
    await m.up();
  }

  // Back-and-forth drag scratch — the DJ wiggle.
  async wiggle(times) {
    const m = this.page.mouse;
    await m.move(this.cx, this.cy);
    await m.down();
    for (let i = 0; i < times && !this.stopped; i++) {
      await m.move(this.cx + 120, this.cy, { steps: 4 });
      await sleep(90);
      await m.move(this.cx - 120, this.cy, { steps: 4 });
      await sleep(90);
    }
    await m.move(this.cx, this.cy, { steps: 3 });
    await m.up();
  }

  // Flick: quick throw then release → wheel spins free, friction glides home.
  async flick(dir) {
    const m = this.page.mouse;
    await m.move(this.cx, this.cy);
    await m.down();
    for (let i = 1; i <= 5 && !this.stopped; i++) {
      await m.move(this.cx + dir * 30 * i, this.cy);
      await sleep(12);
    }
    await m.up();
  }

  async beatJump(n, key) {
    for (let i = 0; i < n && !this.stopped; i++) {
      await this.page.keyboard.press(key);
      await sleep(rnd(120, 240));
    }
  }

  async chop(key, ms) {
    await this.page.keyboard.down(key);
    await sleep(ms);
    await this.page.keyboard.up(key);
  }

  // Drag the top-right rate readout vertically to set a steady rate, hold
  // it a moment, then return to ~1× so the tape can re-sync (the dial holds
  // friction-free while engaged — a manual tempo offset; the jam releases).
  async steadyDial(up) {
    const m = this.page.mouse;
    const x = W - 32;
    const y = 12;
    await m.move(x, y);
    await m.down();
    for (let i = 1; i <= 6 && !this.stopped; i++) {
      await m.move(x, y + (up ? -1 : 1) * 8 * i, { steps: 2 });
      await sleep(70);
    }
    await sleep(700); // Hold the offset audibly...
    for (let i = 6; i >= 0 && !this.stopped; i--) {
      await m.move(x, y + (up ? -1 : 1) * 8 * i, { steps: 2 }); // ...back to center
      await sleep(50);
    }
    await m.up();
  }

  // One freestyle phrase, chosen at random.
  async phrase() {
    const moves = [
      ["scrub fwd", () => this.wheelBurst(-60, 14, 18)],
      ["scrub back", () => this.wheelBurst(60, 14, 18)],
      ["scratch", () => this.wiggle(3)],
      ["drag+", () => this.dragScratch(1, 200, 500)],
      ["drag-", () => this.dragScratch(-1, 200, 500)],
      ["flick fwd", () => this.flick(1)],
      ["flick back", () => this.flick(-1)],
      ["jump ->", () => this.beatJump(3, "ArrowRight")],
      ["jump <-", () => this.beatJump(3, "ArrowLeft")],
      ["chop 1/4", () => this.chop("ArrowUp", 900)],
      ["chop 1/8", () => this.chop("ArrowDown", 700)],
      ["dial up", () => this.steadyDial(true)],
      ["dial down", () => this.steadyDial(false)],
    ];
    const [name, fn] = pick(moves);
    this.last.act = name;
    await fn();
  }

  // Freestyle forever: phrase → release (let net-time re-sync) → repeat.
  async run() {
    await this.wake();
    while (!this.stopped) {
      await this.phrase();
      // Release: hands off, tape drifts back toward net-time unison.
      this.last.act += " → release";
      const releaseMs = rnd(3500, 6000);
      const until = Date.now() + releaseMs;
      while (Date.now() < until && !this.stopped) await sleep(150);
    }
  }

  stop() {
    this.stopped = true;
  }
}

const windows = [];

async function launchWindow(tape, idx) {
  const browser = await puppeteer.launch({
    executablePath: CHROME,
    headless: false,
    args: [
      `--window-size=${W},${H}`,
      `--window-position=${40 + idx * (W + 24)},${80}`,
      "--autoplay-policy=no-user-gesture-required",
      "--use-fake-ui-for-media-stream",
      "--no-first-run",
      "--no-default-browser-check",
    ],
    defaultViewport: { width: W, height: H },
  });
  const page = (await browser.pages())[0];
  const url = `${BASE}/video~scrub~${tape}`;
  await page.goto(url, { waitUntil: "domcontentloaded", timeout: 45000 });
  // Give boot + synth a moment.
  await sleep(3500);
  const perf = new Performer(page, tape, idx);
  windows.push({ browser, page, perf, tape });
  return perf;
}

async function main() {
  console.log(
    `🎛️  scrub-jam — ${TAPES.length} window(s) @ ${BASE}\n` +
      `   tapes: ${TAPES.join(", ")}${SECS ? `  (${SECS}s)` : "  (Ctrl+C to stop)"}\n`,
  );

  const perfs = [];
  for (let i = 0; i < TAPES.length; i++) {
    perfs.push(await launchWindow(TAPES[i], i));
  }

  // Kick off each performer's forever-loop.
  perfs.forEach((p) => p.run().catch((e) => console.error(`perf ${p.tape}:`, e.message)));

  // Live readout: sync + rate + current move, per window, ~2Hz.
  const shotsDir = join(REPO, "toolchain", "tapes", ".jam-shots");
  if (SHOTS) await mkdir(shotsDir, { recursive: true });
  let tick = 0;
  const start = Date.now();
  const readout = setInterval(async () => {
    tick++;
    const cells = [];
    for (const w of windows) {
      const t = await w.perf.telemetry();
      const rate = t ? t.rate.toFixed(2) : "--";
      const sync = t ? `${t.syncMs >= 0 ? "+" : ""}${t.syncMs}ms` : "--";
      const lock = t?.locked ? "🟢" : "🟠";
      cells.push(
        `${w.tape.padEnd(6)} ${lock} ${sync.padStart(7)} ${String(rate).padStart(6)}x  ${w.perf.last.act}`,
      );
    }
    const secs = ((Date.now() - start) / 1000).toFixed(0);
    process.stdout.write(`\r[${secs}s] ` + cells.join("  |  ") + "          \n");

    if (SHOTS && tick % 8 === 0) {
      for (const w of windows) {
        try {
          const buf = await w.page.screenshot({ type: "jpeg", quality: 70 });
          await writeFile(join(shotsDir, `${w.tape}-${tick}.jpg`), buf);
        } catch {}
      }
    }
  }, 500);

  let closing = false;
  const shutdown = async () => {
    if (closing) return;
    closing = true;
    clearInterval(readout);
    console.log("\n🛑 stopping jam...");
    windows.forEach((w) => w.perf.stop());
    await sleep(300);
    for (const w of windows) {
      try {
        await w.browser.close();
      } catch {}
    }
    process.exit(0);
  };

  process.on("SIGINT", shutdown);
  process.on("SIGTERM", shutdown);
  if (SECS) setTimeout(shutdown, SECS * 1000);
}

main().catch((e) => {
  console.error("scrub-jam failed:", e);
  process.exit(1);
});
