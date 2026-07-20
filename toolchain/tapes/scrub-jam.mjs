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
//
// Drive the AC Electron app (or any Chrome) instead of launching windows —
// start the app with a remote-debugging port, then attach:
//   ac-electron: launch with --remote-debugging-port=9333 (dev.fish already
//   exposes CDP for injection), then:
//   node toolchain/tapes/scrub-jam.mjs --cdp 9333 --tapes house
// One tape per attached window; the harness navigates each to the tape and
// freestyles it in-app. --prod/--base still choose which server the app loads.

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
const CDP_PORT = val("--cdp", null); // Attach to a running Chrome/Electron
const CDP_URL = val("--connect", null); // ...or a full ws:// endpoint
const TAPES = val("--tapes", "sine,house").split(",").map((s) => s.trim());
const SECS = parseFloat(val("--secs", "0")) || 0; // 0 = until Ctrl+C
const RELEASE_S = parseFloat(val("--release", "15")); // Hands-off per phrase
const DENSITY = val("--density", null); // AC pixel density (1 = chunky)
const SOLO = has("--solo"); // One performer messes at a time; others hold
let soloTurn = 0; // Whose turn it is (round-robin by window index)
const SHOTS = has("--shots");
const W = 720;
const H = 480;

const rnd = (a, b) => a + Math.random() * (b - a);
const pick = (arr) => arr[Math.floor(Math.random() * arr.length)];
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

// One performer per window. Holds a puppeteer page and drives it forever.
class Performer {
  constructor(page, tape, idx, size) {
    this.page = page;
    this.tape = tape;
    this.idx = idx;
    // Attached windows (Electron) size themselves — measure rather than
    // assume, or the dial hit box at `w - 32` lands off-window.
    this.w = size?.w || W;
    this.h = size?.h || H;
    this.cx = this.w / 2;
    this.cy = this.h / 2;
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

  // 👻 Virtual cursor: a visible in-page arrow that mirrors every synthetic
  // mouse move, so a human (or a `frame` capture) can SEE each driver's
  // hand. Machine-readable twin lives at window.__jamCursor {x,y,down}.
  async installCursor() {
    const color = CURSOR_COLORS[this.idx % CURSOR_COLORS.length];
    try {
      await this.page.evaluate(
        ({ label, color }) => {
          window.__jamCursorEl?.remove();
          const el = document.createElement("div");
          el.id = "jam-cursor";
          el.setAttribute("role", "img");
          el.setAttribute("aria-label", `jam-cursor-${label}`);
          el.style.cssText =
            "position:fixed;left:0;top:0;z-index:2147483647;pointer-events:none;" +
            `font:bold 10px monospace;color:${color};text-shadow:1px 1px 0 #000;`;
          el.innerHTML =
            `<svg width="15" height="15" viewBox="0 0 15 15">` +
            `<path d="M1 1 L1 11.5 L4.2 8.4 L6.3 13 L8.4 12 L6.3 7.4 L10.5 7 Z"` +
            ` fill="${color}" stroke="#000" stroke-width="1"/></svg>` +
            `<div style="margin-top:-3px">${label}</div>`;
          document.body.appendChild(el);
          window.__jamCursorEl = el;
          window.__jamCursor = { x: 0, y: 0, down: false };
          window.__jamCursorMove = (x, y, down) => {
            window.__jamCursor = { x, y, down };
            el.style.transform = `translate(${x}px,${y}px)`;
            el.style.filter = down ? "brightness(1.7) drop-shadow(0 0 3px #fff)" : "none";
          };
        },
        { label: this.tape, color },
      );
    } catch {}
  }

  // Mirror a pointer state onto the virtual cursor (fire-and-forget).
  cur(x, y, down = this.curDown) {
    this.curDown = down;
    this.curX = x;
    this.curY = y;
    this.page
      .evaluate((a) => window.__jamCursorMove?.(a.x, a.y, a.down), { x, y, down })
      .catch(() => {});
  }

  // Pointer wrappers: every synthetic move/press also moves the visible
  // cursor, so gestures are watchable and frame-capturable.
  async mmove(x, y, opts) {
    this.cur(x, y);
    await this.page.mouse.move(x, y, opts);
  }
  async mdown() {
    this.cur(this.curX ?? this.cx, this.curY ?? this.cy, true);
    await this.page.mouse.down();
  }
  async mup() {
    this.cur(this.curX ?? this.cx, this.curY ?? this.cy, false);
    await this.page.mouse.up();
  }

  // Unlock audio with a trusted click, then let the loop play a beat.
  async wake() {
    await this.installCursor();
    await this.mmove(this.cx, this.cy);
    this.cur(this.cx, this.cy, true);
    await this.page.mouse.click(this.cx, this.cy); // Trusted → unlocks AudioContext
    this.cur(this.cx, this.cy, false);
    await sleep(400);
  }

  async wheelBurst(deltaX, count, gap) {
    await this.mmove(this.cx, this.cy);
    for (let i = 0; i < count && !this.stopped; i++) {
      await this.page.mouse.wheel({ deltaX });
      await sleep(gap);
    }
  }

  // A rubber-band drag: grab, displace horizontally (rate), release.
  async dragScratch(dir, reach, hold) {
    await this.mmove(this.cx, this.cy);
    await this.mdown();
    const steps = 8;
    for (let i = 1; i <= steps && !this.stopped; i++) {
      await this.mmove(this.cx + dir * reach * (i / steps), this.cy);
      await sleep(hold / steps);
    }
    await this.mup();
  }

  // Back-and-forth drag scratch — the DJ wiggle.
  async wiggle(times) {
    await this.mmove(this.cx, this.cy);
    await this.mdown();
    for (let i = 0; i < times && !this.stopped; i++) {
      await this.mmove(this.cx + 120, this.cy, { steps: 4 });
      await sleep(90);
      await this.mmove(this.cx - 120, this.cy, { steps: 4 });
      await sleep(90);
    }
    await this.mmove(this.cx, this.cy, { steps: 3 });
    await this.mup();
  }

  // Flick: quick throw then release → wheel spins free, friction glides home.
  async flick(dir) {
    await this.mmove(this.cx, this.cy);
    await this.mdown();
    for (let i = 1; i <= 5 && !this.stopped; i++) {
      await this.mmove(this.cx + dir * 30 * i, this.cy);
      await sleep(12);
    }
    await this.mup();
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
    ];
    const [name, fn] = pick(moves);
    this.last.act = name;
    await fn();
  }

  // If a stray gesture navigated the pane off the tape (back to prompt),
  // steer it home before the next phrase — the jam must outlive misclicks.
  async ensureOnTape() {
    try {
      const url = this.page.url();
      if (!url.includes(`video~scrub~${this.tape}`)) {
        this.last.act = "re-enter tape";
        await this.page.goto(tapeURL(this.tape), {
          waitUntil: "domcontentloaded",
          timeout: 45000,
        });
        await sleep(3500);
        await this.wake();
      }
    } catch {}
  }

  // Freestyle forever: phrase → release (let net-time re-sync) → repeat.
  // In --solo mode performers take turns: exactly one messes with its
  // tape while the others hold the grid — stem-testing, one at a time.
  async run() {
    await this.wake();
    while (!this.stopped) {
      if (SOLO) {
        while (!this.stopped && soloTurn % TAPES.length !== this.idx) {
          this.last.act = "hold";
          await sleep(250);
        }
        if (this.stopped) break;
      }
      await this.ensureOnTape();
      await this.phrase();
      // Release: hands off, tape drifts back toward net-time unison.
      this.last.act += " → release";
      if (SOLO) soloTurn++; // Next player's turn while we re-lock.
      const releaseMs = rnd(RELEASE_S * 1000, RELEASE_S * 1400);
      const until = Date.now() + releaseMs;
      while (Date.now() < until && !this.stopped) await sleep(150);
    }
  }

  stop() {
    this.stopped = true;
  }
}

const windows = [];
let attached = null; // Shared browser when attaching over CDP.

async function getBrowser(idx) {
  if (CDP_PORT || CDP_URL) {
    // Attach to a running Chrome / AC Electron app (one shared browser).
    if (!attached) {
      attached = await puppeteer.connect(
        CDP_URL
          ? { browserWSEndpoint: CDP_URL, defaultViewport: null }
          : { browserURL: `http://127.0.0.1:${CDP_PORT}`, defaultViewport: null },
      );
    }
    return { browser: attached, launched: false };
  }
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
  return { browser, launched: true };
}

const claimed = new Set();

// Adopt one of the app's existing windows. Electron exposes its BrowserWindows
// as CDP page targets but cannot mint new ones (`newPage()` throws), so we
// claim a window rather than open one — preferring one already on this tape.
async function adoptPage(browser, url) {
  const pages = (await browser.pages()).filter(
    (p) => !claimed.has(p) && /^https?:/.test(p.url()),
  );
  if (!pages.length) {
    throw new Error(
      `no free window to drive — open one per tape first, e.g.\n` +
        `   slab/bin/slab-web "video~scrub~<tape>"`,
    );
  }
  // Match by tape token, not exact URL — boot canonicalizes query params.
  const tapeToken = url.match(/video~scrub~[^?]+/)?.[0];
  const page =
    pages.find((p) => tapeToken && p.url().includes(tapeToken)) || pages[0];
  claimed.add(page);
  return page;
}

// nogap always: the AC canvas reaches the window edge, so the tape's
// bottom progress bar sits on the true bottom of the pane.
const tapeURL = (tape) => {
  const q = ["nogap=true"];
  if (DENSITY) q.push(`density=${DENSITY}`);
  return `${BASE}/video~scrub~${tape}?${q.join("&")}`;
};

// Per-window cursor colors — sine yellow, house cyan, dub pink, break blue.
const CURSOR_COLORS = ["#ffdd00", "#00ffcc", "#ff66aa", "#66aaff"];

async function launchWindow(tape, idx) {
  const { browser, launched } = await getBrowser(idx);
  const url = tapeURL(tape);
  // Attached: claim an existing app window; launched: reuse the sole page.
  const page = launched ? (await browser.pages())[0] : await adoptPage(browser, url);
  // Only navigate if it isn't already there — reloading a window that is
  // already on the tape costs another boot + synth warmup for nothing.
  if (page.url() !== url) {
    await page.goto(url, { waitUntil: "domcontentloaded", timeout: 45000 });
    await sleep(3500); // Give boot + synth a moment.
  }
  await page.bringToFront().catch(() => {});
  const size = await page
    .evaluate(() => ({ w: innerWidth, h: innerHeight }))
    .catch(() => null);
  const perf = new Performer(page, tape, idx, size);
  windows.push({ browser, page, perf, tape, launched });
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
    const syncs = [];
    for (const w of windows) {
      const t = await w.perf.telemetry();
      const rate = t ? t.rate.toFixed(2) : "--";
      const sync = t ? `${t.syncMs >= 0 ? "+" : ""}${t.syncMs}ms` : "--";
      const lock = t?.locked ? "🟢" : "🟠";
      // ✊ = the piece still thinks a finger is down. A pinned rate with an
      // open hand means the drive latched, not that the gesture is ongoing.
      const hand = t?.scrubbing ? "✊" : "  ";
      // 🔇 = AudioContext suspended: the tape is MOVING but SILENT — the
      // exact "I see it shift but hear nothing" condition.
      const mute = t?.audio === "suspended" ? "🔇" : "";
      if (t && !t.scrubbing) syncs.push(t.syncMs);
      cells.push(
        `${w.tape.padEnd(6)} ${lock}${hand}${mute} ${sync.padStart(7)} ${String(rate).padStart(6)}x  ${w.perf.last.act}`,
      );
    }
    // 📐 Band alignment: all syncs measure against the same UTC grid, so
    // the spread between windows IS the band's phase disagreement. 🟢
    // under 120ms means every pane is inside the pocket together.
    let band = "";
    if (syncs.length >= 2) {
      const spread = Math.max(...syncs) - Math.min(...syncs);
      band = `  ‖ band Δ${spread}ms ${spread < 120 ? "🟢" : "🟠"}`;
    }
    const secs = ((Date.now() - start) / 1000).toFixed(0);
    process.stdout.write(`\r[${secs}s] ` + cells.join("  |  ") + band + "          \n");

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
        // Only tear down windows we launched. Attached windows are the
        // user's own app — we adopted them, so we leave them open.
        if (w.launched) await w.browser.close();
      } catch {}
    }
    if (attached) {
      try {
        attached.disconnect();
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
