// ac-harness, 2026.05.18
// A small reusable model for driving Aesthetic Computer in a real local
// Chrome (puppeteer) — types, presses keys, drags, screenshots, and reads
// prompt state via the window.__acPromptTest hook. Mirrors the launch
// conventions already used in tests/performance/chrome-devtools-test.mjs.
//
// Env:
//   AC_TEST_URL   base URL (default production https://aesthetic.computer)
//   AC_HEADED=1   show the browser (default headless)
//   AC_SLOWMO     ms slow-mo between input ops (default 0)
//   AC_SHOT_DIR   screenshot output dir (default tests/browser/__screens__)
//
// The AC prompt renders to <canvas>, so DOM queries don't work — assertions
// read window.__acPromptTest() (present only when the page was booted with
// window.acDEBUG, which this harness sets before navigation). When the hook
// is absent (e.g. current production, before this branch ships) scenarios
// degrade to screenshot-only smoke and `state()` returns null.

import puppeteer from "puppeteer";
import { mkdirSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));

export const CONFIG = {
  baseURL: process.env.AC_TEST_URL || "https://aesthetic.computer",
  headed: process.env.AC_HEADED === "1",
  slowMo: parseInt(process.env.AC_SLOWMO || "0", 10) || 0,
  shotDir: process.env.AC_SHOT_DIR || join(HERE, "__screens__"),
};

export class ACSession {
  browser = null;
  page = null;
  shots = [];

  // One Chrome instance only (this laptop is 8 GB — never run parallel).
  static async open() {
    const s = new ACSession();
    mkdirSync(CONFIG.shotDir, { recursive: true });
    s.browser = await puppeteer.launch({
      headless: !CONFIG.headed,
      slowMo: CONFIG.slowMo,
      args: [
        "--no-sandbox",
        "--disable-setuid-sandbox",
        "--ignore-certificate-errors", // local https://localhost:8888
        "--disable-web-security",
        "--window-size=900,1200",
      ],
    });
    s.page = await s.browser.newPage();
    await s.page.setViewport({ width: 900, height: 1200 });
    // Surface the test hook at boot (prompt.mjs gates it on acDEBUG).
    await s.page.evaluateOnNewDocument(() => {
      window.acDEBUG = true;
    });
    s.page.on("pageerror", (e) => console.warn("  ⚠️  pageerror:", e.message));
    return s;
  }

  // Navigate to a piece (default `prompt`) and wait for boot.
  async boot(piece = "prompt") {
    const url = `${CONFIG.baseURL}/${piece}`;
    console.log(`  → ${url}`);
    await this.page.goto(url, { waitUntil: "networkidle2", timeout: 45000 });
    // Prefer the prompt test hook; fall back to the boot marker; then a
    // fixed settle so the canvas has painted.
    try {
      await this.page.waitForFunction(
        () =>
          typeof window.__acPromptTest === "function" ||
          window.acBOOT_START_TIME,
        { timeout: 20000 },
      );
    } catch {
      console.warn("  ⚠️  no boot/test marker — continuing on a timer");
    }
    await this.wait(2500);
    return this;
  }

  wait(ms) {
    return new Promise((r) => setTimeout(r, ms));
  }

  // AC captures keys globally, but a tap focuses/activates the keyboard
  // (mobile-style). Click low-center where the prompt input sits.
  async focusPrompt() {
    const vp = this.page.viewport();
    await this.page.mouse.click(vp.width / 2, Math.round(vp.height * 0.82));
    await this.wait(400);
    return this;
  }

  async type(text, { delay = 45 } = {}) {
    await this.page.keyboard.type(text, { delay });
    return this;
  }

  async press(key) {
    await this.page.keyboard.press(key);
    await this.wait(120);
    return this;
  }

  async clearInput() {
    // Select-all + delete works whether or not text is present.
    await this.page.keyboard.down("Meta");
    await this.page.keyboard.press("KeyA");
    await this.page.keyboard.up("Meta");
    await this.page.keyboard.press("Backspace");
    await this.wait(150);
    return this;
  }

  // Vertical drag (rolodex history scrub). x defaults to mid-screen.
  async dragVertical({ fromY, toY, x, steps = 24 }) {
    const vp = this.page.viewport();
    const px = x ?? vp.width / 2;
    await this.page.mouse.move(px, fromY);
    await this.page.mouse.down();
    for (let i = 1; i <= steps; i++) {
      await this.page.mouse.move(px, fromY + ((toY - fromY) * i) / steps);
      await this.wait(12);
    }
    await this.page.mouse.up();
    await this.wait(300);
    return this;
  }

  // Wait out the autocomplete debounce (250ms) + best-effort network.
  async settleSearch(ms = 1400) {
    await this.wait(ms);
    return this;
  }

  // Prompt state via the window hook, or null if the hook is absent.
  async state() {
    return await this.page.evaluate(() =>
      typeof window.__acPromptTest === "function"
        ? window.__acPromptTest()
        : null,
    );
  }

  async shot(name) {
    const path = join(CONFIG.shotDir, `${name}.png`);
    await this.page.screenshot({ path });
    this.shots.push(path);
    console.log(`  📸 ${path}`);
    return path;
  }

  async close() {
    if (this.browser) await this.browser.close();
  }
}

// ─── tiny dependency-free scenario runner ─────────────────────────────────
const results = [];

export async function scenario(name, fn) {
  const checks = [];
  const expect = (cond, msg) => {
    checks.push({ ok: !!cond, msg });
    console.log(`  ${cond ? "✓" : "✗"} ${msg}`);
  };
  process.stdout.write(`\n▶ ${name}\n`);
  let error = null;
  try {
    await fn(expect);
  } catch (e) {
    error = e;
    console.error("  💥", e.message);
  }
  const failed = checks.filter((c) => !c.ok).length;
  results.push({ name, total: checks.length, failed, error });
}

export function report() {
  console.log("\n──────── summary ────────");
  let bad = 0;
  for (const r of results) {
    const status = r.error
      ? "ERROR"
      : r.failed > 0
        ? `FAIL ${r.failed}/${r.total}`
        : `ok ${r.total}`;
    if (r.error || r.failed > 0) bad++;
    console.log(`  ${r.error || r.failed ? "✗" : "✓"} ${r.name} — ${status}`);
  }
  console.log(`\n${bad === 0 ? "✅ all green" : `❌ ${bad} scenario(s) bad`}`);
  return bad === 0 ? 0 : 1;
}
