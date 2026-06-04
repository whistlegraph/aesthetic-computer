#!/usr/bin/env node
// bills/bin/build-pdf.mjs — render the paper-format billing statement to PDF
// via headless Chrome's --print-to-pdf, without leaving orphan Chromes behind
// (same reaping + hard-timeout pattern as toolchain/macos/chrome-shot.mjs).
//
// Input:  bills/pdf/statement.html  (+ optional bills/out/bills-illy.png)
// Output: bills/out/aesthetic-computer-bills-<date>.pdf
//
// Usage:
//   node bills/bin/build-pdf.mjs [--date YYYY-MM-DD] [--out PATH] [--copy DIR]
//     --date   date stamp in the filename (default: from statement, else today via arg)
//     --out    explicit output path
//     --copy   also copy the finished PDF into this directory (e.g. ~/Desktop)

import { spawn, spawnSync } from "node:child_process";
import { mkdtempSync, rmSync, existsSync, statSync, readFileSync, writeFileSync, copyFileSync, mkdirSync } from "node:fs";
import { tmpdir } from "node:os";
import { join, resolve, dirname, basename } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");           // bills/
const REPO = resolve(LANE, "..");

const af = (k, d) => { const i = process.argv.indexOf(k); return i >= 0 && process.argv[i + 1] ? process.argv[i + 1] : d; };
const DATE = af("--date", "2026-06-04");
// --src selects which document to render: "statement" (internal, default) or
// "share" (the donor-facing one-pager), or an explicit path.
const SRC_ARG = af("--src", "statement");
const SRC = SRC_ARG.includes("/") ? resolve(process.cwd(), SRC_ARG) : resolve(LANE, "pdf", `${SRC_ARG}.html`);
const DEFAULT_OUT = SRC_ARG === "share"
  ? `${LANE}/out/aesthetic-computer-support-${DATE}.pdf`
  : `${LANE}/out/aesthetic-computer-bills-${DATE}.pdf`;
const OUT = resolve(process.cwd(), af("--out", DEFAULT_OUT));
const COPY_DIR = af("--copy", null);
mkdirSync(dirname(OUT), { recursive: true });

const CHROME = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
if (!existsSync(CHROME)) { console.error(`✗ Google Chrome not at ${CHROME}`); process.exit(1); }
if (!existsSync(SRC)) { console.error(`✗ statement not found at ${SRC}`); process.exit(1); }

// If the illy isn't generated yet, strip the banner so the PDF stays clean.
const ILLY = resolve(LANE, "out", "bills-illy.png");
let html = readFileSync(SRC, "utf8");
let renderSrc = SRC;
if (!existsSync(ILLY)) {
  html = html.replace(/\s*<img class="illy"[^>]*>\s*/, "\n  ");
  renderSrc = resolve(LANE, "pdf", ".statement.tmp.html");
  writeFileSync(renderSrc, html);
  console.error("! no bills-illy.png yet — rendering without the banner");
}

reapStaleHeadless();

// Remove any prior output so the size-poller doesn't mistake the stale file
// for a freshly-rendered one and kill Chrome before it re-renders.
try { rmSync(OUT, { force: true }); } catch {}

const profile = mkdtempSync(join(tmpdir(), "bills-pdf-"));
const fileUrl = "file://" + renderSrc;
const args = [
  "--headless=new",
  "--no-sandbox",
  "--no-pdf-header-footer",
  `--user-data-dir=${profile}`,
  `--print-to-pdf=${OUT}`,
  fileUrl,
];

const WAIT = 30000;
const child = spawn(CHROME, args, { stdio: ["ignore", "ignore", "inherit"] });
let timedOut = false, captured = false, lastSize = -1, stableTicks = 0;

const killer = setTimeout(() => { timedOut = true; try { child.kill("SIGKILL"); } catch {} }, WAIT);
const poller = setInterval(() => {
  if (!existsSync(OUT)) return;
  let size = 0; try { size = statSync(OUT).size; } catch { return; }
  if (size === 0) return;
  if (size === lastSize) {
    if (++stableTicks >= 3) { captured = true; clearInterval(poller); try { child.kill("SIGKILL"); } catch {} }
  } else { lastSize = size; stableTicks = 0; }
}, 200);

child.on("exit", (code, signal) => {
  clearTimeout(killer); clearInterval(poller);
  try { rmSync(profile, { recursive: true, force: true }); } catch {}
  if (renderSrc !== SRC) { try { rmSync(renderSrc, { force: true }); } catch {} }

  if (!captured && timedOut) { console.error(`✗ chrome timed out after ${WAIT}ms`); process.exit(2); }
  if (!existsSync(OUT) || statSync(OUT).size === 0) {
    console.error(`✗ no PDF produced at ${OUT} (code ${code}, ${signal || "no signal"})`); process.exit(1);
  }
  const kb = (statSync(OUT).size / 1024).toFixed(0);
  console.log(`✓ ${OUT.replace(REPO + "/", "")} (${kb} KB)`);
  if (COPY_DIR) {
    const dest = join(COPY_DIR.replace(/^~/, process.env.HOME), basename(OUT));
    try { copyFileSync(OUT, dest); console.log(`✓ copied → ${dest}`); }
    catch (e) { console.error(`✗ copy to ${COPY_DIR} failed: ${e.message}`); }
  }
});

function reapStaleHeadless() {
  const pgrep = spawnSync("pgrep", ["-f", "Google Chrome.*--headless"], { encoding: "utf8" });
  const pids = (pgrep.stdout || "").trim().split(/\s+/).filter(Boolean);
  if (!pids.length) return;
  const ps = spawnSync("ps", ["-o", "pid=,etimes=", ...pids], { encoding: "utf8" });
  const stale = [];
  for (const line of (ps.stdout || "").trim().split("\n")) {
    const [pid, etimes] = line.trim().split(/\s+/);
    if (Number(etimes) > 120) stale.push(pid);
  }
  if (!stale.length) return;
  console.error(`! reaping ${stale.length} stale headless chrome(s): ${stale.join(", ")}`);
  spawnSync("kill", ["-9", ...stale]);
}
