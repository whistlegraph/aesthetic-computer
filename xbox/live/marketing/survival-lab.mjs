#!/usr/bin/env node
// survival-lab.mjs — how good is the climb bot, and why did it die?
//
// The reel factory spends about two minutes turning one survival run into a
// video. That is far too slow to ask "would a shorter jump cooldown summit?"
// even once. This runs the same simulation with the rendering taken out: no
// screenshots, no audio pass, no encode — just the fixed-step stepper and the
// completion envelope the game already files at the end of a climb.
//
// Two questions, one instrument:
//
//   node xbox/live/marketing/survival-lab.mjs                 # what does it do now?
//   node xbox/live/marketing/survival-lab.mjs --runs 5
//   node xbox/live/marketing/survival-lab.mjs --tune jumpCooldownUs=460000
//   node xbox/live/marketing/survival-lab.mjs --sweep jumpCooldownUs=580000,520000,460000,400000
//
// `--sweep` prices one knob across values and prints a summit rate for each.
// That table is the whole point: a bot change ships because a number moved,
// not because a run looked better.

import { existsSync, realpathSync } from "node:fs";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { serveShell, repo, survivalLadder, survivalLevelFor } from "./shell.mjs";
import { seed32 } from "./source.mjs";

const argv = process.argv.slice(2);
const flags = {};
for (let at = 0; at < argv.length; at++) {
  if (!argv[at].startsWith("--")) continue;
  const next = argv[at + 1];
  flags[argv[at].slice(2)] = next && !next.startsWith("--") ? (at++, next) : true;
}

const chrome = [
  process.env.PUPPETEER_EXECUTABLE_PATH,
  "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
  "/Applications/Google Chrome Canary.app/Contents/MacOS/Google Chrome Canary",
].find((path) => path && existsSync(path));
if (!chrome) throw new Error("no Chrome found — set PUPPETEER_EXECUTABLE_PATH");

async function loadPuppeteer() {
  const dir = [`${repo}/node_modules/puppeteer`, `${repo}/oven/node_modules/puppeteer`,
    "/opt/oven/node_modules/puppeteer"].find((path) => existsSync(path));
  if (!dir) throw new Error("puppeteer not found");
  return (await import(`${dir}/lib/esm/puppeteer/puppeteer.js`)).default;
}

// The envelope reports height in world units; "died two decks from the top" is
// the sentence a person can act on. `shell.mjs` owns the conversion.
const survivalLevelCount = survivalLadder.levels;
const levelFor = survivalLevelFor;

// Parse `a=1,b=2` into the tune object the game reads. Values are numbers —
// every knob the climb bot has is a number, and a silent NaN would poison a
// whole sweep with runs that look fine and mean nothing.
function parseTune(text) {
  const tune = {};
  if (!text || text === true) return tune;
  for (const pair of String(text).split(",")) {
    const [key, value] = pair.split("=");
    if (!key || value === undefined)
      throw new Error(`--tune wants key=value, got "${pair}"`);
    const number = Number(value);
    if (!Number.isFinite(number))
      throw new Error(`--tune ${key} needs a number, got "${value}"`);
    tune[key.trim()] = number;
  }
  return tune;
}

// One climb. The page is torn down between runs: survival seeds its bot from
// the round clock, and a reload is the only thing that reliably moves it.
async function climb({ browser, shell, seed, tune, cap = 240, log }) {
  const page = await browser.newPage();
  try {
    // The sim does not care how big the canvas is, and a small one keeps this
    // affordable on an 8 GB box running other work.
    await page.setViewport({ width: 480, height: 854, deviceScaleFactor: 1 });
    page.on("pageerror", (error) => log(`  ⚠ page ${error.message.slice(0, 140)}`));
    await page.evaluateOnNewDocument((seedValue, tuneValue) => {
      globalThis.__oskiewarSurvivalTune = tuneValue;
      let state = seedValue >>> 0;
      Math.random = () => {
        state = (state + 0x6d2b79f5) >>> 0;
        let value = state;
        value = Math.imul(value ^ (value >>> 15), value | 1);
        value ^= value + Math.imul(value ^ (value >>> 7), value | 61);
        return ((value ^ (value >>> 14)) >>> 0) / 4294967296;
      };
      globalThis.__oskiewarSelfPlay = true;
      // The same seam the replay oven uses: survival files one local
      // completion envelope so a fixed-step pass knows when the climb ended.
      globalThis.__oskiewarCaptureSurvival = true;
      globalThis.__oskiewarDenseReplay = true;
      globalThis.__oskiewarOfflineAudioEvents = [];
      const realFetch = globalThis.fetch;
      globalThis.fetch = function (input, init) {
        const address = String(input?.url || input);
        if (address.includes("/api/oskiewar-replays")) {
          const url = new URL(address);
          return realFetch(url.pathname + url.search, init);
        }
        return realFetch(input, init);
      };
      globalThis.WebSocket = function () {
        return { readyState: 3, send() {}, close() {},
          addEventListener() {}, removeEventListener() {} };
      };
    // The run's own seed drives the climb's variation unless a sweep is
    // explicitly pinning it, so `--runs 5` is five different climbs rather
    // than the same one measured five times.
    }, seed32(seed), { seed: seed32(seed), ...tune });

    // No `reel-hud`: the dress costs draw time and this pass is never seen.
    await page.goto(`${shell.origin}/?social-preview&replay-oven&offline-render`,
      { waitUntil: "domcontentloaded", timeout: 45000 });
    await page.waitForFunction(() => globalThis.__oskiewarOfflineReady === true,
      { timeout: 15000 });

    const limit = Math.max(1, Math.ceil(cap * 60));
    const before = shell.demos.length;
    // Stepping in browser-sized chunks rather than one round trip per tick is
    // the whole speed-up — a tick is cheap, the await around it is not.
    for (let index = 0; index < limit && shell.demos.length === before; index += 120)
      await page.evaluate((steps) => {
        for (let step = 0; step < steps; step++) globalThis.__oskiewarOfflineStep();
      }, Math.min(120, limit - index));
    if (shell.demos.length === before)
      throw new Error(`climb never ended inside the ${cap}s cap`);

    const demo = shell.replayBodies.get(shell.demos.at(-1).roundName);
    const events = await page.evaluate(() =>
      globalThis.__oskiewarOfflineAudioEvents || []);
    const jumps = events.filter((cue) => cue.event === "jump").map((cue) => cue.at);
    // A bot that has settled into one jump per fixed interval is climbing at a
    // constant rate. The lava is not constant — it accelerates with height —
    // so a metronome always loses eventually. The spread between consecutive
    // jumps is how that shows up in the data.
    const gaps = jumps.slice(1).map((at, index) => (at - jumps[index]) / 1000);
    gaps.sort((a, b) => a - b);
    const medianGap = gaps.length ? gaps[gaps.length >> 1] : null;
    return {
      cause: demo?.cause ?? null,
      height: Math.round(demo?.height ?? 0),
      level: levelFor(demo?.height ?? 0),
      seconds: +((demo?.durationTicks ?? 0) / 60).toFixed(1),
      jumps: jumps.length,
      medianGap: medianGap === null ? null : +medianGap.toFixed(3),
      // A tight spread means a metronome; a wide one means it is reacting.
      gapSpread: gaps.length > 2
        ? +(gaps.at(-1) - gaps[0]).toFixed(3) : null,
      round: demo?.roundName ?? null,
    };
  } finally {
    await page.close();
  }
}

function report(label, runs) {
  const summits = runs.filter((run) => run.cause === "SUMMIT").length;
  const heights = runs.map((run) => run.height).sort((a, b) => a - b);
  const median = heights[heights.length >> 1] ?? 0;
  const best = heights.at(-1) ?? 0;
  console.log(`\n${label}`);
  console.log(`  summit ${summits}/${runs.length}` +
    ` · median height ${median} (deck ${levelFor(median)}/${survivalLevelCount})` +
    ` · best ${best} (deck ${levelFor(best)})`);
  for (const run of runs)
    console.log(`    ${String(run.cause).padEnd(7)} deck ${String(run.level).padStart(2)}` +
      `/${survivalLevelCount} · ${String(run.height).padStart(5)} high` +
      ` · ${String(run.seconds).padStart(5)}s · ${String(run.jumps).padStart(3)} jumps` +
      ` · gap ${run.medianGap ?? "—"}s ±${run.gapSpread ?? "—"}`);
  return { summits, median, best, runs: runs.length };
}

// One shell and one browser, held open across many climbs. Standing them up
// costs more than a run does, so anything measuring more than a single ladder
// — a sweep here, the auto-tuner next door — should borrow this rather than
// shelling out per run.
export async function openLab({ log = () => {} } = {}) {
  const shell = await serveShell({ replays: "stub", log: () => {} });
  const puppeteer = await loadPuppeteer();
  const browser = await puppeteer.launch({
    headless: true, executablePath: chrome,
    args: ["--no-sandbox", "--mute-audio", "--autoplay-policy=no-user-gesture-required",
      "--disable-background-timer-throttling"],
  });
  return {
    climb: (options) => climb({ browser, shell, log, ...options }),
    close: async () => {
      await browser.close();
      await shell.close?.();
    },
  };
}

export { levelFor, survivalLevelCount };

// Everything below is the command line. Imported, this module is just the lab.
//
// `import.meta.url` reports the REAL path while `argv[1]` keeps whatever the
// caller typed, so comparing them raw silently decides "imported" whenever any
// path component is a symlink — /tmp against /private/tmp on a Mac, or a
// worktree reached through a linked directory. The script then exits 0 having
// printed nothing, which reads exactly like a passing run. Ask node when it can
// answer, and compare real paths when it cannot.
const invoked = typeof import.meta.main === "boolean" ? import.meta.main : (() => {
  try {
    return Boolean(process.argv[1]) &&
      realpathSync(process.argv[1]) === realpathSync(fileURLToPath(import.meta.url));
  } catch { return false; }
})();
if (invoked) {
const log = console.log;
const lab = await openLab({ log });
const climbOne = (options) => lab.climb(options);
let failed = false;

try {
  const runs = Math.max(1, Number(flags.runs || 3));
  const cap = Number(flags.cap || 240);
  const seeds = Array.from({ length: runs }, (_, index) =>
    flags.seed ? `${flags.seed}#${index}` : `lab#${index}`);

  if (flags.sweep) {
    // One knob, several values, everything else held. The sweep is the only
    // honest way to claim a tune is better: same seeds down every column.
    const [key, list] = String(flags.sweep).split("=");
    if (!list) throw new Error("--sweep wants key=v1,v2,v3");
    const base = parseTune(flags.tune);
    const table = [];
    for (const raw of list.split(",")) {
      const value = Number(raw);
      if (!Number.isFinite(value))
        throw new Error(`--sweep ${key} needs numbers, got "${raw}"`);
      const tune = { ...base, [key]: value };
      const results = [];
      for (const seed of seeds)
        results.push(await climbOne({ seed, tune, cap }));
      table.push({ value, ...report(`${key}=${value}`, results) });
    }
    console.log(`\n── ${key} ──`);
    for (const row of table)
      console.log(`  ${String(row.value).padStart(8)} → summit ${row.summits}/${row.runs}` +
        ` · median ${row.median} · best ${row.best}`);
    const winner = table.slice().sort((a, b) =>
      b.summits - a.summits || b.median - a.median)[0];
    console.log(`\n  best: ${key}=${winner.value}` +
      ` (${winner.summits}/${winner.runs} summits, median ${winner.median})`);
  } else {
    const tune = parseTune(flags.tune);
    if (Object.keys(tune).length) console.log(`tune ${JSON.stringify(tune)}`);
    const results = [];
    for (const seed of seeds)
      results.push(await climbOne({ seed, tune, cap }));
    report(Object.keys(tune).length ? "tuned" : "shipping bot", results);
  }
} catch (error) {
  // `process.exit(0)` in the finally used to run before a rejection could be
  // reported, so a lab that never got the page open printed nothing and exited
  // 0 — a broken run and a clean run were the same output. Say what went wrong
  // and exit non-zero, because a sweep is only worth anything if its silence
  // means "no summits", not "no simulation".
  failed = true;
  console.error(`✗ ${error?.stack || error?.message || error}`);
} finally {
  await lab.close();
  // Chrome and the shell can both leave handles behind; exit explicitly rather
  // than waiting on the loop to drain.
  process.exit(failed ? 1 : 0);
}
}
