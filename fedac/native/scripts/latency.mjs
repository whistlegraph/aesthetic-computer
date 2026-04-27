#!/usr/bin/env node
// latency.mjs — ac-native audio jitter / latency floor measurement
//
// Drives the AC_LATENCY_BENCH=1 mode added in audio.c. The audio thread
// emits one "[ac-latency] ..." line per ~1024 periods (≈1s of audio
// at the 1ms HDA period, ≈20s at the 20ms SOF period). This script
// parses those lines and reports the empirical audio-side latency
// floor — the period interval the kernel actually delivers, plus the
// jitter on it.
//
// What the floor means:
//   period_us = 1000 / sample_rate * period_frames  (ALSA configured)
//   min       = best observed period delivery time   (idle floor)
//   p50/mean  = typical period delivery time         (steady-state)
//   p99/max   = worst-case delivery time             (jitter ceiling)
//   over_period = count of periods that took >1.5×   (audible drift)
//   xruns     = total ALSA underruns since boot      (audible glitches)
//
// Total key-to-DAC latency, per the latency paper:
//   key→evdev (~100µs) + dispatch (~100µs) + ALSA period (this number)
//   + DMA turnaround (codec-specific, ~1ms HDA / ~80ms SOF firmware)
//
// Usage:
//   # 1. Spawn a built ac-native binary directly (e.g. on a dev Linux
//   #    box with ALSA, or under QEMU). Best for A/B comparing C
//   #    changes against a baseline:
//   node latency.mjs --bin /path/to/ac-native [--seconds 30]
//
//   # 2. Tail journalctl on a running AC OS device. AC_LATENCY_BENCH=1
//   #    must be set in the kernel cmdline or unit env. Pipe the
//   #    matching log stream into stdin:
//   journalctl -f -o cat -u ac-native | node latency.mjs --tail
//   ssh ac-os-device 'journalctl -f -o cat' | node latency.mjs --tail
//
//   # 3. Read a static log file (offline analysis):
//   node latency.mjs --file /tmp/ac-native.log
//
// Output:
//   Streaming per-report stats as they arrive, plus a final summary
//   when the stream ends or --seconds elapses.

import { spawn } from "node:child_process";
import { createReadStream } from "node:fs";
import { createInterface } from "node:readline";

const args = process.argv.slice(2);
const opt = (name, fallback = null) => {
  const i = args.indexOf(name);
  return i >= 0 ? args[i + 1] : fallback;
};
const has = (name) => args.includes(name);

if (has("--help") || has("-h")) {
  process.stdout.write(
    `latency.mjs — ac-native audio jitter / latency floor measurement\n\n` +
      `  --bin <path>     spawn ac-native binary, capture stderr\n` +
      `  --tail           parse [ac-latency] lines from stdin\n` +
      `  --file <path>    parse [ac-latency] lines from a log file\n` +
      `  --seconds <n>    auto-stop spawn mode after n seconds (default: 30)\n` +
      `  --json           emit per-report records as JSON to stdout\n` +
      `  --help           show this message\n`
  );
  process.exit(0);
}

const mode = has("--tail") ? "tail" : opt("--file") ? "file" : opt("--bin") ? "spawn" : null;
if (!mode) {
  process.stderr.write("latency.mjs: choose --bin, --tail, or --file (see --help)\n");
  process.exit(2);
}

const seconds = parseInt(opt("--seconds", "30"), 10);
const jsonOut = has("--json");

// Rolling aggregation across all reports in this run.
const all = {
  reports: 0,
  period_us: null,
  min: Infinity,
  max: 0,
  meanSum: 0, // sum of per-report means
  p50Sum: 0,
  p99Sum: 0,
  overPeriodTotal: 0,
  xrunsLast: 0,
  xrunsFirst: null,
  samplesTotal: 0,
};

// One-line ac-latency report parser.
const RE = /^\[ac-latency\]\s+period_us=(\d+)\s+n=(\d+)\s+min=(\d+)\s+p50=(\d+)\s+mean=(\d+)\s+p99=(\d+)\s+max=(\d+)\s+over_period=(\d+)\s+xruns=(\d+)/;

function parseLine(line) {
  const m = RE.exec(line);
  if (!m) return null;
  return {
    period_us: +m[1],
    n: +m[2],
    min: +m[3],
    p50: +m[4],
    mean: +m[5],
    p99: +m[6],
    max: +m[7],
    over_period: +m[8],
    xruns: +m[9],
  };
}

function fmt(us) {
  if (us < 1000) return `${us}µs`;
  return `${(us / 1000).toFixed(2)}ms`;
}

function onReport(r) {
  all.reports++;
  if (all.period_us == null) all.period_us = r.period_us;
  if (r.min < all.min) all.min = r.min;
  if (r.max > all.max) all.max = r.max;
  all.meanSum += r.mean;
  all.p50Sum += r.p50;
  all.p99Sum += r.p99;
  all.overPeriodTotal += r.over_period;
  if (all.xrunsFirst == null) all.xrunsFirst = r.xruns;
  all.xrunsLast = r.xruns;
  all.samplesTotal += r.n;

  if (jsonOut) {
    process.stdout.write(JSON.stringify(r) + "\n");
    return;
  }
  process.stdout.write(
    `[#${String(all.reports).padStart(3)}] ` +
      `period=${fmt(r.period_us)} ` +
      `min=${fmt(r.min)} ` +
      `p50=${fmt(r.p50)} ` +
      `mean=${fmt(r.mean)} ` +
      `p99=${fmt(r.p99)} ` +
      `max=${fmt(r.max)} ` +
      `over=${r.over_period}/${r.n} ` +
      `xruns=${r.xruns}\n`
  );
}

function summarize() {
  if (all.reports === 0) {
    process.stderr.write(
      "latency.mjs: no [ac-latency] lines seen — was AC_LATENCY_BENCH=1 set?\n"
    );
    process.exit(1);
  }
  const meanAvg = Math.round(all.meanSum / all.reports);
  const p50Avg = Math.round(all.p50Sum / all.reports);
  const p99Avg = Math.round(all.p99Sum / all.reports);
  const xrunsDelta = all.xrunsLast - (all.xrunsFirst ?? 0);
  const jitterMin = all.min - all.period_us;
  const jitterP50 = p50Avg - all.period_us;
  const jitterP99 = p99Avg - all.period_us;
  const jitterMax = all.max - all.period_us;

  process.stdout.write(
    "\n" +
      "═══ summary ═══════════════════════════════════════════════════════\n" +
      `reports         : ${all.reports} (${all.samplesTotal} samples total)\n` +
      `configured period: ${fmt(all.period_us)}\n` +
      `delivered min   : ${fmt(all.min)}   (jitter ${jitterMin >= 0 ? "+" : "-"}${fmt(Math.abs(jitterMin))})\n` +
      `delivered p50   : ${fmt(p50Avg)}   (jitter ${jitterP50 >= 0 ? "+" : "-"}${fmt(Math.abs(jitterP50))})\n` +
      `delivered mean  : ${fmt(meanAvg)}\n` +
      `delivered p99   : ${fmt(p99Avg)}   (jitter ${jitterP99 >= 0 ? "+" : "-"}${fmt(Math.abs(jitterP99))})\n` +
      `delivered max   : ${fmt(all.max)}   (jitter ${jitterMax >= 0 ? "+" : "-"}${fmt(Math.abs(jitterMax))})\n` +
      `over-period     : ${all.overPeriodTotal} periods >1.5× expected\n` +
      `xruns delta     : ${xrunsDelta} (during this run)\n` +
      "\n" +
      "estimated audio-side floor (period + DMA turnaround):\n" +
      `  HDA-direct    : ${fmt(all.period_us + 1000)} (1ms codec turnaround)\n` +
      `  SOF firmware  : ${fmt(all.period_us + 80000)} (80ms DAPM ceiling)\n` +
      "\n" +
      "key-to-DAC ≈ ~200µs (kbd+IRQ+dispatch) + audio-side floor + jitter ceiling\n" +
      "═══════════════════════════════════════════════════════════════════\n"
  );
}

async function readStream(stream) {
  const rl = createInterface({ input: stream, crlfDelay: Infinity });
  for await (const raw of rl) {
    // Strip ANSI + journalctl prefixes, find the [ac-latency] tag.
    const idx = raw.indexOf("[ac-latency]");
    if (idx < 0) continue;
    const r = parseLine(raw.slice(idx));
    if (r) onReport(r);
  }
}

async function runSpawn(binPath) {
  const env = { ...process.env, AC_LATENCY_BENCH: "1" };
  const child = spawn(binPath, [], { env, stdio: ["ignore", "inherit", "pipe"] });
  let timer = null;
  if (seconds > 0) {
    timer = setTimeout(() => {
      process.stderr.write(`latency.mjs: ${seconds}s elapsed — stopping ac-native\n`);
      child.kill("SIGTERM");
    }, seconds * 1000);
  }
  child.on("exit", () => clearTimeout(timer));
  await readStream(child.stderr);
}

(async () => {
  process.on("SIGINT", () => {
    summarize();
    process.exit(0);
  });

  if (mode === "spawn") {
    await runSpawn(opt("--bin"));
  } else if (mode === "file") {
    await readStream(createReadStream(opt("--file")));
  } else {
    await readStream(process.stdin);
  }
  summarize();
})().catch((err) => {
  process.stderr.write(`latency.mjs: ${err.message}\n`);
  process.exit(1);
});
