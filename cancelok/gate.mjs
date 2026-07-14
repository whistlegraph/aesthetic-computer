// gate.mjs — the machine's half of the judgment.
//
// Correctness is not a human problem. Before a candidate is allowed to cost
// @jeffrey three seconds of attention, it has to prove it runs: real fps, no
// leak, no thrown errors. Everything this gate can answer, it answers, so the
// only question left at the top of the stack is the one no script can answer —
// do you want it.
//
// It's pad-doctor underneath (marketing/av-reels/bin/pad-doctor.mjs), which is
// the same probe the 47 shipped pads were certified with. We don't re-implement
// the measurement; we just read its verdict.
//
//   node cancelok/gate.mjs <pad>              # needs serve-local on :8899

import { execFileSync } from "node:child_process";
import { resolve } from "node:path";
import { REPO } from "./taste.mjs";

const DOCTOR = resolve(REPO, "marketing/av-reels/bin/pad-doctor.mjs");
const BASE = process.env.CANCELOK_BASE || "http://localhost:8899";

// An ABSOLUTE fps floor is a trap. Headless Chrome pins the render cadence to
// whatever the host feels like — on this Mac every pad reads exactly 30fps,
// including all 47 shipped ones. A floor of 40 would reject `prism`. A floor of
// 25 would pass a pad that's genuinely half-speed on a machine that can do 60.
// Neither floor means anything, because the number isn't about the pad.
//
// So we measure a KNOWN-GOOD pad in the same session and grade against it. `lull`
// is the reference: shipped, certified, cheap. Whatever the host is doing to
// lull, it's doing to the candidate too, and the ratio survives it.
// And heap growth over a 12-second window is NOISE. Measured twice, the same pad
// swings from +11MB to +42MB — that's the garbage collector's mood, not a leak.
// `lull`, shipped and certified, reads +40MB on a bad run. So a single bad
// measurement means nothing, and the gate CONFIRMS before it convicts: anything
// that fails gets measured again, and only a failure that REPEATS is real.
// A leak repeats. A GC hiccup doesn't.
const REFERENCE = "lull";
const FPS_RATIO = 0.85; // a candidate may be 15% slower than a shipped pad
const MAX_HEAP_MB = 60; // above the observed noise band of known-good pads
const MAX_WORK_MS = 14; // onPaint work — host-independent, unlike cadence

let baseline = null; // measured once per process

function measure(pad, secs) {
  let out = "";
  try {
    out = execFileSync("node", [DOCTOR, pad, "--base", BASE, "--secs", String(secs)], {
      encoding: "utf8",
      cwd: REPO,
      timeout: (secs + 60) * 1000,
    });
  } catch (e) {
    return { dead: true, out: String(e.message).slice(0, 200) };
  }
  return {
    fps: parseFloat(/fps min=([\d.]+)/.exec(out)?.[1] ?? "0"),
    heap: parseFloat(/\(([+-][\d.]+)MB\//.exec(out)?.[1] ?? "0"),
    work: parseFloat(/work=([\d.]+)ms/.exec(out)?.[1] ?? "0"),
    errs: parseInt(/JS errors[^:]*:\s*(\d+)/.exec(out)?.[1] ?? "0", 10),
    out: out.trim(),
  };
}

// Judge one measurement. Returns the complaints, or [] if it's clean.
function complaints(m, floor) {
  const why = [];
  if (!(m.fps > 0)) why.push("it never rendered a frame");
  else if (m.fps < floor)
    why.push(`${m.fps}fps vs ${baseline.fps} for ${REFERENCE} — too slow for this host`);
  if (m.work > MAX_WORK_MS) why.push(`onPaint takes ${m.work}ms — over the frame budget`);
  if (m.heap > MAX_HEAP_MB) why.push(`heap grew ${m.heap}MB — a leak`);
  if (m.errs > 0) why.push(`${m.errs} JS error(s)`);
  return why;
}

export function gate(pad, secs = 12) {
  if (!baseline) {
    baseline = measure(REFERENCE, secs);
    console.log(`📏 baseline (${REFERENCE}): ${baseline.fps}fps on this host`);
  }
  const floor = Math.max(1, baseline.fps * FPS_RATIO);

  let m = measure(pad, secs);
  if (m.dead) return { pass: false, why: "the probe died — it likely hung the page", ...m };

  // A thrown error is deterministic — believe it the first time. Everything else
  // is a measurement, and one measurement is a rumor.
  let why = complaints(m, floor);
  if (why.length && m.errs === 0) {
    console.log(`   …${pad} failed on "${why.join("; ")}" — measuring again to be sure`);
    const second = measure(pad, secs);
    if (second.dead) return { pass: false, why: "the probe died on retry", ...second };
    const again = complaints(second, floor);
    if (!again.length) {
      console.log(`   …clean on the second look. It was noise.`);
      return { pass: true, why: "", ...second };
    }
    m = second;
    why = again;
  }

  return { pass: why.length === 0, why: why.join("; "), ...m };
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const pad = process.argv[2];
  if (!pad) {
    console.error("usage: gate.mjs <pad>");
    process.exit(1);
  }
  const r = gate(pad);
  console.log(r.out);
  console.log(r.pass ? `\n✅ ${pad} passes the machine gate` : `\n❌ ${pad} rejected: ${r.why}`);
  process.exit(r.pass ? 0 : 1);
}
