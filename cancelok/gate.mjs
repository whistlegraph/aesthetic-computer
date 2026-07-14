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

// The thresholds are pad-doctor's own, restated as a pass/fail because a gate
// has to actually decide. 40fps is the floor the shipped pads clear on native;
// a 40MB heap climb in 12 seconds is the signature of a runaway allocation.
const MIN_FPS = 40;
const MAX_HEAP_MB = 40;

export function gate(pad, secs = 12) {
  let out = "";
  try {
    out = execFileSync(
      "node",
      [DOCTOR, pad, "--base", BASE, "--secs", String(secs)],
      { encoding: "utf8", cwd: REPO, timeout: (secs + 60) * 1000 },
    );
  } catch (e) {
    return { pass: false, why: "the probe itself died — the pad likely hung the page", out: String(e.message).slice(0, 200) };
  }

  const fps = parseFloat(/fps min=([\d.]+)/.exec(out)?.[1] ?? "0");
  const heap = parseFloat(/\(([+-][\d.]+)MB\//.exec(out)?.[1] ?? "0");
  const errs = parseInt(/JS errors[^:]*:\s*(\d+)/.exec(out)?.[1] ?? "0", 10);
  const ran = /\[pads:fps\]|fps min=/.test(out) && fps > 0;

  const why = [];
  if (!ran) why.push("it never rendered a frame");
  if (ran && fps < MIN_FPS) why.push(`${fps}fps — under the ${MIN_FPS} floor`);
  if (heap > MAX_HEAP_MB) why.push(`heap grew ${heap}MB — a leak`);
  if (errs > 0) why.push(`${errs} JS error(s)`);

  return { pass: why.length === 0, why: why.join("; "), fps, heap, errs, out: out.trim() };
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
