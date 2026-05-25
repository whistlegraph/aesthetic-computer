// master.mjs — thin node-side helpers around pop/dsp/c/acdsp.
//
// The actual DSP lives in C (pop/dsp/c/) so the same code is portable to
// the browser runtime (wasm) and eventually AC Native. From node land we
// just (a) build the chain spec string, (b) shell out to the native CLI,
// (c) hand the resulting WAV back to ffmpeg for whatever encode/limit/
// loudnorm passes the lane still needs.
//
// Eventually loudnorm + limiter move into C too; for now ffmpeg handles
// the boundaries and acdsp owns the character (compressors + EQ).

import { spawnSync } from "node:child_process";
import { existsSync, readFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../..");
const BIN  = resolve(REPO, "pop/dsp/c/acdsp");
const EQ_GRAPH_PATH = resolve(REPO, "pop/dsp/eq-graph.json");

// Lazy: only read the JSON when someone asks (the C side has its own
// compiled-in mirror). Surfaces the graph to node callers that want to
// inspect intents, ranges, conflicts.
let _graph = null;
export function eqGraph() {
  if (!_graph) _graph = JSON.parse(readFileSync(EQ_GRAPH_PATH, "utf8"));
  return _graph;
}

export function acdspAvailable() {
  return existsSync(BIN);
}

// Build a `1176:...` spec from named knobs. Returns a string fragment.
//   compressor("1176", { ratio: 4, in: -12, out: +6, attack: 4, release: 4, iron: 0.5 })
export function compressor(model, opts = {}) {
  if (model !== "1176") throw new Error(`master: unknown comp model '${model}'`);
  const parts = ["1176"];
  for (const k of ["ratio", "in", "out", "attack", "release", "iron",
                   "attack-us", "release-ms", "threshold", "knee"]) {
    if (opts[k] !== undefined && opts[k] !== null) parts.push(`${k}=${opts[k]}`);
  }
  return parts.join(":");
}

// EQ stage builder.
//   eq("presence", +2)         → "eq:presence=+2"
//   eq("air")                  → "eq:air"           (uses gain_default from graph)
//   eq({ type: "peak", f: 3500, q: 1.2, g: 2 }) → raw biquad
//   eq({ type: "hp", f: 30 })
export function eq(intentOrSpec, gainDb) {
  if (typeof intentOrSpec === "string") {
    return gainDb === undefined ? `eq:${intentOrSpec}` : `eq:${intentOrSpec}=${gainDb}`;
  }
  const { type, f, q, g } = intentOrSpec;
  if (!type) throw new Error("master.eq: { type, f, q, g } required");
  const parts = [`eq:${type}`];
  if (f !== undefined) parts.push(`f=${f}`);
  if (q !== undefined) parts.push(`q=${q}`);
  if (g !== undefined) parts.push(`g=${g}`);
  return parts.join(":");
}

// Join stage fragments into a single chain spec.
export function chain(...stages) { return stages.filter(Boolean).join(" "); }

// Run a chain over a WAV file. Returns { ok, stderr, ms, realtime }.
//   await processWav("in.wav", "out.wav", chain(eq("sub"), compressor("1176", { ratio: 4, in: -10, out: +4 })))
export function processWav(inPath, outPath, chainSpec, opts = {}) {
  if (!existsSync(BIN)) {
    throw new Error(`master: acdsp binary missing at ${BIN}\n  build it with: (cd pop/dsp/c && make)`);
  }
  const args = [inPath, outPath, "--chain", chainSpec];
  if (opts.bits)     args.push("--bits", String(opts.bits));
  if (opts.float)    args.push("--float");
  if (opts.quiet)    args.push("--quiet");
  const t0 = process.hrtime.bigint();
  const r = spawnSync(BIN, args, { encoding: "utf8" });
  const ms = Number((process.hrtime.bigint() - t0) / 1000000n);
  return {
    ok:     r.status === 0,
    status: r.status,
    stderr: r.stderr || "",
    stdout: r.stdout || "",
    ms,
  };
}

// ── canonical chains (curated starting points; lanes override freely) ──
// Each is a function so lane scripts can A/B-swap without forking the lib.

export const presets = {
  // Bus-glue for trance/dance masters: cleanup, 1176 4:1 with mild iron
  // for upper-harmonic brightness, knowledge-graph mud/presence/air.
  danceMasterBus(opts = {}) {
    const { in_db = -3, out_db = +3, iron = 0.5 } = opts;
    return chain(
      eq("sub"),
      compressor("1176", { ratio: 4, in: in_db, out: out_db,
                           attack: 5, release: 3, iron }),
      eq("mud",      -1.5),
      eq("presence", +2),
      eq("air",      +1.5),
    );
  },

  // Vocal lead: faster attack (knob 6), heavier ratio (8:1), more iron.
  vocalLead(opts = {}) {
    const { in_db = +2, out_db = -2, iron = 0.6 } = opts;
    return chain(
      eq("rumble"),
      compressor("1176", { ratio: 8, in: in_db, out: out_db,
                           attack: 6, release: 4, iron }),
      eq("nasal",    -1.5),
      eq("presence", +2),
      eq("sibilance", -2),
      eq("air",       +1.5),
    );
  },

  // Drum bus: 4:1 with the famous "all-buttons" feel approximated by
  // ratio 20 + heavy iron. Use sparingly — this is character, not glue.
  drumSlam(opts = {}) {
    const { in_db = -2, out_db = +1, iron = 1.2 } = opts;
    return chain(
      compressor("1176", { ratio: 20, in: in_db, out: out_db,
                           attack: 6, release: 5, iron }),
      eq("bite", +1.5),
      eq("air",  +1),
    );
  },
};
