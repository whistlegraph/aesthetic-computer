// master.mjs — spinging's finishing surface. The implementation lives in
// marketing/podcast/bin/master.mjs (high-pass + substrate EQ + glue
// compressor + 2-pass loudnorm + true-peak limiter) and is entangled with
// the podcast pipeline (produce.mjs, kits.mjs import it there), so it is
// re-exported FROM its home rather than moved.
//
// Note the register difference: the podcast TARGET is spoken-word
// (-16 LUFS / -1.5 dBTP). Sung spinging output masters to MUSIC loudness:
// -14 LUFS / -1.0 dBTP — SING_TARGET below. Pass it explicitly until a
// target-aware master() lands upstream.
export { TARGET, measure, master } from "../../marketing/podcast/bin/master.mjs";

export const SING_TARGET = { I: -14, TP: -1.0, LRA: 11 };
