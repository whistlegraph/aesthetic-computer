#!/usr/bin/env node
// bake-c.mjs — same finalize chain as pop/hellsine/bin/bake.mjs, but the
// engine step calls the C binary at pop/hellsine/c/hellsine instead of
// `node hellsine.mjs`. Output goes to a sibling .hellsine-c-MASTER.wav
// + hellsine-c.mp3 so both pipelines can co-exist for A/B.
//
// Usage:  node pop/hellsine/c/bake-c.mjs [outDir]
//         [-- <extra engine flags, e.g. --ultimate --bpm 186>]
//
// Pre-master (engine output) → final-drop tempo bump → dynamic envelope
// → loudnorm I=-14 / TP=-1.5 / LRA=11 → alimiter 0.88 → -1.2 dB makeup
// → 2.8 s out-fade → truncate at 162.0 s → 320k mp3.

import { spawnSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";
import { acdspAvailable, processWav, chain, eq, compressor } from "../../lib/master.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
let args = process.argv.slice(2);
// --acdsp: OFF by default (the DistroKid release used the legacy chain,
// and acdsp introduces a ~-0.098 DC offset that DistroKid doesn't have).
// Pass --acdsp explicitly for the 1176 character pass.
const USE_ACDSP = args.includes("--acdsp");
args = args.filter((a) => a !== "--acdsp" && a !== "--no-acdsp");
const sep = args.indexOf("--");
const engineExtra = sep >= 0 ? args.slice(sep + 1) : [];
const outDir = (sep >= 0 ? args.slice(0, sep) : args)[0] ||
  `${homedir()}/Documents/Working Desktop/hellsine`;

const ENGINE = `${REPO}/pop/hellsine/c/hellsine`;
const pre = `${outDir}/.hellsine-c-pre.wav`;
const tempoOut = `${outDir}/.hellsine-c-tempo.wav`;
const finalWav = `${outDir}/hellsine-c-MASTER.wav`;
const finalMp3 = `${outDir}/hellsine-c.mp3`;

const run = (cmd, a, label, opts = {}) => {
  console.log(`\n[bake-c] ${label}`);
  const r = spawnSync(cmd, a, { stdio: "inherit", ...opts });
  if (r.status !== 0) { console.error(`[bake-c] FAILED: ${label}`); process.exit(1); }
};

// 1 — C engine render. Default to --ultimate since that's what the JS
//     bake defaults to (`--strategy ultimate`).
const hasUltimate = engineExtra.some((a) => a === "--ultimate" || a === "--no-ultimate");
const engineArgs = hasUltimate
  ? engineExtra.filter((a) => a !== "--no-ultimate")
  : ["--ultimate", ...engineExtra];
// Engine resolves sample paths (pop/hellsine/samples/*) relative to cwd —
// must run from repo root or jeffrey/grenade/etc samples silently no-op.
run(ENGINE, ["--out", pre, ...engineArgs], "C engine render", { cwd: REPO });

// 2 — FINAL-DROP PERMA SPEED-UP (mirrors bake.mjs). Climax onward gets
//     atempo 1.06 so the second half tightens. Pre-AC-stamp untouched.
const TEMPO_CUT = 110.77;
const TEMPO_MUL = 1.06;
const tempoChain = [
  "asplit=2[a0][a1]",
  `[a0]atrim=start=0:end=${TEMPO_CUT},asetpts=PTS-STARTPTS[t0]`,
  `[a1]atrim=start=${TEMPO_CUT},asetpts=PTS-STARTPTS,atempo=${TEMPO_MUL}[t1]`,
  "[t0][t1]concat=n=2:v=0:a=1[out]",
].join(";");
// 32-bit float (not s24le) — acdsp needs float; s24le emits WAVE_FORMAT_EXTENSIBLE
// (format=65534) which acdsp's loader can't parse.
run("ffmpeg", ["-y", "-i", pre, "-filter_complex", tempoChain, "-map", "[out]",
  "-ar", "48000", "-c:a", "pcm_f32le", tempoOut],
  `final-drop perma speed-up (atempo=${TEMPO_MUL} from ${TEMPO_CUT}s onward)`);

// 3 — optional acdsp CHARACTER PASS (1176 4:1 with iron + EQ shaping).
// This is what gives the "pop master glue" — heavier than ffmpeg's
// gentle acompressor. Default ON; --no-acdsp uses the legacy chain.
let finalizeIn = tempoOut;
if (USE_ACDSP) {
  if (!acdspAvailable()) {
    console.error("[bake-c] --acdsp requested but pop/dsp/c/acdsp not built.");
    console.error("         build it: (cd pop/dsp/c && make)");
    process.exit(1);
  }
  const acOut = `${outDir}/.hellsine-c-acdsp.wav`;
  const spec = chain(
    eq({ type: "peak", f: 150, q: 1.0, g: -1 }),
    eq("mud", -2),
    compressor("1176", { ratio: 4, in: -3, out: +3, attack: 4, release: 4, iron: 0.5 }),
    eq("air", +2),
  );
  console.log(`\n[bake-c] acdsp character pass (1176 + EQ via C lib)\n         chain: ${spec}`);
  const r = processWav(tempoOut, acOut, spec, { float: true });
  if (!r.ok) { console.error("[bake-c] acdsp failed:\n" + r.stderr); process.exit(1); }
  process.stderr.write(r.stderr);
  finalizeIn = acOut;
}

// 4 — LOUDNESS NARRATIVE — pre-drop dips before BOTH drops so the
// drops earn their impact through contrast. Lead-ups intentionally
// QUIETER than the previous curve so the music breathes. Each drop
// jumps ~12+ dB from its anticipation low. (@jeffrey 2026-05-26
// "final post render / volume / intensity around the drops / so the
// lead up to the drops is a bit quieter / better overall loudness
// narrative that matches the music")
//   0.0  - 12.50 : 0.40 → 0.55 (hushed intro)
//   12.50- 15.82 : 0.55 → 0.30 (BREATH before 1st drop)
//   15.82- 17.20 : 0.30 → 1.15 (1st DROP — ~12 dB jump)
//   17.20- 47.5  : 1.15 → 0.78 (statement settling)
//   47.5 - 79.1  : 0.55 (bridge — intimate)
//   79.1 -100.0  : 0.55 → 0.65 (gentle climb)
//  100.0 -110.77 : 0.65 → 0.28 (DEEPER BREATH before 2nd drop)
//  110.77-115.0  : 0.28 → 1.40 (THE 2nd BIG JUMP — ~14 dB contrast)
//  115.0 -140.0  : 1.40 → 1.10 (climax sustain, slight settle)
//  140.0 -159.5  : 1.10 → 0.40 (coda taper)
const dynEnv =
  "volume=eval=frame:volume='" +
    "if(lt(t,12.50), 0.40+0.15*(t/12.50)," +
    "if(lt(t,15.82), 0.55+(0.30-0.55)*((t-12.50)/3.32)," +
    "if(lt(t,17.20), 0.30+(1.15-0.30)*((t-15.82)/1.38)," +
    "if(lt(t,47.5), 1.15+(0.78-1.15)*((t-17.20)/30.30)," +
    "if(lt(t,79.1), 0.55," +
    "if(lt(t,100.0), 0.55+0.10*((t-79.1)/20.9)," +
    "if(lt(t,110.77), 0.65+(0.28-0.65)*((t-100.0)/10.77)," +
    "if(lt(t,115.0), 0.28+(1.40-0.28)*((t-110.77)/4.23)," +
    "if(lt(t,140.0), 1.40+(1.10-1.40)*((t-115.0)/25.0)," +
    "1.10+(0.40-1.10)*((t-140.0)/19.5))))))))))'";

// With acdsp: skip the ffmpeg EQ + acompressor (acdsp already shaped).
// Without acdsp: legacy ffmpeg-only chain (highshelf + EQ + gentle comp).
// Legacy chain settings match the DistroKid release master (commit
// 495903bea, 2026-05-24): more conservative limiter (0.85 / 6ms / 110ms)
// + more aggressive but slower acompressor (-18dB / 2:1 / 15ms / 240ms).
const ffChain = USE_ACDSP
  ? [dynEnv,
     "loudnorm=I=-14:TP=-1.5:LRA=11",
     "alimiter=limit=0.85:attack=6:release=110:level=disabled",
     "volume=-1.2dB",
     // Fade pushed later (was st=145 d=17) to give the outro stack
     // (train @ 144 + bells @ 147-152 + meow @ 155 + flick @ 156.5)
     // full amplitude. Coda kicks are cut at t>=140 so the pop source
     // that motivated the earlier fade is gone.
     "afade=t=out:st=159.7:d=2.3:curve=hsin"].join(",")
  : [dynEnv,
     // Brightness lift: 9k +3 dB (was +2) + a 4 kHz presence push so the
     // top end carries the brass + cymbal-like SFX. Plus a tiny 12k air
     // sparkle (+1) for tape-style top.
     "highshelf=f=9000:g=3",
     "equalizer=f=4000:t=q:w=1.2:g=1",
     "highshelf=f=12000:g=1",
     "equalizer=f=150:t=q:w=1.0:g=-1",
     "equalizer=f=320:t=q:w=1.4:g=-2",
     // INDUSTRIAL squash — heavier compression: lower threshold (-24),
     // higher ratio (3:1), faster attack (8ms). The mix gets pushed
     // together hard. NIN/Skinny-Puppy mastering: dense, dynamic compression
     // wall, very little headroom between rms and peak.
     "acompressor=threshold=-24dB:ratio=3:attack=8:release=180:makeup=2:knee=6",
     "loudnorm=I=-14:TP=-1.5:LRA=11",
     "alimiter=limit=0.85:attack=6:release=110:level=disabled",
     "volume=-1.2dB",
     // Fade pushed later (was st=145 d=17) to give the outro stack
     // (train @ 144 + bells @ 147-152 + meow @ 155 + flick @ 156.5)
     // full amplitude. Coda kicks are cut at t>=140 so the pop source
     // that motivated the earlier fade is gone.
     "afade=t=out:st=159.7:d=2.3:curve=hsin"].join(",");

run("ffmpeg", ["-y", "-i", finalizeIn, "-af", ffChain,
  "-ar", "44100", "-sample_fmt", "s16",
  "-to", "162.0",
  finalWav], `finalize → -14 LUFS ${USE_ACDSP ? "[acdsp]" : "(legacy)"}`);

run("ffmpeg", ["-y", "-i", finalWav, "-codec:a", "libmp3lame", "-b:a", "320k",
  finalMp3], "320k mp3");

// Keep tempoOut around for per-stage A/B comparison (see hellsine-c-pre →
// hellsine-c-tempo → hellsine-c-MASTER → hellsine-c.mp3).
spawnSync("open", ["-a", "QuickTime Player", finalWav]);
console.log(`\n[bake-c] done → ${finalWav}`);
console.log(`[bake-c]      → ${finalMp3}`);
