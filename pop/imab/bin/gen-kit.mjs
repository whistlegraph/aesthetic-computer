#!/usr/bin/env node
// gen-kit.mjs — the imab one-shot sample kit, rendered sample-free from
// the AC engines: the perc synth kit, bass-perc, the xylophone hook
// notes, and the click ticks. Manifest lands in samples/kit/kit.json.
//
//   node pop/imab/bin/gen-kit.mjs

import { renderMarimba } from "../../marimba/synths/marimba.mjs";
import { renderKick, renderBassPerc, renderSnare, renderHat, renderShaker,
         renderReverseBell, renderReverseKick } from "../../marimba/synths/perc.mjs";
import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const KIT = resolve(HERE, "../samples/kit");
mkdirSync(KIT, { recursive: true });
const SR = 48_000;
const BEAT = 60 / 124;

const NOTE = { 69:"A4",72:"C5",74:"D5",76:"E5",79:"G5",81:"A5",83:"B5",84:"C6",86:"D6",88:"E6",
               33:"A1",45:"A2",43:"G2",48:"C3",41:"F2" };
function tickBuf(freq) {
  const n = Math.floor(0.030 * SR), out = new Float32Array(n);
  for (let i = 0; i < n; i++) {
    const t = i / SR;
    out[i] = Math.tanh(1.6 * Math.sin(2 * Math.PI * freq * t) * Math.exp(-t / 0.005));
  }
  return out;
}

const manifest = { lane: "imab", sampleRate: SR, source: "AC engines (marimba.mjs, perc.mjs) — sample-free", samples: [] };
function save(name, buf, meta = {}) {
  let pk = 0; for (let i = 0; i < buf.length; i++) pk = Math.max(pk, Math.abs(buf[i]));
  if (pk > 0.95) for (let i = 0; i < buf.length; i++) buf[i] *= 0.95 / pk;
  const raw = resolve(KIT, `.${name}.raw`);
  writeFileSync(raw, Buffer.from(Float32Array.from(buf).buffer));
  const dest = resolve(KIT, `${name}.wav`);
  const ff = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
    "-f", "f32le", "-ar", String(SR), "-ac", "1", "-i", raw, "-c:a", "pcm_s16le", dest], { stdio: "inherit" });
  if (ff.status !== 0) process.exit(1);
  unlinkSync(raw);
  manifest.samples.push({ file: `${name}.wav`, ...meta });
  console.log(`✓ ${name}.wav`);
}

save("kick", renderKick({ gain: 0.9 }, { sampleRate: SR }), { engine: "renderKick", params: "defaults" });
save("kick-deep", renderKick({ gain: 0.9, fEnd: 40, ampDecay: 0.5 }, { sampleRate: SR }), { engine: "renderKick", params: "fEnd 40, ampDecay 0.5" });
save("snare", renderSnare({ gain: 0.85 }, { sampleRate: SR }), { engine: "renderSnare", params: "defaults" });
save("hat-closed", renderHat({ gain: 0.8 }, { sampleRate: SR }), { engine: "renderHat", params: "closed" });
save("hat-open", renderHat({ gain: 0.8, open: true }, { sampleRate: SR }), { engine: "renderHat", params: "open" });
save("shaker", renderShaker({ gain: 0.8 }, { sampleRate: SR }), { engine: "renderShaker", params: "defaults" });
save("reverse-bell", renderReverseBell({ gain: 0.6 }, { sampleRate: SR }), { engine: "renderReverseBell", params: "defaults" });
save("reverse-kick", renderReverseKick({ gain: 0.8 }, { sampleRate: SR }), { engine: "renderReverseKick", params: "defaults" });
for (const midi of [33, 45]) save(`bassperc-${NOTE[midi]}`,
  renderBassPerc({ midi, gain: 0.85, durSec: BEAT }, { sampleRate: SR }),
  { engine: "renderBassPerc", midi, note: NOTE[midi] });
for (const midi of [69, 72, 74, 76, 79, 81, 83, 84, 86, 88]) save(`xylo-${NOTE[midi]}`,
  renderMarimba({ midi, gain: 0.85, durSec: BEAT, preset: "xylophone", decayMul: 1.1 }, { sampleRate: SR }),
  { engine: "renderMarimba", preset: "xylophone", midi, note: NOTE[midi] });
for (const midi of [45, 43, 48, 41]) save(`bass-${NOTE[midi]}`,
  renderMarimba({ midi, gain: 0.85, durSec: 2 * BEAT, preset: "bass", decayMul: 0.8 }, { sampleRate: SR }),
  { engine: "renderMarimba", preset: "bass", midi, note: NOTE[midi] });
save("click-hi", tickBuf(1700), { engine: "tick", freq: 1700, role: "beat-1 accent" });
save("click-lo", tickBuf(1100), { engine: "tick", freq: 1100, role: "sub-beat" });
save("click-door", tickBuf(2600), { engine: "tick", freq: 2600, role: "8-bar phrase door" });

writeFileSync(resolve(KIT, "kit.json"), JSON.stringify(manifest, null, 2) + "\n");
console.log(`✓ kit.json (${manifest.samples.length} samples)`);
