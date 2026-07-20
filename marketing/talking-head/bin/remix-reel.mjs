// remix-reel.mjs — chopped-and-screwed audiovisual finish for a talking reel.
//
// Keeps the intelligible synced reel as its spine, then adds beat-locked vocal
// gates, slowed tape ghosts, heavier low end, RGB separation, temporal trails,
// noise and macroblock tears at the same authored events.
//
//   node remix-reel.mjs clean.mp4 --out remix.mp4

import { spawnSync } from "node:child_process";
import { existsSync } from "node:fs";
import { resolve } from "node:path";

const argv = process.argv.slice(2);
const flag = (n, d = null) => {
  const i = argv.indexOf(`--${n}`);
  return i >= 0 && argv[i + 1] ? argv[i + 1] : d;
};
const SRC = resolve(argv.find((a) => !a.startsWith("--")) || "");
const OUT = resolve(flag("out", "talking-head-remix.mp4"));
if (!existsSync(SRC)) {
  console.error("usage: remix-reel.mjs <reel.mp4> [--out remix.mp4]");
  process.exit(2);
}

// Phrase-end cuts. Short events chatter; the three longer events throw a
// syrupy slowed ghost forward under the clean vocal.
const chops = [5.10, 11.52, 19.72, 29.12, 33.40, 39.40, 46.00, 53.04, 59.16];
const screws = [19.72, 39.40, 53.04];
const between = (t, d) => `between(t,${t.toFixed(2)},${(t + d).toFixed(2)})`;
const any = (times, d) => times.map((t) => between(t, d)).join("+");

// Four fast transformer cuts per event. The dry never vanishes completely, so
// consonants remain legible and the remix reads as performance, not corruption.
const gate = chops.map((t) => {
  const q = `mod(t-${t.toFixed(2)},0.115)`;
  const smooth = `if(lt(${q},0.060),1,if(lt(${q},0.070),` +
    `0.59+0.41*cos(PI*(${q}-0.060)/0.010),if(lt(${q},0.109),0.18,` +
    `0.59-0.41*cos(PI*(${q}-0.109)/0.006))))`;
  return `if(${between(t, 0.46)},${smooth},`;
}).join("") + "1" + ")".repeat(chops.length);

const screwChains = screws.map((t, i) =>
  `[sg${i}]atrim=start=${Math.max(0, t - 0.18)}:duration=0.72,asetpts=PTS-STARTPTS,` +
  `asetrate=44100*${i === 1 ? "0.70" : "0.76"},aresample=44100,` +
  `lowpass=f=4200,volume=${i === 1 ? "0.54" : "0.44"},` +
  `adelay=${Math.round(t * 1000)}|${Math.round(t * 1000)}[ghost${i}]`,
).join(";");
const ghostInputs = screws.map((_, i) => `[ghost${i}]`).join("");
const adlibs = [
  { at: 18.95, pitch: 0.5, delay: 19850, name: "adLow", volume: 0.28 },
  { at: 38.55, pitch: 2.0, delay: 39600, name: "adHigh", volume: 0.22 },
  { at: 52.15, pitch: 1.4983, delay: 53400, name: "adFifth", volume: 0.25 },
];
const adlibChains = adlibs.map((a, i) => {
  const stretch = a.pitch >= 1.9 ? ",atempo=0.5,atempo=0.5" : a.pitch > 1 ? ",atempo=0.5" : "";
  return `[ad${i}]atrim=start=${a.at}:duration=0.88,asetpts=PTS-STARTPTS,` +
    `asetrate=44100*${a.pitch},aresample=44100${stretch},` +
    `aecho=0.72:0.58:135|270|405:0.32|0.19|0.10,highpass=f=110,lowpass=f=8200,` +
    `volume=${a.volume},adelay=${a.delay}|${a.delay}[${a.name}]`;
}).join(";");

// Two sub-heavy ceremonial impacts and three inharmonic gong blooms announce
// the tape's provenance. They are synthesized locally, not sampled.
const boom = (i, t) =>
  `aevalsrc='0.92*sin(2*PI*(58*t-18*t*t))*exp(-5.2*t)':s=44100:d=1.1,` +
  `lowpass=f=180,adelay=${Math.round(t * 1000)}[boom${i}]`;
const gong = (i, t) =>
  `aevalsrc='0.13*(sin(2*PI*109*t)+0.7*sin(2*PI*173*t)+0.5*sin(2*PI*281*t))*exp(-0.72*t)':s=44100:d=3.4,` +
  `aecho=0.7:0.62:90|173:0.32|0.22,adelay=${Math.round(t * 1000)}[gong${i}]`;
const ceremonies = [boom(0, 0.22), boom(1, 1.48), gong(0, 0.22), gong(1, 1.48), gong(2, 5.10)].join(";");
const snare =
  `anoisesrc=d=62.2:c=white:r=44100,highpass=f=1350,lowpass=f=9200,` +
  `volume='if(lt(mod(t-0.83,1.6667),0.18),0.24*exp(-24*mod(t-0.83,1.6667)),0)':eval=frame[snareN];` +
  `aevalsrc='if(lt(mod(t-0.83,1.6667),0.20),0.25*sin(2*PI*185*t)*exp(-20*mod(t-0.83,1.6667)),0)':s=44100:d=62.2[snareB]`;

// Three escalating chorus zones. Doubles retain duration while shifting pitch:
// a bright third-ish voice, a low body double, and two short wide slap copies.
const chorus = `between(t,9.6,19.8)+between(t,27.8,40.2)+between(t,44.8,62.2)`;
// Four warm pop chords, changing every two beats: Cmaj7, Am7, Fmaj7, G(add9).
const chordFreqs = [[130.81,164.81,196.00,246.94],[110.00,130.81,164.81,196.00],[87.31,110.00,130.81,164.81],[98.00,123.47,146.83,220.00]];
const chordExpr = chordFreqs.map((fs, i) => {
  const tone = fs.map((f, j) => `${j ? "0.72*" : ""}sin(2*PI*${f}*t)`).join("+");
  return `between(mod(t,6.6668),${(i*1.6667).toFixed(4)},${((i+1)*1.6667).toFixed(4)})*(${tone})`;
}).join("+");
const chords = `aevalsrc='0.040*(${chordExpr})':s=44100:d=62.2,lowpass=f=1700,` +
  `aecho=0.8:0.7:95|190:0.18|0.10,volume='0.55+0.45*(${chorus})':eval=frame[pad]`;

const audio =
  `[0:a]asplit=${screws.length + adlibs.length + 7}[dry][low][hi][lo][dblL][dblR][air]${screws.map((_, i) => `[sg${i}]`).join("")}${adlibs.map((_, i) => `[ad${i}]`).join("")};` +
  `[dry]volume='${gate}':eval=frame,acompressor=threshold=-18dB:ratio=2.4:attack=5:release=140[voice];` +
  `[low]lowpass=f=145,bass=g=10:f=82,volume=0.72[sub];` +
  `[hi]asetrate=44100*1.1892,aresample=44100,atempo=0.8409,highpass=f=170,` +
  `volume='0.29*(${chorus})':eval=frame,pan=stereo|c0=0.25*c0|c1=0.95*c1[harmonyHi];` +
  `[lo]asetrate=44100*0.8409,aresample=44100,atempo=1.1892,lowpass=f=7200,` +
  `volume='0.23*(${chorus})':eval=frame,pan=stereo|c0=0.95*c0|c1=0.25*c1[harmonyLo];` +
  `[dblL]adelay=23|23,volume='0.32*(${chorus})':eval=frame,pan=stereo|c0=c0|c1=0.08*c1[doubleL];` +
  `[dblR]adelay=47|47,volume='0.27*(${chorus})':eval=frame,pan=stereo|c0=0.08*c0|c1=c1[doubleR];` +
  `[air]highpass=f=4200,aecho=0.7:0.55:74|149:0.28|0.16,volume='0.16*(${chorus})':eval=frame[chorusAir];` +
  `${screwChains};${adlibChains};${ceremonies};${snare};${chords};` +
  `[voice][sub][harmonyHi][harmonyLo][doubleL][doubleR][chorusAir]${ghostInputs}` +
  `${adlibs.map(a => `[${a.name}]`).join("")}[pad][boom0][boom1][gong0][gong1][gong2][snareN][snareB]` +
  `amix=inputs=${screws.length + adlibs.length + 15}:duration=first:normalize=0,` +
  `adeclick=w=55:o=75:a=2:t=2:b=2:m=s,highpass=f=28,` +
  `firequalizer=gain_entry='entry(28,-8);entry(55,1.5);entry(110,0.8);entry(280,-1.2);entry(3200,1.1);entry(9000,1.5);entry(16000,-1)',` +
  `stereotools=mlev=1:slev=1.18,acompressor=threshold=-12dB:ratio=1.8:attack=12:release=170,` +
  `aexciter=amount=1.2:drive=3.5:ceil=14000,alimiter=limit=0.89:attack=5:release=70,` +
  `loudnorm=I=-12.5:TP=-1:LRA=6[aout]`;

// Visual events share the same clock as the audio cuts. RGB separation and
// temporal trails hit every chop; macroblocks/noise are reserved for the three
// screw-downs, giving the reel dynamics instead of a permanent glitch preset.
const chopOn = any(chops, 0.50);
const screwOn = any(screws, 0.92);
const finish = [
  "eq=contrast=1.10:saturation=1.22",
  `rgbashift=rh=11:rv=-2:bh=-13:bv=3:edge=smear:enable='${chopOn}'`,
  `tmix=frames=4:weights='1 0.72 0.42 0.20':enable='${chopOn}'`,
  `pixelize=w=18:h=10:mode=avg:enable='${screwOn}'`,
  `chromashift=cbh=18:crh=-18:edge=smear:enable='${screwOn}'`,
  `noise=alls=14:allf=t+u:enable='${screwOn}'`,
  "unsharp=5:5:0.55:5:5:0",
].join(",");

// A 3×3 Brady-Bunch wall lands with the chorus. Each tile gets its own color,
// mirror, crop position, and tiny motion offset so the wall stays alive.
const gridSplits = Array.from({ length: 9 }, (_, i) => `[g${i}]`).join("");
const tiles = Array.from({ length: 9 }, (_, i) => {
  const hue = (i - 4) * 13;
  const flip = i % 2 ? ",hflip" : "";
  const zoom = 1.02 + (i % 3) * 0.025;
  return `[g${i}]scale=360:640:force_original_aspect_ratio=increase,crop=360:640,` +
    `eq=saturation=${(1.05 + (i % 3) * .16).toFixed(2)}:contrast=1.08,` +
    `hue=h=${hue}${flip},scale=iw*${zoom}:ih*${zoom},crop=360:640[t${i}]`;
}).join(";");
const gridInputs = Array.from({ length: 9 }, (_, i) => `[t${i}]`).join("");
const gridOn = `between(t,9.6,19.8)+between(t,27.8,40.2)+between(t,44.8,62.2)`;
const visuals = `[0:v]split=10[clean]${gridSplits};${tiles};` +
  `${gridInputs}xstack=inputs=9:layout=0_0|360_0|720_0|0_640|360_640|720_640|0_1280|360_1280|720_1280[wall];` +
  `[clean][wall]overlay=enable='${gridOn}'[picture];[picture]${finish}[vout]`;

console.log(`· ${chops.length} chop events · ${screws.length} screw ghosts`);
const r = spawnSync("ffmpeg", [
  "-hide_banner", "-loglevel", "error", "-y", "-i", SRC,
  "-filter_complex", `${audio};${visuals}`,
  "-map", "[vout]", "-map", "[aout]",
  "-c:v", "libx264", "-preset", "slow", "-crf", "16", "-pix_fmt", "yuv420p",
  "-c:a", "aac", "-b:a", "256k", "-movflags", "+faststart", "-shortest", OUT,
], { stdio: "inherit" });
if (r.status !== 0) process.exit(r.status || 1);
console.log(`✓ ${OUT}`);
