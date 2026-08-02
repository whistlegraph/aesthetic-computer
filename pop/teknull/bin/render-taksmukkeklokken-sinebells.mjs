#!/usr/bin/env node
// Taksmukkeklokken sine-bell refinement: a bar-10 recut with composed bell lines.

import { spawnSync } from "node:child_process";
import { createHash } from "node:crypto";
import { mkdirSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { dirname, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "../../..");
const OUT = resolve(HERE, "../out");
const GREEK_V2 = process.argv.includes("--greek-theatre-v2");
const GREEK_THEATRE = GREEK_V2 || process.argv.includes("--greek-theatre");
const SLOW_BUILD = GREEK_THEATRE || process.argv.includes("--slow-build");
const SOURCE = resolve(OUT, "taksmukkeklokken-spatial-orchestral-harsh-c.mp3");
const STEM = GREEK_V2 ? "taksmukkeklokken-sinebells-greektheatre-v2"
  : GREEK_THEATRE ? "taksmukkeklokken-sinebells-greektheatre"
  : SLOW_BUILD ? "taksmukkeklokken-sinebells-slowbuild" : "taksmukkeklokken-sinebells";
const TARGET = resolve(OUT, `${STEM}.mp3`);
const RECEIPT = resolve(OUT, `${STEM}.json`);
const RAW = resolve(OUT, `.${STEM}.f32le`);
const SR = 48_000, BPM = 140, BEAT = 60 / BPM, BAR = 4 * BEAT;
const SOURCE_START = 9 * BAR; // original bar 10: 15.428571428571429 s
const DURATION = 120 - SOURCE_START;
const frames = Math.floor(DURATION * SR);
const pcm = new Float32Array(frames * 2);
mkdirSync(OUT, { recursive: true });

let seed = 0x51be11;
const noise = () => ((seed = (Math.imul(seed, 1664525) + 1013904223) >>> 0) / 0x80000000 - 1);
const hz = midi => 440 * 2 ** ((midi - 69) / 12);
function add(time, length, pan, voice) {
  const first = Math.max(0, Math.round(time * SR));
  const count = Math.min(Math.round(length * SR), frames - first);
  const l = Math.sqrt((1 - pan) * .5), r = Math.sqrt((1 + pan) * .5);
  for (let i = 0; i < count; i++) {
    const v = voice(i / SR, i);
    pcm[(first + i) * 2] += v * l;
    pcm[(first + i) * 2 + 1] += v * r;
  }
}
function kick(t, gain = 1) {
  add(t, .33, 0, x => gain * (.15 * Math.sin(2 * Math.PI * (48 * x + 2.8 * (1 - Math.exp(-x / .026)))) * Math.exp(-x / .12)
    + .012 * Math.sin(2 * Math.PI * 1350 * x) * Math.exp(-x / .005)));
}
function snare(t) {
  add(t, .105, 0, x => .044 * Math.sin(2 * Math.PI * 178 * x) * Math.exp(-x / .05)
    + .052 * (noise() - .5 * noise()) * Math.exp(-x / .027));
}
function sineBell(t, midi, gain, pan, decay = 1.15) {
  const f = hz(midi);
  add(t, decay * 4.3, pan, x => {
    const attack = 1 - Math.exp(-x / .006);
    const env = attack * Math.exp(-x / decay);
    // A clean fundamental carries the tune; quiet octave/12th give it identity.
    return gain * env * (Math.sin(2 * Math.PI * f * x) + .19 * Math.sin(2 * Math.PI * 2 * f * x + .25)
      + .07 * Math.sin(2 * Math.PI * 3 * f * x + .8));
  });
}
function femBell(t, midi, gain, pan, material = "bronze") {
  const f = hz(midi);
  const bronze = [[1, 1, 1.75], [2.01, .31, 1.1], [2.72, .18, .72], [4.08, .09, .42]];
  const glass = [[1, 1, 1.35], [2.32, .2, .78], [3.86, .11, .48], [5.41, .045, .3]];
  const modes = material === "bronze" ? bronze : glass;
  add(t + (material === "glass" ? .011 : .022), 5.6, pan, x => {
    const attack = 1 - Math.exp(-x / (material === "bronze" ? .018 : .009));
    let y = 0;
    // Sub-cent platter drift makes the material breathe without detuning the hook.
    const drift = 1 + .00045 * Math.sin(2 * Math.PI * .31 * x + midi);
    for (const [ratio, amp, decay] of modes) y += amp * Math.sin(2 * Math.PI * f * ratio * drift * x) * Math.exp(-x / decay);
    return gain * attack * y;
  });
}

function protagonist(t, midi, gain = .036) {
  // Solo: centered-left, pure and speech-like, with a decisive short release.
  sineBell(t, midi, gain, -.13, .72);
}
function coryphaeus(t, midi, gain = .03) {
  // Leader's response: warmer and opposite the protagonist in the theatre.
  sineBell(t, midi, gain, .2, 1.02);
}

function grooveBar(t, intensity) {
  let previous = 0;
  // Authored surface drag: strongest after the downbeat, then it recedes to
  // leave the backbeat and phrase ending clear. Differenced noise avoids hiss.
  add(t, BAR * .88, 0, x => {
    const n = noise(), grain = n - previous; previous = n;
    const pulse = .28 + .72 * Math.exp(-((x % BEAT) / .115));
    const phraseFade = Math.max(0, 1 - x / (BAR * .88));
    return intensity * .007 * grain * pulse * (.45 + .55 * phraseFade);
  });
  // Two quiet groove-wall contacts, deliberately away from kick/snare attacks.
  for (const off of [1.72, 3.68]) add(t + off * BEAT, .012, off < 2 ? -.12 : .12,
    x => intensity * .018 * noise() * Math.exp(-x / .0027));
}

// Centered classic pulse, with alternating lighter kicks to keep it dance-pop.
for (let b = 0; b * BEAT < DURATION; b++) {
  const bar = Math.floor(b / 4), inBar = b % 4;
  if (!SLOW_BUILD || bar >= 16) kick(b * BEAT, inBar === 0 ? 1 : .82);
  else if (bar >= 8 && inBar % 2 === 0) kick(b * BEAT, inBar === 0 ? .68 : .48);
  else if (bar >= 4 && inBar === 0) kick(b * BEAT, .45);
  if ((!SLOW_BUILD || bar >= 16) && (inBar === 1 || inBar === 3)) snare(b * BEAT);
  else if (SLOW_BUILD && bar >= 12 && inBar === 3) {
    // A single cool backbeat foreshadows the full 2/4 snare lane.
    add(b * BEAT, .09, 0, x => .032 * (noise() - .5 * noise()) * Math.exp(-x / .024));
  }
}

// 8-bar harmonic sentence: Am9 | Fmaj9/A | Cmaj9/G | G6/9 | Dm9/F |
// Am/E | Bdim7/F | E7(b9). Hook is stable enough to sing, with two altered
// answers supplying the adventurous harmony. Rests at each phrase end matter.
const roots = [57, 57, 55, 55, 53, 52, 53, 52];
const hook = [
  [[0,76],[1,79],[2.5,81]], [[0,76],[1.5,74],[2.5,72]],
  [[0,76],[1,79],[2.5,83]], [[0,81],[1.5,79],[2.5,76]],
  [[0,77],[1,81],[2.5,84]], [[0,76],[1.5,79],[2.5,83]],
  [[0,77],[1,80],[2.5,86]], [[0,76],[1,74],[2.5,71]],
];
const answer = [69,69,67,67,65,64,65,68];
// Slow-build mode is a new composition, not a postponed version of the hook.
// Each harmony lasts two bars: Am11 | Cmaj7/E | Fmaj9 | E7sus(b9) |
// Dm9 | G13 | Cmaj9 | E7(#5). Its four-note "pressing" cell expands by form.
const slowRoots = [45, 52, 53, 52, 50, 55, 48, 52];
const slowCells = [
  [69,72,71,76], [67,71,72,76], [69,72,76,79], [68,71,74,77],
  [69,72,76,77], [71,74,76,81], [67,71,74,79], [68,72,77,76],
];
const bars = Math.ceil(DURATION / BAR);
for (let bar = 0; bar < bars; bar++) {
  const phase = bar % 8, slowPhase = Math.floor(bar / 2) % 8, base = bar * BAR;
  const section = SLOW_BUILD ? (bar < 8 ? 0 : bar < 16 ? 1 : bar < 32 ? 2 : 3)
    : (bar < 8 ? 0 : bar < 24 ? 1 : bar < 40 ? 2 : 3);
  grooveBar(base, bar % 8 === 7 ? .35 : section === 2 ? .8 : .58);
  // Slow build's new pressing-cell melody grows across long harmonic fields.
  if (GREEK_THEATRE && bar >= 4 && bar % 16 !== 15) {
    const cell = slowCells[slowPhase];
    // V2 reserves the chorus for the late climax. Earlier structural turns are
    // solo/source battles and therefore cannot silently inflate voice count.
    const structural = GREEK_V2 ? (bar === 40 || bar === 48)
      : (bar === 16 || bar === 32 || bar === 40 || bar === 48);
    if (structural) {
      // Chorus: three separated registers only at a major turn. It speaks once,
      // then yields an entire beat of stage space.
      protagonist(base, cell[0], .038);
      coryphaeus(base, cell[1] + 7, .027);
      sineBell(base, cell[2] + 12, .021, 0, .88);
    } else if (bar % 4 === 0 || bar % 4 === 1) {
      // Episode: protagonist states one line; no simultaneous melodic reply.
      protagonist(base + (bar < 12 ? 1.5 : .5) * BEAT, cell[bar % 2 ? 2 : 0], bar < 12 ? .024 : .035);
    } else if (bar % 4 === 2) {
      // Coryphaeus answers antiphonally after the prior bar's phrase has cleared.
      coryphaeus(base + 1.25 * BEAT, cell[3] - (bar % 8 === 6 ? 2 : 0), bar < 16 ? .024 : .031);
    }
    // Every fourth bar is a stasimon/rest; surface and rhythm carry the scene.
  } else if (SLOW_BUILD && bar >= 4 && bar % 16 !== 15) {
    const cell = slowCells[slowPhase];
    const count = bar < 8 ? 1 : bar < 16 ? 2 : bar < 32 ? 4 : 5;
    const events = [[0,cell[0]],[1.5,cell[1]],[2.5,cell[2]],[3.25,cell[3]],[3.625,cell[0]+12]];
    for (let i = 0; i < count; i++) {
      // Second bar of each harmony answers later and in contrary contour.
      const [beat, pitch] = events[i];
      const answered = bar % 2 ? (i % 2 ? pitch - 2 : pitch + 3) : pitch;
      const climax = bar >= 40 && bar % 4 < 2 ? 12 : 0;
      sineBell(base + (beat + (bar % 2 ? .25 : 0)) * BEAT, answered + climax,
        bar < 8 ? .02 : bar < 16 ? .027 : bar >= 40 ? .044 : .037,
        i % 2 ? .16 : -.1, i >= 3 ? .76 : 1.15);
    }
  // Original accepted composition remains unchanged in default mode.
  } else if (!SLOW_BUILD && bar >= 2 && bar % 8 !== 7) {
    for (const [noteIndex, [beat, note]] of hook[phase].entries()) {
      if (section === 0 && beat === 1) continue;
      const lift = section === 3 && bar % 4 < 2 ? 12 : 0;
      const openingGain = section === 2 ? .045 : .038;
      sineBell(base + beat * BEAT, note + lift, openingGain, beat === 1 ? .18 : -.12, .9 + .12 * (beat === 2.5));
    }
  }
  // Contrary-motion counterpoint only in alternating phrases: legible space.
  if (!SLOW_BUILD && section > 0 && bar % 8 >= 4 && bar % 8 <= 6) {
    sineBell(base + .5 * BEAT, answer[phase], .026, .23, 1.28);
    sineBell(base + 2 * BEAT, answer[phase] + (phase === 6 ? 6 : 4), .022, .1, 1.05);
  }
  // One warm tactile body per bar, below the melodic register. Alternating
  // material and side; staggered attack avoids a hard layered clang.
  if (bar % 8 !== 7 && (!SLOW_BUILD || bar >= 6) && (!GREEK_THEATRE || bar % 8 === 7 || bar % 4 === 3)) {
    const bodyGain = SLOW_BUILD && bar < 10 ? .0045 : SLOW_BUILD && bar < 16 ? .008 : .0125;
    femBell(base, SLOW_BUILD ? slowRoots[slowPhase] : roots[phase], bodyGain, bar % 2 ? .2 : -.2, bar % 4 === 2 ? "glass" : "bronze");
  }
}
writeFileSync(RAW, Buffer.from(pcm.buffer));

const v2FirstAntagonist = 16 * BAR;
const v2SecondAntagonist = 24 * BAR;
const v2ThirdAntagonist = 32 * BAR;
const v2ClimaxStart = 40 * BAR;
const v2ClimaxEnd = 48 * BAR;
const bedTreatment = GREEK_V2
  // Hard zero until bar 17. Three isolated, sub-bar source utterances follow,
  // each separated by at least seven bars of literal inherited-bed silence.
  // Only the bar 41–48 climax permits a continuous ensemble field.
  ? `asplit=2[antSrc][climaxSrc];` +
    `[antSrc]highpass=f=260,lowpass=f=2100,volume='if(between(t,${v2FirstAntagonist.toFixed(6)},${(v2FirstAntagonist+.62).toFixed(6)}),.34*sin(PI*(t-${v2FirstAntagonist.toFixed(6)})/.62)^2,if(between(t,${v2SecondAntagonist.toFixed(6)},${(v2SecondAntagonist+.74).toFixed(6)}),.31*sin(PI*(t-${v2SecondAntagonist.toFixed(6)})/.74)^2,if(between(t,${v2ThirdAntagonist.toFixed(6)},${(v2ThirdAntagonist+.88).toFixed(6)}),.36*sin(PI*(t-${v2ThirdAntagonist.toFixed(6)})/.88)^2,0)))':eval=frame[ant];` +
    `[climaxSrc]highpass=f=45,lowpass=f=7600,volume='if(between(t,${v2ClimaxStart.toFixed(6)},${v2ClimaxEnd.toFixed(6)}),.36*min(1,min(max(0,(t-${v2ClimaxStart.toFixed(6)})/.08),max(0,(${v2ClimaxEnd.toFixed(6)}-t)/.12))),0)':eval=frame[climax];` +
    `[ant][climax]amix=inputs=2:normalize=0,stereotools=mlev=1.08:slev=.58[bed];`
  : GREEK_THEATRE
  ? "highpass=f=28,equalizer=f=3150:t=q:w=1.1:g=-4.2,equalizer=f=6900:t=q:w=.9:g=-3.1,lowpass=f=520,stereotools=mlev=1.1:slev=.48,volume=.56[bed];"
  : "highpass=f=28,equalizer=f=3150:t=q:w=1.1:g=-3.5,equalizer=f=6900:t=q:w=.9:g=-2.6,equalizer=f=980:t=q:w=.7:g=1.3,stereotools=mlev=1.07:slev=.7,volume=.88[bed];";
const filter = [
  `[0:a]atrim=start=${SOURCE_START.toFixed(12)},asetpts=PTS-STARTPTS,afade=t=in:st=0:d=0.014,afade=t=out:st=${(DURATION-.14).toFixed(6)}:d=0.14,`,
  bedTreatment,
  // Bell bus is kept coherent and warm; sub-250 Hz stays mono after summing.
  "[1:a]highpass=f=42,equalizer=f=2600:t=q:w=.8:g=-1.6,equalizer=f=720:t=q:w=.7:g=1.4,volume=.92[bells];",
  "[bed][bells]amix=inputs=2:weights='1 1':normalize=0,acompressor=threshold=.17:ratio=1.8:attack=22:release=175:makeup=1.03,",
  `stereotools=mlev=1.03:slev=.82,loudnorm=I=-13:LRA=6:TP=-1:linear=false,${GREEK_V2 ? "alimiter=limit=.82:attack=5:release=50:level=false,volume=-1.4dB," : ""}aresample=48000[out]`
].join("");
const ffmpegArgs = ["-y","-hide_banner","-loglevel","warning","-i",SOURCE,"-f","f32le","-ar",String(SR),"-ac","2","-i",RAW,
  "-filter_complex",filter,"-map","[out]","-t",DURATION.toFixed(9),"-ar",String(SR),"-ac","2","-codec:a","libmp3lame","-b:a","320k",
  "-metadata",`title=${STEM}`,"-metadata","artist=aesthetic.computer",TARGET];
const result = spawnSync("ffmpeg", ffmpegArgs, { stdio: "inherit" });
rmSync(RAW, { force: true });
if (result.status !== 0) process.exit(result.status ?? 1);
const sha256 = p => createHash("sha256").update(readFileSync(p)).digest("hex");
writeFileSync(RECEIPT, JSON.stringify({
  schema: "aesthetic.computer/pop-remaster/v1", track: STEM,
  source: relative(ROOT, SOURCE), output: relative(ROOT, TARGET), sourceSha256: sha256(SOURCE), outputSha256: sha256(TARGET),
  renderer: relative(ROOT, fileURLToPath(import.meta.url)), sourceStartSec: SOURCE_START, durationSec: DURATION, sampleRate: SR, channels: 2,
  harmony: SLOW_BUILD
    ? ["Am11 (2 bars)","Cmaj7/E (2 bars)","Fmaj9 (2 bars)","E7sus(b9) (2 bars)","Dm9 (2 bars)","G13 (2 bars)","Cmaj9 (2 bars)","E7(#5) (2 bars)"]
    : ["Am9","Fmaj9/A","Cmaj9/G","G6(add9)","Dm9/F","Am/E","Bdim7/F","E7(b9)"],
  arc: SLOW_BUILD
    ? ["four-bar material-only threshold","single-note hook fragments and downbeat kick","two-note phrases with gradual FEM bloom and one backbeat","bar-17 full classic drums and complete hook","contrary-motion payoff and octave-bright return"]
    : ["two-bar sparse hook entrance","full eight-bar sine hook","alternating contrary-motion answer","octave-bright final return"],
  bellDesign: "sine fundamental hook/counterpoint with staggered warm bronze/glass modal FEM-style bodies",
  composition: SLOW_BUILD ? "new two-bar harmonic rhythm and four-note pressing-cell development; no reuse of the accepted sinebells hook" : "accepted eight-bar sinebells hook",
  dramaturgy: GREEK_THEATRE ? {
    normalMelodicVoiceCap: 1, climaxMelodicVoiceCap: 3,
    protagonist: "short pure sine, center-left; states episode lines alone",
    coryphaeus: "warm sine, right; answers in a later bar",
    chorus: "three-register structural utterance only at bars 17, 33, 41, and 49",
    stasimon: "every fourth bar clears pitched melody for rhythm and record material"
  } : null,
  inheritedBedPlan: GREEK_V2 ? {
    bars1To16: "hard-muted (0.0); no inherited harmonic or melodic voices",
    isolatedAntagonists: ["bar 17: 0.62 s", "bar 25: 0.74 s", "bar 33: 0.88 s"],
    silenceBetweenAntagonists: "more than seven bars; source gain exactly 0",
    lateClimax: "bars 41–48 only, continuous ensemble at 0.36 gain",
    outsideListedWindows: "source gain exactly 0"
  } : null,
  surface: "bar-authored responsive groove drag, offbeat groove-wall contacts, and sub-cent material drift; no constant hiss",
  mix: "narrow coherent bell field, centered drums/sub, reduced 3.15/6.9 kHz harshness, -13 LUFS / -1 dBTP target",
  slowBuild: SLOW_BUILD, greekTheatre: GREEK_THEATRE, greekTheatreV2: GREEK_V2, ffmpegArgs
}, null, 2) + "\n");
console.log(TARGET);
