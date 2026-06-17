#!/usr/bin/env node
// hooks.mjs — the full-phrase combination machine.
//
// every utterance in utterances/<group>/ gets autotuned ("snapped") to every
// target MIDI note the hook variants in melody.json ask of its slot, plus a
// whispered twin (FFT phase-randomization — breathy ghost of the same word).
// the snapped library lives in snapped/<group>/ and is indexed by hooks.json.
//
// from there every combination of the phrase — america × computer × dora —
// is playable instantly in hooks.html (no per-combo render), and `flight`
// stitches a batch of random combos to mp3 for couch listening.
//
// usage:
//   node bin/hooks.mjs                 # build + page (the default)
//   node bin/hooks.mjs build           # snap library only
//   node bin/hooks.mjs page            # regenerate hooks.html from hooks.json
//   node bin/hooks.mjs flight          # 24 random stitched combos → out/hooks/
//   node bin/hooks.mjs flight --count 64 --variant 4 --whisper
//   node bin/hooks.mjs flight --seed 7 # reproducible flight
//   node bin/hooks.mjs render --fav hooks-favorites.json  # stitch exported favorites

import { spawn, spawnSync } from "node:child_process";
import {
  existsSync, mkdirSync, readdirSync, readFileSync, writeFileSync, unlinkSync,
} from "node:fs";
import { resolve, dirname, join, basename } from "node:path";
import { fileURLToPath } from "node:url";
import { tmpdir } from "node:os";

import { readWavMono } from "../../lib/wav.mjs";

const SR = 48_000;
const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = dirname(HERE);
const GROUPS = ["america", "computer", "dora"];

const argv = process.argv.slice(2);
const cmd = argv[0] && !argv[0].startsWith("--") ? argv[0] : "all";
const flags = {};
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const k = a.slice(2), n = argv[i + 1];
    if (n !== undefined && !n.startsWith("--")) { flags[k] = n; i++; } else flags[k] = true;
  }
}

const melody = JSON.parse(readFileSync(join(ROOT, "melody.json"), "utf8"));

// ── YIN-lite pitch detection (same as stitch.mjs) ───────────────────────
function detectPitchHz(samples, sr) {
  const n = samples.length;
  const seg = samples.subarray(Math.floor(n * 0.2), Math.floor(n * 0.8));
  const minLag = Math.floor(sr / 600);
  const maxLag = Math.floor(sr / 80);
  let bestLag = 0, bestScore = -Infinity;
  for (let lag = minLag; lag <= maxLag; lag++) {
    let r = 0, e0 = 0, e1 = 0;
    for (let i = 0; i + lag < seg.length; i++) {
      r += seg[i] * seg[i + lag];
      e0 += seg[i] * seg[i];
      e1 += seg[i + lag] * seg[i + lag];
    }
    const norm = Math.sqrt(e0 * e1) || 1;
    const score = r / norm;
    if (score > bestScore) { bestScore = score; bestLag = lag; }
  }
  return bestLag === 0 ? null : sr / bestLag;
}
const hzToMidi = (hz) => 69 + 12 * Math.log2(hz / 440);
const midiName = (m) => {
  const names = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"];
  return names[m % 12] + (Math.floor(m / 12) - 1);
};

function chainAtempo(target) {
  const stages = [];
  let r = target;
  while (r > 2.0) { stages.push(2.0); r /= 2.0; }
  while (r < 0.5) { stages.push(0.5); r /= 0.5; }
  stages.push(+r.toFixed(6));
  return stages.map((s) => `atempo=${s}`).join(",");
}

// whisper = keep the magnitude spectrum, randomize all phase. the word's
// formants survive but the pitch dissolves into breath.
const WHISPER =
  "highpass=f=250," +
  "afftfilt=real='hypot(re,im)*cos(random(0)*2*PI)':imag='hypot(re,im)*sin(random(1)*2*PI)':win_size=512:overlap=0.75";

// ── tiny ffmpeg job pool (8 GB machine: cap 4 concurrent) ──────────────
function ffmpegAsync(args) {
  return new Promise((res) => {
    const p = spawn("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error", ...args], {
      stdio: ["ignore", "ignore", "inherit"],
    });
    p.on("close", (code) => res(code === 0));
  });
}

// WORLD-vocoder hard tune (pop/bin/pitchsnap_world.py): replace the f0
// curve with the target note so sung melisma actually SITS on the melody.
// retain 0.85 keeps 15% of the original movement; light vibrato for life.
// applied to the sung groups (america/dora) — TTS is already flat and the
// whisper twins have no pitch to tune.
const TUNE_GROUPS = new Set(["america", "dora"]);
const PY = join(ROOT, "..", ".venv", "bin", "python3");
const PITCHSNAP = join(ROOT, "..", "bin", "pitchsnap_world.py");
function tuneAsync(wav, noteName) {
  return new Promise((res) => {
    const p = spawn(PY, [
      PITCHSNAP, wav, wav, "--notes", noteName,
      "--retain", "0.85", "--vibrato-hz", "5.2", "--vibrato-cents", "14",
    ], { stdio: ["ignore", "ignore", "inherit"] });
    p.on("close", (code) => res(code === 0));
  });
}
async function pool(jobs, width = 4) {
  let i = 0, done = 0, fail = 0;
  const workers = Array.from({ length: width }, async () => {
    while (i < jobs.length) {
      const job = jobs[i++];
      const ok = await job();
      if (ok) done++; else fail++;
      if ((done + fail) % 50 === 0) console.log(`  … ${done + fail}/${jobs.length}`);
    }
  });
  await Promise.all(workers);
  return { done, fail };
}

// what does each utterance actually SAY? cross-reference clips.json spans
// against the sources' word-level transcripts (<src>-words.json, fromMs/toMs)
// so the audition page can show "do" vs "Dora" at a glance.
function transcripts() {
  const clipsPath = join(ROOT, "clips.json");
  if (!existsSync(clipsPath)) return {};
  const byName = {};
  for (const c of JSON.parse(readFileSync(clipsPath, "utf8"))) {
    if (c.start == null || c.end == null) continue;
    const srcBase = basename(c.source, ".mp3");
    const wordsPath = join(ROOT, dirname(c.source), `${srcBase}-words.json`);
    if (!existsSync(wordsPath)) continue;
    const wl = JSON.parse(readFileSync(wordsPath, "utf8"));
    const text = wl
      .filter((w) => w.fromMs / 1000 < c.end && w.toMs / 1000 > c.start && w.text !== "♪")
      .map((w) => w.text).join(" ");
    const tag = c.tag.toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "");
    byName[`${srcBase}-${tag}`] = text || null;
  }
  return byName;
}

// ── build: snap every utterance to every target note (+ whisper twin) ──
async function build() {
  const saidBy = transcripts();
  // each slot's needed notes come straight from the hook variants
  const targets = {};
  for (let w = 0; w < melody.hook.words.length; w++) {
    targets[melody.hook.words[w]] = [...new Set(melody.hook.variants.map((v) => v.notes[w]))];
  }
  console.log("# snap targets per slot:");
  for (const g of GROUPS) console.log(`  ${g}: ${targets[g].map(midiName).join(" ")}`);

  const manifest = { sr: SR, groups: {} };
  const jobs = [];
  for (const g of GROUPS) {
    const srcDir = join(ROOT, "utterances", g);
    const outDir = join(ROOT, "snapped", g);
    mkdirSync(outDir, { recursive: true });
    manifest.groups[g] = [];
    if (!existsSync(srcDir)) continue;
    const files = readdirSync(srcDir).filter((f) => f.endsWith(".wav")).sort();
    for (const f of files) {
      const abs = join(srcDir, f);
      const name = basename(f, ".wav");
      let srcHz = null, srcMidi = null, dur = null;
      try {
        const { samples } = readWavMono(abs);
        srcHz = detectPitchHz(samples, SR);
        srcMidi = srcHz ? hzToMidi(srcHz) : null;
        dur = +(samples.length / SR).toFixed(3);
      } catch {}
      const entry = {
        name, src: `utterances/${g}/${f}`, srcHz, srcMidi, dur,
        word: saidBy[name] ?? (g === "computer" ? "computer" : null),
        notes: {}, whisper: {},
      };
      for (const t of targets[g]) {
        const outRel = `snapped/${g}/${name}__t${t}.wav`;
        const whRel = `snapped/${g}/${name}__t${t}__w.wav`;
        entry.notes[t] = outRel;
        entry.whisper[t] = whRel;
        const outAbs = join(ROOT, outRel);
        const whAbs = join(ROOT, whRel);
        // fold the shift by octaves so we never stretch more than ±6 semis —
        // an octave-equivalent landing keeps the voice human.
        let semis = 0;
        if (srcMidi != null) {
          semis = t - srcMidi;
          while (semis > 6) semis -= 12;
          while (semis < -6) semis += 12;
        }
        const ratio = Math.pow(2, semis / 12);
        const shift = Math.abs(semis) < 0.01
          ? ""
          : `asetrate=${SR}*${ratio},aresample=${SR},${chainAtempo(1 / ratio)},`;
        if (!existsSync(outAbs)) {
          const noteName = midiName(t);
          jobs.push(() => ffmpegAsync([
            "-i", abs, "-af", `${shift}loudnorm=I=-15:TP=-1.2`, "-ac", "1", "-ar", String(SR), outAbs,
          ]).then((ok) => (ok && TUNE_GROUPS.has(g) ? tuneAsync(outAbs, noteName) : ok)));
        }
        if (!existsSync(whAbs)) {
          jobs.push(() => ffmpegAsync([
            "-i", abs, "-af", `${shift}${WHISPER},loudnorm=I=-16:TP=-1.5`, "-ac", "1", "-ar", String(SR), whAbs,
          ]));
        }
      }
      manifest.groups[g].push(entry);
    }
  }

  const counts = GROUPS.map((g) => `${g} ${manifest.groups[g].length}`).join(" · ");
  const comboCount = GROUPS.reduce((a, g) => a * (manifest.groups[g].length || 1), 1);
  console.log(`\n# utterances: ${counts}`);
  console.log(`# phrase combinations available: ${comboCount.toLocaleString()} (× ${melody.hook.variants.length} melody variants × whisper toggles)`);
  console.log(`# rendering ${jobs.length} snapped wavs (cached files skipped)…`);
  const { done, fail } = await pool(jobs);
  console.log(`  ✓ ${done} rendered${fail ? `, ✗ ${fail} FAILED` : ""}`);

  writeFileSync(join(ROOT, "hooks.json"), JSON.stringify(manifest, null, 2));
  console.log("✓ hooks.json");
}

// ── page: hooks.html — every combination, instantly playable ───────────
function page() {
  const html = `<!doctype html>
<meta charset="utf-8">
<title>americomputadora · hooks</title>
<style>
  :root { color-scheme: dark; }
  body { background:#0a0a12; color:#eee; font:14px/1.45 "Berkeley Mono", ui-monospace, monospace; margin:0; }
  header { position:sticky; top:0; background:#0a0a12ee; border-bottom:1px solid #223; padding:10px 16px; z-index:5;
           display:flex; gap:10px; align-items:center; flex-wrap:wrap; }
  h1 { font-size:15px; margin:0 8px 0 0; color:#7df9ff; font-weight:normal; }
  button { background:#16213e; color:#eee; border:1px solid #335; border-radius:6px; padding:6px 12px;
           font:inherit; cursor:pointer; }
  button:hover { background:#1f2d52; }
  button.big { background:#e94560; border-color:#e94560; font-weight:bold; }
  button.big:hover { background:#ff5d77; }
  select { background:#16213e; color:#eee; border:1px solid #335; border-radius:6px; padding:5px 8px; font:inherit; }
  label.tog { display:flex; gap:4px; align-items:center; color:#9af; cursor:pointer; }
  main { display:grid; grid-template-columns:1fr 1fr 1fr; gap:0; }
  .col { border-right:1px solid #181830; min-height:60vh; }
  .col h2 { position:sticky; top:52px; margin:0; padding:8px 12px; background:#11112255; backdrop-filter:blur(6px);
            font-size:13px; letter-spacing:2px; text-transform:uppercase; }
  .col.america h2 { color:#ff6b6b; } .col.computer h2 { color:#7df9ff; } .col.dora h2 { color:#ffd93d; }
  .clip { padding:4px 12px; cursor:pointer; display:flex; gap:8px; align-items:baseline; white-space:nowrap;
          overflow:hidden; text-overflow:ellipsis; }
  .clip:hover { background:#181830; }
  .clip.sel { background:#23234d; outline:1px solid #447; }
  .clip .hz { color:#667; font-size:11px; margin-left:auto; }
  .clip .said { color:#9c8; font-size:12px; font-style:italic; }
  .clip .pv { color:#557; }
  .clip .pv:hover { color:#9af; }
  footer { position:sticky; bottom:0; background:#0a0a12ee; border-top:1px solid #223; padding:10px 16px;
           display:flex; gap:14px; align-items:center; flex-wrap:wrap; }
  #now { color:#ffd93d; min-width:280px; }
  #favs { padding:8px 16px 60px; }
  #favs .fav { display:flex; gap:10px; padding:3px 0; align-items:baseline; }
  #favs .fav button { padding:2px 8px; }
  .keys { color:#556; font-size:12px; }
</style>
<header>
  <h1>americomputadora · ${"${COMBOS}"} combinations</h1>
  <button class="big" id="play">▶ play (space)</button>
  <button id="surf">🎲 surf (s)</button>
  <button id="fav">★ keep (f)</button>
  <select id="variant"></select>
  <label class="tog"><input type="checkbox" id="wa"> whisper america</label>
  <label class="tog"><input type="checkbox" id="wc"> whisper computer</label>
  <label class="tog"><input type="checkbox" id="wd"> whisper dora</label>
  <label class="tog"><input type="checkbox" id="grid" checked> beat grid</label>
  <span class="keys">↑↓ america · ←→ computer · ,/. dora</span>
</header>
<main>
  <div class="col america"><h2>america</h2><div id="list-america"></div></div>
  <div class="col computer"><h2>computer</h2><div id="list-computer"></div></div>
  <div class="col dora"><h2>dora</h2><div id="list-dora"></div></div>
</main>
<footer>
  <span id="now">pick clips, hit space.</span>
  <button id="export">⇩ export favorites json</button>
  <span class="keys" id="favcount"></span>
</footer>
<div id="favs"></div>
<script>
const GROUPS = ["america", "computer", "dora"];
let M = null, H = null;            // melody.json, hooks.json
const ctx = new (window.AudioContext || window.webkitAudioContext)();
const cache = new Map();           // path → AudioBuffer
const sel = { america: 0, computer: 0, dora: 0 };
let favs = JSON.parse(localStorage.getItem("acd-hook-favs") || "[]");

async function buf(path) {
  if (cache.has(path)) return cache.get(path);
  const ab = await (await fetch("/" + path)).arrayBuffer();
  const b = await ctx.decodeAudioData(ab);
  cache.set(path, b);
  return b;
}

function whisperOn(g) {
  return document.getElementById("w" + g[0]).checked;
}
function curVariant() {
  return M.hook.variants[+document.getElementById("variant").value];
}
function comboDesc() {
  const v = curVariant();
  return GROUPS.map((g, i) =>
    H.groups[g][sel[g]].name + (whisperOn(g) ? "·w" : "") ).join("  +  ") + "   [" + v.label + "]";
}

// play three paths as ONE merged phrase — each word crossfades into the
// next on the beat grid ("america aaaaawwwmputerrrrr dora!"). off-grid =
// plain back-to-back stitch.
async function playPhrase(paths) {
  await ctx.resume();
  const beat = 60 / M.bpm;
  const onGrid = document.getElementById("grid").checked;
  const t0 = ctx.currentTime + 0.06;
  const onsets = [0];
  for (let i = 0; i < paths.length - 1; i++) onsets.push(onsets[i] + M.hook.beats_per_word[i] * beat);
  const phraseEnd = onsets[paths.length - 1] + M.hook.beats_per_word[paths.length - 1] * beat;
  let tight = t0;
  for (let i = 0; i < paths.length; i++) {
    const b = await buf(paths[i]);
    const src = ctx.createBufferSource();
    src.buffer = b;
    if (!onGrid) {
      src.connect(ctx.destination);
      src.start(tight);
      tight += b.duration;
      continue;
    }
    // layered merge — every word finishes: it ducks (not dies) when the
    // next word enters, and is only choked one word later. dora rings hot.
    const gn = ctx.createGain();
    src.connect(gn); gn.connect(ctx.destination);
    const at = t0 + onsets[i];
    const last = i === paths.length - 1;
    if (i > 0) { gn.gain.setValueAtTime(0, at); gn.gain.linearRampToValueAtTime(last ? 1.15 : 1, at + (last ? 0.008 : 0.05)); }
    else gn.gain.setValueAtTime(1, at);
    if (last) {
      const fadeAt = t0 + phraseEnd + 0.06;
      gn.gain.setValueAtTime(1.15, fadeAt);
      gn.gain.linearRampToValueAtTime(0, fadeAt + 0.22);
      src.start(at); src.stop(fadeAt + 0.4);
    } else {
      const duckAt = t0 + onsets[i + 1] - 0.06;
      const chokeAt = (i + 2 < paths.length ? t0 + onsets[i + 2] : t0 + phraseEnd) - 0.06;
      gn.gain.setValueAtTime(1, Math.max(duckAt, at + 0.06));
      gn.gain.linearRampToValueAtTime(i === 0 ? 0.38 : 0.30, duckAt + 0.20);
      gn.gain.setValueAtTime(i === 0 ? 0.38 : 0.30, chokeAt);
      gn.gain.linearRampToValueAtTime(0, chokeAt + 0.22);
      src.start(at); src.stop(chokeAt + 0.4);
    }
  }
}

async function play() {
  const v = curVariant();
  await playPhrase(GROUPS.map((g, i) => {
    const e = H.groups[g][sel[g]];
    return whisperOn(g) ? e.whisper[v.notes[i]] : e.notes[v.notes[i]];
  }));
  document.getElementById("now").textContent = "▶ " + comboDesc();
}

function preview(g, i) {
  const v = curVariant();
  const e = H.groups[g][i];
  const note = v.notes[GROUPS.indexOf(g)];
  const path = whisperOn(g) ? e.whisper[note] : e.notes[note];
  buf(path).then((b) => {
    const src = ctx.createBufferSource();
    src.buffer = b; src.connect(ctx.destination); src.start();
  });
}

function renderLists() {
  for (const g of GROUPS) {
    const root = document.getElementById("list-" + g);
    root.innerHTML = "";
    H.groups[g].forEach((e, i) => {
      const d = document.createElement("div");
      d.className = "clip" + (sel[g] === i ? " sel" : "");
      const said = e.word ? '“' + e.word + '” ' : "";
      d.innerHTML = '<span class="pv">▶</span> ' + e.name + ' <span class="said">' + said + '</span>' +
        '<span class="hz">' + (e.dur ? e.dur.toFixed(1) + "s · " : "") + (e.srcHz ? e.srcHz.toFixed(0) + "hz" : "—") + "</span>";
      d.onclick = (ev) => {
        sel[g] = i; renderLists();
        if (ev.target.classList.contains("pv")) preview(g, i); else play();
      };
      root.appendChild(d);
    });
  }
  document.getElementById("favcount").textContent = favs.length + " kept";
}

function surf() {
  for (const g of GROUPS) sel[g] = Math.floor(Math.random() * H.groups[g].length);
  document.getElementById("variant").value = Math.floor(Math.random() * M.hook.variants.length);
  for (const g of GROUPS) document.getElementById("w" + g[0]).checked = Math.random() < 0.25;
  renderLists();
  document.querySelectorAll(".clip.sel").forEach((el) => el.scrollIntoView({ block: "nearest" }));
  play();
}

function keep() {
  const v = curVariant();
  favs.push({
    variant: +document.getElementById("variant").value,
    label: v.label,
    picks: Object.fromEntries(GROUPS.map((g, i) => [g, {
      name: H.groups[g][sel[g]].name,
      note: v.notes[i],
      whisper: whisperOn(g),
      path: whisperOn(g) ? H.groups[g][sel[g]].whisper[v.notes[i]] : H.groups[g][sel[g]].notes[v.notes[i]],
    }])),
  });
  localStorage.setItem("acd-hook-favs", JSON.stringify(favs));
  renderFavs(); renderLists();
}

function renderFavs() {
  const root = document.getElementById("favs");
  root.innerHTML = favs.length ? "<h3>★ kept</h3>" : "";
  favs.forEach((f, i) => {
    const d = document.createElement("div");
    d.className = "fav";
    const desc = GROUPS.map((g) => f.picks[g].name + (f.picks[g].whisper ? "·w" : "")).join(" + ") + " [" + f.label + "]";
    d.innerHTML = "<button>▶</button><button>✕</button><span>" + desc + "</span>";
    const [pb, xb] = d.querySelectorAll("button");
    pb.onclick = () => playPhrase(GROUPS.map((g) => f.picks[g].path));
    xb.onclick = () => { favs.splice(i, 1); localStorage.setItem("acd-hook-favs", JSON.stringify(favs)); renderFavs(); renderLists(); };
    root.appendChild(d);
  });
}

document.getElementById("play").onclick = play;
document.getElementById("surf").onclick = surf;
document.getElementById("fav").onclick = keep;
document.getElementById("export").onclick = () => {
  const a = document.createElement("a");
  a.href = URL.createObjectURL(new Blob([JSON.stringify(favs, null, 2)], { type: "application/json" }));
  a.download = "hooks-favorites.json";
  a.click();
};
const CYCLE = { arrowup: ["america", -1], arrowdown: ["america", 1], arrowleft: ["computer", -1],
                arrowright: ["computer", 1], ",": ["dora", -1], ".": ["dora", 1] };
document.addEventListener("keydown", (e) => {
  if (e.target.tagName === "INPUT" || e.target.tagName === "SELECT") return;
  const k = e.key.toLowerCase();
  if (k === " ") { e.preventDefault(); play(); }
  else if (k === "s") surf();
  else if (k === "f") keep();
  else if (CYCLE[k]) {
    e.preventDefault();
    const [g, d] = CYCLE[k];
    sel[g] = (sel[g] + d + H.groups[g].length) % H.groups[g].length;
    renderLists();
    document.querySelectorAll(".clip.sel").forEach((el) => el.scrollIntoView({ block: "nearest" }));
    play();
  }
});

Promise.all([fetch("/melody.json").then((r) => r.json()), fetch("/hooks.json").then((r) => r.json())])
  .then(([m, h]) => {
    M = m; H = h;
    const vs = document.getElementById("variant");
    M.hook.variants.forEach((v, i) => {
      const o = document.createElement("option");
      o.value = i; o.textContent = i + " · " + v.label;
      vs.appendChild(o);
    });
    renderLists(); renderFavs();
  });
</script>
`;
  // bake the combo count into the title
  let combos = "?";
  const hooksPath = join(ROOT, "hooks.json");
  if (existsSync(hooksPath)) {
    const h = JSON.parse(readFileSync(hooksPath, "utf8"));
    combos = GROUPS.reduce((a, g) => a * (h.groups[g]?.length || 1), 1).toLocaleString();
  }
  writeFileSync(join(ROOT, "hooks.html"), html.replace("${COMBOS}", combos));
  console.log(`✓ hooks.html (${combos} combos) — node bin/serve.mjs, then open /hooks.html`);
}

// ── flight: stitch a batch of random combos to mp3 ──────────────────────
function mulberry32(seed) {
  let a = seed >>> 0;
  return () => {
    a |= 0; a = (a + 0x6d2b79f5) | 0;
    let t = Math.imul(a ^ (a >>> 15), 1 | a);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

function stitchCombo(picks, outAbs, { title }) {
  // picks: [{path}, {path}, {path}] — already snapped to target notes.
  // mixed as ONE merged phrase on the beat grid: each word crossfades into
  // the next ("america aaaaawwwmputerrrrr dora!"), dora choked at the bar.
  const beat = 60 / melody.bpm;
  const lens = melody.hook.beats_per_word;
  const onsets = [0];
  for (let i = 0; i < picks.length - 1; i++) onsets.push(onsets[i] + lens[i] * beat);
  const phraseEnd = onsets[picks.length - 1] + lens[picks.length - 1] * beat;
  const N = Math.ceil((phraseEnd + 0.45) * SR);
  const mix = new Float32Array(N);
  picks.forEach((p, i) => {
    const { samples } = readWavMono(join(ROOT, p.path));
    const last = i === picks.length - 1;
    const attack = i === 0 ? 0 : last ? 0.008 : 0.05;
    const gain = last ? 1.15 : 1;
    // layered merge (clip-relative): duck under the next word, choke one
    // word later; dora rings hot past the phrase end.
    const duckAt = last ? Infinity : onsets[i + 1] - 0.06 - onsets[i];
    const duckTo = i === 0 ? 0.38 : 0.30;
    const chokeAt = last
      ? phraseEnd + 0.06 - onsets[i]
      : (i + 2 < picks.length ? onsets[i + 2] : phraseEnd) - 0.06 - onsets[i];
    const chokeOver = 0.22;
    const s0 = Math.floor(onsets[i] * SR);
    for (let j = 0; j < samples.length; j++) {
      const tt = j / SR;
      let lvl = attack > 0 && tt < attack ? tt / attack : 1;
      if (tt >= duckAt) lvl *= Math.max(duckTo, 1 - ((tt - duckAt) / 0.20) * (1 - duckTo));
      if (tt >= chokeAt) lvl *= Math.max(0, 1 - (tt - chokeAt) / chokeOver);
      if (tt > chokeAt + chokeOver) break;
      const k = s0 + j;
      if (k < N) mix[k] += samples[j] * lvl * gain;
    }
  });
  let peak = 0;
  for (let i = 0; i < N; i++) { const a = Math.abs(mix[i]); if (a > peak) peak = a; }
  if (peak > 1) for (let i = 0; i < N; i++) mix[i] /= peak;
  const rawPath = join(tmpdir(), `acd-hooks-${process.pid}-${Math.random().toString(36).slice(2, 8)}.f32.raw`);
  const b = Buffer.alloc(N * 4);
  for (let i = 0; i < N; i++) b.writeFloatLE(mix[i], i * 4);
  writeFileSync(rawPath, b);
  const ok = spawnSync("ffmpeg", [
    "-hide_banner", "-y", "-loglevel", "error",
    "-f", "f32le", "-ar", String(SR), "-ac", "1", "-i", rawPath,
    "-c:a", "libmp3lame", "-q:a", "2",
    "-metadata", `title=${title}`,
    "-metadata", "artist=jeffrey",
    "-metadata", "album=pixsies",
    outAbs,
  ], { stdio: ["ignore", "ignore", "inherit"] }).status === 0;
  try { unlinkSync(rawPath); } catch {}
  return ok;
}

function flight() {
  const H = JSON.parse(readFileSync(join(ROOT, "hooks.json"), "utf8"));
  const count = Number(flags.count ?? 24);
  const rand = mulberry32(Number(flags.seed ?? Date.now()) | 0);
  const outDir = join(ROOT, "out", "hooks");
  mkdirSync(outDir, { recursive: true });
  console.log(`# flight — ${count} random combos → out/hooks/`);
  const made = [];
  for (let i = 0; i < count; i++) {
    const vIdx = flags.variant !== undefined ? Number(flags.variant) : Math.floor(rand() * melody.hook.variants.length);
    const v = melody.hook.variants[vIdx];
    const picks = GROUPS.map((g, w) => {
      const e = H.groups[g][Math.floor(rand() * H.groups[g].length)];
      const whisper = flags.whisper ? true : rand() < 0.2;
      return { name: e.name, whisper, path: whisper ? e.whisper[v.notes[w]] : e.notes[v.notes[w]] };
    });
    const slug = picks.map((p) => p.name + (p.whisper ? "w" : "")).join("__");
    const out = join(outDir, `${String(i + 1).padStart(2, "0")}__v${vIdx}__${slug}.mp3`);
    if (stitchCombo(picks, out, { title: `americomputadora hook ${i + 1} (${v.label})` })) {
      made.push(out);
      console.log(`  ✓ ${basename(out)}`);
    }
  }
  // one contact-sheet mp3: every combo back-to-back with a breath between
  if (made.length) {
    const listPath = join(tmpdir(), `acd-flight-${process.pid}.txt`);
    const silence = join(tmpdir(), `acd-flight-sil-${process.pid}.mp3`);
    spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
      "-f", "lavfi", "-i", `anullsrc=r=${SR}:cl=mono`, "-t", "0.7",
      "-c:a", "libmp3lame", "-q:a", "2", silence], { stdio: "ignore" });
    writeFileSync(listPath, made.flatMap((m) => [`file '${m}'`, `file '${silence}'`]).join("\n") + "\n");
    const all = join(outDir, "_flight-all.mp3");
    spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
      "-f", "concat", "-safe", "0", "-i", listPath, "-ac", "1", "-ar", String(SR),
      "-c:a", "libmp3lame", "-q:a", "2", all], { stdio: ["ignore", "ignore", "inherit"] });
    try { unlinkSync(listPath); unlinkSync(silence); } catch {}
    console.log(`\n✓ ${all}  (all ${made.length} in one listen)`);
  }
}

// ── render favorites exported from hooks.html ───────────────────────────
function renderFavs() {
  const favPath = resolve(flags.fav ?? join(ROOT, "hooks-favorites.json"));
  if (!existsSync(favPath)) {
    console.error(`✗ no favorites file at ${favPath} — export from hooks.html first`);
    process.exit(1);
  }
  const favs = JSON.parse(readFileSync(favPath, "utf8"));
  const outDir = join(ROOT, "out", "hooks");
  mkdirSync(outDir, { recursive: true });
  favs.forEach((f, i) => {
    const picks = GROUPS.map((g) => f.picks[g]);
    const slug = picks.map((p) => p.name + (p.whisper ? "w" : "")).join("__");
    const out = join(outDir, `fav-${String(i + 1).padStart(2, "0")}__${f.label}__${slug}.mp3`);
    if (stitchCombo(picks, out, { title: `americomputadora fav ${i + 1} (${f.label})` })) {
      console.log(`  ✓ ${basename(out)}`);
    }
  });
}

// ── main ────────────────────────────────────────────────────────────────
if (cmd === "build") await build();
else if (cmd === "page") page();
else if (cmd === "flight") flight();
else if (cmd === "render") renderFavs();
else { await build(); page(); }
