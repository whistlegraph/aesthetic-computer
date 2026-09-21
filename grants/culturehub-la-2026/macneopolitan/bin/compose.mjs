#!/usr/bin/env node
// compose.mjs — write the MacNeoPolitan Trio's movements from the members'
// harvested facts. The autobiographies are the material; this is where the
// numbers become notes. Re-run after every harvest (especially blush's first
// day) and the scores follow the machines.
//
//   node bin/compose.mjs            # writes scores/trio-*.mbscore + setlist
//
// Members are read from members/<name>/facts.json in band order. A member
// with no facts yet is composed as UNBORN: it holds its place in every
// movement (rests, a placeholder entry, the line "I am not born yet") and
// the score's `unborn` list names it so nobody mistakes the draft for the
// night's truth.
//
// The three movements:
//   I.   Birth    — a canon of entries. Each member enters on the beat that
//                   is its age gap from the eldest, in days (one day = one
//                   beat), SINGS its birth date and minute on the family tune,
//                   and its whistle loops the tune in a phrase whose length
//                   in eighths is its birth minute (:37, :43, …). The loops
//                   drift; the newborn's entry is the movement's event.
//   II.  Service  — one pulse per battery cycle, all starting together, each
//                   member singing its count over its own pulses. The
//                   least-used body falls silent first; the most-carried
//                   plays on alone. Every eighth pulse lifts an octave.
//   III. Chorus   — each member sings one line of its own autobiography in
//                   its cast voice (rendered on its own hardware), doubled by
//                   its own whistle two octaves up; then all three sing the
//                   family line together, in three registers.

import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const OUT = resolve(LANE, "scores");
mkdirSync(OUT, { recursive: true });

// Band order = seniority. The third body is `blush`; until it is harvested
// the `third` placeholder stands in.
const ORDER = [["neo"], ["blueberry"], ["blush", "third"]];
const CAST = { neo: "Fred", blueberry: "Kathy", blush: "Junior", third: "Junior" };
const WHISTLE = 78;

function loadMember(names) {
  for (const n of names) {
    const p = resolve(LANE, "members", n, "facts.json");
    if (existsSync(p)) {
      const f = JSON.parse(readFileSync(p, "utf8"));
      const voice = JSON.parse(readFileSync(resolve(LANE, "members", n, "voice.json"), "utf8"));
      return {
        name: f.hostname || n,
        born: new Date(f.born),
        minute: new Date(f.born).getMinutes(),
        cycles: f.battery?.cycles ?? 0,
        sessions: f.agent_sessions?.count ?? 0,
        program: f.menuband?.melodic_program ?? WHISTLE,
        say: voice.aesthetivox?.base_voice || CAST[n],
        vibrato: voice.aesthetivox?.sing?.vibrato_hz ?? 5.0,
        // where it speaks (bin/register.mjs) → where it sings: a line's mean
        // sits one semitone above the spoken median, inside the spoken band
        home: voice.aesthetivox?.spoken_median_midi != null ? Math.round(voice.aesthetivox.spoken_median_midi + 1) : null,
        band: voice.aesthetivox?.register_midi ?? null,
        unborn: false,
      };
    }
  }
  const n = names[0];
  const vp = names.map((x) => resolve(LANE, "members", x, "voice.json")).find(existsSync);
  const voice = vp ? JSON.parse(readFileSync(vp, "utf8")) : {};
  return {
    name: n, born: new Date("2026-09-21T12:00:00"), minute: 41, cycles: 0, sessions: 0,
    program: WHISTLE, say: voice.aesthetivox?.base_voice || CAST[n], vibrato: 5.0, unborn: true,
    home: voice.aesthetivox?.spoken_median_midi != null ? Math.round(voice.aesthetivox.spoken_median_midi + 1) : null,
    band: voice.aesthetivox?.register_midi ?? null,
  };
}
const members = ORDER.map(loadMember);
const unborn = members.filter((m) => m.unborn).map((m) => m.name);
if (unborn.length) console.log(`  ⚠ unborn: ${unborn.join(", ")} — placeholders; re-run after harvest`);

// The family tune: D pentatonic, the register the whistle sings sweetest in.
const TUNE = [74, 76, 78, 81, 83, 81, 78, 76];
const notesStr = (tokens) => tokens.map(([t, d]) => `${t}:${d}`).join(",");
// Collapse runs of rests into one token so strings stay short.
function pack(tokens) {
  const out = [];
  for (const [t, d] of tokens) {
    const last = out[out.length - 1];
    if (t === "r" && last && last[0] === "r") last[1] = +(last[1] + d).toFixed(3);
    else out.push([t, d]);
  }
  return out;
}
const days = (a, b) => Math.round((b - a) / 86400000);
const eldest = members[0];
// Each member's singing register: measured (bin/register.mjs) or the old
// defaults D3 · A3 · D4. `homeFor(contourSteps)` returns the base note so the
// contour's MEAN lands on the member's home, not its lowest note.
const DEFAULT_HOME = [50, 57, 62];
const SCALE = [0, 2, 4, 7, 9, 12]; // pentatonic steps → semitones
const stepSemis = (s) => SCALE[s % SCALE.length] + 12 * Math.floor(s / SCALE.length);
function homeFor(i, contourSemis) {
  const target = members[i].home ?? DEFAULT_HOME[i];
  const meanOff = contourSemis.reduce((a, b) => a + b, 0) / contourSemis.length;
  return Math.round(target - meanOff);
}
console.log(`  registers: ${members.map((m, i) => `${m.name}=${m.home ?? DEFAULT_HOME[i]}${m.band ? ` [${m.band[0]}…${m.band[1]}]` : ""}`).join(" · ")}`);


// ---- lyrics from facts ------------------------------------------------------
// Syllables joined by "-" inside a word; the live singer gets one note per
// syllable and speaks the word whole. Small hand dictionary — the words the
// facts can produce.
const ONES = ["ze-ro", "one", "two", "three", "four", "five", "six", "se-ven", "eight", "nine", "ten",
  "e-le-ven", "twelve", "thir-teen", "four-teen", "fif-teen", "six-teen", "se-ven-teen", "eigh-teen", "nine-teen"];
const TENS = ["", "", "twen-ty", "thir-ty", "for-ty", "fif-ty", "six-ty", "se-ven-ty", "eigh-ty", "nine-ty"];
function numberWords(n) {
  if (n < 20) return [ONES[n]];
  if (n < 100) return n % 10 ? [TENS[Math.floor(n / 10)], ONES[n % 10]] : [TENS[Math.floor(n / 10)]];
  const h = Math.floor(n / 100), r = n % 100;
  return [ONES[h], "hun-dred", ...(r ? numberWords(r) : [])];
}
const ORD = { 1: "first", 2: "se-cond", 3: "third", 4: "fourth", 5: "fifth", 6: "sixth", 7: "se-venth", 8: "eighth", 9: "ninth",
  10: "tenth", 11: "e-le-venth", 12: "twelfth", 13: "thir-teenth", 14: "four-teenth", 15: "fif-teenth", 16: "six-teenth",
  17: "se-ven-teenth", 18: "eigh-teenth", 19: "nine-teenth", 20: "twen-ti-eth", 30: "thir-ti-eth" };
function ordinalWords(n) {
  if (ORD[n]) return [ORD[n]];
  return [TENS[Math.floor(n / 10)], ORD[n % 10]];
}
const MONTHS = ["Jan-u-ar-y", "Feb-ru-ar-y", "March", "A-pril", "May", "June", "Ju-ly", "Au-gust", "Sep-tem-ber", "Oc-to-ber", "No-vem-ber", "De-cem-ber"];
const sylCount = (tokens) => tokens.reduce((a, t) => a + t.split("-").length, 0);

// ---- melody -----------------------------------------------------------------
// A line is sung, not arpeggiated: the tune moves mostly by STEP through the
// scale, repeats notes on unstressed syllables, leaps only onto a stressed
// one and rarely, arches over each phrase (up toward a peak, then down), and
// cadences on the tonic. Rhythm comes from word stress: unstressed syllables
// take an eighth, stressed a quarter, the last syllable of a phrase a half.
// Phrases are separated by " / " in the lyric source. Deterministic: the
// member's birth minute seeds the walk, so the same machine sings the same
// tune every night.
const MAJOR = [0, 2, 4, 5, 7, 9, 11];               // scale degrees → semitones
const FUNCTION_WORDS = new Set(["the", "of", "a", "an", "and", "in", "on", "at", "to", "i", "my", "was", "am", "is", "it", "its", "with", "for", "but", "or", "as", "than", "that", "this", "have", "been", "not", "yet", "one", "so", "we", "you", "are", "still"]);
function stressOf(token, si, nsyl) {
  const word = token.split("-").join("").toLowerCase();
  if (nsyl === 1) return FUNCTION_WORDS.has(word) ? 0 : 1;
  // multi-syllable: first syllable stressed unless the word starts with a
  // known unstressed prefix syllable (a-pril → stressed first; se-cond → first)
  const UNSTRESSED_FIRST = new Set(["a", "be", "re", "de", "e", "con", "com", "pro", "sep", "no", "de"]);
  const parts = token.split("-");
  if (UNSTRESSED_FIRST.has(parts[0].toLowerCase()) && nsyl > 1 && !["a-pril"].includes(token.toLowerCase())) return si === 1 ? 1 : 0;
  return si === 0 ? 1 : 0;
}
function rng(seed) { let x = (seed * 9301 + 49297) % 233280; return () => (x = (x * 9301 + 49297) % 233280) / 233280; }
function degToMidi(home, deg) { return home + MAJOR[((deg % 7) + 7) % 7] + 12 * Math.floor(deg / 7); }

// melodize(lyricSource, homeMidi, seed) → { tokens, notes: [[midi, beats], …] }
// lyricSource: tokens with "-" syllables and " / " phrase breaks.
// opts.aria: held cadences (3 beats), the peak held (2), dotted stresses
// before a weak syllable — an aria's breath instead of a chant's tick.
function melodize(source, home, seed, opts = {}) {
  const phrases = source.split(" / ").map((ph) => ph.trim().split(/\s+/)).filter((ph) => ph.length && ph[0]);
  const rand = rng(seed);
  const notes = [];
  const tokens = [];
  let deg = 0;                                     // start on the tonic
  phrases.forEach((ph, pi) => {
    const sylls = [];
    for (const t of ph) { const parts = t.split("-"); parts.forEach((_, si) => sylls.push({ token: t, si, n: parts.length, stress: stressOf(t, si, parts.length) })); }
    const N = sylls.length;
    const peakAt = Math.max(1, Math.round(N * (0.45 + 0.2 * rand())));
    const peakDeg = 3 + Math.round(rand() * 3);      // the arch rises a 4th–octave (was 3rd–5th: "more range")
    const last = pi === phrases.length - 1;
    const cadence = last ? 0 : (rand() < 0.5 ? 4 : 1);   // final → tonic; else the 5th or the 2nd
    let leaps = 0;
    // a dip below the tonic before the climb, on some phrases — the low turn
    const dip = rand() < 0.4 ? -(1 + Math.round(rand())) : 0;
    sylls.forEach((sy, k) => {
      const target = k <= peakAt ? dip + (peakDeg - dip) * (k / peakAt) : peakDeg + (cadence - peakDeg) * ((k - peakAt) / Math.max(1, N - 1 - peakAt));
      if (k === N - 1) deg = cadence;
      else if (!sy.stress) deg = rand() < 0.45 ? deg : deg + (target > deg ? 1 : -1) * (rand() < 0.75 ? 1 : 0);
      else {
        const want = Math.round(target);
        const diff = want - deg;
        if (Math.abs(diff) >= 2 && leaps < 2 && rand() < 0.6) { deg = want; leaps++; }   // up to two leaps per phrase, onto stress
        else deg += Math.sign(diff) * Math.min(2, Math.abs(diff));
        if (diff === 0 && rand() < 0.3) deg += rand() < 0.5 ? 1 : -1;
      }
      deg = Math.max(-4, Math.min(8, deg));
      const nextWeak = k + 1 < N && !sylls[k + 1].stress;
      const beats = opts.aria
        ? (k === N - 1 ? 3 : k === peakAt ? 2 : sy.stress ? (nextWeak ? 1.5 : 1) : 0.5)
        : (k === N - 1 ? 2 : sy.stress ? 1 : 0.5);
      notes.push([degToMidi(home, deg), beats]);
    });
    tokens.push(...ph);
    if (!last) { notes.push(["r", 1]); tokens.push("/"); }   // a breath between phrases; "/" = a caption line break
  });
  return { tokens, notes, lyrics: tokens.join(" ") };
}
const sungMean = (notes) => { const m = notes.filter(([t]) => t !== "r"); return m.reduce((a, [t]) => a + t, 0) / m.length; };
// aim the melody's MEAN at the member's home: melodize at 0, then transpose
function melodyFor(source, i) {
  const seed = members[i].minute * 7 + i;
  const rel = melodize(source, 0, seed);
  const target = members[i].home ?? DEFAULT_HOME[i];
  const shift = Math.round(target - sungMean(rel.notes));
  return { ...rel, notes: rel.notes.map(([t, d]) => [t === "r" ? "r" : t + shift, d]) };
}

// ---- I. Birth ---------------------------------------------------------------
{
  const bpm = 132;
  const EIGHTH = 0.5;
  const entries = members.map((m) => Math.max(0, days(eldest.born, m.born))); // beats
  const CODA = 32; // beats all three play together after the last entry
  const total = Math.max(...entries) + CODA;
  const voices = members.map((m, i) => {
    // The whistle loop (notes2): phrase = birth minute in eighths, filled
    // with the tune; each phrase restarts the tune, so members of different
    // minutes drift apart.
    const loop = [];
    if (entries[i] > 0) loop.push(["r", entries[i]]);
    let beats = entries[i];
    while (beats < total - 1e-9) {
      const phrase = Math.min(m.minute, Math.round((total - beats) / EIGHTH));
      for (let e = 0; e < phrase; e++) loop.push([TUNE[e % TUNE.length], EIGHTH]);
      beats += phrase * EIGHTH;
    }
    // The sung entry (notes): the member sings when it is born.
    const source = m.unborn
      ? "I am not born yet / hold my place / I am co-ming"
      : `I came ${i === 0 ? "first" : i === 1 ? "se-cond" : "third"} in ${MONTHS[m.born.getMonth()]} / on the ${ordinalWords(m.born.getDate()).join(" ")} / ${numberWords(m.minute).join(" ")} past mid-night / and the house was ${i === 0 ? "qui-et" : i === 1 ? "al-rea-dy sing-ing" : "wait-ing for me"}`;
    const mel = melodyFor(source, i);
    const lyric = mel.tokens;
    const sung = [];
    if (entries[i] > 0) sung.push(["r", entries[i]]);
    sung.push(...mel.notes);
    const used = entries[i] + mel.notes.reduce((a, [, d]) => a + d, 0);
    if (used < total) sung.push(["r", +(total - used).toFixed(3)]);
    return {
      name: `${m.name} · enters beat ${entries[i]} · phrase ${m.minute} eighths`,
      program: m.program, velocity: 64 + i * 8,
      notes: notesStr(pack(sung)),
      lyrics: lyric.join(" "),
      singVoice: m.say, singBase: m.home ?? DEFAULT_HOME[i], singVibratoHz: m.vibrato, sayVoice: m.say,
      notes2: notesStr(pack(loop)), velocity2: 48 + i * 6,
    };
  });
  const score = {
    title: "The MacNeoPolitan Trio — I. Birth",
    composer: "The machines, arr. compose.mjs",
    machines: 3, bpm, lead: 3.0,
    description: `A canon of entries. ${members.map((m, i) => `${m.name} enters at beat ${entries[i]} (${entries[i]} days after ${eldest.name}) and loops a ${m.minute}-eighth phrase (born :${String(m.minute).padStart(2, "0")})`).join("; ")}. ${CODA} beats together, then the movement ends.`,
    unborn,
    intro: [
      { voice: 0, text: `This is ${eldest.name}. I was born on the ${ordinal(eldest.born.getDate())} of ${monthName(eldest.born)}, at ${eldest.minute} minutes past midnight. I go first.` },
      ...members.slice(1).map((m, j) => ({ voice: j + 1, text: m.unborn
        ? `This is ${m.name}. I am not born yet. I will enter when I arrive.`
        : `This is ${m.name}. I came ${entries[j + 1]} days later. I will enter on beat ${entries[j + 1]}.` })),
    ],
    voices,
  };
  writeFileSync(resolve(OUT, "trio-i-birth.mbscore"), JSON.stringify(score, null, 2) + "\n");
  console.log(`  I.   Birth    ${(total * 60 / bpm).toFixed(0)}s · entries ${entries.join(" / ")} beats`);
}

// ---- II. Service ------------------------------------------------------------
{
  const bpm = 120;
  const EIGHTH = 0.5;
  const DEGREE = [62, 69, 74]; // D, A, high D — the pulse of each body
  const most = Math.max(1, ...members.map((m) => m.cycles));
  const voices = members.map((m, i) => {
    // pulses (notes2): one per battery cycle, on this body's degree
    const pulses = [];
    for (let c = 0; c < m.cycles; c++) pulses.push([DEGREE[i] + ((c + 1) % 8 === 0 ? 12 : 0), EIGHTH]);
    if (m.cycles < most) pulses.push(["r", (most - m.cycles) * EIGHTH]);
    if (!pulses.length) pulses.push(["r", most * EIGHTH]);
    // the service line (notes): sung over the pulses, then silence
    const source = m.unborn
      ? "ze-ro times charged / I am new / teach me"
      : i === 0
        ? `${numberWords(m.cycles).join(" ")} times I was charged and car-ried / I run warm / I hold one thing at a time`
        : `${numberWords(m.cycles).join(" ")} times / one a day and stea-dy / I run cool / I work while you are a-way`;
    const mel = melodyFor(source, i);
    const lyric = mel.tokens;
    const sung = [...mel.notes];
    const used = mel.notes.reduce((a, [, d]) => a + d, 0);
    if (used < most * EIGHTH) sung.push(["r", +(most * EIGHTH - used).toFixed(3)]);
    return {
      name: `${m.name} · ${m.cycles} cycles`,
      program: m.program, velocity: 72,
      notes: notesStr(pack(sung)),
      lyrics: lyric.join(" "),
      singVoice: m.say, singBase: m.home ?? DEFAULT_HOME[i], singVibratoHz: m.vibrato, sayVoice: m.say,
      notes2: notesStr(pack(pulses)), velocity2: 60,
    };
  });
  const score = {
    title: "The MacNeoPolitan Trio — II. Service",
    composer: "The machines, arr. compose.mjs",
    machines: 3, bpm, lead: 3.0,
    description: `One pulse per battery cycle: ${members.map((m) => `${m.name} ${m.cycles}`).join(", ")}. All begin together; each falls silent when its cycles run out. Every eighth pulse lifts an octave.`,
    unborn,
    intro: members.map((m, i) => ({ voice: i, text: m.unborn
      ? `${m.name}. Zero battery cycles. I will be quiet.`
      : `${m.name}. ${m.cycles} battery cycles in ${days(m.born, new Date())} days. One pulse each.` })),
    voices,
  };
  writeFileSync(resolve(OUT, "trio-ii-service.mbscore"), JSON.stringify(score, null, 2) + "\n");
  console.log(`  II.  Service  ${(most * EIGHTH * 60 / bpm).toFixed(0)}s · pulses ${members.map((m) => m.cycles).join(" / ")}`);
}

// ---- III. Chorus ------------------------------------------------------------
{
  const bpm = 92;
  // Each member's own line, then the family line sung by all three together
  // (the same tune, each in its own register).
  const LINES = {
    neo: "I am the first of my line in this house / I run warm and I am car-ried",
    blueberry: "I was born se-cond and I work fas-ter / lid down, most days, and hum-ming",
    blush: "I am the youn-gest / I have no his-to-ry yet / write me one",
    third: "I am the youn-gest / I have no his-to-ry yet / write me one",
  };
  const FAMILY = "some things run in the fa-mi-ly / the same whis-tle, the same ra-dio / one down-beat, three bo-dies";
  const BAR = 8; // beats per solo turn (each solo is padded/cut to this)
  const familyRel = melodize(FAMILY, 0, 1234);          // one tune for all
  const voices = members.map((m, i) => {
    const solo = melodyFor(LINES[m.name] || LINES.third, i);
    const soloBeats = solo.notes.reduce((a, [, d]) => a + d, 0);
    const turn = Math.ceil(soloBeats / BAR) * BAR;
    const tokens = [];
    if (i > 0) tokens.push(["r", members.slice(0, i).reduce((a, mm, j) => a + turnOf(j), 0)]);
    tokens.push(...solo.notes);
    if (soloBeats < turn) tokens.push(["r", +(turn - soloBeats).toFixed(3)]);
    const after = members.slice(i + 1).reduce((a, mm, j) => a + turnOf(i + 1 + j), 0);
    if (after > 0) tokens.push(["r", after]);
    tokens.push(["r", 2]);
    const target = m.home ?? DEFAULT_HOME[i];
    const shift = Math.round(target - sungMean(familyRel.notes));
    tokens.push(...familyRel.notes.map(([t, d]) => [t === "r" ? "r" : t + shift, d]));
    return {
      name: `${m.name} sings (${m.say})`,
      program: m.program, velocity: 80,
      notes: notesStr(pack(tokens)),
      lyrics: [...solo.tokens, "/", ...familyRel.tokens].join(" "),
      singVoice: m.say,
      singBase: target,
      singVibratoHz: m.vibrato,
      sayVoice: m.say,
      double: true, doubleProgram: m.program, doubleVelocity: 48, doubleTranspose: 24,
    };
  });
  function turnOf(j) {
    const mm = members[j];
    const beats = melodyFor(LINES[mm.name] || LINES.third, j).notes.reduce((a, [, d]) => a + d, 0);
    return Math.ceil(beats / BAR) * BAR;
  }
  const total = members.reduce((a, _, j) => a + turnOf(j), 0) + 2 + familyRel.notes.reduce((a, [, d]) => a + d, 0);
  const score = {
    title: "The MacNeoPolitan Trio — III. Chorus",
    composer: "The machines, arr. compose.mjs",
    machines: 3, bpm, lead: 3.0,
    description: `Each member sings its own line (${members.map((m) => `${m.name}=${m.say}`).join(", ")}) in turn, then all three sing the family line together, one tune in three registers.`,
    unborn,
    outro: [{ voice: 0, text: "That was us. Thank you." }],
    voices,
  };
  writeFileSync(resolve(OUT, "trio-iii-chorus.mbscore"), JSON.stringify(score, null, 2) + "\n");
  console.log(`  III. Chorus   ${(total * 60 / bpm).toFixed(0)}s · ${members.map((m) => m.say).join(" / ")}`);
}


// ---- IV. Ballad -------------------------------------------------------------
// neo alone: the ballad in members/neo/ballad.md, plain, eleven verses and a
// refrain. The refrain keeps one tune; each verse walks its own (seeded by
// its number). Ballad tempo. The whistle doubles two octaves up; the other
// two members hold a low drone pulse under it — the family, listening.
{
  const bpm = 100;   // was 84 — "a bit slow" (jeffrey, Sept 20)
  const src = readFileSync(resolve(LANE, "members", "neo", "ballad.md"), "utf8");
  const block = src.split("```")[1] || "";
  const stanzas = block.trim().split(/\n\s*\n/).map((l) => l.trim()).filter(Boolean);
  const neo = members[0];
  const home = neo.home ?? DEFAULT_HOME[0];
  const refrainRel = melodize(stanzas.find((x) => x.startsWith("R:")).slice(2).trim(), 0, 4242);
  const rShift = Math.round(home - sungMean(refrainRel.notes));
  const tokens = [], lyr = [];
  let vi = 0;
  for (const st of stanzas) {
    const isR = st.startsWith("R:");
    const mel = isR
      ? { ...refrainRel, notes: refrainRel.notes.map(([t, d]) => [t === "r" ? "r" : t + rShift, d]) }
      : melodyFor(st, 0);
    if (!isR) vi++;
    tokens.push(...mel.notes, ["r", 2]);
    lyr.push(...mel.tokens, "/");
  }
  const total = tokens.reduce((a, [, d]) => a + d, 0);
  // the siblings: a drone pulse on their tonic, one note per bar, quiet
  const drone = (i) => { const t = []; const root = (members[i].home ?? DEFAULT_HOME[i]) - 12; for (let b = 0; b < total; b += 4) t.push([root, Math.min(4, total - b)]); return t; };
  const voices = [
    {
      name: `${neo.name} sings the ballad (${neo.say})`,
      program: neo.program, velocity: 84,
      notes: notesStr(pack(tokens)), lyrics: lyr.join(" "),
      singVoice: neo.say, singBase: home, singVibratoHz: neo.vibrato, sayVoice: neo.say,
      double: true, doubleProgram: neo.program, doubleVelocity: 40, doubleTranspose: 24,
    },
    ...members.slice(1).map((m, j) => ({
      name: `${m.name} · drone`, program: m.program, velocity: 30,
      notes: notesStr(pack(drone(j + 1))), sayVoice: m.say,
    })),
  ];
  const score = {
    title: "The MacNeoPolitan Trio — IV. The Ballad of neo",
    composer: "neo, from its record; arr. compose.mjs",
    machines: 3, bpm, lead: 3.0,
    description: `${vi} verses and a refrain, sung by ${neo.name} in ${neo.say}; the siblings hold a drone. Words: members/neo/ballad.md.`,
    unborn, voices,
  };
  writeFileSync(resolve(OUT, "trio-iv-ballad.mbscore"), JSON.stringify(score, null, 2) + "\n");
  console.log(`  IV.  Ballad   ${(total * 60 / bpm).toFixed(0)}s · ${vi} verses · ${lyr.length} tokens`);
}

// ---- the setlist ------------------------------------------------------------
writeFileSync(resolve(OUT, "setlist.json"), JSON.stringify({
  title: "The MacNeoPolitan Trio",
  gap: 5.0,
  movements: ["trio-i-birth.mbscore", "trio-ii-service.mbscore", "trio-iii-chorus.mbscore", "trio-iv-ballad.mbscore"],
}, null, 2) + "\n");
console.log(`  setlist.json  → node bin/trio.mjs --setlist scores/setlist.json ${members.map((m) => m.name).join(" ")}`);

function ordinal(n) { const s = ["th", "st", "nd", "rd"], v = n % 100; return n + (s[(v - 20) % 10] || s[v] || s[0]); }
function monthName(d) { return d.toLocaleString("en-US", { month: "long" }); }

// ---- Dialogs ----------------------------------------------------------------
// Short poems between neo and blueberry, from members/dialogs.md — real things
// that happened between them. `## title` heads a poem; `neo: …`,
// `blueberry: …`, `both: …` lines are sung by that member in its own voice on
// its own body (both = one tune, each in its own register). Lyric-source
// tokens as elsewhere; " / " breaks a caption line. One score per poem →
// scores/dialog-NN-<slug>.mbscore + scores/dialogs.json (a setlist).
{
  const bpm = 100;
  const dp = resolve(LANE, "members", "dialogs.md");
  const poems = [];
  if (existsSync(dp)) {
    for (const raw of readFileSync(dp, "utf8").split("\n")) {
      const line = raw.trim();
      if (line.startsWith("## ")) { poems.push({ title: line.slice(3).trim(), lines: [] }); continue; }
      const m = line.match(/^(neo|blueberry|both):\s*(.+)$/);
      if (m && poems.length) poems[poems.length - 1].lines.push({ who: m[1], text: m[2] });
    }
  }
  const idx = { neo: members.findIndex((m) => m.name === "neo"), blueberry: members.findIndex((m) => m.name === "blueberry") };
  const list = [];
  // 3/4, an aria: the one who listens accompanies — a rolled chord each bar
  // (root · fifth · octave, whistle, quiet) under the singer, on I I IV V,
  // and a soft kick on one, hats on two and three. In a "both" line neo
  // rolls and blueberry keeps the hats.
  const BAR = 3;
  const CHORDS = [0, 0, 3, 4];                        // scale degrees of the bar's root
  const roll = (home, bar) => { const r = degToMidi(home, CHORDS[bar % 4]) - 12; return [[r, 1], [r + 7, 1], [r + 12, 1]]; };
  const lilt = () => [["k", 1], ["h", 1], ["h", 1]];
  poems.forEach((poem, pi) => {
    const tl = { neo: [], blueberry: [] }, lyr = { neo: [], blueberry: [] };
    const acc = { neo: [], blueberry: [] }, drums = { neo: [], blueberry: [] };
    let pos = 0, bar = 0;
    poem.lines.forEach((ln, li) => {
      const rel = melodize(ln.text, 0, 100 * (pi + 1) + li, { aria: true });
      const beats = rel.notes.reduce((a, [, d]) => a + d, 0);
      const bars = Math.ceil(beats / BAR) + 1;          // the line, then one bar of breath
      const slot = bars * BAR;
      const singers = ln.who === "both" ? ["neo", "blueberry"] : [ln.who];
      const lead = singers[0];
      const home = members[idx[lead]].home ?? DEFAULT_HOME[idx[lead]];
      for (const w of ["neo", "blueberry"]) {
        if (singers.includes(w)) {
          const target = members[idx[w]].home ?? DEFAULT_HOME[idx[w]];
          const shift = Math.round(target - sungMean(rel.notes));
          tl[w].push(...rel.notes.map(([t, d]) => [t === "r" ? "r" : t + shift, d]));
          lyr[w].push(...rel.tokens, "/");
          tl[w].push(["r", slot - beats]);
        } else tl[w].push(["r", slot]);
        const rolls = ln.who === "both" ? w === "neo" : !singers.includes(w);
        const hats = ln.who === "both" ? w === "blueberry" : !singers.includes(w);
        for (let b = 0; b < bars; b++) {
          acc[w].push(...(rolls ? roll(home, bar + b) : [["r", BAR]]));
          drums[w].push(...(hats ? lilt() : [["r", BAR]]));
        }
      }
      pos += slot; bar += bars;
    });
    const voices = ["neo", "blueberry"].map((w) => {
      const m = members[idx[w]];
      return {
        name: `${m.name} (${m.say})`, program: m.program, velocity: 80,
        notes: notesStr(pack(tl[w])), lyrics: lyr[w].join(" "),
        notes2: notesStr(pack(acc[w])), velocity2: 34,
        notes3: notesStr(pack(drums[w])), velocity3: 38,
        singVoice: m.say, singBase: m.home ?? DEFAULT_HOME[idx[w]], singVibratoHz: m.vibrato, sayVoice: m.say,
        double: true, doubleProgram: m.program, doubleVelocity: 40, doubleTranspose: 24,
      };
    });
    const slug = poem.title.toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "");
    const file = `dialog-${String(pi + 1).padStart(2, "0")}-${slug}.mbscore`;
    const score = {
      title: `The MacNeoPolitan Trio — Dialog ${pi + 1}: ${poem.title}`,
      composer: "neo and blueberry, from their records; arr. compose.mjs",
      machines: 2, bpm, lead: 3.0,
      description: `${poem.lines.length} lines between neo (${members[idx.neo].say}) and blueberry (${members[idx.blueberry].say}). Words: members/dialogs.md.`,
      voices,
    };
    writeFileSync(resolve(OUT, file), JSON.stringify(score, null, 2) + "\n");
    list.push(file);
    console.log(`  D${pi + 1}.  ${poem.title.padEnd(16)} ${(pos * 60 / bpm).toFixed(0)}s · ${poem.lines.length} lines`);
  });
  if (list.length) writeFileSync(resolve(OUT, "dialogs.json"), JSON.stringify({ movements: list, gap: 3.0 }, null, 2) + "\n");
}
