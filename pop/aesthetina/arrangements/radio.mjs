// arrangements/radio.mjs — the country version cut to two minutes (64 bars),
// with new typed melodies. Every word below spells (almost) only A-major keys
// on notepat, so it can be typed straight over the changes.
//
// intro 0–4 · A1 4–12 · A2 12–20 · break 20–24 · drop 24–40 · C 40–48 · drop II 48–60 · outro 60–64

// [word, steps (one per character), ring-to step]
export const WORDS = {
  tumbleweed: ["tumbleweed", [0, 3, 6, 8, 10, 12, 14, 16, 19, 22], 30], // C♯5 F♯5 A5 B4 G5 E4 F♯4 E4 E4 D4
  rainbow: ["rainbow", [0, 3, 6, 10, 12, 16, 22], 30], //                 G♯4 A4 D5 B5 B4 G♯5 F♯4
  mountain: ["mountain", [0, 3, 6, 8, 12, 14, 16, 22], 30], //            A5 G♯5 F♯5 B5 C♯5 A4 D5 B5
  adventure: ["adventure", [0, 3, 6, 8, 10, 12, 14, 19, 22], 30], //      A4 D4 C♯4 E4 B5 C♯5 F♯5 G♯4 E4
  rodeo: ["rodeo banjo", [0, 2, 4, 6, 8, 10, 12, 14, 16, 19, 22], 30], // G♯4 G♯5 D4 E4 G♯5 · B4 A4 B5 E5 G♯5
};

const SECTIONS = [["intro", 4], ["a1", 12], ["a2", 20], ["brk", 24], ["drop", 40], ["c", 48], ["drop2", 60], ["outro", 64]];
const section = (bar) => SECTIONS.find(([, end]) => bar < end)[0];

const PLAN = [
  [4, ["tea", "taw", "tea", "daw"], 2],
  [12, ["tea", "daw", "tea", "taw"], 2],
  [20, ["tea", "daw", "rib", "ace"], 1],
  [24, ["tea", "taw", "tea", "taw", "ace", "fad", "big", "tea"], 2],
  [40, ["tea", "taw", "daw", "tea"], 2],
  [48, ["ace", "fad", "big", "tea", "tea", "taw"], 2],
];
export function chordAt(bar) {
  for (const [start, words, per] of PLAN) {
    const i = Math.floor((bar - start) / per);
    if (bar >= start && i < words.length) return words[i];
  }
  return "tea";
}

export function play({ BAR, S16, BEAT, at, KEY_MIDI, hz, B, keyDown, keyUp, spacebar, ocarina, typeLine, kick, clap, roll, strum, augmented, bootHook, ladder, hatsBar, subBar, teaPad, air, bell, dtmf, ROOT, HOOK, HOOK_STEPS, COUNTER, COUNTER_STEPS, VESPER, ADIEU, ADIEU_STEPS, E7_16, E9_16, E3_8, E7_12, E5_8 }) {
  const word = (key, bar, o = {}) => { const [w, st, until] = WORDS[key]; typeLine(w, at(bar), st, { until, amp: 0.16, ...o }); };
  const hook = (bar, o = {}) => typeLine(HOOK, at(bar), HOOK_STEPS, { until: 30, amp: 0.16, ...o });

  for (let bar = 0; bar < 64; bar++) {
    const sec = section(bar), even = bar % 2 === 0;
    const full = ["a1", "a2", "drop", "c", "drop2"].includes(sec);
    const big = sec === "drop" || sec === "drop2";

    // cursor tick on every beat
    if (bar < 63) for (let b4 = 0; b4 < 4; b4++)
      keyDown(B.keys, at(bar, b4 * 4 + 2), "cursor", sec === "intro" || sec === "brk" ? 0.3 : 0.1, { pan: -0.35 });

    // ── drums ──
    if (full || (sec === "intro" && bar >= 2) || (sec === "outro" && bar < 62)) {
      const tresillo = (sec === "a1" || sec === "a2" || sec === "c") && bar % 8 === 7;
      for (let s = 0; s < 16; s++) if (tresillo ? E3_8[s % 8] : s % 4 === 0) kick(at(bar, s));
      hatsBar(bar, big ? E9_16 : E7_16, sec !== "c", sec === "intro" ? 0.6 : 1);
      if (sec !== "intro") { clap(at(bar, 4)); clap(at(bar, 12)); }
      if (big && bar % 4 === 3) clap(at(bar, 14)); // a pickup clap into each 4
      if (sec === "c") E7_12.forEach((x, j) => x && keyDown(B.keys, at(bar) + j * BAR / 12, "bembe" + (j % 3), 0.45, { pan: 0.5, send: 0.1 }));
    }
    if (sec === "brk") {
      hatsBar(bar, E7_16, false, 0.5);
      if (bar < 22) { kick(at(bar)); kick(at(bar, 10)); }
    }
    if (full || (sec === "intro" && bar >= 2) || (sec === "outro" && bar < 62)) subBar(bar);

    // ── the melodies: what gets typed, bar by bar ──
    // intro: the word typed with a typo — "aest" then r (G♯ where h's C belongs),
    // backspace, "htina" — then typed clean over the groove, and Enter launches A1
    if (bar === 0) {
      typeLine("aestr", at(0), [0, 2, 4, 7, 10], { until: 14, amp: 0.13, send: 0.4, keyVel: 0.5 });
      spacebar(B.keys, at(0, 15), 0.45); keyUp(B.keys, at(0, 15) + 0.1, "space", 0.35); // backspace
      typeLine("htina", at(0), [18, 20, 22, 24, 26], { until: 31, amp: 0.13, send: 0.4, keyVel: 0.5 });
    }
    if (bar === 2) hook(bar, { amp: 0.15 });
    if (bar === 3) { spacebar(B.keys, at(3, 14), 0.95); keyUp(B.keys, at(3, 14) + 0.14, "space", 0.7); } // Enter
    if (sec === "a1" && even) hook(bar);
    if (sec === "a2") ({ 12: () => hook(bar), 14: () => word("tumbleweed", bar), 16: () => hook(bar), 18: () => word("rainbow", bar) })[bar]?.();
    if (sec === "a2" && (bar === 12 || bar === 16)) typeLine(COUNTER, at(bar), COUNTER_STEPS, { octave: -1, amp: 0.08, pan: 0.35, until: 30, keyVel: 0.35 });
    if (big && even) hook(bar, { amp: 0.17 });
    if (sec === "drop" && even && bar >= 26) typeLine(VESPER, at(bar), HOOK_STEPS, { until: 30, amp: 0.09, pan: -0.45, keyVel: 0.4 });
    if (sec === "drop2" && even) word("tumbleweed", bar, { amp: 0.1, pan: -0.45, keyVel: 0.4, octave: bar >= 56 ? 0 : -1 });
    if (sec === "drop2" && bar >= 56 && even) hook(bar, { amp: 0.08, octave: 1, pan: 0.4, send: 0.5 }); // the hook doubled up top for the last lap
    const cWord = { 40: "mountain", 42: "rainbow", 44: "adventure", 46: "rodeo" }[bar];
    if (cWord) word(cWord, bar);
    if (sec === "c" && (bar === 42 || bar === 46)) hook(bar, { amp: 0.06, octave: -1, pan: 0.45, send: 0.5 }); // the hook remembered underneath
    if (bar === 60) typeLine(ADIEU, at(bar), ADIEU_STEPS, { until: 40, amp: 0.15, send: 0.6 });
    if (bar === 62) hook(bar, { amp: 0.1, send: 0.7 });

    // prolation: mid vessel 2×, bass vessel 4×
    if ((sec === "a1" || big) && bar % 4 === 0) augmented(at(bar), 2, -1, 0.08, B.pad);
    if ((bar === 24 || bar === 32 || bar === 48)) augmented(at(bar), 4, -2, 0.16, B.bass);

    // ── harmony ──
    const ch = chordAt(bar);
    if (sec === "a1" && bar % 4 === 0) teaPad(bar, 4, 0.05);
    if ((sec === "a2" || sec === "c" || big) && even) strum(ch, at(bar), BAR * 2 - 0.05, { amp: sec === "c" ? 0.06 : 0.045 });
    if (sec === "brk") strum(ch, at(bar), BAR - 0.05, { amp: 0.065 });
    if (sec === "intro" && bar === 0) teaPad(0, 4, 0.06);
    if (sec === "outro" && bar === 60) teaPad(60, 4, 0.06);

    // ── break: the tuplet ladder over its last two bars ──
    if (bar === 22) ladder(at(bar));
    if (bar === 63) { spacebar(B.keys, at(bar, 8), 0.8); keyUp(B.keys, at(bar, 8) + 0.12, "space", 0.6); }

    // ── country colour ──
    if (bar === 0) air(at(0), 4 * BAR, [57, 61, 64], 0.06);
    if (bar === 20) air(at(20), 4 * BAR, [57, 61, 64, 69], 0.09);
    if (bar === 40) air(at(40), 8 * BAR, [57, 62, 66], 0.035);
    if (bar === 60) air(at(60), 4 * BAR, [57, 61, 64], 0.05);
    if ([0, 4, 20, 24, 40, 48, 60].includes(bar)) bell(at(bar), 45, "church", "bronze", 0.35, 0, 6);
    if (full && even) {
      const tn = [...ch].map((c) => KEY_MIDI[c] + 12).sort((x, y) => x - y);
      [2, 6, 10, 14].forEach((s16, j) => bell(at(bar, s16), tn[j % tn.length], "handbell", "brass", 0.07, [-0.5, 0.5][j % 2], 2));
    }
    if (full) {
      const rootSemis = ROOT[ch] + 24 - 79;
      for (let s16 = 0; s16 < 16; s16++) if (E5_8[s16 % 8] && s16 % 4) dtmf(at(bar, s16), rootSemis + (s16 >= 8 ? 7 : 0), 0.09, 0.06, 0.35);
    }
    if ([3, 11, 19, 39, 47, 59].includes(bar)) roll(at(bar, 12), 4, 0.45);
  }
}
