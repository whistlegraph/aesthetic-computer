// arrangements/reveal.mjs — the slow reveal (96 bars, 3:00). The radio cut's material,
// let in one layer at a time, with a section that belongs to each family of sound:
//
// boot 0–8     switches alone; the word gains breath a few letters at a time
// kick 8–16    the spacebar kick, then hats; the statements begin, one voice alone
// A 16–32      bass + claps, then strums and bells; a new line every two bars, a harmony joins at 24
// keys 32–40   the switches' section: konnakol, bembe clicks, rolls, the tuplet ladder
// drop 40–56   everything
// bells 56–64  drums and bass out: bells, air, the choir, aeiou
// C 64–72      the typed tunes over a tresillo
// build 72–76  ladder, kick thinning out
// drop II 76–92  everything, on the mirrored changes, the hook doubled up top for the last lap
// outro 92–96  adieu, one spacebar alone

export const WORDS = {
  tumbleweed: ["tumbleweed", [0, 3, 6, 8, 10, 12, 14, 16, 19, 22], 30],
  rainbow: ["rainbow", [0, 3, 6, 10, 12, 16, 22], 30],
  mountain: ["mountain", [0, 3, 6, 8, 12, 14, 16, 22], 30],
  adventure: ["adventure", [0, 3, 6, 8, 10, 12, 14, 19, 22], 30],
  rodeo: ["rodeo banjo", [0, 2, 4, 6, 8, 10, 12, 14, 16, 19, 22], 30],
};

const SECTIONS = [["boot", 8], ["kick", 16], ["a", 32], ["keys", 40], ["drop", 56], ["bells", 64], ["c", 72], ["build", 76], ["drop2", 92], ["outro", 96]];
const section = (bar) => SECTIONS.find(([, end]) => bar < end)[0];

const PLAN = [
  [16, ["tea", "taw", "tea", "daw", "tea", "daw", "tea", "taw"], 2],
  [32, ["tea", "daw", "rib", "ace", "tea", "daw", "rib", "ace"], 1],
  [40, ["tea", "taw", "tea", "taw", "ace", "fad", "big", "tea"], 2],
  [56, ["tea", "tea", "daw", "tea"], 2],
  [64, ["tea", "taw", "daw", "tea"], 2],
  [72, ["tea", "daw", "rib", "ace"], 1],
  [76, ["ace", "fad", "big", "tea", "ace", "fad", "big", "tea"], 2],
];
export function chordAt(bar) {
  for (const [start, words, per] of PLAN) {
    const i = Math.floor((bar - start) / per);
    if (bar >= start && i < words.length) return words[i];
  }
  return "tea";
}

// the level arc (bin/master.mjs, after the bus): dB by section, so the drops arrive louder than
// the verses and the last lap is the top. Each change ramps over the half bar before it.
export const LEVELS = [[0, 0], [8, -2], [16, -1.5], [32, -0.5], [40, 0], [56, -1.5], [64, -1], [72, -0.5], [76, 0], [84, 0.5], [92, -1]];

// the room's impacts (bin/master.mjs): every stem is a body on a spring around the listener.
// An impact throws them outward, each by its own mass and direction; they slide out (a
// Doppler bend), ring across the head and settle. keepTime grounds the drums on the long ones.
export const IMPACTS = [
  { bar: 16, kind: "snap", duration: 1.4, strength: 0.6, hz: 2.4, damping: 1.8, glitch: 0.3, attack: 0.03, release: 0.3 },
  { bar: 38, kind: "gravity", duration: 3.6, strength: 0.7, hz: 0.5, damping: 0.4, glitch: 0.08, attack: 0.3, release: 0.5, keepTime: 0.4 },
  { bar: 40, kind: "blast", duration: 4.6, strength: 1.4, hz: 0.95, damping: 0.6, glitch: 0.34, attack: 0.03, release: 0.6, dispersion: 0.24 },
  { bar: 48, kind: "recoil", duration: 3.4, strength: 0.95, hz: 1.2, damping: 0.75, glitch: 0.2, attack: 0.06, release: 0.7 },
  { bar: 56, kind: "exhale", duration: 5.5, strength: 0.5, hz: 0.5, damping: 0.5, glitch: 0.05, attack: 0.25, release: 1.2 },
  { bar: 64, kind: "snap", duration: 1.6, strength: 0.7, hz: 2.2, damping: 1.6, glitch: 0.3, attack: 0.03, release: 0.3 },
  { bar: 74, kind: "gravity", duration: 3.6, strength: 0.8, hz: 0.5, damping: 0.4, glitch: 0.1, attack: 0.3, release: 0.5, keepTime: 0.4 },
  { bar: 76, kind: "burst", duration: 4.8, strength: 1.55, hz: 1.3, damping: 0.8, glitch: 0.45, attack: 0.02, release: 0.6, dispersion: 0.3 },
  { bar: 84, kind: "shatter", duration: 2.4, strength: 1.15, hz: 2.1, damping: 1.2, glitch: 0.42, attack: 0.03, release: 0.4 },
  { bar: 92, kind: "exhale", duration: 5, strength: 0.45, hz: 0.55, damping: 0.55, glitch: 0.05, attack: 0.2, release: 1.1 },
];

export function play({ BAR, S16, BEAT, at, KEY_MIDI, hz, B, keyDown, keyUp, spacebar, ocarina, typeLine, kick, clap, snare, crash, tomFill, guitar, synthPad, pluck, piano, roll, strum, augmented, bootHook, ladder, hatsBar, subBar, teaPad, air, bell, dtmf, ROOT, HOOK, HOOK_STEPS, COUNTER, COUNTER_STEPS, VESPER, ADIEU, ADIEU_STEPS, AEIOU, E7_16, E9_16, E3_8, E7_12, E5_8 }) {
  const word = (key, bar, o = {}) => { const [w, st, until] = WORDS[key]; typeLine(w, at(bar), st, { until, amp: 0.16, ...o }); };
  const hook = (bar, o = {}) => typeLine(HOOK, at(bar), HOOK_STEPS, { until: 30, amp: 0.16, ...o });
  const bembe = (bar, vel = 0.45) => E7_12.forEach((x, j) => x && keyDown(B.keys, at(bar) + j * BAR / 12, "bembe" + (j % 3), vel, { pan: 0.5, send: 0.1 }));
  const handbells = (bar, ch, amp = 0.07) => {
    const tn = [...ch].map((c) => KEY_MIDI[c] + 12).sort((x, y) => x - y);
    [2, 6, 10, 14].forEach((s16, j) => bell(at(bar, s16), tn[j % tn.length], "handbell", "brass", amp, [-0.5, 0.5][j % 2], 2));
  };

  for (let bar = 0; bar < 96; bar++) {
    const sec = section(bar), even = bar % 2 === 0, ch = chordAt(bar);
    const big = sec === "drop" || sec === "drop2";

    // cursor tick on every beat: loud while it is alone, a trace once the drums are in
    if (bar < 95) for (let b4 = 0; b4 < 4; b4++)
      keyDown(B.keys, at(bar, b4 * 4 + 2), "cursor", ["boot", "keys", "bells", "build"].includes(sec) ? 0.3 : 0.1, { pan: -0.35 });

    // ── drums: kick, then hats, then claps, one section apart ──
    const kicked = ["kick", "a", "drop", "c", "drop2"].includes(sec) || (sec === "outro" && bar < 94);
    if (kicked) {
      const tresillo = (sec === "a" && bar % 8 === 7) || sec === "c";
      for (let s = 0; s < 16; s++) if (tresillo ? E3_8[s % 8] : s % 4 === 0) kick(at(bar, s), sec === "c" ? 0.8 : 1); // six hits a bar in C: each a little lighter
    }
    if (sec === "kick" && bar >= 12) hatsBar(bar, E7_16, false, 0.6);
    if (sec === "a" || sec === "c") hatsBar(bar, E7_16, sec === "a" && bar >= 24, 1);
    if (big) hatsBar(bar, E9_16, true, 1);
    if (sec === "outro" && bar < 94) hatsBar(bar, E7_16, false, 0.6);
    if (sec === "a" || big || sec === "c") { clap(at(bar, 4), 0.7); clap(at(bar, 12), 0.7); }
    // the rock kit: a snare on 2 and 4 once the band is in (ghosted in on the swung 16th before 4 in the drops),
    // crashes on the section downbeats, tom fills down the kit into each new section
    if ((sec === "a" && bar >= 20) || big || sec === "c") { snare(at(bar, 4), big ? 1 : 0.8); snare(at(bar, 12), big ? 1 : 0.8); }
    if (big) { snare(at(bar, 11), 0.3); if (bar % 2) snare(at(bar, 15), 0.4); }
    if ([16, 24, 40, 48, 64, 76, 84].includes(bar)) crash(at(bar));
    if ([15, 23, 31, 39, 47, 55, 63, 71, 75, 83, 91].includes(bar)) tomFill(bar, [39, 75].includes(bar) ? 8 : 4);
    // ── electric guitar: one ringing chord per change as the band rolls in, a swung strum in the drops ──
    {
      const r = ROOT[ch];
      if (sec === "a" && bar >= 24 && even) guitar(at(bar), r, BAR * 2 - 0.1, { amp: 0.07, pan: 0.3, drive: 2.2 });
      // the drops: power chords (root, fifth, octave), double-tracked hard left and right and driven hard:
      // one rings from the downbeat, two palm-muted chugs, a pushed one rings into the next bar, an upstroke out
      if (big) for (const [side, det] of [[-0.75, -0.05], [0.75, 0.05]])
        [[0, "ring"], [6, "mute"], [8, "mute"], [11, "ring"], [14, "up"]].forEach(([s16, how], j, a) =>
          guitar(at(bar, s16), r + det, (at(bar, a[j + 1]?.[0] ?? 16) - at(bar, s16)) * (how === "mute" ? 0.6 : 0.97),
            { power: true, drive: 5.5, amp: how === "ring" ? 0.085 : 0.06, mute: how === "mute", up: how === "up", pan: side }));
      if (sec === "c" && even) guitar(at(bar), r, BAR * 2 - 0.1, { amp: 0.09, pan: 0.3 });
      if (sec === "build") for (let e8 = 0; e8 < 8; e8++) guitar(at(bar, e8 * 2), r, S16 * 1.6, { amp: 0.05 + 0.012 * (bar - 72) + 0.004 * e8, mute: true, pan: 0.3 });
      if (bar === 38 || bar === 39) for (let e8 = 0; e8 < 8; e8++) guitar(at(bar, e8 * 2), r, S16 * 1.6, { amp: 0.05 + 0.03 * (bar - 38) + 0.004 * e8, mute: true, pan: 0.3 });
      if (bar === 92) guitar(at(bar), 45, BAR * 3.5, { amp: 0.1, pan: 0.3 });
    }
    if (big && bar % 4 === 3) clap(at(bar, 14));
    // the keys section: the kick only marks the bar; the switches are the kit
    if (sec === "keys") {
      if (bar < 38) { kick(at(bar)); if (bar >= 34) kick(at(bar, 10)); }
      hatsBar(bar, E7_16, false, 0.5);
      if (bar < 38) bembe(bar, 0.5);
      if (bar % 2 === 1 && bar < 38) roll(at(bar, 8), 8, 0.4);
    }
    if (sec === "build") { hatsBar(bar, E7_16, false, 0.5); if (bar < 74) { kick(at(bar)); kick(at(bar, 10)); } }
    if (sec === "c") bembe(bar);

    // ── bass: in at A, out for the keys and bells sections ──
    if (sec === "a" || big || sec === "c" || (sec === "outro" && bar < 94)) subBar(bar);

    // ── the melodies ──
    // boot: the word typed with a typo, then typed as switches gaining breath letter by letter
    if (bar === 0) {
      typeLine("aestr", at(0), [0, 2, 4, 7, 10], { until: 14, amp: 0.13, send: 0.4, keyVel: 0.5 });
      spacebar(B.keys, at(0, 15), 0.45); keyUp(B.keys, at(0, 15) + 0.1, "space", 0.35); // backspace
      typeLine("htina", at(0), [18, 20, 22, 24, 26], { until: 31, amp: 0.13, send: 0.4, keyVel: 0.5 });
    }
    if (sec === "boot" && bar >= 2 && even) bootHook(at(bar), { 2: 3, 4: 6, 6: 10 }[bar]);
    if (bar === 7) { spacebar(B.keys, at(7, 14), 0.95); keyUp(B.keys, at(7, 14) + 0.14, "space", 0.7); } // Enter
    // the hook is typed under every statement (voices/reveal.py: one every two bars)
    if ((sec === "kick" || sec === "a") && even) hook(bar, { amp: sec === "kick" ? 0.13 : 0.15 });
    if (sec === "a" && bar >= 24 && bar % 4 === 0) typeLine(COUNTER, at(bar), COUNTER_STEPS, { octave: -1, amp: 0.08, pan: 0.35, until: 30, keyVel: 0.35 });
    if (sec === "a" && bar % 8 === 0) augmented(at(bar), 2, -1, 0.08, B.pad);
    if (big && even) hook(bar, { amp: 0.16 });
    if (sec === "drop" && even && bar >= 44) typeLine(VESPER, at(bar), HOOK_STEPS, { until: 30, amp: 0.09, pan: -0.45, keyVel: 0.4 });
    if (sec === "drop2" && even) word("tumbleweed", bar, { amp: 0.1, pan: -0.45, keyVel: 0.4, octave: bar >= 84 ? 0 : -1 });
    if (sec === "drop2" && bar >= 84 && even) hook(bar, { amp: 0.08, octave: 1, pan: 0.4, send: 0.5 }); // doubled up top for the last lap
    // bells: aeiou typed slowly, one letter a bar, each with a glass bell
    if (sec === "bells" && bar >= 57 && bar < 62) {
      const c = AEIOU[bar - 57];
      keyDown(B.keys, at(bar), c, 0.6, { send: 0.3 }); keyUp(B.keys, at(bar) + BAR * 0.9, c, 0.4);
      ocarina(B.lead, at(bar) + 0.008, hz(KEY_MIDI[c]), BAR * 0.9, 0.11, { send: 0.6, attack: 0.05, release: 0.3, vib: 0.16 });
      bell(at(bar), KEY_MIDI[c] + 12, "glass", "glass", 0.14, [-0.4, 0.4][bar % 2], 4);
    }
    if (bar === 62) hook(bar, { amp: 0.09, octave: 1, send: 0.7 }); // the hook remembered, far off
    const cWord = { 64: "mountain", 66: "rainbow", 68: "adventure", 70: "rodeo" }[bar];
    if (cWord) word(cWord, bar);
    if (sec === "c" && (bar === 66 || bar === 70)) hook(bar, { amp: 0.06, octave: -1, pan: 0.45, send: 0.5 });
    if (bar === 92) typeLine(ADIEU, at(bar), ADIEU_STEPS, { until: 40, amp: 0.15, send: 0.6 });
    if (bar === 94) hook(bar, { amp: 0.1, send: 0.7 });
    if (bar === 95) { spacebar(B.keys, at(bar, 8), 0.8); keyUp(B.keys, at(bar, 8) + 0.12, "space", 0.6); }

    // prolation: mid vessel 2×, bass vessel 4×
    if (big && bar % 4 === 0) augmented(at(bar), 2, -1, 0.08, B.pad);
    if ([40, 48, 76, 84].includes(bar)) augmented(at(bar), 4, -2, 0.16, B.bass);

    // ── harmony ──
    if (bar === 0 || bar === 4) teaPad(bar, 4, 0.05);
    if (sec === "kick" && bar % 4 === 0) teaPad(bar, 4, 0.05);
    if ((sec === "a" && bar >= 24) || sec === "c" || big) { if (even) strum(ch, at(bar), BAR * 2 - 0.05, { amp: sec === "c" ? 0.06 : 0.045 }); }
    if (sec === "keys" || sec === "build") strum(ch, at(bar), BAR - 0.05, { amp: 0.065 });
    if (sec === "bells" && even) strum(ch, at(bar), BAR * 2 - 0.05, { amp: 0.05, attack: 0.2 });
    if (bar === 92) teaPad(92, 4, 0.06);

    // ── grand piano: it opens the track, comps under the first statements, returns where the drums leave ──
    // the opening is a music box: two high notes, then the hook up in the top octaves, short and
    // soft, a glass bell on each note; the left hand only arrives in the last two bars
    if (bar === 0) { piano(at(0), 81, BEAT, 0.3, -0.2); piano(at(0, 4), 88, BEAT * 2, 0.26, 0.2); }
    if (sec === "boot" && bar >= 2 && even) {
      HOOK_STEPS.forEach((st, i) => {
        piano(at(bar, st), KEY_MIDI[HOOK[i]] + 12, 0.28, 0.26 + 0.02 * bar, [-0.3, 0.3][i % 2]);
        if (bar >= 4) bell(at(bar, st), KEY_MIDI[HOOK[i]] + 12, "glass", "glass", 0.035 + 0.006 * (bar - 4), [0.4, -0.4][i % 2], 2);
      });
      if (bar === 6) [57, 61, 64].forEach((m, j) => piano(at(bar) + j * 0.03, m, BAR * 1.9, 0.3, -0.3));
    }
    if (sec === "kick") {
      piano(at(bar), ROOT[ch] - 12, BEAT * 3, 0.45, -0.3);
      for (const st of [6, 14]) [...ch].forEach((c, j) => piano(at(bar, st) + j * 0.012, KEY_MIDI[c], BEAT * 0.9, 0.34, 0.2));
    }
    if (sec === "bells") {
      const tn = [...ch].map((c) => KEY_MIDI[c]).sort((x, y) => x - y), up = [tn[0] - 12, ...tn, tn[0] + 12, tn[1] + 12, tn[2] + 12, tn[1] + 12];
      for (let e8 = 0; e8 < 8; e8++) piano(at(bar, e8 * 2), up[e8 % up.length], BEAT * 1.5, 0.45 + 0.03 * (e8 % 4 === 0), (e8 - 3.5) * 0.1);
      piano(at(bar), ROOT[ch] - 12, BAR * 0.95, 0.55, -0.3);
    }
    if (bar === 92) [33, 45, 52, 57, 61, 64, 69].forEach((m, j) => piano(at(92) + j * 0.03, m, BAR * 3.5, 0.75 - j * 0.03, (j - 3) * 0.12));

    // ── synths: the opening is tones before it is a band ──
    // a saw pad whose filter opens across the whole boot, a low A swelling under it, and a
    // plucked arpeggio up the A chord that thickens from 8ths to 16ths into the kick
    if (bar === 0) synthPad(at(0), [57, 61, 64, 69], 8 * BAR, { amp: 0.024, from: 300, to: 2200, attack: 6 }); // higher, lighter
    if (bar === 4) {
      ocarina(B.bass, at(4), hz(33), 4 * BAR, 0.025, { attack: 5, release: 0.5, vib: 0, h2: 0.3, h3: 0.1, send: 0.05, scoop: 0 });
    }
    if (bar === 8) synthPad(at(8), [45, 52, 57, 64, 69], 8 * BAR, { amp: 0.03, from: 900, to: 3200, attack: 0.3 });
    if (sec === "boot" && bar >= 4) { // a trace at first
      const tn = [57, 61, 64, 69, 73, 76, 81, 76, 73, 69, 64, 61], step = bar >= 6 ? 1 : 2;
      for (let s16 = 0; s16 < 16; s16 += step) { const j = ((bar - 2) * 16 + s16) / step;
        pluck(at(bar, s16), tn[j % tn.length] + (bar >= 6 && j % 4 === 3 ? 12 : 0), S16 * step * 0.9, { amp: 0.007 + 0.004 * (bar - 4), pan: [-0.6, 0.6, -0.25, 0.25][j % 4], send: 0.4 }); }
    }
    if (sec === "kick") {
      const tn = [...ch].map((c) => KEY_MIDI[c]).sort((x, y) => x - y), cyc = [...tn, tn[0] + 12, ...tn.slice().reverse()];
      for (let s16 = 0; s16 < 16; s16++) if (s16 % 4 !== 0) pluck(at(bar, s16), cyc[(bar * 16 + s16) % cyc.length] + (s16 % 8 === 7 ? 12 : 0), S16 * 0.9, { amp: 0.022, pan: [-0.6, 0.6, -0.25, 0.25][s16 % 4], send: 0.35 });
    }
    // the pad returns where the drums leave, and at the end
    if (bar === 56) synthPad(at(56), [45, 52, 57, 61, 64], 8 * BAR, { amp: 0.045, from: 2400, to: 300, attack: 1 });
    if (bar === 92) synthPad(at(92), [45, 52, 57, 61], 4 * BAR, { amp: 0.04, from: 1800, to: 200, attack: 0.5 });

    // ── the ladders into the drops ──
    if (bar === 38 || bar === 74) ladder(at(bar));
    if ([15, 31, 55, 71, 91].includes(bar)) roll(at(bar, 12), 4, 0.45);

    // ── air and bells ──
    if (bar === 0) air(at(0), 8 * BAR, [57, 61, 64], 0.06);
    if (bar === 32) air(at(32), 8 * BAR, [57, 61, 64, 69], 0.06);
    if (bar === 56) air(at(56), 8 * BAR, [57, 61, 64, 69], 0.1);
    if (bar === 64) air(at(64), 8 * BAR, [57, 62, 66], 0.035);
    if (bar === 92) air(at(92), 4 * BAR, [57, 61, 64], 0.05);
    if ([0, 16, 40, 56, 60, 64, 76, 92].includes(bar)) bell(at(bar), 45, "church", "bronze", 0.35, 0, 6);
    if (((sec === "a" && bar >= 20) || big || sec === "c") && even) handbells(bar, ch);
    if (sec === "bells") { handbells(bar, ch, 0.11); if (bar >= 60) [0, 4, 8, 12].forEach((s16, j) => bell(at(bar, s16), KEY_MIDI[[...ch][j % 3]] + 24, "handbell", "brass", 0.05, [0.6, -0.6][j % 2], 2)); }
    // phone keys: only once the track is whole
    if (big || sec === "c") {
      const rootSemis = ROOT[ch] + 24 - 79;
      for (let s16 = 0; s16 < 16; s16++) if (E5_8[s16 % 8] && s16 % 4) dtmf(at(bar, s16), rootSemis + (s16 >= 8 ? 7 : 0), 0.09, 0.06, 0.35);
    }
  }
}
