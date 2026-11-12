// arrangements/phasing.mjs — aesthetina as Piano Phase (Reich, 1967).
//
// Typists loop "aesthetina" through notepat's map; one at a time they step
// ahead of the others by a 16th per two-bar cycle, so the hook canons against
// itself and, a full cycle later, snaps back into unison. 152 bars, straight.
//
// | bars    | section        | process |
// |---------|----------------|---------|
// | 0–8     | solo           | typist A alone, cabin air |
// | 8–16    | ground         | kick, hats, A loops |
// | 16–48   | phase I        | typist B enters in unison, steps an 8th ahead each cycle (0 → 30) |
// | 48–64   | realignment I  | B arrives home: unison climax |
// | 64–96   | phase II       | B locks a half-bar back; typist C phases the counter, a 16th per cycle |
// | 96–112  | breakdown      | drums out; augmented hook, glass bells; C keeps drifting, slower |
// | 112–144 | phase III      | B (+1/cycle from 16) and C (+2/cycle from 0) converge on bar 144 |
// | 144–152 | unison         | all three typists together; then A alone; one spacebar |

const CYCLE = 32; // the hook's loop: two bars of 16ths

// where each typist sits (16ths ahead of A) at a given cycle start
export function offB(bar) {
  if (bar < 16) return null;
  if (bar < 48) return (bar - 16); // +2 per 2-bar cycle: 0, 2, … 30
  if (bar < 64) return 0;
  if (bar < 96) return 8;
  if (bar < 112) return null; // B rests through the breakdown
  if (bar < 144) return 16 + (bar - 112) / 2; // 16 … 31
  return 0;
}
export function offC(bar) {
  if (bar < 64) return null;
  if (bar < 96) return (bar - 64) / 2; // 0 … 15
  if (bar < 112) return 16 + Math.floor((bar - 96) / 4); // drifting slower: 16 … 19
  if (bar < 144) return bar - 112; // +2 per cycle: 0 … 30
  if (bar < 150) return 0;
  return null;
}
export function wordAt(bar) {
  if (bar < 32) return "tea";
  if (bar < 48) return ["tea", "taw"][((bar - 32) >> 2) % 2];
  if (bar < 64) return "tea";
  if (bar < 96) return ["tea", "taw", "daw", "tea"][((bar - 64) >> 3) % 4];
  if (bar < 112) return ["ace", "fad"][((bar - 96) >> 2) % 2];
  if (bar < 144) return ["ace", "fad", "big", "tea"][((bar - 112) >> 1) % 4];
  return "tea";
}

export function play(ctx) {
  const { BEAT, BAR, S16, at, KEY_MIDI, hz, B, keyDown, keyUp, spacebar, ocarina, typeLine, kick, clap, roll,
    augmented, ladder, hatsBar, teaPad, air, bell, dtmf, ROOT, HOOK, HOOK_STEPS, COUNTER, COUNTER_STEPS,
    E7_16, E9_16 } = ctx;

  // a phased line: the word's steps pushed k 16ths ahead, wrapped inside the cycle
  // (the letters rotate, so the typed text starts wherever the wrap lands)
  function phased(word, steps, k, t0, o = {}) {
    const pairs = steps.map((s, i) => [(s + k) % CYCLE, word[i]]).sort((a, b) => a[0] - b[0]);
    typeLine(pairs.map((p) => p[1]).join(""), t0, pairs.map((p) => p[0]), { until: CYCLE - 0.5, ...o });
  }
  // boom-chick on an explicit chord word (the engine's subBar follows the country plan)
  function boom(bar, word, vel = 1) {
    const r = ROOT[word], fifth = r + 7 <= 52 ? r + 7 : r - 5;
    [r, fifth].forEach((m, k) => ocarina(B.bass, at(bar, k * 8), hz(m), BEAT * 0.8, 0.36 * vel,
      { h2: 0.5, h3: 0.25, vib: 0, scoop: -1.5, send: 0.03, attack: 0.004, release: 0.05, decay: 0.2 }));
    for (const beat of [1, 3]) {
      const t = at(bar, beat * 4);
      [...word].forEach((ch, i) => {
        keyDown(B.keys, t + i * 0.012, ch, 0.3 * vel, { send: 0.05 });
        ocarina(B.pad, t + i * 0.012, hz(KEY_MIDI[ch] - 12), BEAT * 0.28, 0.035 * vel,
          { pan: (i - 1) * 0.5, send: 0.1, attack: 0.004, release: 0.03, vib: 0, scoop: -0.8, decay: 0.06 });
      });
    }
  }
  const chordTones = (word, oct = 12) => [...word].map((c) => KEY_MIDI[c] + oct).sort((x, y) => x - y);

  for (let bar = 0; bar < 152; bar++) {
    const even = bar % 2 === 0;
    const sec = bar < 8 ? "solo" : bar < 16 ? "ground" : bar < 48 ? "p1" : bar < 64 ? "real" : bar < 96 ? "p2"
      : bar < 112 ? "brk" : bar < 144 ? "p3" : "uni";
    const word = wordAt(bar);

    // ── typists ──
    if (even) {
      const bk = sec === "brk";
      // A: the hook at home, every cycle (every other cycle in the breakdown); alone at the very end
      if (!bk || bar % 4 === 0) typeLine(HOOK, at(bar), HOOK_STEPS, { until: CYCLE - 0.5, amp: bk ? 0.13 : 0.15, pan: -0.45, send: bk ? 0.55 : 0.3 });
      const kb = offB(bar);
      if (kb !== null) {
        phased(HOOK, HOOK_STEPS, kb, at(bar), { amp: 0.14, pan: 0.45 });
        const prev = offB(bar - 2);
        if (prev !== null && prev !== kb) dtmf(at(bar), (kb % 12) - 12, 0.11, 0.08, 0.45); // the step, heard
      }
      const kc = offC(bar);
      if (kc !== null) {
        phased(COUNTER, COUNTER_STEPS, kc, at(bar), { octave: -1, amp: 0.085, pan: 0, keyVel: 0.35, send: bk ? 0.5 : 0.3 });
        const prev = offC(bar - 2);
        if (prev !== null && prev !== kc) dtmf(at(bar, 2), (kc % 12) - 5, 0.08, 0.06, -0.3);
      }
    }

    // ── drums ──
    const drums = !["solo", "brk"].includes(sec) && bar < 150 && !(sec === "ground" && bar < 10);
    if (drums) {
      for (let b4 = 0; b4 < 4; b4++) kick(at(bar, b4 * 4), sec === "ground" ? 0.8 : 1);
      hatsBar(bar, sec === "p3" || sec === "uni" || sec === "real" ? E9_16 : E7_16, bar >= 16, sec === "ground" ? 0.6 : 1);
      if (bar >= 24) { clap(at(bar, 4)); clap(at(bar, 12)); }
    }
    if (sec === "brk" && bar >= 108) for (let s = 0; s < 16; s += bar === 111 ? 2 : 4) kick(at(bar, s), 0.35 + (bar - 108) * 0.12);
    // cursor tick in the solo
    if (sec === "solo") for (let b4 = 0; b4 < 4; b4++) keyDown(B.keys, at(bar, b4 * 4 + 2), "cursor", 0.3, { pan: -0.35 });

    // ── bass + harmony ──
    if (bar >= 24 && bar < 150 && sec !== "brk") boom(bar, word, sec === "real" || sec === "uni" ? 1.1 : 1);
    if (sec === "brk" && bar % 4 === 0) teaPad(bar, 4, 0.06);
    if (sec === "brk" && bar % 8 === 0) augmented(at(bar), 4, -1, 0.12, B.pad);
    if (sec === "uni" && bar === 144) augmented(at(bar), 2, -1, 0.1, B.pad);
    if (sec === "real" && bar % 4 === 0) teaPad(bar, 4, 0.045);

    // ── bells ──
    if ([16, 48, 64, 96, 112, 144].includes(bar)) bell(at(bar), 45, "church", "bronze", 0.4, 0, 6);
    // handbells at the lock points: every time B lands on a quarter of the cycle, and through the unisons
    const kb = offB(bar);
    if (even && ((sec === "p1" && kb % 8 === 0) || sec === "real" || sec === "uni" || (sec === "p3" && bar % 8 === 0))) {
      const tn = chordTones(word);
      [0, 2, 4, 6, 8, 10, 12, 14].forEach((s16, j) => bell(at(bar, s16), tn[j % tn.length] + (j > 3 ? 12 : 0), "handbell", "brass", 0.07, [-0.5, 0.5][j % 2], 2));
    }
    if (sec === "p2" && even) {
      const tn = chordTones(word);
      [2, 6, 10, 14].forEach((s16, j) => bell(at(bar, s16), tn[j % tn.length], "handbell", "brass", 0.055, [-0.5, 0.5][j % 2], 2));
    }
    if (sec === "brk" && bar % 4 === 0) [0, 3, 6, 8].forEach((st, j) => bell(at(bar, HOOK_STEPS[st]), KEY_MIDI[HOOK[st]] + 12, "glass", "glass", 0.1, [-0.4, 0.4][j % 2], 4));

    // ── air ──
    if (bar === 0) air(at(0), 8 * BAR, [57, 61, 64], 0.05);
    if (bar === 96) air(at(96), 16 * BAR, [57, 60, 64], 0.06);
    if (bar === 148) air(at(148), 4 * BAR, [57, 61, 64, 69], 0.06);

    // ── transitions ──
    if (bar === 46 || bar === 142) ladder(at(bar));
    if ([15, 63, 95, 111].includes(bar)) roll(at(bar, 12), 4, 0.45);
    if (bar === 151) { spacebar(B.keys, at(bar, 8), 0.8); keyUp(B.keys, at(bar, 8) + 0.12, "space", 0.6); }
  }
}
