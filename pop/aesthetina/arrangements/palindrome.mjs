// arrangements/palindrome.mjs — aesthetina as a palindrome.
//
// 72 bars forward, one center bar, then the same 72 bars exactly backwards
// (145 bars, straight time so the mirror is exact). Every event is placed by
// its forward time t; in the mirrored half a hit lands at M(t) = 2·Tc − t and a
// held note of length d starts at M(t + d). So typed words come out reversed
// ("aesthetina" → "anitehtsea": the retrograde melody, still notepat's map),
// chord-word sequences run backwards, rhythms are retrograded inside each bar,
// and everything that plucked going forward swells coming back: ocarinas get
// long attacks and short releases, bells and cabin air are rendered and then
// time-reversed so they pull in toward the beat instead of drifting out.
//
// forward plan (mirrored after bar 72):
//   0–8 boot · 8–16 lift-off · 16–28 A1 · 28–36 stillness · 36–44 B1 ·
//   44–52 B2 · 52–56 prepare · 56–72 drop · 72 center (one shared held chord,
//   a bell toll rung forwards and backwards, the axis on the spacebar)

export function play(ctx) {
  const { SR, BEAT, BAR, S16, LEN, at, KEY_MIDI, hz, rand, B, keyDown, keyUp, spacebar, ocarina, typeLine,
    kick, hat, clap, strum, augmented, bootHook, teaPad, air, bell, dtmf, ROOT, HOOK, HOOK_STEPS, COUNTER,
    COUNTER_STEPS, VESPER, AEIOU, E7_16, E9_16, E3_8, E7_12, E5_8, B1_WORDS, DROP_WORDS } = ctx;

  const CENTER = 72, Tc = at(CENTER, 8), M = (t) => 2 * Tc - t;
  const T = (mir, t) => (mir ? M(t) : t); // a hit

  // a held ocarina: forward it plucks; mirrored it swells into its end
  function note(mir, bus, t, f, d, amp, o = {}) {
    if (!mir) return ocarina(bus, t, f, d, amp, o);
    const { decay, floor, ...rest } = o;
    ocarina(bus, M(t + d), f, d, amp, { ...rest, attack: Math.max(0.01, d * 0.85), release: 0.025, scoop: 0 });
  }

  // a typed word; mirrored = the reversed word on the retrograde steps, each key a swell
  function line(mir, word, t0, steps, until, o = {}) {
    if (!mir) return typeLine(word, t0, steps, { until, ...o });
    const n = word.length, end = t0 + until * S16, kv = o.keyVel ?? 0.55;
    for (let i = 0; i < n; i++) {
      const s = t0 + steps[i] * S16, e = i + 1 < n ? t0 + steps[i + 1] * S16 : end, ch = word[i];
      if (ch === " ") { if (kv) spacebar(B.keys, M(s), 0.5); continue; }
      if (kv) { keyDown(B.keys, M(e), ch, kv); keyUp(B.keys, M(s) - 0.02, ch, kv * 0.7); }
      if (i >= (o.breath ?? n)) continue;
      const m = KEY_MIDI[ch] + (o.octave ?? 0) * 12, d = Math.max(0.05, e - s - 0.05);
      ocarina(o.bus ?? B.lead, M(e) + 0.008, hz(m), d, o.amp ?? 0.16,
        { pan: o.pan ?? 0, send: o.send ?? 0.3, attack: Math.max(0.02, d * 0.8), release: 0.03, scoop: 0, vib: 0.1 });
    }
  }

  // a chord word strummed; mirrored the strum runs backwards and each vessel swells
  function chordWord(mir, word, t, dur, o = {}) {
    if (!mir) return strum(word, t, dur, o);
    [...word].forEach((ch, i) => {
      const tk = t + i * 0.028;
      keyDown(B.keys, M(tk + dur), ch, 0.5); keyUp(B.keys, M(tk), ch, 0.35);
      ocarina(B.pad, M(tk + dur), hz(KEY_MIDI[ch] + (o.octave ?? 0) * 12), dur - 0.06, o.amp ?? 0.05,
        { pan: (i - 1) * 0.45, send: 0.5, attack: dur * 0.7, release: 0.03, vib: 0.08 });
    });
  }

  // boom-chick (the engine's subBar, mirror-aware)
  function boomChick(mir, bar, word) {
    const r = ROOT[word], fifth = r + 7 <= 52 ? r + 7 : r - 5;
    [r, fifth].forEach((m, k) => note(mir, B.bass, at(bar, k * 8), hz(m), BEAT * 0.8, 0.36,
      { h2: 0.5, h3: 0.25, vib: 0, scoop: -1.5, send: 0.03, attack: 0.004, release: 0.05, decay: 0.2 }));
    for (const beat of [1, 3]) {
      const t = at(bar, beat * 4);
      [...word].forEach((ch, i) => {
        keyDown(B.keys, T(mir, t + i * 0.012), ch, 0.3, { send: 0.05 });
        note(mir, B.pad, t + i * 0.012, hz(KEY_MIDI[ch] - 12), BEAT * 0.28, 0.035,
          { pan: (i - 1) * 0.5, send: 0.1, attack: 0.004, release: 0.03, vib: 0, scoop: -0.8, decay: 0.06 });
      });
    }
  }

  function hats(mir, bar, p, ups = true, vel = 1) {
    for (let s = 0; s < 16; s++) {
      const t = at(bar, s) + (s % 2 ? S16 * 0.08 : 0);
      if (p[s]) hat(T(mir, t), vel * (s % 4 === 0 ? 0.55 : 0.75), s % 4 === 2);
      else if (ups) hat(T(mir, t), vel * 0.25);
    }
  }

  function roll(mir, t0, n16, vel = 0.5) {
    const letters = "asdfjkl;ghqwer";
    for (let i = 0; i < n16 * 2; i++) {
      const t = t0 + i * S16 / 2 + (rand() - 0.5) * 0.008;
      keyDown(B.keys, T(mir, t), letters[Math.floor(rand() * letters.length)], vel * (0.5 + i / (n16 * 4)));
    }
  }
  // forward: beat k of 8 gets k strokes (accelerating); mirrored it decelerates
  function ladder(mir, t0) {
    const letters = "asdfghjkl";
    for (let k = 1; k <= 8; k++) for (let j = 0; j < k; j++)
      keyDown(B.keys, T(mir, t0 + (k - 1) * BEAT + j * BEAT / k), letters[(k + j) % letters.length], 0.35 + k * 0.05, { send: 0.15 });
  }

  // render into scratch buses, then lay the result in backwards so it ends at tEnd
  function reversedInto(names, n, tEnd, fn) {
    const save = {}, tmp = {};
    for (const k of names) { save[k] = B[k]; tmp[k] = { L: new Float32Array(n), R: new Float32Array(n) }; B[k] = tmp[k]; }
    try { fn(); } finally { for (const k of names) B[k] = save[k]; }
    const iEnd = Math.round(tEnd * SR);
    for (const k of names) for (const ch of ["L", "R"]) {
      const src = tmp[k][ch], dst = B[k][ch];
      for (let j = 0; j < n; j++) { const i = iEnd - j; if (i < 0) break; if (i < LEN) dst[i] += src[j]; }
    }
  }
  function bellX(mir, t, midi, geom = "handbell", mat = "brass", amp = 0.2, pan = 0, dur = 3) {
    if (!mir) return bell(t, midi, geom, mat, amp, pan, dur);
    reversedInto(["bells", "hall"], Math.ceil(dur * SR) + 16, M(t), () => bell(0, midi, geom, mat, amp, pan, dur));
  }
  function airX(mir, t0, dur, midis, amp) {
    if (!mir) return air(t0, dur, midis, amp);
    reversedInto(["pad", "verb"], Math.ceil((dur + 6) * SR), M(t0), () => air(0, dur, midis, amp));
  }
  function pad(mir, bar, bars, amp) { teaPad(mir ? 2 * CENTER + 1 - bar - bars : bar, bars, amp); }

  const secOf = (bar) => (bar < 8 ? "boot" : bar < 16 ? "lift" : bar < 28 ? "a1" : bar < 36 ? "still"
    : bar < 44 ? "b1" : bar < 52 ? "b2" : bar < 56 ? "prep" : "drop");
  function chordFwd(bar) {
    if (bar >= 16 && bar < 28) return ["tea", "taw", "tea", "daw", "tea", "taw"][(bar - 16) >> 1];
    if (bar >= 36 && bar < 44) return B1_WORDS[(bar - 36) >> 1];
    if (bar >= 44 && bar < 52) return B1_WORDS[(bar - 44) % 4];
    if (bar >= 56 && bar < 64) return ["tea", "taw"][((bar - 56) >> 1) % 2];
    if (bar >= 64 && bar < 72) return DROP_WORDS[(bar - 64) >> 1];
    return "tea";
  }

  function playBar(bar, mir) {
    const sec = secOf(bar), even = bar % 2 === 0;
    if (sec !== "still") for (let b4 = 0; b4 < 4; b4++)
      keyDown(B.keys, T(mir, at(bar, b4 * 4 + 2)), "cursor", sec === "boot" || sec === "prep" ? 0.3 : 0.1, { pan: -0.35 });

    if (sec === "boot") {
      const breath = bar === 6 ? 10 : [0, 2, 5][bar / 2];
      if (even) mir ? line(true, HOOK, at(bar), HOOK_STEPS, 30, { breath, amp: 0.14, send: 0.45, keyVel: 0.5 }) : bootHook(at(bar), breath);
      if (bar === 7) roll(mir, at(bar, 8), 8, 0.5);
      if (bar === 0) airX(mir, at(0), 8 * BAR, [57, 61, 64], 0.05);
      return;
    }
    // drums
    if (sec !== "still" && sec !== "prep") {
      const tresillo = sec === "a1" && bar % 8 === 7;
      for (let s = 0; s < 16; s++) if (tresillo ? E3_8[s % 8] : s % 4 === 0) kick(T(mir, at(bar, s)));
      hats(mir, bar, sec === "drop" ? E9_16 : E7_16, sec !== "b2", sec === "lift" && bar < 10 ? 0.6 : 1);
      if (sec !== "lift") { clap(T(mir, at(bar, 4))); clap(T(mir, at(bar, 12))); }
      if (sec === "b2") E7_12.forEach((x, j) => x && keyDown(B.keys, T(mir, at(bar) + j * BAR / 12), "bembe" + (j % 3), 0.45, { pan: 0.5, send: 0.1 }));
    }
    if (["a1", "b1", "b2", "drop"].includes(sec)) {
      boomChick(mir, bar, chordFwd(bar));
      const rootSemis = ROOT[chordFwd(bar)] + 24 - 79;
      for (let s16 = 0; s16 < 16; s16++) if (E5_8[s16 % 8] && s16 % 4) dtmf(T(mir, at(bar, s16)), rootSemis + (s16 >= 8 ? 7 : 0), 0.09, 0.06, 0.35);
    }

    // the hook and its company
    if (even && ["lift", "a1", "drop"].includes(sec)) line(mir, HOOK, at(bar), HOOK_STEPS, 30, { amp: 0.16 });
    if (even && sec === "prep") line(mir, HOOK, at(bar), HOOK_STEPS, 30, { amp: 0.18, send: 0.6 });
    if (even && sec === "b1" && bar % 4 === 2) line(mir, HOOK, at(bar), HOOK_STEPS, 30, { amp: 0.1, send: 0.5 });
    if (even && sec === "a1" && bar >= 20) line(mir, COUNTER, at(bar), COUNTER_STEPS, 30, { octave: -1, amp: 0.08, pan: 0.35, keyVel: 0.35 });
    if ((sec === "a1" || sec === "drop") && bar % 4 === 0)
      mir ? line(true, HOOK, at(bar), HOOK_STEPS.map((s) => s * 2), 60, { octave: -1, amp: 0.08, bus: B.pad, keyVel: 0, send: 0.2 })
        : augmented(at(bar), 2, -1, 0.08, B.pad);
    if (sec === "drop" && bar % 8 === 0)
      mir ? line(true, HOOK, at(bar), HOOK_STEPS.map((s) => s * 4), 120, { octave: -2, amp: 0.16, bus: B.bass, keyVel: 0, send: 0.2 })
        : augmented(at(bar), 4, -2, 0.16, B.bass);
    if (sec === "drop" && even && bar >= 58) line(mir, VESPER, at(bar), HOOK_STEPS, 30, { amp: 0.09, pan: -0.45, keyVel: 0.4 });

    // pads and chord words
    if (sec === "a1" && bar % 4 === 0) pad(mir, bar, 4, 0.05);
    if (sec === "still" && bar % 4 === 0) pad(mir, bar, 4, 0.08);
    if (sec === "drop" && bar < 64 && bar % 4 === 0) pad(mir, bar, 4, 0.04);
    if (sec === "prep" && bar === 52) pad(mir, bar, 4, 0.06);
    if (sec === "b1" && even) chordWord(mir, B1_WORDS[((bar - 36) / 2) % 4], at(bar), BAR * 2 - 0.05);
    if (sec === "b2") chordWord(mir, B1_WORDS[(bar - 44) % 4], at(bar), BAR - 0.05, { amp: 0.055 });
    if (sec === "drop" && bar >= 64 && even) chordWord(mir, DROP_WORDS[((bar - 64) / 2) % 4], at(bar), BAR * 2 - 0.05, { amp: 0.05 });

    // stillness: aeiou typed slowly
    if (sec === "still" && bar >= 29 && bar < 34) {
      const ch = AEIOU[bar - 29];
      keyDown(B.keys, T(mir, mir ? at(bar + 1) - 0.1 : at(bar)), ch, 0.4);
      keyUp(B.keys, T(mir, mir ? at(bar) : at(bar + 1) - 0.1), ch, 0.3);
      note(mir, B.lead, at(bar) + 0.008, hz(KEY_MIDI[ch]), BAR - 0.15, 0.13, { send: 0.6, vib: 0.18, attack: 0.02, decay: 0.9, floor: 0.15 });
      bellX(mir, at(bar), KEY_MIDI[ch] + 12, "glass", "glass", 0.12, 0.3, 4);
    }
    if (bar === 54) ladder(mir, at(bar));

    // air, bells, fills
    if (bar === 28) airX(mir, at(28), 8 * BAR, [57, 61, 64], 0.06);
    if (bar === 52) airX(mir, at(52), 4 * BAR, [57, 61, 64, 69], 0.09);
    if ([16, 28, 36, 56].includes(bar)) bellX(mir, at(bar), 45, "church", "bronze", 0.35, 0, 6);
    if ((sec === "a1" || sec === "drop") && even) {
      const tn = [...chordFwd(bar)].map((c) => KEY_MIDI[c] + 12).sort((x, y) => x - y);
      [2, 6, 10, 14].forEach((s16, j) => bellX(mir, at(bar, s16), tn[j % tn.length], "handbell", "brass", 0.07, [-0.5, 0.5][j % 2], 2));
    }
    if (bar === 15 || bar === 27 || bar === 43 || bar === 71) roll(mir, at(bar, 12), 4, 0.45);
  }

  for (let bar = 0; bar < CENTER; bar++) { playBar(bar, false); playBar(bar, true); }

  // ── the center bar: everything meets on one held chord ───────────────────
  // swells to the axis and dies away after it — the one envelope that is its own mirror
  const half = BAR / 2;
  for (const [m, bus, amp, pan] of [[45, B.bass, 0.3, 0], [57, B.pad, 0.08, -0.4], [61, B.pad, 0.06, 0.1], [64, B.pad, 0.07, 0.4], [69, B.lead, 0.12, 0]])
    ocarina(bus, at(CENTER), hz(m), half + 0.04, amp, { pan, send: 0.6, attack: half - 0.04, release: half / 3, vib: 0.1, scoop: 0 });
  bell(Tc, 45, "church", "bronze", 0.5, 0, 6); bellX(true, Tc, 45, "church", "bronze", 0.5, 0, 6);
  bell(Tc, 81, "glass", "glass", 0.12, 0.3, 4); bellX(true, Tc, 81, "glass", "glass", 0.12, -0.3, 4);
  air(Tc, BAR, [57, 61, 64, 69], 0.08); airX(true, Tc, BAR, [57, 61, 64, 69], 0.08);
  spacebar(B.keys, Tc, 0.9); keyUp(B.keys, Tc + 0.12, "space", 0.6);
}
