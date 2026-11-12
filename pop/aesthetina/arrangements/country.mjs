// arrangements/country.mjs — the square-dance version (80 bars). See ../ARRANGEMENT.md.
export function play({ SR, BPM, BEAT, BAR, S16, BARS, LEN, TAU, at, sw, KEY_MIDI, hz, rand, B, put, mode, burst, keyDown, keyUp, spacebar, ocarina, slideVoice, typeLine, kick, hat, clap, roll, strum, augmented, bootHook, ladder, hatsBar, subBar, teaPad, air, bell, dtmf, chordAt, ROOT, HOOK, HOOK_STEPS, COUNTER, COUNTER_STEPS, VESPER, ADIEU, ADIEU_STEPS, AEIOU, pat, E7_16, E9_16, E3_8, E7_12, E5_8, B1_WORDS, DROP_WORDS }) {
  for (let bar = 0; bar < BARS; bar++) {
    const sec = bar < 8 ? "boot" : bar < 16 ? "lift" : bar < 28 ? "a1" : bar < 36 ? "still" : bar < 44 ? "b1"
      : bar < 52 ? "b2" : bar < 56 ? "prep" : bar < 72 ? "drop" : "outro";
    const even = bar % 2 === 0;
    // cursor tick: a key every beat, under everything except the stillness
    if (sec !== "still" && bar < 78) for (let b4 = 0; b4 < 4; b4++)
      keyDown(B.keys, at(bar, b4 * 4 + 2), "cursor", sec === "boot" || sec === "prep" ? 0.3 : 0.1, { pan: -0.35 });

    if (sec === "boot") {
      if (even) bootHook(at(bar), [0, 2, 5, 8][bar / 2] + (bar === 6 ? 2 : 0));
      if (bar === 7) roll(at(bar, 8), 8, 0.5);
      continue;
    }
    // drums
    const drums = !["still", "prep"].includes(sec);
    if (drums) {
      const half = sec === "outro";
      const tresillo = sec === "a1" && bar % 8 === 7;
      for (let s = 0; s < 16; s++) {
        const four = s % 4 === 0 && (!half || s % 8 === 0);
        if (tresillo ? E3_8[s % 8] && s < 16 : four) kick(at(bar, s));
      }
      hatsBar(bar, sec === "drop" ? E9_16 : E7_16, !(sec === "b2" || half), sec === "lift" && bar < 10 ? 0.6 : 1);
      if (sec !== "lift" && !half) { clap(at(bar, 4)); clap(at(bar, 12)); }
      if (sec === "b2") E7_12.forEach((x, j) => x && keyDown(B.keys, at(bar) + j * BAR / 12, "bembe" + (j % 3), 0.45, { pan: 0.5, send: 0.1 }));
    }
    if (["a1", "b1", "b2", "drop"].includes(sec) || (sec === "outro" && bar < 76)) subBar(bar);

    // the hook
    if (even && ["lift", "a1", "drop"].includes(sec)) typeLine(HOOK, at(bar), HOOK_STEPS, { until: 30, amp: 0.16 });
    if (even && sec === "outro" && bar < 76) typeLine(HOOK, at(bar), HOOK_STEPS, { until: 30, amp: 0.12, send: 0.5 });
    if (even && sec === "prep") typeLine(HOOK, at(bar), HOOK_STEPS, { until: 30, amp: 0.18, send: 0.6 });
    if (even && sec === "b1" && bar % 4 === 2) typeLine(HOOK, at(bar), HOOK_STEPS, { until: 30, amp: 0.1, send: 0.5 });
    // counter on the hook's complement
    if (even && (sec === "a1" && bar >= 20)) typeLine(COUNTER, at(bar), COUNTER_STEPS, { octave: -1, amp: 0.08, pan: 0.35, until: 30, keyVel: 0.35 });
    // prolation: mid vessel 2× (A1, drop), bass vessel 4× (drop)
    if ((sec === "a1" || sec === "drop") && bar % 4 === 0) augmented(at(bar), 2, -1, 0.08, B.pad);
    if (sec === "drop" && bar % 8 === 0) augmented(at(bar), 4, -2, 0.16, B.bass);
    // second typist in the drop: vespertine on the hook's steps, the other side
    if (sec === "drop" && even && bar >= 58) typeLine(VESPER, at(bar), HOOK_STEPS, { until: 30, amp: 0.09, pan: -0.45, keyVel: 0.4 });

    // pads + chord words
    if (sec === "a1" && bar % 4 === 0) teaPad(bar, 4);
    if (sec === "still" && bar % 4 === 0) teaPad(bar, 4, 0.08);
    if (sec === "b1" && even) strum(B1_WORDS[((bar - 36) / 2) % 4], at(bar), BAR * 2 - 0.05);
    if (sec === "b2") strum(B1_WORDS[(bar - 44) % 4], at(bar), BAR - 0.05, { amp: 0.055 });
    if (sec === "drop" && bar < 64 && bar % 4 === 0) teaPad(bar, 4, 0.04);
    if (sec === "drop" && bar >= 64 && even) strum(DROP_WORDS[((bar - 64) / 2) % 4], at(bar), BAR * 2 - 0.05, { amp: 0.05 });
    if (sec === "prep" && bar === 52) teaPad(bar, 4, 0.06);

    // stillness: aeiou typed slowly, one letter a bar
    if (sec === "still" && bar >= 29 && bar < 34) {
      const ch = AEIOU[bar - 29];
      keyDown(B.keys, at(bar), ch, 0.4); keyUp(B.keys, at(bar + 1) - 0.1, ch, 0.3);
      ocarina(B.lead, at(bar) + 0.008, hz(KEY_MIDI[ch]), BAR - 0.15, 0.13, { send: 0.6, vib: 0.18, attack: 0.02, decay: 0.9, floor: 0.15 });
    }
    // prepare: the tuplet ladder over its last two bars
    if (bar === 54) ladder(at(bar));
    // outro: adieu, then one spacebar alone
    if (bar === 76) typeLine(ADIEU, at(bar), ADIEU_STEPS, { until: 40, amp: 0.15, send: 0.6 });
    if (bar === 79) { spacebar(B.keys, at(bar, 8), 0.8); keyUp(B.keys, at(bar, 8) + 0.12, "space", 0.6); }
    // country: cabin air under the quiet sections, tuned to the chord
    if (bar === 0) air(at(0), 8 * BAR, [57, 61, 64], 0.05);
    if (bar === 28) air(at(28), 8 * BAR, [57, 61, 64], 0.06);
    if (bar === 52) air(at(52), 4 * BAR, [57, 61, 64, 69], 0.09);
    if (bar === 72) air(at(72), 8 * BAR, [57, 61, 64], 0.05);
    // bells: a bronze toll opening each section, handbells ringing the chord on swung offbeats
    if ([16, 28, 36, 56, 72].includes(bar)) bell(at(bar), 45, "church", "bronze", 0.35, 0, 6);
    if (["a1", "drop"].includes(sec) && even) {
      const tn = [...chordAt(bar)].map((c) => KEY_MIDI[c] + 12).sort((x, y) => x - y);
      [2, 6, 10, 14].forEach((s16, j) => bell(at(bar, s16), tn[j % tn.length], "handbell", "brass", 0.07, [-0.5, 0.5][j % 2], 2));
    }
    if (sec === "still" && bar >= 29 && bar < 34) bell(at(bar), KEY_MIDI[AEIOU[bar - 29]] + 12, "glass", "glass", 0.12, 0.3, 4);
    // phone keys: a cinquillo of DTMF blips riding the chord's root
    if (["a1", "b1", "b2", "drop"].includes(sec)) {
      const rootSemis = ROOT[chordAt(bar)] + 24 - 79; // dtmf "6" low tone ≈ G5 (770 Hz)
      for (let s16 = 0; s16 < 16; s16++) if (E5_8[s16 % 8] && s16 % 4) dtmf(at(bar, s16), rootSemis + (s16 >= 8 ? 7 : 0), 0.09, 0.06, 0.35);
    }
    // fills
    if (bar === 15 || bar === 27 || bar === 43 || bar === 71) roll(at(bar, 12), 4, 0.45);
  }
}
