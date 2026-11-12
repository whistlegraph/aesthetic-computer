// arrangements/callresponse.mjs — the caller and the band (152 bars, ~4:45).
//
// jeffrey calls a square-dance word (voices/callresponse.py sings it on the
// word's own notepat pitches); here the caller only types it, and the band
// answers two bars later with the same word transformed: echoed, inverted
// around A4, augmented, rung on handbells, dialled as phone keys, or strummed
// as a chord word. Calls grow from single words to pairs; the fiddle breakdown
// saws every call word as swung 16ths; the finale overlaps call and answer
// as a one-bar canon.
//
// | bars    | section     | what happens |
// |---------|-------------|--------------|
// | 0–8     | tuning up   | air, a toll, "allemande" typed as switches; yee-haw into the groove |
// | 8–24    | first calls | swing · dosido · swing · home — the band echoes, then strums |
// | 24–40   | figures     | allemande · promenade · circle · dosido — inverted, belled, augmented, dialled |
// | 40–48   | the bow     | half-time; promenade answered at 2× and 4×; arpeggio choir |
// | 48–64   | fiddle      | every call word sawed as 16ths (twang, pluck); the second half inverted |
// | 64–72   | home        | no drums; "home" sung slow, answered on glass bells |
// | 72–96   | big set     | two-word calls, each answered with two transforms at once |
// | 96–104  | hush        | air swell, the tuplet ladder, a scream climbing into the finale |
// | 104–136 | canon       | a call every bar; the band answers the previous one underneath |
// | 136–152 | going home  | half density, home, a whispered yee-haw, one spacebar |

export const CALL_WORDS = ["swing", "dosido", "allemande", "promenade", "circle", "home", "yeehaw"];
export const CHORD_OF = { swing: "tea", dosido: "daw", allemande: "ace", promenade: "big", circle: "fad", home: "tea", yeehaw: "tea" };
// letter steps for a typed call: 8ths up to six letters, else spread over the bar
export const callSteps = (n) => Array.from({ length: n }, (_, i) => (n <= 6 ? 2 * i : Math.floor((i * 14) / (n - 1) + 0.5)));

// calls: [bar, word, answer kinds]; answers land two bars after the call
const CALLS = [
  [6, "yeehaw", ["bells"]],
  [8, "swing", ["echo"]], [12, "dosido", ["echo"]], [16, "swing", ["echo", "chord"]], [20, "home", ["echo", "chord"]],
  [24, "allemande", ["invert", "bells"]], [28, "promenade", ["invert", "bells"]], [32, "circle", ["augment"]], [36, "dosido", ["dtmf", "invert"]],
  [40, "promenade", ["augment", "augment4"]],
  [66, "home", ["glass"]],
  [72, "swing", ["invert", "dtmf"]], [73, "promenade", ["echo", "chord"]],
  [76, "allemande", ["bells", "invert"]], [77, "home", ["echo", "chord"]],
  [80, "dosido", ["dtmf", "echo"]], [81, "circle", ["invert", "chord"]],
  [84, "promenade", ["augment"]], [85, "home", ["bells", "chord"]],
  [88, "yeehaw", ["echo", "dtmf"]], [89, "swing", ["invert", "chord"]],
  [92, "allemande", ["bells", "echo"]], [93, "promenade", ["invert", "chord"]],
  [140, "home", ["echo", "bells"]], [144, "home", ["glass"]],
];
const CANON_CYCLE = ["swing", "dosido", "allemande", "promenade", "circle", "home"];
const CANON_KINDS = [["echo", "chord"], ["invert", "bells"], ["dtmf", "echo"], ["invert", "chord"], ["bells", "echo"], ["augment"]];
for (let bar = 104; bar < 136; bar++) {
  const j = bar - 104;
  CALLS.push([bar, CANON_CYCLE[j % 6], CANON_KINDS[j % 6], 1]); // answered one bar later: a canon
}
export const CALL_LIST = CALLS;

export function play({ BAR, BEAT, S16, BARS, at, KEY_MIDI, hz, B, keyDown, keyUp, spacebar, ocarina, slideVoice,
  kick, hat, clap, roll, strum, ladder, hatsBar, air, bell, dtmf, ROOT, pat, E7_16, E9_16, E3_8, E5_8 }) {
  const sec = (bar) => bar < 8 ? "tune" : bar < 24 ? "set1" : bar < 40 ? "set2" : bar < 48 ? "bow" : bar < 64 ? "fiddle"
    : bar < 72 ? "home" : bar < 96 ? "set3" : bar < 104 ? "hush" : bar < 136 ? "canon" : "out";

  // the chord under each bar: the latest call's chord word
  const chordAt = (bar) => {
    const s = sec(bar);
    if (s === "fiddle") return ["tea", "daw", "tea", "ace"][((bar - 48) >> 1) % 4];
    if (s === "home" || s === "hush" || s === "tune") return "tea";
    let w = "tea";
    for (const [b, word] of CALLS) if (b <= bar) w = CHORD_OF[word];
    return w;
  };
  // boom (root, fifth on 1 and 3) – chick (short strum on 2 and 4)
  const boomChick = (bar, half = false) => {
    const word = chordAt(bar), r = ROOT[word], fifth = r + 7 <= 52 ? r + 7 : r - 5;
    [r, fifth].forEach((m, k) => ocarina(B.bass, at(bar, k * 8), hz(m), BEAT * 0.8, 0.36,
      { h2: 0.5, h3: 0.25, vib: 0, scoop: -1.5, send: 0.03, attack: 0.004, release: 0.05, decay: 0.2 }));
    if (half) return;
    for (const beat of [1, 3]) {
      const t = at(bar, beat * 4);
      [...word].forEach((ch, i) => {
        keyDown(B.keys, t + i * 0.012, ch, 0.3, { send: 0.05 });
        ocarina(B.pad, t + i * 0.012, hz(KEY_MIDI[ch] - 12), BEAT * 0.28, 0.035,
          { pan: (i - 1) * 0.5, send: 0.1, attack: 0.004, release: 0.03, vib: 0, scoop: -0.8, decay: 0.06 });
      });
    }
  };

  // the caller types the call (the voice sings it); a faint breath doubles it
  const typeCall = (bar, word, slow = 1) => {
    const st = callSteps(word.length);
    [...word].forEach((ch, i) => {
      const t = at(bar, st[i] * slow);
      keyDown(B.keys, t, ch, 0.55, { send: 0.1 });
      keyUp(B.keys, t + S16 * 1.6 * slow, ch, 0.35);
    });
    const notes = [...word].map((ch, i) => ({ t: at(bar, st[i] * slow) + 0.008, m: KEY_MIDI[ch] }));
    slideVoice(B.lead, notes, at(bar, (st[st.length - 1] + 3) * slow), { amp: 0.045, send: 0.3, decay: 0.25, twang: 1.6 });
  };

  // the band's answers
  const answer = (bar, word, kind) => {
    const st = callSteps(word.length), ms = [...word].map((c) => KEY_MIDI[c]);
    const T = (i, f = 1) => at(bar, st[i] * f);
    const clicks = (f = 1, vel = 0.4) => [...word].forEach((ch, i) => keyDown(B.keys, T(i, f), ch, vel, { send: 0.08 }));
    if (kind === "echo" || kind === "invert") {
      const m2 = kind === "invert" ? ms.map((m) => 138 - m) : ms;
      clicks();
      slideVoice(B.lead, m2.map((m, i) => ({ t: T(i) + 0.008, m })), T(word.length - 1) + BEAT * 1.2,
        { amp: 0.15, pan: kind === "invert" ? 0.3 : -0.2, send: 0.3, decay: 0.3, twang: 1.4 });
    } else if (kind === "augment" || kind === "augment4") {
      const f = kind === "augment" ? 2 : 4, oct = kind === "augment" ? -12 : -24;
      slideVoice(kind === "augment" ? B.pad : B.bass, ms.map((m, i) => ({ t: T(i, f) + 0.008, m: m + oct })),
        T(word.length - 1, f) + BEAT * f, { amp: kind === "augment" ? 0.1 : 0.16, send: 0.25, decay: 0.45 * f, floor: 0.3, vib: 0.05 });
    } else if (kind === "bells" || kind === "glass") {
      ms.forEach((m, i) => bell(T(i, kind === "glass" ? 2 : 1), m + (kind === "glass" ? 12 : 0), kind === "glass" ? "glass" : "handbell",
        kind === "glass" ? "glass" : "brass", kind === "glass" ? 0.13 : 0.11, i % 2 ? 0.45 : -0.45, kind === "glass" ? 4 : 2.5));
    } else if (kind === "dtmf") {
      ms.forEach((m, i) => dtmf(T(i), m - 79, 0.13, 0.09, i % 2 ? 0.4 : -0.4));
    } else if (kind === "chord") {
      strum(CHORD_OF[word], at(bar, 0), BAR * 1.9, { amp: 0.055 });
    }
  };

  for (const [bar, word, kinds, lag = 2] of CALLS) {
    typeCall(bar, word, word === "home" && sec(bar) === "home" ? 2 : 1);
    for (const k of kinds) if (bar + lag < BARS) answer(bar + lag, word, k);
  }

  // ── the fiddle: every call word sawed as swung 16ths ─────────────────────
  const FIDDLE = "swingdosidoallemandepromenadecirclehome";
  for (const [b0, inv] of [[48, false], [56, true]]) {
    const notes = [];
    for (let k = 0; k < 8 * 16; k++) {
      const ch = FIDDLE[k % FIDDLE.length], bar = b0 + (k >> 4), s = k % 16;
      const m = inv ? 138 - KEY_MIDI[ch] : KEY_MIDI[ch];
      notes.push({ t: at(bar, s) + 0.006, m });
      keyDown(B.keys, at(bar, s), ch, s % 4 === 0 ? 0.45 : 0.28, { send: 0.04 });
    }
    slideVoice(B.lead, notes, at(b0 + 8) - 0.05, { amp: 0.13, pan: inv ? 0.25 : -0.25, send: 0.22, glide: 0.035, decay: 0.11, floor: 0.12, twang: 1.6, vib: 0.08 });
  }

  // ── the groove ───────────────────────────────────────────────────────────
  for (let bar = 0; bar < BARS; bar++) {
    const s = sec(bar);
    const groove = ["set1", "set2", "fiddle", "set3", "canon"].includes(s);
    if (s !== "home" && s !== "hush" && bar < 150) for (let b4 = 0; b4 < 4; b4++)
      keyDown(B.keys, at(bar, b4 * 4 + 2), "cursor", s === "tune" ? 0.3 : 0.1, { pan: -0.35 });
    if (groove) {
      const tresillo = (s === "fiddle" && bar % 4 === 3) || (s === "set2" && bar % 8 === 7);
      for (let st = 0; st < 16; st++) if (tresillo ? E3_8[st % 8] : st % 4 === 0) kick(at(bar, st), bar < 10 ? 0.75 : 1);
      hatsBar(bar, s === "canon" || s === "fiddle" ? E9_16 : E7_16, s !== "set1" || bar >= 12, bar < 10 ? 0.6 : 1);
      if (bar >= 16) { clap(at(bar, 4)); clap(at(bar, 12)); }
      boomChick(bar);
      if (s === "canon") for (let st = 0; st < 16; st++)
        if (E5_8[st % 8] && st % 4) dtmf(at(bar, st), ROOT[chordAt(bar)] + 24 - 79 + (st >= 8 ? 7 : 0), 0.08, 0.06, 0.35);
    } else if (s === "bow" || s === "out") {
      kick(at(bar, 0)); kick(at(bar, 8), 0.85);
      hatsBar(bar, E7_16, false, 0.7);
      boomChick(bar, s === "bow");
    }
    // handbells ringing the chord on swung offbeats in the big sections
    if ((s === "set3" || s === "canon") && bar % 2 === 0) {
      const tn = [...chordAt(bar)].map((c) => KEY_MIDI[c] + 12).sort((x, y) => x - y);
      [2, 6, 10, 14].forEach((s16, j) => bell(at(bar, s16), tn[j % tn.length], "handbell", "brass", 0.06, [-0.5, 0.5][j % 2], 2));
    }
    // a bronze toll opening each section
    if ([0, 8, 24, 40, 48, 64, 72, 96, 104, 120, 136].includes(bar)) bell(at(bar), 45, "church", "bronze", 0.35, 0, 6);
    // fills into sections
    if ([7, 23, 39, 47, 63, 71, 95, 135].includes(bar)) roll(at(bar, 12), 4, 0.45);
  }

  // tuning up: allemande typed as bare switches, twice
  for (const b of [2, 4]) callSteps(9).forEach((st, i) => keyDown(B.keys, at(b, st), "allemande"[i], 0.45));
  // cabin air under the quiet parts
  air(at(0), 8 * BAR, [57, 61, 64], 0.05);
  air(at(64), 8 * BAR, [57, 61, 64], 0.06);
  air(at(96), 8 * BAR, [57, 61, 64, 69], 0.08);
  air(at(136), 14 * BAR, [57, 61, 64], 0.05);
  // the hush: tuplet ladder into the finale
  ladder(at(102));
  for (let st = 8; st < 16; st++) kick(at(103, st), 0.3 + (st - 8) * 0.08);
  // the last sound: one spacebar alone
  spacebar(B.keys, at(151, 4), 0.8); keyUp(B.keys, at(151, 4) + 0.12, "space", 0.6);
}
