// Shared by live conducting and silent fleet preparation.
export function singerPayload({p, vj, score, bpm, epoch, face=true}) {
    const prof = vj.aesthetivox || {};
    const kv = [`bpm=${bpm}`, `startEpoch=${epoch}`, `program=${p.voice.program ?? 78}`,
      `notes=${p.voice.notes}`, `lyrics=${String(p.voice.lyrics).replace(/[;=]/g, " ")}`,
      `singVoice=${p.voice.singVoice || prof.base_voice || "Fred"}`,
      `singVibratoHz=${p.voice.singVibratoHz ?? prof.sing?.vibrato_hz ?? 5}`,
      `singVibCents=${p.voice.singVibCents ?? prof.sing?.vibrato_depth_cents ?? 18}`, `singLock=${p.voice.singLock ?? prof.sing?.harmony_lock ?? 0.875}`,
      `singF0Floor=${p.voice.singF0Floor ?? prof.f0_floor ?? 55}`];
    if (p.voice.noteGains) kv.push(`singNoteGains=${p.voice.noteGains.map(row => row.join(",")).join("/")}`);
    if (p.voice.lineGains) kv.push(`singLineGains=${p.voice.lineGains.map(g => .9 * g).join(",")}`);
    // The self-choir: the member sings its line again on the chord tones the
    // band offers around each note (vowels only), under the lead. The chord
    // comes from the payload's own notes2..4 and `singChord`, the timeline
    // of everything every voice in the score sounds (bandChord below).
    if (p.voice.choir != null && Number(p.voice.choir) > 0) {
      kv.push(`singChoir=${p.voice.choir}`);
      if (p.voice.choirVoices) kv.push(`singChoirVoices=${p.voice.choirVoices}`);
      if (p.voice.choirLock != null) kv.push(`singChoirLock=${p.voice.choirLock}`);
      const chord = score.bandChord ?? bandChord(score.voices || []);
      if (chord) kv.push(`singChord=${chord}`);
    }
    if (p.voice.performance ?? score.performance) kv.push(`performance=${Buffer.from(JSON.stringify(p.voice.performance ?? score.performance)).toString("base64")}`);
    if (score.phonemeOnly) kv.push("singGapMs=20", "singSustainDb=5", "singShimmerFrames=2", "singLegatoMs=15");
    if (p.speech) kv.push(`stemPath=${p.speech.stem}`, `wordsPath=${p.speech.meta}`);
    if (vj.color) kv.push(`captionColor=${vj.color}`);   // the member's color on its caption banner
    if (p.member && face) kv.push(`face=${p.member}`);   // its cartoon face, mouth on the onsets
    if (p.voice.faceAlpha != null) kv.push(`faceAlpha=${p.voice.faceAlpha}`);   // < 1 lets the desktop through
    if (p.voice.captionSize != null) kv.push(`captionSize=${p.voice.captionSize}`);
    for (const k of ["notes2", "notes3", "notes4", "velocity2", "velocity3", "velocity4"]) if (p.voice[k] != null) kv.push(`${k}=${p.voice[k]}`);
    if (p.voice.double) {
      const up = Number(p.voice.doubleTranspose) || 0;
      const doubled = String(p.voice.notes).split(",").map((t) => { const [tok, d] = t.split(":"); return /^\d+$/.test(tok) ? `${Number(tok) + up}:${d}` : t; }).join(",");
      const slot = ["notes2", "notes3", "notes4"].find((k) => !p.voice[k]);
      if (slot) kv.push(`${slot}=${doubled}`, `velocity${slot.slice(5)}=${p.voice.doubleVelocity ?? 48}`);
    }
    if (score.title) kv.push(`title=${score.title.replace(/[;'=]/g, " ").trim()}`);
    if (p.sim) kv.push(`sim=${p.sim}`);
    if (p.corner) kv.push(`corner=${p.corner}`);   // the ghost tile under the keys (see trio.mjs --corner)
    return kv;
}

// Everything the band sounds, beat by beat, as one `.mbscore`-grammar track
// of dotted chords: `62.66.69:4,r:2,…`. Every voice's notes and notes2..4
// count; the grid is a sixteenth; equal neighbours merge. This is what a
// choir voice listens to.
export function bandChord(voices) {
  const events = [];
  for (const v of voices) for (const k of ["notes", "notes2", "notes3", "notes4"]) {
    if (!v[k]) continue;
    let pos = 0;
    for (const tok of String(v[k]).split(",")) {
      const [head, d] = tok.split(":"); const dur = parseFloat(d) || 1;
      if (head !== "r") for (const m of head.split(".")) if (/^\d+$/.test(m)) events.push({ at: pos, dur, midi: Number(m) });
      pos += dur;
    }
  }
  if (!events.length) return "";
  const end = Math.max(...events.map((e) => e.at + e.dur));
  const steps = Math.ceil(end * 4);
  const cells = [];
  for (let i = 0; i < steps; i++) {
    const t = i / 4;
    const set = [...new Set(events.filter((e) => e.at <= t + 1e-6 && t < e.at + e.dur - 1e-6).map((e) => e.midi))].sort((a, b) => a - b);
    cells.push(set.length ? set.join(".") : "r");
  }
  const out = [];
  for (const c of cells) {
    if (out.length && out[out.length - 1].head === c) out[out.length - 1].n++;
    else out.push({ head: c, n: 1 });
  }
  return out.map((o) => `${o.head}:${o.n / 4}`).join(",");
}
