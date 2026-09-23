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
    if (p.voice.performance ?? score.performance) kv.push(`performance=${Buffer.from(JSON.stringify(p.voice.performance ?? score.performance)).toString("base64")}`);
    if (score.phonemeOnly) kv.push("singGapMs=20", "singSustainDb=5", "singShimmerFrames=2", "singLegatoMs=15");
    if (p.speech) kv.push(`stemPath=${p.speech.stem}`, `wordsPath=${p.speech.meta}`);
    if (vj.color) kv.push(`captionColor=${vj.color}`);   // the member's color on its caption banner
    if (p.member && face) kv.push(`face=${p.member}`);   // its cartoon face, mouth on the onsets
    for (const k of ["notes2", "notes3", "notes4", "velocity2", "velocity3", "velocity4"]) if (p.voice[k] != null) kv.push(`${k}=${p.voice[k]}`);
    if (p.voice.double) {
      const up = Number(p.voice.doubleTranspose) || 0;
      const doubled = String(p.voice.notes).split(",").map((t) => { const [tok, d] = t.split(":"); return /^\d+$/.test(tok) ? `${Number(tok) + up}:${d}` : t; }).join(",");
      const slot = ["notes2", "notes3", "notes4"].find((k) => !p.voice[k]);
      if (slot) kv.push(`${slot}=${doubled}`, `velocity${slot.slice(5)}=${p.voice.doubleVelocity ?? 48}`);
    }
    if (score.title) kv.push(`title=${score.title.replace(/[;'=]/g, " ").trim()}`);
    if (p.sim) kv.push(`sim=${p.sim}`);
    return kv;
}
