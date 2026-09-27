// A display follower only; never schedules audio, DMX, or device volume.
export function visualPerformance(score, transport, receivedAt, now = Date.now()) {
  if (!transport?.playing || now - receivedAt > 1250 || now < receivedAt) return null;
  const elapsed = transport.elapsed + (now - receivedAt) / 1000;
  if (elapsed < 0 || elapsed >= score.duration) return null;
  const section = score.sections.find(s => elapsed >= s.startSec && elapsed < s.endSec);
  // Binary search a small window, rather than scanning the entire arrangement each tick.
  let lo = 0, hi = score.events.length;
  while (lo < hi) { const mid = (lo + hi) >>> 1;
    if (score.events[mid].t < elapsed - .6) lo = mid + 1; else hi = mid; }
  const hits = [];
  for (let i = lo; i < score.events.length && score.events[i].t <= elapsed + 1; i++) {
    const e = score.events[i];
    hits.push({t:e.t, seat:e.seat, midi:e.midi, sample:e.sample, gain:Math.min(1,e.gain)});
  }
  return {title:score.title, source:'Femrag++ · ' + (section?.name || ''),
    playing:true, phase:'playing', elapsed, duration:score.duration, bpm:score.bpm,
    dance:'femrag-round-v1', section:section?.name, hits:hits.slice(0,48).map(e=>[Math.round((e.t-elapsed)*100),e.seat==='sub'?6:e.seat,Number.isFinite(e.midi)?e.midi:-1]),
    notes:hits.filter(e=>e.t<=elapsed && Number.isFinite(e.midi)).slice(-2).map(e=>e.midi)};
}

// The MacNeoPolitan Trio: a lyric display follower. The transport itself
// carries the sung phrase, so no score gate applies; nothing here schedules audio.
export function trioPerformance(transport, receivedAt, now = Date.now()) {
  if (transport?.dance !== 'trio-round-v1' || !transport.playing) return null;
  if (now - receivedAt > 1250 || now < receivedAt) return null;
  const duration = Number(transport.duration) || 0;
  const elapsed = transport.elapsed + (now - receivedAt) / 1000;
  if (elapsed < 0 || (duration > 0 && elapsed > duration + 1)) return null;
  const phrase = value => value && typeof value.text === 'string' ? value : null;
  const next = phrase(transport.next);
  // Syllables travel as compact [t, text] tuples so the stage packet stays small;
  // the renderer picks the sung syllable from its own extrapolated clock.
  let lyric = phrase(transport.lyric);
  if (lyric) {
    const {syllables, ...rest} = lyric;
    lyric = {...rest, syl:(Array.isArray(syllables) ? syllables : []).slice(0, 40)
      .filter(s => s && Number.isFinite(Number(s.t))).map(s => [Math.round(Number(s.t) * 100) / 100, String(s.text ?? '')])};
  }
  const faces = {};
  for (const member of ['neo', 'blueberry', 'frisbee']) {
    const face = phrase(transport.faces?.[member]);
    faces[member] = face ? {text:face.text, rgb:face.rgb, t:face.t, dur:face.dur, role:face.role} : null;
  }
  return {title:String(transport.title || 'The MacNeoPolitan Trio').slice(0, 110),
    source:transport.source !== undefined ? String(transport.source).slice(0, 60) : 'MacNeoPolitan Trio · ' + (lyric?.member || ''),   // a transport may name (or blank) its own second line
    playing:true, phase:'playing', elapsed, duration, bpm:Number(transport.bpm) || 0,
    sentAt:Number(transport.sentAt) || null,
    dance:'trio-round-v1', section:lyric?.member || null, lyric, next, faces, hits:[], notes:[]};
}
