// Read-only adapters for the concert's two existing transports.
export function menuBandCue({action, at, info = {}}, now = Date.now() / 1000) {
  if (action === 'stop') return null;
  const bpm = Number(info.bpm) || 132, beat = 60 / Math.max(1, bpm);
  const events = []; let duration = 0;
  for (const key of ['notes','notes2','notes3','notes4']) {
    let t = 0;
    for (const token of String(info[key] || '').split(',')) {
      const [pitch, beats] = token.split(':');
      const dur = Number(beats) * beat;
      if (!(dur > 0) || !Number.isFinite(dur)) continue;
      if (/^\d+$/.test(pitch)) events.push({t,dur,note:Number(pitch)});
      t += dur;
    }
    duration = Math.max(duration, t);
  }
  if (!duration) return null;
  return {title:info.title || 'Menu Band', source:'Macneopolitan Trio',
    start:Number(info.startEpoch) || at || now, duration, events,
    lyricParts:[lyricPart(info)]};
}
export function currentMenuBand(cue, now = Date.now() / 1000) {
  if (!cue || now > cue.start + cue.duration) return null;
  const elapsed = Math.max(0, now - cue.start);
  const notes = cue.events.filter(e => elapsed >= e.t && elapsed < e.t + e.dur).map(e=>e.note).slice(0,8);
  return {title:cue.title, source:cue.source, elapsed, duration:cue.duration,
    playing:now >= cue.start, phase:now < cue.start ? 'countdown':'playing', notes,
    intensity:notes.length ? .7 : 0, lyrics:now >= cue.start ? currentLyrics(cue.lyricParts,elapsed) : []};
}
export function spatialPerformance(status, score = null, plan = null) {
  if (!status || !(status.scoreDuration > 0 || status.duration > 0)) return null;
  // A conductor may already have prepared the next score. Never animate
  // its notes or label against a receiver still running a different one.
  if (status.arrangementHash && score?.hash !== status.arrangementHash) score = null;
  const position = Number.isFinite(status.scoreTime) ? status.scoreTime :
    Number.isFinite(status.origin) && Number.isFinite(status.audioTime) ? status.audioTime - status.origin : null;
  const playing = Number.isFinite(position) && position >= 0 && ['playing','score','running'].includes(status.phase);
  const elapsed = Math.max(0, position || 0), duration = status.scoreDuration || status.duration || 0;
  if (!status.arrangementHash || plan?.arrangementHash !== status.arrangementHash) plan = null;
  const lyrics = playing && elapsed < duration ? currentLyrics(plan?.lyricParts,elapsed) : [];
  const notes = (playing ? score?.events || [] : []).filter(e => elapsed >= e.t && elapsed < e.t + e.dur && e.hz > 0)
    .map(e => 69 + 12 * Math.log2(e.hz / 440)).slice(0,8);
  return {title:status.scoreName || status.title || plan?.title || score?.name || 'Spatial composition',
    source:status.schema === 'trio-native-status-v1' ? 'Macneopolitan Trio' : 'Notepat Spatial', elapsed:Math.min(elapsed,duration), duration, playing,
    phase:status.phase, notes, lyrics, intensity:playing ? Math.min(1, Math.max(0,status.glow || status.outputPeak || .2)) : 0};
}

// Each status file has its own liveness clock. An abandoned playing file
// must not mask a different receiver transport that is still advancing.
export function selectNativeStatus(statuses, clocks, now = Date.now()) {
  const fresh = statuses.map((status, index) => {
    if (!status) return null;
    const signature = JSON.stringify([status.audioTime, status.runId, status.phase, status.scoreTime]);
    if (clocks[index]?.signature !== signature) clocks[index] = {signature, at:now};
    return now - clocks[index].at <= 2500 ? status : {...status, phase:'stale', lastUpdate:clocks[index].at};
  }).filter(Boolean);
  return fresh.find(s => ['playing','countdown','running','score'].includes(s.phase)) ||
    fresh.find(s => s.phase !== 'stale') ||
    fresh.filter(s=>Number.isFinite(s.scoreTime)).sort((a,b)=>(b.lastUpdate || 0)-(a.lastUpdate || 0))[0] || fresh[0] || null;
}

// Lyrics consume sung notes only; rests advance time without consuming a syllable.
export function lyricPart(info, member = info.face || '') {
  const beat = 60 / (Number(info.bpm) || 132);
  const lines = String(info.lyrics || '').split('/').map(line => ({words:line.trim().split(/\s+/).filter(Boolean).map(word=>({text:word.replaceAll('-',''),count:word.split('-').length}))})).filter(line=>line.words.length);
  const syllables=lines.flatMap(line=>line.words.flatMap(word=>Array.from({length:word.count},()=>({line,word}))));
  let t=0,index=0;
  for(const token of String(info.notes || '').split(',')) {
    const [pitch,beats]=token.split(':'),dur=Number(beats)*beat;
    if(!(dur>0)||!Number.isFinite(dur))continue;
    if(/^\d+$/.test(pitch)) {
      const item=syllables[index++];
      if(item){item.word.start??=t;item.word.end=t+dur;item.line.start??=t;item.line.end=t+dur;}
    }
    t+=dur;
  }
  // A mismatched lyric payload must not display plausible but wrong words.
  if(index!==syllables.length)return {member,lines:[]};
  return {member,color:/^#[a-f0-9]{6}$/i.test(info.captionColor || '') ? info.captionColor : '#8fd13f',
    lines:lines.map(line=>({...line,words:line.words.map(({count,...word})=>word)}))};
}
export function lyricPlan(plan) {
  return {...plan,lyricParts:(plan.payloads || []).slice(0,3).map(p=>lyricPart(p.info,p.member))};
}
export function currentLyrics(parts=[],elapsed) {
  return parts.flatMap(part=>{
    const line=part.lines.find(line=>elapsed>=line.start-1.5 && elapsed<line.end+.35);
    return line ? [{member:part.member,color:part.color,...line}] : [];
  });
}
