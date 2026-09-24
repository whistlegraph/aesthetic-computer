export function makeSubScore(score) {
  // Mallet basses carry upper modes; kicks carry a high noise click. Only
  // pitched content in the receiver's 20–200 Hz range belongs in the SUB.
  const source = score.lanes.filter(l => /^(bass|kick)$/.test(l.name)).flatMap(l => l.events.map((e,i) => ({...e,id:`${l.name}-${i}`,lane:l.name,hz:e.hz/2,g:Math.min(.65,Math.max(0,e.g*(score.gain??.35)))})));
  if (source.some(e => ![e.t,e.dur,e.hz,e.g].every(Number.isFinite) || e.t < 0 || e.dur <= 0)) throw Error('Invalid sub score');
  const events = source.filter(e => e.wave !== 'noise' && e.hz >= 20 && e.hz <= 200).sort((a,b)=>a.t-b.t);
  if (!events.length || events.some(e=>![e.t,e.dur,e.hz,e.g].every(Number.isFinite)||e.hz<20||e.hz>200)) throw Error('Invalid sub score');
  return {name:score.name,dur:score.dur,events};
}
export function isLive(s,age) { return age<2000 && !s?.error && s?.phase==='playing' && Number.isFinite(s?.scoreTime) && s.scoreTime>=0 && s.scoreTime<s.scoreDuration; }
export function windowEvents(events,from,to) { return events.filter(e=>e.t>=from && e.t<to); }
