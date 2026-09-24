export function trackInfo(score, state, time) {
  const duration = Number.isFinite(score?.dur) ? Math.max(0, score.dur) : 0;
  const phase = String(state?.phase || 'waiting');
  const elapsed = phase === 'finished' ? duration : state?.scoreTime == null ? 0 : Math.max(0, Math.min(duration, Number.isFinite(time) ? time : 0));
  return {
    title: String(score?.title || score?.name || 'Waiting for a piece').replace(' Native · gm128-fx-kick · selected echo + flange', ' - Echo + Flange').replace(/[—–·]/g, '-'),
    source: state?.source === 'Trio conductor' ? 'MacNeoPolitan Trio' : 'Notepat spatial',
    phase, elapsed, duration, hash: score?.hash || '',
  };
}
export function trackLines(title, max = 52) {
  const words=String(title).split(/\s+/);const lines=[''];
  for(const word of words){const i=lines.length-1;
    if(lines[i] && lines[i].length+word.length+1>max)lines.push(word);
    else lines[i]+=(lines[i]?' ':'')+word;
  }
  return lines.slice(0,2).map((line,i)=>line.length>max||i===1&&lines.length>2?line.slice(0,max-3)+'...':line);
}
