#!/usr/bin/env node
// Controller-side presence publisher. The separate data bridge remains read-only.
const hosts = process.argv.slice(2);
if (!hosts.length || hosts.some(h => !/^[a-zA-Z0-9.-]+(?::\d+)?$/.test(h)))
  throw Error('usage: spatial-presence.mjs HOST... (keep original seat order)');
const delay = ms => new Promise(resolve => setTimeout(resolve, ms));
let sequence = 0, previous = '';
while (true) {
  try {
    const r = await fetch('http://127.0.0.1:8787/api/seats', { signal: AbortSignal.timeout(1500) });
    if (!r.ok) throw Error(`Bridge HTTP ${r.status}`);
    const snapshot = await r.json();
    const seats = hosts.map((host, seat) => {
      const sample = snapshot.seats.find(s => s.host === host);
      let state = 'unknown';
      if (sample?.connected && !sample.stale) state = sample.data?.error ? 'error' : 'online';
      else if (sample) state = !sample.connected && (sample.ageMs === null || sample.ageMs > 3000) ? 'offline' : 'unstable';
      if (sample?.data && sample.data.seat !== seat) state = 'error';
      return { seat, host, state, ageMs: sample?.ageMs ?? null, battery: sample?.data?.battery?.percent ?? null };
    });
    const text = JSON.stringify({ sequence: ++sequence, at: new Date().toISOString(), seats });
    const summary = seats.map(s => `${s.seat + 1}:${s.state}`).join(' ');
    if (summary !== previous) { console.log(summary); previous = summary; }
    await Promise.allSettled(seats.filter(s => s.state !== 'offline').map(async s => {
      const response = await fetch(`http://${s.host}/pieces/spatial-rehearsal-presence.json`, {
        method: 'PUT', body: text, signal: AbortSignal.timeout(1500),
      });
      if (!response.ok) throw Error(`${s.host}: ${response.status}`);
    }));
  } catch (e) { console.error(e.message); } // Screens expire old presence after 5 s.
  await delay(1000);
}
