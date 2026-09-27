// oskiewar-feed.mjs — the room's display feed, shared by the oskiewar and
// venue MCPs. blueberry:8796 holds one transport at a time (stale after
// 1.25 s); neo's stage service polls /api/performance and relays it to the
// Xbox (oskiewar) and the ac7 AC OS display. Field names are the Trio
// runner's contract (grants/culturehub-la-2026/macneopolitan/bin/run-full-trio.py).
export const FEED = (process.env.OSKIEWAR_FEED || "http://192.168.1.234:8796").replace(/\/$/, "");
export const MEMBER_RGB = { neo: [143, 209, 63], blueberry: [90, 87, 211], frisbee: [242, 167, 185] };
export async function feedStatus(ms = 2500) {
  try {
    const r = await fetch(`${FEED}/api/performance`, { signal: AbortSignal.timeout(ms) });
    const p = r.ok ? await r.json() : null;
    return { reachable: r.ok, showing: p && { title: p.title, dance: p.dance, section: p.section, elapsed: p.elapsed == null ? null : Math.round(p.elapsed * 10) / 10, duration: p.duration, lyric: p.lyric ?? null, next: p.next ?? null, hits: Array.isArray(p.hits) ? p.hits.length : undefined } };
  } catch (e) { return { reachable: false, error: String(e.message || e) }; }
}
export async function postTransport(body, ms = 2000) {
  const r = await fetch(`${FEED}/api/transport`, { method: "POST", headers: { "Content-Type": "application/json", Origin: FEED }, body: JSON.stringify(body), signal: AbortSignal.timeout(ms) });
  if (r.status !== 204 && !r.ok) throw new Error(`feed refused the transport: ${r.status} ${await r.text()}`);
  return { status: r.status };
}
/** A playing transport carrying a lyric, kept fresh with heartbeats for `seconds`, then cleared. */
export async function showLyric({ text, member = "neo", rgb, next, seconds = 4, title = "MacNeoPolitan Trio", bpm = 90, elapsedFrom = 0, onBeat }) {
  const start = Date.now(); let beats = 0;
  const body = (elapsed, playing = true) => ({ playing, elapsed, title, dance: "trio-round-v1", bpm, duration: Math.max(seconds + 1, elapsedFrom + seconds + 1),
    lyric: playing ? { text, member, rgb: rgb || MEMBER_RGB[member] || MEMBER_RGB.neo, t: elapsedFrom, dur: seconds } : null,
    next: playing && next ? { text: next.text || String(next), member: next.member || member, rgb: next.rgb || MEMBER_RGB[next.member || member], in: next.in ?? seconds } : null });
  while (Date.now() - start < seconds * 1000) {
    await postTransport(body(elapsedFrom + (Date.now() - start) / 1000)); beats++; onBeat?.(beats);
    await new Promise((r) => setTimeout(r, 500));
  }
  await postTransport(body(0, false));
  return { heartbeats: beats, seconds };
}
export const clearFeed = () => postTransport({ playing: false, elapsed: 0, dance: "trio-round-v1", lyric: null, next: null });
