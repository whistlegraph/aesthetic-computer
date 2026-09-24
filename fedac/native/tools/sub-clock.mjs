// Map an explicitly probed native audio clock to the SUB host's monotonic clock.
import { performance } from 'node:perf_hooks';
import { randomUUID } from 'node:crypto';
export const epochSeconds = () => (performance.timeOrigin + performance.now()) / 1000;

export async function probeAudioClock(host, attempts = 7) {
  if (!/^[-\w.]+(?::\d+)?$/.test(host)) throw new Error('Invalid source host');
  let best;
  for (let i = 0; i < attempts; i++) {
    const id = `sub-clock-${randomUUID()}`;
    const sent = epochSeconds();
    const put = await fetch(`http://${host}/pieces/spatial-rehearsal-command.json`, {
      method: 'PUT', body: JSON.stringify({ id, action: 'clock' }),
      signal: AbortSignal.timeout(3000),
    });
    if (!put.ok) throw new Error(`Clock request failed: ${put.status}`);
    let reply;
    while (epochSeconds() - sent < 3) {
      const response = await fetch(`http://${host}/pieces/spatial-rehearsal-clock.json`, {
        signal: AbortSignal.timeout(1500),
      });
      if (response.ok) {
        try { reply = await response.json(); } catch { /* Retry a concurrently rewritten clock file. */ }
      }
      if (reply?.id === id && Number.isFinite(reply.audioTime)) break;
      await new Promise(resolve => setTimeout(resolve, 10));
    }
    if (reply?.id !== id || !Number.isFinite(reply.audioTime)) throw new Error('Clock reply timed out');
    const received = epochSeconds();
    const rtt = received - sent;
    if (!best || rtt < best.rtt) best = {
      source: host, at: received, rtt,
      offset: reply.audioTime - (sent + received) / 2,
    };
  }
  if (!best) throw new Error('No clock samples');
  return best;
}

export function calibratedScoreTime(calibration, source, status, now = epochSeconds()) {
  if (!calibration || calibration.source !== source ||
      ![calibration.at, calibration.offset, calibration.rtt, now, status?.origin, status?.audioTime].every(Number.isFinite)) return null;
  const age = now - calibration.at;
  if (age < 0 || age > 1800 || calibration.rtt < 0 || calibration.rtt > 0.2) return null;
  const audioNow = now + calibration.offset;
  // A reboot, stale source or invalid mapping must not become a musical clock.
  if (Math.abs(audioNow - status.audioTime) > 2) return null;
  return audioNow - status.origin;
}
