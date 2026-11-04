// Private operational summaries. Opt in with DOOL_DIAGNOSTICS=1; no content,
// handles, room names, owner tokens, URLs, stacks or provider error text.
import { randomUUID } from 'node:crypto';

const events = new Set(['build.accepted', 'build.phase', 'build.finished', 'build.rejected',
  'build.publication', 'client.sample', 'server.sample', 'connection.closed']);
const enums = {
  outcome: ['committed', 'failed', 'cancelled', 'timeout', 'disconnected', 'shutdown', 'published'],
  phase: ['connecting', 'generating', 'correcting', 'validating', 'waiting'],
  reason: ['identity', 'allowance', 'invalid_request', 'busy', 'ownership', 'stale_version',
    'version_limit', 'capacity', 'offline', 'timeout', 'relay_auth', 'relay', 'geometry', 'storage', 'unknown'],
  kind: ['object', 'kidlisp', 'restore'],
  backend: ['webgpu', 'webgl2', 'unknown'],
};
const numbers = new Set(['elapsedMs', 'phaseMs', 'previewCount', 'previewRejected', 'previewMaxMs',
  'commitMs', 'durationMs', 'frames', 'frameGapMaxMs', 'frameWorkMaxMs', 'renderMaxMs',
  'snapshotGapMaxMs', 'longTasks', 'longTaskMaxMs', 'frameErrors', 'messageMaxMs',
  'rttMs', 'jitterMs', 'queuedBytes', 'objects', 'triangles', 'loopGapMaxMs', 'tickMaxMs',
  'clients', 'builds', 'closeCode']);
const uuid = (v) => typeof v === 'string' && /^[a-f0-9]{8}(?:-[a-f0-9]{4}){3}-[a-f0-9]{12}$/i.test(v);

export function buildReason(error) {
  const m = String(error?.message || error || '');
  if (/identity|rejoin/i.test(m)) return 'identity';
  if (/braincells/i.test(m)) return 'allowance';
  if (/characters|choose a change/i.test(m)) return 'invalid_request';
  if (/already making|busy/i.test(m)) return 'busy';
  if (/owner|spectator/i.test(m)) return 'ownership';
  if (/changed/i.test(m)) return 'stale_version';
  if (/version limit/i.test(m)) return 'version_limit';
  if (/capacity/i.test(m)) return 'capacity';
  if (/offline/i.test(m)) return 'offline';
  if (/timed out/i.test(m)) return 'timeout';
  if (/login refreshed/i.test(m)) return 'relay_auth';
  if (/relay/i.test(m)) return 'relay';
  if (/object rejected|geometry|shape|triangle|bound|space|form|source|box/i.test(m)) return 'geometry';
  if (/ENOSPC|EACCES|EIO|EROFS/.test(error?.code || '')) return 'storage';
  return 'unknown';
}

export function createDiagnostics({ enabled = process.env.DOOL_DIAGNOSTICS === '1',
  write = (row) => console.info('dool.diagnostic ' + JSON.stringify(row)), now = Date.now,
  revision = 'local' } = {}) {
  const run = randomUUID();
  let second = 0, count = 0;
  return {
    enabled,
    setRevision(value) { revision = value; },
    emit(event, fields = {}) {
      if (!enabled || !events.has(event)) return;
      const at = now(), bucket = Math.floor(at / 1000);
      if (bucket !== second) { second = bucket; count = 0; }
      if (++count > 120) return; // Global ceiling, including hostile requests.
      const row = { schema: 1, event, at, run,
        revision: /^[a-f0-9]{7,40}$/i.test(revision) ? revision : 'local' };
      for (const [key, value] of Object.entries(fields)) {
        if (['id', 'session'].includes(key) && uuid(value)) row[key] = value;
        else if (key === 'buildIds' && Array.isArray(value)) row[key] = value.filter(uuid).slice(0, 12);
        else if (key === 'code' && typeof value === 'string' && /^[a-z0-9]{2,16}$/i.test(value)) row[key] = value;
        else if (enums[key]?.includes(value)) row[key] = value;
        else if (numbers.has(key) && Number.isFinite(value) && value >= 0)
          row[key] = Math.round(Math.min(value, 3600000) * 10) / 10;
      }
      // Observability must not interrupt movement, persistence, or publication.
      try { write(row); } catch {}
    },
  };
}
