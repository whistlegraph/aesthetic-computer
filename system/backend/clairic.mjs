import { createHash } from 'node:crypto';
import { extractFields, CLAIRIC_TYPES } from '../../aesel/src/clairic.mjs';

// Text in, typed values out. A signed-in handle spends from three counters
// (its minute, its day, everyone's day), reserved before the provider call.
export const LIMITS = { perMinute: 20, perDay: 300, globalPerDay: 5000 };
const MAX_BODY = 64_000, MAX_STATE = 40_000, MAX_FIELDS = 32, MAX_FIELD_JSON = 600;

export function clairicBudget(collection, limits = LIMITS) {
  async function reserve(id, limit, expiresAt) {
    try { await collection.updateOne({ _id: id }, { $setOnInsert: { count: 0, expiresAt } }, { upsert: true }); }
    catch (error) { if (error.code !== 11000) throw error; }
    const row = await collection.findOneAndUpdate({ _id: id, count: { $lt: limit } },
      { $inc: { count: 1 } }, { returnDocument: 'after' });
    return Boolean((row?.value ?? row)?.count);
  }
  return { async consume(subject, now) {
    const day = new Date(now).toISOString().slice(0, 10);
    const user = createHash('sha256').update(subject).digest('hex');
    const expires = new Date(now + 2 * 86400000);
    return await reserve(`minute:${Math.floor(now / 60000)}:${user}`, limits.perMinute, expires)
      && await reserve(`user:${day}:${user}`, limits.perDay, expires)
      && await reserve(`global:${day}`, limits.globalPerDay, expires);
  } };
}

// Only the request shape is checked here; Cloudflare validates per-type options.
export function extractionRequest(body) {
  if (!body || typeof body !== 'object' || Object.keys(body).some(k => !['state', 'fields', 'referenceDate'].includes(k))) throw Error();
  const { state, fields, referenceDate } = body;
  if (typeof state !== 'string' || !state.trim() || state.length > MAX_STATE) throw Error();
  if (!fields || typeof fields !== 'object' || Array.isArray(fields)) throw Error();
  const names = Object.keys(fields);
  if (!names.length || names.length > MAX_FIELDS) throw Error();
  for (const name of names) {
    const spec = fields[name];
    if (!/^[a-z][a-z0-9_]{0,39}$/i.test(name) || !CLAIRIC_TYPES.includes(spec?.type)
      || JSON.stringify(spec).length > MAX_FIELD_JSON) throw Error();
  }
  if (referenceDate !== undefined && !/^\d{4}-\d{2}-\d{2}$/.test(referenceDate)) throw Error();
  return { state, fields, referenceDate };
}

export function createClairicHandler({ authenticate, budget, extract = extractFields, now = Date.now } = {}) {
  const busy = new Set();
  const reply = (statusCode, value) => ({ statusCode, headers: {
    'Content-Type': 'application/json', 'Cache-Control': 'no-store', 'Access-Control-Allow-Origin': '*',
    'Access-Control-Allow-Headers': 'Content-Type, Authorization', 'Access-Control-Allow-Methods': 'POST, OPTIONS',
  }, body: JSON.stringify(value) });
  return async event => {
    if (event.httpMethod === 'OPTIONS') return reply(200, {});
    if (event.httpMethod !== 'POST') return reply(405, { error: 'POST only' });
    if (!event.headers?.authorization) return reply(401, { error: 'Sign in first' });
    if (typeof event.body !== 'string' || event.body.length > MAX_BODY) return reply(400, { error: 'Invalid request' });
    let request;
    try { request = extractionRequest(JSON.parse(event.body)); } catch { return reply(400, { error: 'Invalid request' }); }
    let subject;
    try { subject = await authenticate(event.headers); } catch { return reply(503, { error: 'Account check unavailable' }); }
    if (!subject) return reply(401, { error: 'A valid account with a handle is required' });
    if (busy.has(subject) || busy.size >= 8) return reply(429, { error: 'Extraction already running' });
    busy.add(subject);
    try {
      if (!await budget.consume(subject, now())) return reply(429, { error: 'Extraction allowance reached' });
      const started = performance.now();
      const result = await extract(request, { signal: AbortSignal.timeout(20000) });
      return reply(200, { answers: result.answers, truncated: Boolean(result.truncated),
        usage: result.usage ?? null, model: result.model ?? 'clairic', elapsedMs: Math.round(performance.now() - started) });
    } catch { return reply(503, { error: 'Clairic is unavailable; nothing was extracted.' }); }
    finally { busy.delete(subject); }
  };
}
