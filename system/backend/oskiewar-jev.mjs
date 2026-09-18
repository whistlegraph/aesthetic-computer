import { randomUUID } from 'node:crypto';
import { evaluateChoices } from '../../easel/src/jev-decisions.mjs';
import { decisionRequest, buttons } from '../../xbox/live/jev-vs-jev/model.mjs';

export const MATCH_MS = 60_000;
export const CALLS_PER_SEAT = 150;
const DAILY_CALLS = 20_000;

// Reserve a bounded match budget atomically before issuing its opaque ticket.
// Counters persist across restarts; failures consume their reservation too.
export function mongoStore(collection) {
  return {
    async start(now) {
      const day = `day:${new Date(now).toISOString().slice(0, 10)}`;
      try { await collection.updateOne({ _id: day }, { $setOnInsert: { reserved: 0, expiresAt: new Date(now + 3 * 86400000) } }, { upsert: true }); }
      catch (error) { if (error.code !== 11000) throw error; }
      const daily = await collection.findOneAndUpdate({ _id: day, reserved: { $lte: DAILY_CALLS - CALLS_PER_SEAT * 2 } },
        { $inc: { reserved: CALLS_PER_SEAT * 2 } }, { returnDocument: 'after' });
      if (!(daily?.value ?? daily)?.reserved) return null;
      const id = randomUUID();
      await collection.insertOne({ _id: id, expiresAt: new Date(now + MATCH_MS + 15_000), calls0: 0, calls1: 0 });
      return id;
    },
    async consume(id, seat, now) {
      const key = `calls${seat}`;
      const value = await collection.findOneAndUpdate({ _id: id, expiresAt: { $gt: new Date(now) }, [key]: { $lt: CALLS_PER_SEAT } },
        { $inc: { [key]: 1 } }, { returnDocument: 'after' });
      return !!(value?.value ?? value)?.[key];
    },
  };
}

export function createHandler({ store, evaluate = evaluateChoices, now = Date.now } = {}) {
  const busy = new Set();
  const starts = new Map();
  const reply = (statusCode, body) => ({ statusCode,
    headers: { 'Content-Type': 'application/json', 'Cache-Control': 'no-store' }, body: JSON.stringify(body) });
  return async event => {
    if (event.httpMethod !== 'POST') return reply(405, { error: 'POST only' });
    if (typeof event.body !== 'string' || event.body.length > 12_000) return reply(400, { error: 'Invalid request' });
    let body;
    try { body = JSON.parse(event.body); } catch { return reply(400, { error: 'Invalid JSON' }); }
    if (body?.op === 'start') {
      const ip = event.headers?.['x-forwarded-for']?.split(',')[0] || 'unknown';
      const at = now();
      for (const [key, expiry] of starts) if (expiry <= at) starts.delete(key);
      if (starts.has(ip) || starts.size > 2000) return reply(429, { error: 'Wait a minute before starting another match.' });
      starts.set(ip, at + MATCH_MS);
      try {
        const id = await store.start(at);
        return id ? reply(200, { ticket: id, durationMs: MATCH_MS, callsPerSeat: CALLS_PER_SEAT })
          : reply(429, { error: 'Today’s demo allowance is used up.' });
      } catch { return reply(503, { error: 'Demo allowance is unavailable.' }); }
    }
    if (!/^[a-f0-9-]{36}$/.test(body?.ticket || '') || ![0,1].includes(body?.seat))
      return reply(400, { error: 'Start a match first.' });
    let request;
    try { request = decisionRequest(body.scene); } catch { return reply(400, { error: 'Invalid fighter observation.' }); }
    const slot = `${body.ticket}:${body.seat}`;
    if (busy.has(slot) || busy.size >= 12) return reply(429, { error: 'Decision already running; pause and try again.' });
    busy.add(slot);
    try {
      if (!await store.consume(body.ticket, body.seat, now())) return reply(429, { error: 'Match allowance ended.' });
      const started = performance.now();
      const result = await evaluate(request, { signal: AbortSignal.timeout(1500) });
      const move = result.answers.motion.choice, action = result.answers.action.choice;
      const usage = result.usage;
      if (![usage?.input_tokens, usage?.output_tokens, usage?.cost].every(n => Number.isFinite(n) && n >= 0))
        return reply(502, { error: 'Provider omitted usage; match paused.' });
      return reply(200, { motion: move, action, down: buttons(move, action),
        elapsedMs: Math.round(performance.now() - started), model: result.model,
        usage: { inputTokens: usage.input_tokens, outputTokens: usage.output_tokens, costUsd: usage.cost } });
    } catch { return reply(503, { error: 'Jev is unavailable; no move was substituted.' }); }
    finally { busy.delete(slot); }
  };
}
