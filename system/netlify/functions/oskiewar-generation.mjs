import { createHash } from 'node:crypto';
import { authorize } from '../../backend/authorization.mjs';
import { connect } from '../../backend/database.mjs';
import { pseudonym } from './oskiewar-consent.mjs';
import { gateRoutes, verifyGenerationCapability, readGrantedPhoto, generateAppearance, RECIPE } from '../../backend/oskiewar-generation.mjs';
const respond = (statusCode, body) => ({ statusCode, headers: { 'Content-Type': 'application/json', 'Cache-Control': 'no-store' }, body: JSON.stringify(body) });
export async function handler(event) {
  if (event.httpMethod !== 'POST') return respond(405, { message: 'POST only.' });
  const user = await authorize(event.headers);
  if (!user?.sub) return respond(401, { message: 'Sign in to generate your fighter.' });
  let input;
  try { input = JSON.parse(event.body); } catch { return respond(400, { message: 'Unreadable generation request.' }); }
  return generateForUser(input, user.sub);
}

// Trusted host entry point for recovering an authenticated player's own job.
export async function generateForUser(input, userSub) {
  if (!['generate', 'status'].includes(input?.action) || !/^[a-f0-9]{64}$/.test(input?.hash) || typeof input.capability !== 'string' || input.capability.length > 20000)
    return respond(400, { message: 'A stored photo and capability are required.' });
  const { REGARDE_GATEWAY_URL: gateway, REGARDE_SUBJECT_SALT: salt, REGARDE_GATEWAY_TOKEN: token, OPENAI_API_KEY: key } = process.env;
  if (!gateway || !salt || !token || !key) return respond(503, { message: 'Fighter generation is not configured.' });
  let jobs, id;
  try {
    const subject = pseudonym(userSub, salt), routes = gateRoutes(gateway);
    const keysResponse = await fetch(routes.jwks, { signal: AbortSignal.timeout(10000) });
    if (!keysResponse.ok) throw Object.assign(new Error('REGARDE keys are unavailable.'), { status: 503 });
    const payload = verifyGenerationCapability(input.capability, await keysResponse.json(), subject);
    const read = () => readGrantedPhoto({ routes, token, capability: input.capability, subject, hash: input.hash });
    // The gate checks current grant/withdrawal, source category and receipt
    // binding on every request, including cache reads and after generation.
    const bytes = await read();
    const { db } = await connect();
    jobs = db.collection('oskiewar-generation-jobs');
    await jobs.createIndex({ expiresAt: 1 }, { expireAfterSeconds: 0 });
    id = createHash('sha256').update(JSON.stringify([subject, payload.receipt, input.hash, RECIPE])).digest('hex');
    const existing = await jobs.findOne({ _id: id });
    const result = job => respond(job.status === 'complete' ? 200 : job.status === 'running' ? 202 : 409,
      { id, status: job.status, ...(job.status === 'complete' ? { fighter: job.fighter,
        validUntil: Math.min(payload.exp * 1000, job.expiresAt.getTime()) } : {}),
        ...(job.status === 'failed' ? { message: 'This generation failed or was interrupted. It will not be charged again automatically.' } : {}) });
    if (existing) {
      if (existing.expiresAt.getTime() <= Date.now()) return respond(410, { message: 'This preview expired. Submit a new photo.' });
      if (existing.status === 'running' && Date.now() - existing.startedAt.getTime() > 120000) {
        await jobs.updateOne({ _id: id, status: 'running' }, { $set: { status: 'failed' } });
        existing.status = 'failed';
      }
      return result(existing);
    }
    if (input.action === 'status') return respond(404, { message: 'No generation has started for this photo.' });
    const expiresAt = new Date(payload.exp * 1000);
    try { await jobs.insertOne({ _id: id, status: 'running', startedAt: new Date(), expiresAt }); }
    catch (error) { if (error.code === 11000) return respond(202, { id, status: 'running' }); throw error; }
    const budget = db.collection('oskiewar-generation-budget');
    await budget.createIndex({ expiresAt: 1 }, { expireAfterSeconds: 0 });
    const budgetId = `${subject}:${new Date().toISOString().slice(0, 10)}`;
    await budget.updateOne({ _id: budgetId }, { $setOnInsert: { count: 0, expiresAt: new Date(Date.now() + 2 * 86400000) } }, { upsert: true });
    const allowed = await budget.updateOne({ _id: budgetId, count: { $lt: 3 } }, { $inc: { count: 1 } });
    if (!allowed.modifiedCount) {
      await jobs.updateOne({ _id: id }, { $set: { status: 'failed' } });
      return respond(429, { message: 'Today’s three fighter generations have been used.' });
    }
    const generated = await generateAppearance(bytes, { key, model: process.env.OSKIEWAR_FIGHTER_MODEL || 'gpt-4.1-mini' });
    await read();
    if (Date.now() >= expiresAt.getTime()) throw Object.assign(new Error('The generation capability expired. No preview was released.'), { status: 403 });
    const fighter = { version: 1, ...generated, receipt: payload.receipt, scopeHash: payload.scope_hash, sourceHash: input.hash };
    fighter.hash = createHash('sha256').update(JSON.stringify(fighter)).digest('hex');
    await jobs.updateOne({ _id: id }, { $set: { status: 'complete', fighter } });
    return result({ status: 'complete', fighter, expiresAt });
  } catch (error) {
    if (jobs && id) await jobs.updateOne({ _id: id, status: 'running' }, { $set: { status: 'failed' }, $unset: { fighter: '' } });
    return respond(error.status || 502, { message: error.status ? error.message : 'Generation did not complete. Check status before trying again.' });
  }
}
