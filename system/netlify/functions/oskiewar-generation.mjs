import { createHash, randomUUID } from 'node:crypto';
import { authorize } from '../../backend/authorization.mjs';
import { connect } from '../../backend/database.mjs';
import { pseudonym, frozenFields } from './oskiewar-consent.mjs';
import { gateRoutes, verifyGenerationCapability, readGrantedPhoto, generateAppearance, RECIPE } from '../../backend/oskiewar-generation.mjs';
const respond = (statusCode, body) => ({ statusCode, headers: { 'Content-Type': 'application/json', 'Cache-Control': 'no-store' }, body: JSON.stringify(body) });
const SAVED_MS = 24 * 60 * 60 * 1000;

// AC owns the account binding. Handles are labels and may change; neither
// handles nor Auth0 subjects are sent to REGARDE or the appearance model.
async function fighterAccount(userSub) {
  const { db } = await connect();
  const profile = await db.collection('@handles').findOne({ _id: userSub });
  const handle = profile?.handle ? '@' + profile.handle.replace(/^@/, '') : null;
  const fighters = db.collection('oskiewar-fighters');
  await fighters.createIndex({ expiresAt: 1 }, { expireAfterSeconds: 0 });
  return { db, handle, fighters };
}

async function restoreFighter(account, subject, routes, token) {
  const saved = await account.fighters.findOne({ _id: subject });
  if (!saved) return respond(200, { handle: account.handle, status: 'empty' });
  const remove = async () => {
    await account.fighters.deleteOne({ _id: subject, 'fighter.hash': saved.fighter.hash });
    return respond(200, { handle: account.handle, status: 'empty' });
  };
  if (saved.expiresAt.getTime() <= Date.now()) return remove();
  const resumed = await fetch(routes.media.replace(/media$/, 'resume'), {
    method: 'POST', headers: { 'Content-Type': 'application/json', Authorization: `Bearer ${token}` },
    body: JSON.stringify({ subject, receipt: saved.fighter.receipt }), signal: AbortSignal.timeout(10000),
  });
  if ([403, 404].includes(resumed.status)) return remove();
  if (!resumed.ok) throw Object.assign(new Error('REGARDE could not check your saved fighter.'), { status: 503 });
  const renewal = await resumed.json();
  const keys = await fetch(routes.jwks, { signal: AbortSignal.timeout(10000) });
  if (!keys.ok) throw Object.assign(new Error('REGARDE keys are unavailable.'), { status: 503 });
  const capability = renewal.capability?.jws;
  const grant = verifyGenerationCapability(capability, await keys.json(), subject);
  if (grant.receipt !== saved.fighter.receipt || grant.scope_hash !== saved.fighter.scopeHash) return remove();
  await readGrantedPhoto({ routes, token, capability, subject, hash: saved.fighter.sourceHash });
  return respond(200, { handle: account.handle, status: 'accepted', fighter: saved.fighter,
    acceptedAt: saved.acceptedAt, savedUntil: saved.expiresAt,
    validUntil: Math.min(grant.exp * 1000, saved.expiresAt.getTime()) });
}
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
  if (!userSub) return respond(401, { message: 'Sign in to generate your fighter.' });
  if (input?.action === 'withdraw') {
    const { REGARDE_GATEWAY_URL: gateway, REGARDE_SUBJECT_SALT: salt, REGARDE_GATEWAY_TOKEN: token } = process.env;
    if (!gateway || !salt || !token) return respond(503, { message: 'REGARDE is unavailable.' });
    try {
      const url = new URL(gateRoutes(gateway).media); url.pathname = url.pathname.replace(/media$/, 'withdraw');
      const ff = frozenFields({ source: ['appearance'], outputs: ['fighter_mesh'], distribution: ['private_preview', 'local_gameplay'], retention: 'bound_to_purpose_scope' });
      const response = await fetch(url, { method: 'POST', headers: { 'Content-Type': 'application/json', Authorization: `Bearer ${token}` },
        body: JSON.stringify({ subject: pseudonym(userSub, salt), venue: 'oskiewar', operation_type: 'DATA_OPERATION',
          idempotency_key: randomUUID(), operation_descriptor: { purpose: 'oskiewar_fighter_generation', description: 'Withdraw my Oskiewar material.' },
          frozen_fields: { ...ff, operation_kind: 'WITHDRAW_CONSENT', retention_constraint: 'propagate_to_named_processors', data_handling_chain: ['oskiewar'] } }),
        signal: AbortSignal.timeout(10000) });
      const result = await response.json();
      if (!response.ok || !['withdrawn', 'nothing-to-withdraw'].includes(result.outcome)) return respond(502, { message: 'Withdrawal was not confirmed. Try again.' });
      const { db } = await connect();
      const subject = pseudonym(userSub, salt);
      await db.collection('oskiewar-fighters').deleteOne({ _id: subject });
      await db.collection('oskiewar-generation-jobs').deleteMany({ owner: userSub });
      return respond(200, { status: result.outcome });
    } catch { return respond(502, { message: 'Withdrawal was not confirmed. Try again.' }); }
  }
  if (!['generate', 'status', 'accept', 'account'].includes(input?.action) ||
      (input.action !== 'account' && (!/^[a-f0-9]{64}$/.test(input?.hash) || typeof input.capability !== 'string' || input.capability.length > 20000)))
    return respond(400, { message: 'A stored photo and capability are required.' });
  const { REGARDE_GATEWAY_URL: gateway, REGARDE_SUBJECT_SALT: salt, REGARDE_GATEWAY_TOKEN: token, OPENAI_API_KEY: key } = process.env;
  if (!gateway || !salt || !token) return respond(503, { message: 'Fighter generation is not configured.' });
  let jobs, id;
  try {
    const subject = pseudonym(userSub, salt), routes = gateRoutes(gateway);
    const account = await fighterAccount(userSub);
    if (!account.handle) return respond(409, { code: 'handle_required', message: 'Choose your AC handle before making a fighter.' });
    if (input.action === 'account') return await restoreFighter(account, subject, routes, token);
    const keysResponse = await fetch(routes.jwks, { signal: AbortSignal.timeout(10000) });
    if (!keysResponse.ok) throw Object.assign(new Error('REGARDE keys are unavailable.'), { status: 503 });
    const payload = verifyGenerationCapability(input.capability, await keysResponse.json(), subject);
    const read = () => readGrantedPhoto({ routes, token, capability: input.capability, subject, hash: input.hash });
    // The gate checks current grant/withdrawal, source category and receipt
    // binding on every request, including cache reads and after generation.
    const bytes = await read();
    const { db } = account;
    jobs = db.collection('oskiewar-generation-jobs');
    await jobs.createIndex({ expiresAt: 1 }, { expireAfterSeconds: 0 });
    id = createHash('sha256').update(JSON.stringify([subject, payload.receipt, input.hash, RECIPE])).digest('hex');
    const existing = await jobs.findOne({ _id: id });
    const result = job => respond(job.status === 'complete' ? 200 : job.status === 'running' ? 202 : 409,
      { id, handle: account.handle, status: job.status, ...(job.status === 'complete' ? { fighter: job.fighter,
        validUntil: Math.min(payload.exp * 1000, job.expiresAt.getTime()) } : {}),
        ...(job.status === 'failed' ? { message: 'This generation failed or was interrupted. It will not be charged again automatically.' } : {}) });
    if (existing) {
      if (existing.expiresAt.getTime() <= Date.now()) return respond(410, { message: 'This preview expired. Submit a new photo.' });
      if (input.action === 'accept') {
        if (existing.status !== 'complete') return respond(409, { message: 'Wait for a complete fighter before accepting it.' });
        const prior = await account.fighters.findOne({ _id: subject });
        const acceptedAt = prior?.fighter.hash === existing.fighter.hash ? prior.acceptedAt : new Date();
        const expiresAt = prior?.fighter.hash === existing.fighter.hash ? prior.expiresAt : new Date(Date.now() + SAVED_MS);
        await account.fighters.updateOne({ _id: subject }, { $set: {
          owner: userSub, fighter: existing.fighter, acceptedAt, expiresAt,
        } }, { upsert: true });
        // Close a withdrawal during acceptance before releasing the selection.
        try { await read(); } catch (error) {
          await account.fighters.deleteOne({ _id: subject, 'fighter.hash': existing.fighter.hash });
          throw error;
        }
        return respond(200, { handle: account.handle, status: 'accepted', fighter: existing.fighter,
          acceptedAt, savedUntil: expiresAt, validUntil: Math.min(payload.exp * 1000, expiresAt.getTime()) });
      }
      if (existing.status === 'running' && Date.now() - existing.startedAt.getTime() > 120000) {
        await jobs.updateOne({ _id: id, status: 'running' }, { $set: { status: 'failed' } });
        existing.status = 'failed';
      }
      return result(existing);
    }
    if (input.action !== 'generate') return respond(404, { message: 'No generation has started for this photo.' });
    if (!key) return respond(503, { message: 'Fighter generation is not configured.' });
    const expiresAt = new Date(payload.exp * 1000);
    try { await jobs.insertOne({ _id: id, owner: userSub, status: 'running', startedAt: new Date(), expiresAt }); }
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
