// POST /api/clairic { state, fields, referenceDate? } → { answers, usage, … }
// Cloudflare's Clairic extraction model behind a handle and a daily allowance.
import { connect } from '../../backend/database.mjs';
import { authorize, getHandleOrEmail } from '../../backend/authorization.mjs';
import { createClairicHandler, clairicBudget } from '../../backend/clairic.mjs';

let storage;
async function budget() {
  return storage ??= (async () => {
    const { db } = await connect();
    const collection = db.collection('clairic-budget');
    await collection.createIndex({ expiresAt: 1 }, { expireAfterSeconds: 0 });
    return clairicBudget(collection);
  })().catch(error => { storage = null; throw error; });
}

async function authenticate(headers) {
  const user = await authorize(headers);
  if (!user?.sub) return null;
  const handle = await getHandleOrEmail(user.sub);
  return typeof handle === 'string' && handle.startsWith('@') ? user.sub : null;
}

const run = createClairicHandler({ authenticate, budget: { consume: async (...args) => (await budget()).consume(...args) } });

export const handler = event => {
  if (!process.env.CLOUDFLARE_ACCOUNT_ID || !process.env.CLOUDFLARE_AI_TOKEN)
    return { statusCode: 503, body: JSON.stringify({ error: 'Clairic is not configured.' }) };
  return run(event);
};
