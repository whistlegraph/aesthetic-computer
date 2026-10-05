import { tezosPayments, paymentError, displayPrice } from '../../backend/tezos-credits.mjs';

const headers = { 'Content-Type':'application/json', 'Cache-Control':'no-store',
  'Access-Control-Allow-Origin':'*', 'Access-Control-Allow-Headers':'Authorization, Content-Type',
  'Access-Control-Allow-Methods':'POST, OPTIONS' };
const reply = (statusCode, body) => ({ statusCode, headers, body:JSON.stringify(body) });

export function createHandler({ payments, authorize, handleFor, enabled = true, price = displayPrice }) {
  return async event => {
    if (event.httpMethod === 'OPTIONS') return reply(204, null);
    if (event.httpMethod !== 'POST') return reply(405, { error:'POST only' });
    if ((event.body || '').length > 12000) return reply(413, { error:'Request too large' });
    let body;
    try { body = JSON.parse(event.body || '{}'); } catch { return reply(400, { error:'Invalid JSON' }); }
    if (!body || typeof body !== 'object' || Array.isArray(body)) return reply(400, { error:'Invalid request' });
    try {
      if (body.action === 'price') return reply(200, await price());
      if (body.action === 'create') {
        if (!enabled) throw paymentError(503, 'Tezos purchases are not available yet');
        let user;
        try { user = await authorize(event.headers || {}); } catch { throw paymentError(401, 'Sign in to buy braincells'); }
        if (!user?.sub) throw paymentError(401, 'Sign in to buy braincells');
        const handle = await handleFor(user.sub);
        if (typeof handle !== 'string' || !handle.startsWith('@')) throw paymentError(403, 'Claim an AC handle before buying braincells');
        return reply(200, await payments.create(user.sub, handle));
      }
      // Checkout-scoped bearer capability; never an AC login token in a URL.
      const secret = (event.headers?.authorization || event.headers?.Authorization || '').replace(/^Bearer /, '');
      if (body.action === 'status') return reply(200, await payments.status(secret));
      if (body.action === 'quote') {
        if (!enabled) throw paymentError(503, 'Tezos purchases are not available yet');
        return reply(200, await payments.quote(secret, body));
      }
      if (body.action === 'confirm') return reply(200, await payments.confirm(secret, body.operationHash));
      return reply(400, { error:'Unknown action' });
    } catch (error) {
      if (!error.status) console.error('Tezos checkout failed:', error.name);
      return reply(error.status || 503, { error:error.status ? error.message : 'Could not check payment yet. Your payment can be checked again.' });
    }
  };
}

export async function handler(event) {
  // Public price reads and preflight do not need a database or login.
  if (event.httpMethod !== 'POST' || event.body?.length > 12000) return createHandler({})(event);
  try { if (JSON.parse(event.body || '{}')?.action === 'price') return createHandler({})(event); } catch {}
  const [{ connect }, { authorize, getHandleOrEmail }] = await Promise.all([
    import('../../backend/database.mjs'), import('../../backend/authorization.mjs'),
  ]);
  const connection = await connect();
  try {
    const db = connection.db;
    return await createHandler({ authorize, handleFor:getHandleOrEmail,
      enabled:process.env.AC_TEZOS_CREDITS_ENABLED === 'true',
      payments:tezosPayments({ intents:db.collection('ac-tezos-checkouts'), claims:db.collection('ac-tezos-payments'),
        wallets:db.collection('ac-credit-wallets') }) })(event);
  } finally { await connection.disconnect(); }
}
