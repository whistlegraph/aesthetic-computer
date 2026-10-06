import { makeVerifiers } from '../../backend/whistlegraph-iap.mjs';
import { sandboxAccounts, withWhistlegraphPurchases } from '../../backend/whistlegraph-iap-store.mjs';
const headers = { 'Content-Type': 'application/json', 'Cache-Control': 'no-store', 'Access-Control-Allow-Origin': '*',
  'Access-Control-Allow-Headers': 'Authorization, Content-Type', 'Access-Control-Allow-Methods': 'POST, OPTIONS' };
const reply = (statusCode, value) => ({ statusCode, headers, body: JSON.stringify(value) });

export function createHandler({ service, authorize, salesEnabled = true }) {
  return async event => {
    if (event.httpMethod === 'OPTIONS') return reply(204, null);
    if (event.httpMethod !== 'POST') return reply(405, { error: 'POST only' });
    if (Buffer.byteLength(event.body || '', 'utf8') > 64_000) return reply(413, { error: 'Request too large.' });
    let body;
    try { body = JSON.parse(event.body || '{}'); } catch { return reply(400, { error: 'Invalid request.' }); }
    if (!body || typeof body !== 'object' || Array.isArray(body)) return reply(400, { error: 'Invalid request.' });
    try {
      if (typeof body.signedPayload === 'string') return reply(200, await service.notification(body.signedPayload));
      const user = await authorize(event.headers || {}).catch(() => null);
      if (!user?.sub) return reply(401, { error: 'Sign in to AC to buy braincells.' });
      if (body.action === 'account') return salesEnabled
        ? reply(200, await service.account(user.sub))
        : reply(503, { error: 'App Store purchases are not available yet.' });
      if (body.action === 'redeem') return reply(200, await service.redeem(user.sub, body.jws));
      return reply(400, { error: 'Unknown action.' });
    } catch (error) {
      return reply(error.status || 503, { error: error.status ? error.message : 'Purchase delivery is pending. Reopen Whistlegraph to retry.' });
    }
  };
}

export async function handler(event) {
  if (event.httpMethod === 'OPTIONS' || event.httpMethod !== 'POST') return createHandler({})(event);
  // Pausing new sales must not strand already-paid transactions or refunds.
  const salesEnabled = process.env.WHISTLEGRAPH_IAP_ENABLED === 'true';
  let verifiers;
  const sandboxUsers = sandboxAccounts();
  try {
    verifiers = makeVerifiers({ appAppleId: Number(process.env.WHISTLEGRAPH_APPLE_ID),
      sandbox: process.env.WHISTLEGRAPH_IAP_ALLOW_SANDBOX === 'true' && sandboxUsers.size > 0 });
  } catch { return reply(503, { error: 'App Store purchases are not configured yet.' }); }
  try {
    const { authorize } = await import('../../backend/authorization.mjs');
    return await withWhistlegraphPurchases(service => createHandler({ service, authorize, salesEnabled })(event), { verifiers });
  } catch { return reply(503, { error: 'Purchase delivery is pending. Reopen Whistlegraph to retry.' }); }
}
