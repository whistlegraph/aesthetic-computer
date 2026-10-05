import { whistlegraphMints, mintError } from '../../backend/whistlegraph-mint.mjs';
const headers = { 'Content-Type':'application/json', 'Cache-Control':'no-store',
  'Access-Control-Allow-Origin':'*', 'Access-Control-Allow-Headers':'Authorization, Content-Type',
  'Access-Control-Allow-Methods':'POST, OPTIONS' };
const reply = (statusCode, body) => ({ statusCode, headers, body:JSON.stringify(body) });

export function createHandler({ mints, authorize, handleFor, pilot = false, start = () => {} }) {
  return async event => {
    if (event.httpMethod === 'OPTIONS') return reply(204, null);
    if (event.httpMethod !== 'POST') return reply(405, { error:'POST only' });
    if ((event.body || '').length > 1_500_000) return reply(413, { error:'Request too large' });
    let body;
    try { body = JSON.parse(event.body || '{}'); } catch { return reply(400, { error:'Invalid JSON' }); }
    if (!body || typeof body !== 'object' || Array.isArray(body)) return reply(400, { error:'Invalid request' });
    try {
      const secret = (event.headers?.authorization || event.headers?.Authorization || '').replace(/^Bearer /, '');
      if (body.action === 'create') {
        let user;
        try { user = await authorize(event.headers || {}); } catch {}
        if (!user?.sub) throw mintError(401, 'Sign in to mint your artwork');
        const handle = await handleFor(user.sub);
        // First device test; broaden only after wallet + marketplace validation.
        if (!pilot || handle !== '@jeffrey') throw mintError(403, 'Whistlegraph minting is being tested');
        const result = await mints.create(user.sub, handle, body);
        if (result.status === 'packing') start(body.secret);
        return reply(200, result);
      }
      if (body.action === 'status') {
        const result = await mints.status(secret);
        if (result.status === 'packing') start(secret);
        return reply(200, result);
      }
      if (body.action === 'bind') return reply(200, await mints.bind(secret, body));
      if (body.action === 'begin') return reply(200, await mints.begin(secret));
      if (body.action === 'cancel') return reply(200, await mints.cancel(secret));
      if (body.action === 'confirm') return reply(200, await mints.confirm(secret, body.operationHash));
      return reply(400, { error:'Unknown action' });
    } catch (error) {
      if (!error.status) console.error('Whistlegraph mint failed:', error.name);
      return reply(error.status || 503, { error:error.status ? error.message : 'Mint preparation is unavailable. Reopen this preview to check again.' });
    }
  };
}
const running = new Map();
export async function handler(event) {
  if (event.httpMethod !== 'POST' || event.body?.length > 1_500_000) return createHandler({})(event);
  const [{ connect }, { authorize, getHandleOrEmail }, { packWhistlegraph, normalizeMintCover, pinMintFile }] = await Promise.all([
    import('../../backend/database.mjs'), import('../../backend/authorization.mjs'), import('../../backend/whistlegraph-pack.mjs'),
  ]);
  const connection = await connect();
  const mints = whistlegraphMints({ intents:connection.db.collection('whistlegraph-mints'),
    threads:connection.db.collection('walkieware-threads'), pack:packWhistlegraph, cover:normalizeMintCover, pin:pinMintFile });
  const start = secret => {
    if (running.has(secret) || running.size >= 2) return;
    const job = mints.prepare(secret).catch(error => console.error('Whistlegraph pack failed:', error.name))
      .finally(() => running.delete(secret));
    running.set(secret, job);
  };
  try { return await createHandler({ mints, authorize, handleFor:getHandleOrEmail,
    pilot:process.env.WHISTLEGRAPH_MINT_PILOT === 'true', start })(event); }
  finally { await connection.disconnect(); }
}
