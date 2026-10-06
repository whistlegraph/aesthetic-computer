import {requestKey, noPaintBilling, ensureNoPaintBillingIndexes} from '../../backend/nopaint-billing.mjs';
import {moveOffer, moveInput, createNoPaintProvider} from '../../backend/nopaint-provider.mjs';
import {openRouterOffers, createOpenRouterProvider} from '../../backend/nopaint-openrouter.mjs';

const reply = (statusCode, value) => ({statusCode, headers:{
  'Content-Type':'application/json', 'Cache-Control':'private, no-store',
  'Access-Control-Allow-Origin':'*', 'Access-Control-Allow-Methods':'GET, POST, OPTIONS',
  'Access-Control-Allow-Headers':'Authorization, Content-Type',
}, body:JSON.stringify(value)});

export function createHandler({authorize, getHandleOrEmail, billing, generate, offer=null, offers=offer?[offer]:[], enabled=false, now=Date.now}) {
  const results = new Map(), pending = new Map();
  return async event => {
    if (event.httpMethod === 'OPTIONS') return reply(204, null);
    if (event.httpMethod === 'GET') return reply(200, {models:offers.map(item=>({...item,available:enabled}))});
    if (event.httpMethod !== 'POST') return reply(405, {error:'POST only'});
    if (!event.headers?.authorization) return reply(401, {error:'Sign in with AC for remote moves'});
    let user;
    try { user = await authorize(event.headers); } catch {}
    if (!user?.sub) return reply(401, {error:'Sign in again with AC'});
    if (user.email_verified !== true) return reply(403, {error:'Verify your AC email first'});
    if (!enabled || !offers.length) return reply(503, {error:'AC remote images are not enabled yet'});
    try {
      const handle = await getHandleOrEmail(user.sub);
      if (typeof handle !== 'string' || !handle.startsWith('@')) return reply(403, {error:'Choose an AC handle first'});
      if (typeof event.body !== 'string' || event.body.length > 710_000) return reply(413, {error:'Image too large'});
      let body;
      try { body = JSON.parse(event.body); } catch { return reply(400, {error:'Invalid move'}); }
      const input = moveInput(body), id = requestKey(user.sub, input.requestId);
      const offer = offers.find(item=>item.quote===input.quote && (!input.model || input.model===item.id));
      if (!offer) return reply(409, {error:'The remote price changed. Refresh before generating.'});
      for (const [key,value] of results) if (value.until <= now()) results.delete(key);
      const prior = results.get(id) || pending.get(id);
      if (prior && prior.hash !== input.hash) return reply(409, {error:'Move ID already used for different image or settings'});
      if (results.has(id)) return reply(200, prior.value);
      if (pending.has(id)) return await prior.promise;
      if (pending.size >= 2 || [...pending.values()].some(entry=>entry.user === user.sub)) return reply(429, {error:'A remote move is still processing. Local models remain available.'});
      const work = async () => {
        const receipt = await billing.begin({user:user.sub, handle:handle.slice(1), requestId:input.requestId,
          hash:input.hash, braincells:offer.braincells, model:offer.model});
        let result;
        try { result = await generate(input, offer); }
        catch (error) { await billing.finish(receipt.id, false); throw error; }
        // No/Paint selects artwork, not billing. Completed remote work is charged
        // even if its client stopped waiting. Uncertain or failed work is refunded.
        if (!await billing.finish(receipt.id, true)) throw Object.assign(Error('Move reservation expired'), {status:503});
        const value = {...result, handle, billing:{braincells:receipt.braincells, free:receipt.free, paid:receipt.paid}};
        if (results.size >= 32) results.delete(results.keys().next().value);
        results.set(id, {hash:input.hash, value, until:now()+60_000});
        return reply(200, value);
      };
      const promise = work().finally(()=>pending.delete(id));
      pending.set(id, {user:user.sub, hash:input.hash, promise});
      return await promise;
    } catch (error) {
      return reply(error.status || 503, {error:error.status ? error.message : 'Remote image generation unavailable'});
    }
  };
}

let live;
export async function handler(event) {
  const falOffer = moveOffer(Number(process.env.NOPAINT_FAL_USD_PER_MOVE));
  const offers = [...(process.env.FAL_KEY && falOffer ? [falOffer] : []),
    ...(process.env.OPENROUTER_API_KEY ? openRouterOffers(process.env.NOPAINT_OPENROUTER_OFFERS) : [])];
  const enabled = process.env.NOPAINT_REMOTE_ENABLED === 'true' && offers.length>0;
  // Public catalog and a disabled gateway need neither a DB nor provider call.
  if (event.httpMethod === 'GET') return reply(200, {models:offers.map(item=>({...item,available:enabled}))});
  if (event.httpMethod === 'OPTIONS') return reply(204, null);
  if (!enabled) return reply(503, {error:'AC remote images are not enabled yet'});
  if (!live) live = (async()=>{
    const [{authorize,getHandleOrEmail},{connect}] = await Promise.all([
      import('../../backend/authorization.mjs'), import('../../backend/database.mjs'),
    ]);
    const {db} = await connect();
    await ensureNoPaintBillingIndexes(db);
    return createHandler({authorize, getHandleOrEmail, billing:noPaintBilling(db), offers, enabled,
      generate:(input,offer)=> offer.provider==='openrouter'
        ? createOpenRouterProvider({key:process.env.OPENROUTER_API_KEY})(input,offer)
        : createNoPaintProvider({key:process.env.FAL_KEY})(input)});
  })().catch(error=>{live=null;throw error;});
  try { return await (await live)(event); } catch { return reply(503, {error:'AC remote images unavailable'}); }
}
