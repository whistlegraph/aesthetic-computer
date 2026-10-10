// A screen paired to a Whistlegraph phone over the knot (backend/whistlegraph-screens.mjs).
// Screen (no sign-in; it holds the secret it was given):
//   POST  {}                                   → {code, secret}
//   GET   ?code=ABCD&secret=…&since=<revision> → {paired, name, revision, status, source?}
// Phone (Bearer):
//   POST  ?code=ABCD {action:'pair'}           → pairs the screen to this handle
//   GET   ?code=ABCD                           → the screen's state, without the source
//   GET   (no code)                            → the handle's paired screens
//   PUT   ?code=ABCD {source} | {status}       → the next picture, or what the phone is doing
//   DELETE ?code=ABCD                          → unpair; the screen shows its code again
import {connect} from '../../backend/database.mjs';
import {authenticateMusical} from './easel-musical-jev.mjs';
import {mongoScreenStore, validScreenCode, MAX_SOURCE} from '../../backend/whistlegraph-screens.mjs';
let pending;
export const screenStore = () => pending ??= (async () => { const {db} = await connect(); return {store: mongoScreenStore(db.collection('walkieware-screens')), db}; })().catch(error => { pending = null; throw error; });
export async function handler(event) {
  const headers = {'Content-Type': 'application/json', 'Cache-Control': 'no-store', 'Access-Control-Allow-Origin': '*', 'Access-Control-Allow-Headers': 'Authorization, Content-Type', 'Access-Control-Allow-Methods': 'GET, POST, PUT, DELETE, OPTIONS'};
  const reply = (statusCode, value) => ({statusCode, headers, body: JSON.stringify(value)});
  if (event.httpMethod === 'OPTIONS') return reply(204, {});
  if (!['GET', 'POST', 'PUT', 'DELETE'].includes(event.httpMethod)) return reply(405, {error: 'Use GET, POST, PUT or DELETE'});
  const q = event.queryStringParameters || {};
  const code = typeof q.code === 'string' ? q.code.toUpperCase() : '';
  let store, db;
  try { ({store, db} = await screenStore()); } catch { return reply(503, {error: 'Screen storage unavailable'}); }
  let body = {};
  if (event.body) { try { body = JSON.parse(event.body); } catch { return reply(400, {error: 'Send JSON'}); } }
  // The screen's own calls carry no sign-in.
  if (event.httpMethod === 'POST' && !code) {
    try { return reply(201, await store.create()); } catch (error) { return reply(503, {error: error.message}); }
  }
  if (event.httpMethod === 'GET' && code && typeof q.secret === 'string') {
    if (!validScreenCode(code)) return reply(400, {error: 'Bad screen code'});
    const state = await store.poll(code, q.secret, Number.isFinite(Number(q.since)) ? Number(q.since) : -1);
    return state ? reply(200, state) : reply(404, {error: 'No such screen'});
  }
  let owner;
  try { owner = await authenticateMusical(event.headers || {}); } catch { return reply(503, {error: 'Authentication unavailable'}); }
  if (!owner) return reply(401, {error: 'Sign in to your AC account'});
  if (event.httpMethod === 'GET') {
    if (!code) return reply(200, {screens: await store.mine(owner)});
    const state = await store.read(code, owner);
    return state ? reply(200, state) : reply(404, {error: 'That screen is not paired to you'});
  }
  if (!validScreenCode(code)) return reply(400, {error: 'Type the four letters on the screen'});
  if (event.httpMethod === 'POST') {
    if (body.action !== 'pair') return reply(400, {error: 'Send {action:"pair"}'});
    let name = '';
    try { name = (await db.collection('@handles').findOne({_id: owner}, {projection: {handle: 1}}))?.handle || ''; } catch {}
    return (await store.pair(code, owner, name)) ? reply(200, await store.read(code, owner)) : reply(404, {error: 'No screen shows that code. Open aesthetic.computer/wgtv on the TV and try again.'});
  }
  if (event.httpMethod === 'PUT') {
    if (typeof body.source === 'string') {
      if (Buffer.byteLength(body.source, 'utf8') > MAX_SOURCE) return reply(413, {error: 'Piece too large for the screen'});
      return (await store.push(code, owner, body.source)) ? reply(200, await store.read(code, owner)) : reply(404, {error: 'That screen is not paired to you'});
    }
    if (body.status && typeof body.status === 'object') return (await store.status(code, owner, body.status)) ? reply(200, {ok: true}) : reply(404, {error: 'That screen is not paired to you'});
    return reply(400, {error: 'Send {source} or {status}'});
  }
  return (await store.unpair(code, owner)) ? reply(200, {ok: true}) : reply(404, {error: 'That screen is not paired to you'});
}
