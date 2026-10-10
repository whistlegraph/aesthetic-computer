// Submit a Whistlegraph turn to run off the phone, or read how it is going.
// POST {code, text, displayText?, drawing?, baseVersion, baseHash, model?, requestID?} → the queued turn
// GET ?id=<turn>        → one turn       GET ?code=<thread> → that thread's open turns
// GET ?recent=1         → the owner's last turns
// DELETE ?id=<turn>     → take back a turn that has not started
// Workers claim and finish turns through the queue module directly, never this route.
import {connect} from '../../backend/database.mjs';
import {authenticateMusical} from './easel-musical-jev.mjs';
import {whistlegraphStore} from './whistlegraph.mjs';
import {mongoTurnQueue, validateTurnRequest, publicTurn} from '../../backend/whistlegraph-turns.mjs';
import {threadUpdated} from '../../backend/whistlegraph-live.mjs';
let pending;
export const turnQueue = () => pending ??= (async () => { const {db} = await connect(); return mongoTurnQueue(db.collection('walkieware-turns')); })().catch(error => { pending = null; throw error; });
export async function handler(event) {
  const headers = {'Content-Type': 'application/json', 'Cache-Control': 'no-store', 'Access-Control-Allow-Origin': '*', 'Access-Control-Allow-Headers': 'Authorization, Content-Type', 'Access-Control-Allow-Methods': 'GET, POST, DELETE, OPTIONS'};
  const reply = (statusCode, value) => ({statusCode, headers, body: JSON.stringify(value)});
  if (event.httpMethod === 'OPTIONS') return reply(204, {});
  if (!['GET', 'POST', 'DELETE'].includes(event.httpMethod)) return reply(405, {error: 'Use GET, POST or DELETE'});
  let owner;
  try { owner = await authenticateMusical(event.headers || {}); } catch { return reply(503, {error: 'Authentication unavailable'}); }
  if (!owner) return reply(401, {error: 'Sign in to your AC account'});
  let queue, store;
  try { [queue, store] = await Promise.all([turnQueue(), whistlegraphStore()]); } catch { return reply(503, {error: 'Turn queue unavailable'}); }
  try {
    if (event.httpMethod === 'DELETE') {
      const id = event.queryStringParameters?.id;
      if (!id) return reply(400, {error: 'Specify the turn id'});
      return (await queue.cancel(owner, id)) ? reply(200, {cancelled: true}) : reply(409, {error: 'That turn is not waiting'});
    }
    if (event.httpMethod === 'GET') {
      const q = event.queryStringParameters || {};
      if (q.id) { const row = await queue.read(owner, q.id); return row ? reply(200, publicTurn(row)) : reply(404, {error: 'No such turn'}); }
      if (q.code) {
        const thread = await store.read(owner, q.code);
        if (!thread) return reply(404, {error: 'Thread unavailable'});
        return reply(200, {turns: (await queue.listOpen(owner, thread._id)).map(publicTurn)});
      }
      return reply(200, {turns: (await queue.recent(owner, Number(q.limit) || 20)).map(publicTurn)});
    }
    let body; try { body = JSON.parse(event.body || '{}'); } catch { return reply(400, {error: 'Send JSON'}); }
    let request; try { request = validateTurnRequest(body); } catch (error) { return reply(400, {error: error.message}); }
    const thread = await store.read(owner, request.code);
    if (!thread) return reply(404, {error: 'Thread unavailable'});
    const head = thread.ledger?.versions.find(v => v.id === thread.ledger.head);
    if (!head || head.id !== request.baseVersion) return reply(409, {error: `The piece is at v${head?.id ?? 0}; the request builds on v${request.baseVersion}. Reopen it and try again.`});
    try {
      const row = await queue.enqueue(owner, thread, request);
      // Whoever is watching this thread (the phone, a stand-in) learns a turn began.
      threadUpdated(thread._id, {type: 'turn', turn: publicTurn(row)});
      return reply(202, publicTurn(row));
    }
    catch (error) { return reply(error.statusCode || 500, {error: error.message}); }
  } catch { return reply(503, {error: 'Turn queue unavailable'}); }
}
