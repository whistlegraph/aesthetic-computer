// Private immutable drafts and public immutable map versions, owned by AC accounts.
import { createHash } from 'node:crypto';
import { validateMap } from '../../../xbox/live/oskiewar-map.mjs';

const headers = { 'Content-Type': 'application/json', 'Cache-Control': 'no-store' };
const reply = (statusCode, value) => ({ statusCode, headers, body: JSON.stringify(value) });
const idPattern = /^[a-f0-9]{64}$/;

export function createMapsHandler({ authorize, connect }) {
  return async event => {
    if (!['GET', 'POST'].includes(event.httpMethod)) return reply(405, { message: 'GET or POST only' });
    let database;
    try {
      const query = event.queryStringParameters || {};
      const user = await authorize(event.headers || {});
      const owner = user?.sub;
      let map;
      let body;
      if (event.httpMethod === 'POST') {
        if (!owner) return reply(401, { message: 'Sign in to save maps' });
        if (typeof event.body !== 'string' || Buffer.byteLength(event.body) > 16384)
          return reply(413, { message: 'Map exceeds 16 KiB' });
        try { body = JSON.parse(event.body); map = validateMap(body.map); }
        catch (error) { return reply(400, { message: error.message }); }
      } else {
        if (query.id !== undefined && !idPattern.test(query.id)) return reply(400, { message: 'Invalid map id' });
        if (query.mine === '1' && !owner) return reply(401, { message: 'Sign in to list drafts' });
      }
      database = await connect();
      const collection = database.db.collection('oskiewar-maps');
      if (event.httpMethod === 'POST') {
        const published = body.publish === true;
        const id = createHash('sha256').update(JSON.stringify([owner, published, map])).digest('hex');
        await collection.updateOne({ _id: id }, { $setOnInsert: {
          owner, published, map, createdAt: new Date().toISOString(),
        } }, { upsert: true });
        return reply(201, { id, published, map,
          ...(published ? { url: 'https://oskiewar.com/workshop?map=' + id } : {}) });
      }
      if (query.id) {
        const row = await collection.findOne({ _id: query.id,
          $or: [{ published: true }, ...(owner ? [{ owner }] : [])] });
        if (!row) return reply(404, { message: 'Map not found' });
        return reply(200, { id: row._id, published: row.published, map: row.map });
      }
      const rows = await collection.find(query.mine === '1' ? { owner } : { published: true },
        { projection: { _id: 1, 'map.name': 1, published: 1, createdAt: 1 } })
        .sort({ createdAt: -1 }).limit(50).toArray();
      return reply(200, { maps: rows.map(row => ({ id: row._id, name: row.map.name,
        published: row.published, createdAt: row.createdAt })) });
    } catch {
      return reply(503, { message: 'Map storage unavailable' });
    } finally { if (database) await database.disconnect(); }
  };
}
export async function handler(event) {
  const [{ authorize }, { connect }] = await Promise.all([
    import('../../backend/authorization.mjs'), import('../../backend/database.mjs'),
  ]);
  return createMapsHandler({ authorize, connect })(event);
}
