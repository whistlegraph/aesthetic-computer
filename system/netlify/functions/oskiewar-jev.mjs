import { connect } from '../../backend/database.mjs';
import { createHandler, mongoStore } from '../../backend/oskiewar-jev.mjs';

let ready;
async function storage() {
  if (!ready) ready = (async () => {
    const { db } = await connect();
    const collection = db.collection('oskiewar-jev');
    await collection.createIndex({ expiresAt: 1 }, { expireAfterSeconds: 0 });
    return mongoStore(collection);
  })().catch(error => { ready = null; throw error; });
  return ready;
}
const run = createHandler({ store: {
  async start(now) { return (await storage()).start(now); },
  async consume(id, seat, now) { return (await storage()).consume(id, seat, now); },
} });
export const handler = event => {
  if (!process.env.OPENROUTER_API_KEY) return { statusCode: 503, body: JSON.stringify({ error: 'Jev is not configured.' }) };
  return run(event);
};
