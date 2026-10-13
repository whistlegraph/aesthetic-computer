// Public hand-edited appearance; only the authenticated handle owner can save it.
import { connect } from '../../backend/database.mjs';
import { authorize } from '../../backend/authorization.mjs';
import { fighterIdentity } from '../../backend/oskiewar-identity.mjs';
import { accountLocked } from '../../backend/account-lock.mjs';
import { respond } from '../../backend/http.mjs';
import { characterHandle, validateCharacter } from '../../public/aesthetic.computer/lib/oskiewar-character.mjs';

const COLLECTION = 'oskiewar-characters';
const readIdentities = new Map();
const READ_IDENTITY_MS = 30000;
const reply = (status, value) => respond(status, value, { 'Cache-Control': 'no-store' });
const publicRecord = (handle, record) => ({ handle,
  revision: record?.revision || 0,
  appearance: record?.appearance ? validateCharacter(record.appearance) : null,
});

async function resolveIdentity(sub, writing) {
  // Native seats refresh their public look every two seconds. Repeating the
  // OTP sibling discovery each time would exhaust Auth0 management quotas.
  // This cache is read-only: every edit resolves ownership afresh.
  const cached = readIdentities.get(sub);
  if (!writing && cached && cached.until > Date.now()) {
    if (await accountLocked(cached.identity.sub))
      throw Object.assign(Error('This AC account is locked.'), { status: 401 });
    return cached.identity;
  }
  const identity = await fighterIdentity(sub);
  if (identity.handle) {
    if (readIdentities.size >= 256) readIdentities.delete(readIdentities.keys().next().value);
    readIdentities.set(sub, { identity, until: Date.now() + READ_IDENTITY_MS });
  }
  return identity;
}

export async function handler(event) {
  if (event.httpMethod === 'OPTIONS') return reply(204, '');
  if (!['GET', 'POST'].includes(event.httpMethod)) return reply(405, { error: 'GET or POST only.' });
  let database;
  try {
    const requested = event.queryStringParameters?.handle;
    let identity;
    if (event.httpMethod === 'POST' || !requested) {
      const user = await authorize(event.headers || {});
      if (!user?.sub) return reply(401, { error: 'Log in to edit your character.' });
      identity = await resolveIdentity(user.sub, event.httpMethod === 'POST');
      if (!identity.handle) return reply(403, { error: 'This account needs an AC handle.' });
    }
    database = await connect();
    const records = database.db.collection(COLLECTION);
    if (event.httpMethod === 'GET') {
      if (requested) {
        const handle = characterHandle(requested);
        if (!handle) return reply(400, { error: 'Invalid handle.' });
        const owner = await database.db.collection('@handles').findOne({ handle: {
          $regex: new RegExp(`^${handle.replace(/\./g, '\\.')}$`, 'i'),
        } });
        if (!owner) return reply(404, { error: 'Handle not found.' });
        identity = { sub: owner._id, handle: owner.handle };
      }
      return reply(200, publicRecord(identity.handle, await records.findOne({ _id: identity.sub })));
    }

    if (typeof event.body !== 'string' || event.body.length > 8192)
      return reply(400, { error: 'Invalid character request.' });
    let body, appearance;
    try {
      body = JSON.parse(event.body);
      appearance = validateCharacter(body.appearance);
    } catch (error) { return reply(400, { error: error.message || 'Invalid character.' }); }
    if (characterHandle(body.handle) !== characterHandle(identity.handle))
      return reply(403, { error: 'You can only edit your own character.' });
    if (!Number.isSafeInteger(body.baseRevision) || body.baseRevision < 0)
      return reply(400, { error: 'Reload your character before saving.' });
    const now = new Date();
    let record;
    try {
      if (body.baseRevision === 0) {
        record = { _id: identity.sub, revision: 1, appearance, updatedAt: now };
        await records.insertOne(record);
      } else {
        record = await records.findOneAndUpdate({ _id: identity.sub, revision: body.baseRevision },
          { $set: { appearance, updatedAt: now }, $inc: { revision: 1 } },
          { returnDocument: 'after' });
      }
    } catch (error) {
      if (error.code !== 11000) throw error;
      record = null;
    }
    if (!record)
      return reply(409, { error: 'Your character changed in another editor. Reload to continue.' });
    return reply(200, publicRecord(identity.handle, record));
  } catch (error) {
    return reply(error.status || 503, { error: error.status ? error.message : 'Character service unavailable. Try again.' });
  } finally { if (database?.disconnect) await database.disconnect(); }
}
