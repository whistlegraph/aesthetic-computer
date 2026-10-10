// Screens paired to a Whistlegraph phone over the knot: a TV's browser (the
// Xbox's Edge, a laptop, anything that runs aesthetic.computer) opens /wgtv,
// shows a four-letter code, and the phone pairs by typing it. From then on
// the phone pushes each painted checkpoint here and the screen polls for it,
// the same shape as the LAN lane to AC OS (WhistlegraphTV.swift) with the
// knot in the middle, so it works off the home Wi-Fi and on consoles that
// cannot run a LAN server.
import {randomUUID, randomInt, createHash} from 'node:crypto';

// No I, O, 0 or 1: the code is read off a TV across a room.
const ALPHABET = 'ABCDEFGHJKLMNPQRSTUVWXYZ';
export const validScreenCode = value => typeof value === 'string' && /^[A-HJ-NP-Z]{4}$/.test(value);
export const MAX_SOURCE = 512_000;                 // As the LAN lane allows.
export const SCREEN_IDLE_MS = 60 * 60_000;         // A screen that stops polling is forgotten after an hour.
const isSecret = value => typeof value === 'string' && /^[a-f0-9-]{36}$/i.test(value);
const hash = source => createHash('sha256').update(source).digest('hex');

export function mongoScreenStore(collection, {now = () => new Date()} = {}) {
  const ready = Promise.all([
    collection.createIndex({seenAt: 1}, {expireAfterSeconds: Math.floor(SCREEN_IDLE_MS / 1000)}),
    collection.createIndex({owner: 1}),
  ]).catch(() => {});
  const code = () => Array.from({length: 4}, () => ALPHABET[randomInt(ALPHABET.length)]).join('');
  return {
    // A fresh screen: its code, and the secret only the page that made it holds.
    async create() {
      await ready;
      for (let attempt = 0; attempt < 24; attempt++) {
        const row = {_id: code(), secret: randomUUID(), createdAt: now(), seenAt: now(), revision: 0, owner: null, name: '', source: '', sourceHash: '', status: null};
        try { await collection.insertOne(row); return {code: row._id, secret: row.secret}; }
        catch (error) { if (error?.code !== 11000) throw error; }
      }
      throw Error('No free screen code');
    },
    // The screen asks what to show. The source rides along only when it changed.
    async poll(id, secret, since = -1) {
      if (!validScreenCode(id) || !isSecret(secret)) return null;
      const row = await collection.findOneAndUpdate({_id: id, secret}, {$set: {seenAt: now()}}, {returnDocument: 'after'});
      if (!row) return null;
      const out = {code: row._id, paired: !!row.owner, name: row.name || '', revision: row.revision, status: row.status || null};
      if (row.revision > Number(since)) { out.source = row.source || ''; out.sourceHash = row.sourceHash || ''; }
      return out;
    },
    async pair(id, owner, name) {
      if (!validScreenCode(id) || !owner) return false;
      const result = await collection.updateOne({_id: id}, {$set: {owner, name: String(name || ''), pairedAt: now()}});
      return result.matchedCount === 1;
    },
    async read(id, owner) {
      if (!validScreenCode(id) || !owner) return null;
      const row = await collection.findOne({_id: id, owner}, {projection: {secret: 0, source: 0}});
      return row ? {code: row._id, paired: true, name: row.name || '', revision: row.revision, sourceHash: row.sourceHash || '', status: row.status || null, seenAt: row.seenAt} : null;
    },
    // Only the phone that paired can push; a new picture bumps the revision.
    async push(id, owner, source) {
      if (!validScreenCode(id) || !owner || typeof source !== 'string' || Buffer.byteLength(source, 'utf8') > MAX_SOURCE) return false;
      const result = await collection.updateOne({_id: id, owner}, {$set: {source, sourceHash: hash(source), updatedAt: now()}, $inc: {revision: 1}});
      return result.matchedCount === 1;
    },
    async status(id, owner, status) {
      if (!validScreenCode(id) || !owner || !status || typeof status !== 'object') return false;
      const clean = {code: String(status.code || '').slice(0, 24), phase: String(status.phase || '').slice(0, 120), busy: status.busy === true, updatedAt: now().toISOString()};
      const result = await collection.updateOne({_id: id, owner}, {$set: {status: clean}});
      return result.matchedCount === 1;
    },
    // Unpairing clears the picture; the screen shows its code again.
    async unpair(id, owner) {
      if (!validScreenCode(id) || !owner) return false;
      const result = await collection.updateOne({_id: id, owner}, {$set: {owner: null, name: '', source: '', sourceHash: '', status: null}, $inc: {revision: 1}});
      return result.matchedCount === 1;
    },
    async mine(owner) {
      if (!owner) return [];
      const rows = await collection.find({owner}, {projection: {secret: 0, source: 0}}).sort({pairedAt: -1}).limit(8).toArray();
      return rows.map(row => ({code: row._id, name: row.name || '', revision: row.revision, seenAt: row.seenAt}));
    },
  };
}
