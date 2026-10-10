// The queue of Whistlegraph turns that run off the phone (apple/whistlegraph/TURNS.md).
// One row per submitted request. Workers claim with an atomic update and hold a
// lease by heartbeat; a lease that goes quiet is claimable again. Rows are the
// durable record of what was asked, what happened, and what it cost.
import {randomUUID} from 'node:crypto';
import {validCode} from './whistlegraph.mjs';

export const LEASE_MS = 90_000;           // A worker that misses three 30 s heartbeats has lost the job.
export const MAX_QUEUED_PER_OWNER = 3;    // Backpressure: a person waits on their own queue, never on everyone's.
export const MAX_TEXT = 1200;             // The phone's 96-grapheme line, or the full mixed-input prompt.
const STATUSES = ['queued','running','done','failed'];
const isHex = value => typeof value === 'string' && /^[a-f0-9]{64}$/.test(value);

export function validateTurnRequest(value) {
  if (!value || typeof value !== 'object') throw Error('Send a turn');
  const request = {};
  if (!validCode(value.code)) throw Error('Specify the thread code');
  request.code = value.code;
  if (typeof value.text !== 'string' || value.text.length > MAX_TEXT) throw Error(`Text must be a string of at most ${MAX_TEXT} characters`);
  request.text = value.text;
  if (value.displayText !== undefined) {
    if (typeof value.displayText !== 'string' || value.displayText.length > 200) throw Error('displayText must be short');
    request.displayText = value.displayText;
  }
  if (value.drawing !== undefined && value.drawing !== null) {
    const d = value.drawing;
    if (d?.schema !== 'whistlegraph-drawing/v1' || !Array.isArray(d.strokes) || !d.strokes.length || d.strokes.length > 32 || JSON.stringify(d).length > 200_000) throw Error('Invalid drawing');
    request.drawing = d;
  }
  if (!request.text.trim() && !request.drawing) throw Error('Type a few words or add chalk');
  if (!Number.isSafeInteger(value.baseVersion) || value.baseVersion < 0) throw Error('baseVersion must be the version the request builds on');
  request.baseVersion = value.baseVersion;
  if (!isHex(value.baseHash)) throw Error('baseHash must be the sha256 of that version\'s source');
  request.baseHash = value.baseHash;
  if (value.model !== undefined) {
    if (typeof value.model !== 'string' || !/^[a-z0-9./-]{3,80}$/i.test(value.model)) throw Error('Invalid model');
    request.model = value.model;
  }
  if (value.requestID !== undefined) {
    if (typeof value.requestID !== 'string' || !/^[a-f0-9-]{36}$/i.test(value.requestID)) throw Error('requestID must be a UUID');
    request.requestID = value.requestID;
  }
  return request;
}

// The row as the phone and the admin see it: never the owner's subject.
export function publicTurn(row) {
  if (!row) return null;
  const {owner, ...rest} = row;
  return {...rest, id: row._id};
}

export function mongoTurnQueue(collection, {now = () => new Date()} = {}) {
  let indexes;
  const ready = () => indexes ??= Promise.all([
    collection.createIndex({status: 1, createdAt: 1}),
    collection.createIndex({owner: 1, status: 1}),
    collection.createIndex({threadID: 1, status: 1}),
    collection.createIndex({requestID: 1}, {unique: true}),
  ]);
  return {
    // One running turn per thread, at most MAX_QUEUED_PER_OWNER waiting per person,
    // one row per requestID: a resubmit of the same request returns the same row.
    async enqueue(owner, thread, request) {
      await ready();
      const requestID = request.requestID || randomUUID();
      const existing = await collection.findOne({requestID});
      if (existing) { if (existing.owner !== owner) throw Object.assign(Error('Request unavailable'), {statusCode: 404}); return existing; }
      const waiting = await collection.countDocuments({owner, status: 'queued'});
      if (waiting >= MAX_QUEUED_PER_OWNER) throw Object.assign(Error(`You already have ${waiting} turns waiting. Let one finish first.`), {statusCode: 429});
      const at = now().toISOString();
      const row = {_id: randomUUID(), owner, threadID: thread._id, code: thread.code, requestID,
        request: {text: request.text, displayText: request.displayText ?? '', drawing: request.drawing ?? null},
        baseVersion: request.baseVersion, baseHash: request.baseHash, model: request.model ?? null,
        status: 'queued', createdAt: at, claimedBy: null, claimedAt: null, heartbeatAt: null, finishedAt: null,
        attempts: 0, checkpoint: null, result: null};
      await collection.insertOne(row);
      return row;
    },
    // Oldest queued job whose thread has nothing running; or a running job whose lease lapsed.
    async claim(worker) {
      await ready();
      const at = now();
      const stale = new Date(+at - LEASE_MS).toISOString();
      const running = await collection.distinct('threadID', {status: 'running', heartbeatAt: {$gte: stale}});
      const set = {status: 'running', claimedBy: worker, claimedAt: at.toISOString(), heartbeatAt: at.toISOString()};
      const lapsed = await collection.findOneAndUpdate({status: 'running', heartbeatAt: {$lt: stale}}, {$set: set, $inc: {attempts: 1}}, {sort: {heartbeatAt: 1}, returnDocument: 'after'});
      if (lapsed) return lapsed;
      return collection.findOneAndUpdate({status: 'queued', threadID: {$nin: running}}, {$set: set, $inc: {attempts: 1}}, {sort: {createdAt: 1}, returnDocument: 'after'});
    },
    async heartbeat(id, worker, checkpoint) {
      const update = {$set: {heartbeatAt: now().toISOString()}};
      if (typeof checkpoint === 'string') update.$set.checkpoint = checkpoint;
      const result = await collection.updateOne({_id: id, status: 'running', claimedBy: worker}, update);
      return result.modifiedCount === 1;
    },
    // Only the worker holding the lease can finish a job; a late worker's result is dropped.
    async complete(id, worker, result) {
      const r = await collection.updateOne({_id: id, status: 'running', claimedBy: worker}, {$set: {status: 'done', finishedAt: now().toISOString(), result}});
      return r.modifiedCount === 1;
    },
    async fail(id, worker, error, {draft = null} = {}) {
      const r = await collection.updateOne({_id: id, status: 'running', claimedBy: worker}, {$set: {status: 'failed', finishedAt: now().toISOString(), result: {error: String(error).slice(0, 2000), draft}}});
      return r.modifiedCount === 1;
    },
    async read(owner, id) { return collection.findOne({_id: id, owner}); },
    async listOpen(owner, threadID) { return collection.find({owner, threadID, status: {$in: ['queued','running']}}).sort({createdAt: 1}).toArray(); },
    async recent(owner, limit = 20) { return collection.find({owner}).sort({createdAt: -1}).limit(Math.min(100, Math.max(1, limit))).toArray(); },
    // Queue depth is the autoscale signal.
    async depth() {
      const [queued, running] = await Promise.all([collection.countDocuments({status: 'queued'}), collection.countDocuments({status: 'running'})]);
      return {queued, running};
    },
  };
}
export {STATUSES as TURN_STATUSES};
