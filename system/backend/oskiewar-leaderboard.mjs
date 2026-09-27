// Verified-account, dual-report match ledger. Replay uploads never enter this ledger.
import { createHash } from 'node:crypto';

export const COLLECTION = 'oskiewar-verified-matches';
const MATCH_WINS = 5;
export function normalizeHandle(value) {
  if (typeof value !== 'string') return null;
  const handle = value.replace(/^@/, '').toLowerCase();
  return /^[a-z0-9_-]{1,64}$/.test(handle) ? handle : null;
}
export function validateResult(body) {
  if (!body || typeof body.matchId !== 'string' || !/^ow-[a-zA-Z0-9_-]{3,93}$/.test(body.matchId)) return 'Invalid matchId.';
  if (body.seat !== 0 && body.seat !== 1) return 'Invalid seat.';
  if (!Array.isArray(body.handles) || body.handles.length !== 2 || body.handles.some(h => !normalizeHandle(h))) return 'Two handles are required.';
  if (normalizeHandle(body.handles[0]) === normalizeHandle(body.handles[1])) return 'Two distinct accounts are required.';
  if (body.winner !== 0 && body.winner !== 1) return 'Invalid winner.';
  if (!Array.isArray(body.roundWins) || body.roundWins.length !== 2 || body.roundWins.some(n => !Number.isInteger(n) || n < 0 || n > MATCH_WINS)) return 'Invalid roundWins.';
  if (body.roundWins[body.winner] !== MATCH_WINS || body.roundWins[1 - body.winner] >= MATCH_WINS) return 'Only completed first-to-five matches count.';
  return null;
}
export function canonicalResult(body, accounts) {
  const result = { matchId: body.matchId, subjects: accounts.map(a => a._id), roundWins: body.roundWins, winner: body.winner };
  return { ...result, digest: createHash('sha256').update(JSON.stringify(result)).digest('hex') };
}

// All mutations are conditional on an immutable result digest. The built-in _id
// unique index makes retries/concurrent completion one match, without $inc races.
export async function recordReport(collection, body, accounts, now = new Date()) {
  const result = canonicalResult(body, accounts);
  const players = accounts.map((a, seat) => ({ subject: a._id, handle: a.handle, seat,
    won: seat === body.winner ? 1 : 0, roundsWon: body.roundWins[seat], roundsLost: body.roundWins[1 - seat] }));
  try {
    await collection.insertOne({ _id: body.matchId, digest: result.digest, players,
      winner: body.winner, roundWins: body.roundWins, createdAt: now, reports: {},
      expiresAt: new Date(now.getTime() + 7 * 86400000) });
  } catch (error) { if (error.code !== 11000) throw error; }
  const existing = await collection.findOne({ _id: body.matchId });
  if (!existing || existing.digest !== result.digest) return { statusCode: 409, body: { error: 'Reports disagree for this matchId.', status: 'conflict' } };
  if (!existing.confirmedAt) {
    await collection.updateOne({ _id: body.matchId, digest: result.digest, confirmedAt: { $exists: false } },
      { $set: { [`reports.${body.seat}`]: now } });
    await collection.updateOne({ _id: body.matchId, digest: result.digest, confirmedAt: { $exists: false },
      'reports.0': { $exists: true }, 'reports.1': { $exists: true } },
      { $set: { confirmedAt: now }, $unset: { expiresAt: '' } });
  }
  const saved = await collection.findOne({ _id: body.matchId });
  return { statusCode: saved.confirmedAt ? 200 : 202, body: { matchId: body.matchId,
    status: saved.confirmedAt ? 'recorded' : 'pending', recorded: !!saved.confirmedAt } };
}

export function standingsPipeline(match = {}) {
  return [
    { $match: { confirmedAt: { $exists: true }, ...match } },
    { $unwind: '$players' },
    { $group: { _id: '$players.subject', matchesPlayed: { $sum: 1 }, matchesWon: { $sum: '$players.won' },
      roundsWon: { $sum: '$players.roundsWon' }, roundsLost: { $sum: '$players.roundsLost' }, lastAt: { $max: '$confirmedAt' } } },
    { $lookup: { from: '@handles', localField: '_id', foreignField: '_id', as: 'account' } },
    { $unwind: '$account' },
    { $project: { _id: 1, handle: '$account.handle', colors: '$account.colors', matchesPlayed: 1, matchesWon: 1,
      matchesLost: { $subtract: ['$matchesPlayed', '$matchesWon'] }, roundsWon: 1, roundsLost: 1, lastAt: 1 } },
    { $sort: { matchesWon: -1, roundsWon: -1, matchesLost: 1, handle: 1 } },
  ];
}
const publicRow = ({ _id, colors, ...row }) => ({ ...row, colors:
  Array.isArray(colors) && colors.length === row.handle.length + 1 &&
  colors.every(c => c && [c.r,c.g,c.b].every(v => Number.isFinite(v) && v >= 0 && v <= 255))
    ? colors.map(({r,g,b}) => ({r,g,b})) : null });
const zeroRow = handle => ({ handle, matchesPlayed: 0, matchesWon: 0, matchesLost: 0, roundsWon: 0, roundsLost: 0, lastAt: null });
export async function readStandings(db, handles) {
  const collection = db.collection(COLLECTION);
  const accounts = handles.length ? await db.collection('@handles').find({ handle: { $in: handles } }).toArray() : [];
  const subjects = accounts.map(a => a._id);
  const result = await collection.aggregate([...standingsPipeline(), { $facet: {
    top: [{ $limit: 20 }], players: [{ $match: { _id: { $in: subjects } } }],
  } }]).toArray();
  const { top = [], players = [] } = result[0] || {};
  const empty = handle => ({ ...zeroRow(handle), colors: accounts.find(a => a.handle === handle)?.colors });
  let pair = null;
  if (subjects.length === 2) {
    const rows = await collection.aggregate(standingsPipeline({ 'players.subject': { $all: subjects } })).toArray();
    pair = { matchesPlayed: rows[0]?.matchesPlayed || 0,
      players: handles.map(handle => publicRow(rows.find(r => r.handle === handle) || empty(handle))) };
  }
  return { verification: 'dual-authenticated-reports', top: top.map(publicRow),
    players: handles.map(handle => publicRow(players.find(r => r.handle === handle) || empty(handle))), pair };
}

export function createLeaderboardHandler({ authorize, connect, respond, now = () => new Date() }) {
  let indexes;
  return async function handler(event) {
    const reply = (code, value) => respond(code, value, { 'Cache-Control': 'no-store' });
    if (event.httpMethod === 'OPTIONS') return reply(204, '');
    if (!['GET', 'POST'].includes(event.httpMethod)) return reply(405, { error: 'GET or POST required.' });
    let body, user, handles;
    if (event.httpMethod === 'POST') {
      user = await authorize({ authorization: event.headers?.authorization || event.headers?.Authorization });
      if (!user?.sub) return reply(401, { error: 'Sign in to report a match.' });
      if (typeof event.body !== 'string' || Buffer.byteLength(event.body) > 4096) return reply(413, { error: 'Result body must be at most 4096 bytes.' });
      try { body = JSON.parse(event.body); } catch { return reply(400, { error: 'Invalid JSON.' }); }
      const error = validateResult(body);
      if (error) return reply(400, { error });
      handles = body.handles.map(normalizeHandle);
    } else {
      const requested = event.queryStringParameters?.handles || '';
      handles = requested ? requested.split(',').map(normalizeHandle) : [];
      if (handles.length > 2 || handles.some(h => !h) || new Set(handles).size !== handles.length) return reply(400, { error: 'Request up to two distinct handles.' });
    }
    let database;
    try {
      database = await connect();
      const { db } = database;
      const collection = db.collection(COLLECTION);
      if (event.httpMethod === 'GET') return reply(200, await readStandings(db, handles));
      const accounts = await Promise.all(handles.map(handle => db.collection('@handles').findOne({ handle })));
      if (accounts.some(a => !a)) return reply(400, { error: 'Both players need registered handles.' });
      if (accounts[0]._id === accounts[1]._id) return reply(400, { error: 'Two distinct accounts are required.' });
      if (accounts[body.seat]._id !== user.sub) return reply(403, { error: 'The signed-in account does not own this seat.' });
      if (!indexes) indexes = Promise.all([
        collection.createIndex({ expiresAt: 1 }, { expireAfterSeconds: 0 }),
        collection.createIndex({ confirmedAt: -1 }),
      ]).catch(error => { indexes = undefined; throw error; });
      await indexes;
      const result = await recordReport(collection, body, accounts, now());
      return reply(result.statusCode, result.body);
    } catch (error) {
      console.error('Oskiewar leaderboard unavailable:', error?.name || 'Error');
      return reply(503, { error: 'Leaderboard temporarily unavailable. Retry the same matchId.' });
    } finally { await database?.disconnect(); }
  };
}
