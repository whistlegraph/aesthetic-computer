// macpal-status, 26.06.16
// The remote status line under a MacPal star — affirmations @jeffrey pushes to
// Fía's desktop pal (see macpal/Sources/AffirmationsPlugin.swift).
//
//   GET  /api/macpal-status?to=<key>   → { to, text, seq, at }   (public read)
//   POST /api/macpal-status            → set it (admin-only: @jeffrey)
//        body: { to, text }                     → one message
//        body: { to, playlist: [...], every }   → rotate through messages,
//                                                 one per `every` seconds
//
// Stored as a JSON string in the Redis hash "macpal", keyed by recipient.
// A playlist is rotated at read time: the GET computes the current entry from
// the elapsed time since it was set, and derives `seq` so the pal celebrates
// each turn of the wheel. Nothing on the server ticks; the poll does the work.

import { respond } from "../../backend/http.mjs";
import { authorize, hasAdmin } from "../../backend/authorization.mjs";
import * as KeyValue from "../../backend/kv.mjs";

const COLLECTION = "macpal";
const MAX_LEN = 240;
const MAX_PLAYLIST = 24;
const MIN_EVERY = 30; // seconds — the star polls every ~45s
const MAX_EVERY = 86400;

// Recipient keys are short, lowercase slugs — keep them tame so they're safe
// hash fields and predictable from the pal's `--to` flag.
function cleanKey(raw) {
  return (raw || "fia")
    .toString()
    .trim()
    .toLowerCase()
    .replace(/[^a-z0-9_-]/g, "")
    .slice(0, 64) || "fia";
}

// Resolve what a stored record says *right now*: a plain message passes
// through; a playlist yields the current entry and a seq advanced by how many
// rotations have elapsed since it was set.
function currentView(stored, now = Date.now()) {
  if (!stored) return { text: "", seq: 0, at: null };
  const { playlist, every, seq = 0, at = null } = stored;
  if (!Array.isArray(playlist) || playlist.length === 0) {
    return { text: stored.text ?? "", seq, at };
  }
  const elapsed = Math.max(0, now - Date.parse(at));
  const turns = Math.floor(elapsed / (every * 1000));
  return {
    text: playlist[turns % playlist.length],
    seq: seq + turns,
    at,
    playlist,
    every,
  };
}

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(204, null);

  if (event.httpMethod === "GET") {
    const to = cleanKey(event.queryStringParameters?.to);
    await KeyValue.connect();
    const raw = await KeyValue.get(COLLECTION, to);
    await KeyValue.disconnect();
    return respond(200, { to, ...currentView(raw ? JSON.parse(raw) : null) });
  }

  if (event.httpMethod === "POST") {
    const user = await authorize(event.headers);
    if (!user?.sub) return respond(401, { message: "Unauthorized." });
    if (!(await hasAdmin(user))) return respond(403, { message: "Admins only." });

    let body;
    try {
      body = JSON.parse(event.body || "{}");
    } catch {
      return respond(400, { message: "Bad JSON." });
    }
    const to = cleanKey(body.to);

    let entry;
    if (Array.isArray(body.playlist)) {
      const playlist = body.playlist
        .map((t) => (t ?? "").toString().slice(0, MAX_LEN))
        .filter((t) => t.length > 0)
        .slice(0, MAX_PLAYLIST);
      if (playlist.length < 2) {
        return respond(400, { message: "A playlist needs at least 2 messages." });
      }
      const every = Math.min(
        MAX_EVERY,
        Math.max(MIN_EVERY, Math.round(Number(body.every) || 120)),
      );
      entry = { playlist, every };
    } else {
      entry = { text: (body.text ?? "").toString().slice(0, MAX_LEN) };
    }

    await KeyValue.connect();
    const raw = await KeyValue.get(COLLECTION, to);
    // Base the new seq on the *effective* seq — a playlist that has been
    // turning for hours has advanced well past its stored base, and the new
    // message must still read as new.
    const prevSeq = currentView(raw ? JSON.parse(raw) : null).seq;
    const next = { ...entry, seq: prevSeq + 1, at: new Date().toISOString() };
    await KeyValue.set(COLLECTION, to, JSON.stringify(next));
    await KeyValue.disconnect();
    return respond(200, { to, ...currentView(next), set: true });
  }

  return respond(405, { message: "Method Not Allowed." });
}
