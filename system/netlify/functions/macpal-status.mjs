// macpal-status, 26.06.16
// The remote status line under a MacPal star — affirmations @jeffrey pushes to
// Fía's desktop pal (see macpal/Sources/AffirmationsPlugin.swift).
//
//   GET  /api/macpal-status?to=<key>   → { to, text, seq, at }   (public read)
//   POST /api/macpal-status            → set it (admin-only: @jeffrey)
//        body: { to, text }            → { to, text, seq, at, set: true }
//
// Stored as a JSON string in the Redis hash "macpal", keyed by recipient. Each
// POST bumps `seq` so the pal only celebrates a genuinely new message.

import { respond } from "../../backend/http.mjs";
import { authorize, hasAdmin } from "../../backend/authorization.mjs";
import * as KeyValue from "../../backend/kv.mjs";

const COLLECTION = "macpal";
const MAX_LEN = 240;

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

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(204, null);

  if (event.httpMethod === "GET") {
    const to = cleanKey(event.queryStringParameters?.to);
    await KeyValue.connect();
    const raw = await KeyValue.get(COLLECTION, to);
    await KeyValue.disconnect();
    const stored = raw ? JSON.parse(raw) : { text: "", seq: 0, at: null };
    return respond(200, { to, ...stored });
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
    const text = (body.text ?? "").toString().slice(0, MAX_LEN);

    await KeyValue.connect();
    const raw = await KeyValue.get(COLLECTION, to);
    const prevSeq = raw ? JSON.parse(raw).seq || 0 : 0;
    const next = { text, seq: prevSeq + 1, at: new Date().toISOString() };
    await KeyValue.set(COLLECTION, to, JSON.stringify(next));
    await KeyValue.disconnect();
    return respond(200, { to, ...next, set: true });
  }

  return respond(405, { message: "Method Not Allowed." });
}
