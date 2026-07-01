// manifestations, 26.06.26
// A rotating list of manifestations for a MacPal star — Fía's "Star" list,
// transcribed from her note and served live so it can change without a rebuild
// (see macpal/Sources/ManifestationsPlugin.swift). Sibling of macpal-status:
// that holds the single affirmation @jeffrey pushes; this holds the standing
// list the star cycles through on its own (one per hour).
//
//   GET  /api/manifestations?to=<key>  → { to, items, seq, at }   (public read)
//   POST /api/manifestations           → set the list (admin-only: @jeffrey)
//        body: { to, items: [..] }      → { to, items, seq, at, set: true }
//
// Stored as a JSON string in the KV hash "manifestations", keyed by recipient.
// Each POST bumps `seq` so the pal only reloads on a genuinely new list.

import { respond } from "../../backend/http.mjs";
import { authorize, hasAdmin } from "../../backend/authorization.mjs";
import * as KeyValue from "../../backend/kv.mjs";

const COLLECTION = "manifestations";
const MAX_ITEMS = 128;
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

// Normalize an incoming list: trim, drop blanks, cap length + count.
function cleanItems(raw) {
  if (!Array.isArray(raw)) return [];
  return raw
    .map((s) => (s ?? "").toString().trim().slice(0, MAX_LEN))
    .filter(Boolean)
    .slice(0, MAX_ITEMS);
}

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(204, null);

  if (event.httpMethod === "GET") {
    const to = cleanKey(event.queryStringParameters?.to);
    await KeyValue.connect();
    const raw = await KeyValue.get(COLLECTION, to);
    await KeyValue.disconnect();
    const stored = raw ? JSON.parse(raw) : { items: [], seq: 0, at: null };
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
    const items = cleanItems(body.items);

    await KeyValue.connect();
    const raw = await KeyValue.get(COLLECTION, to);
    const prevSeq = raw ? JSON.parse(raw).seq || 0 : 0;
    const next = { items, seq: prevSeq + 1, at: new Date().toISOString() };
    await KeyValue.set(COLLECTION, to, JSON.stringify(next));
    await KeyValue.disconnect();
    return respond(200, { to, ...next, set: true });
  }

  return respond(405, { message: "Method Not Allowed." });
}
