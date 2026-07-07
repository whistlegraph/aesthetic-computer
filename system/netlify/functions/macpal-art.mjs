// macpal-art, 26.07.06
// The art wire for a MacPal star — a twin of macpal-status, but carrying the
// star's glyph SVGs instead of a caption. @jeffrey pushes new poses and the
// star hot-swaps them on its next poll (see macpal/Sources/ArtPlugin.swift).
//
//   GET  /api/macpal-art?to=<key>            → { to, rev, at, names, poses }
//   GET  /api/macpal-art?to=<key>&names=1    → { to, rev, at, names }  (no bodies)
//   GET  /api/macpal-art?to=<key>&from=device→ what the star reports it renders
//   POST /api/macpal-art                     → set poses (admin-only: @jeffrey)
//        body: { to, poses: { <name>: "<svg>" | null, ... } }  merge/add/delete
//        body: { to, name, svg }                               one pose (sugar)
//        body: { to, clear: true }                             drop all → bundle art
//        body: { to, report: { poses } }                       star's live state
//
// Poses are a named map. Every pose except "sing" is an idle-cycle frame, drawn
// in name order (glyph, glyph-2, glyph-3, glyph-4, …); "sing" is the open-mouth
// pose. Adding a new name grows the wiggle; sending null for a name removes it.
// Stored as a JSON string in the Redis hash "macpalart", keyed by recipient
// (the star's reported state lives under "<recipient>:device"). Nothing ticks
// on the server — the star's ~45s poll does the work, exactly like the caption.

import { respond } from "../../backend/http.mjs";
import { authorize, hasAdmin } from "../../backend/authorization.mjs";
import * as KeyValue from "../../backend/kv.mjs";
import { MAX_POSES, cleanKey, cleanReport, mergePoses, view } from "./macpal-art-lib.mjs";

const COLLECTION = "macpalart";

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(204, null);

  if (event.httpMethod === "GET") {
    const q = event.queryStringParameters || {};
    const to = cleanKey(q.to);
    if (q.from === "device") {
      // The star's compact self-report (status, no bodies) — see POST below.
      await KeyValue.connect();
      const raw = await KeyValue.get(COLLECTION, `${to}:device`);
      await KeyValue.disconnect();
      const rec = raw ? JSON.parse(raw) : { rev: 0, at: null, poses: [] };
      return respond(200, { to, from: "device", rev: rec.rev ?? 0, at: rec.at ?? null, names: (rec.poses || []).map((p) => p.name), poses: rec.poses || [] });
    }
    const withBodies = q.names !== "1" && q.names !== "true";
    await KeyValue.connect();
    const raw = await KeyValue.get(COLLECTION, to);
    await KeyValue.disconnect();
    return respond(200, { to, from: "server", ...view(raw ? JSON.parse(raw) : null, { withBodies }) });
  }

  if (event.httpMethod === "POST") {
    let body;
    try {
      body = JSON.parse(event.body || "{}");
    } catch {
      return respond(400, { message: "Bad JSON." });
    }
    const to = cleanKey(body.to);

    // The star reporting its live state → the device slot, read back with
    // ?from=device. Unauthenticated (the star holds no admin token) but a
    // compact, tightly-capped status only — never touches the desired art, so a
    // spoofed report can at worst misreport telemetry, not change what shows.
    if (body.report && typeof body.report === "object") {
      const rec = { ...cleanReport(body.report), at: new Date().toISOString() };
      await KeyValue.connect();
      await KeyValue.set(COLLECTION, `${to}:device`, JSON.stringify(rec));
      await KeyValue.disconnect();
      return respond(200, { to, from: "device", reported: true, rev: rec.rev, names: rec.poses.map((p) => p.name) });
    }

    // Everything past here mutates the desired art → admins only (@jeffrey).
    const user = await authorize(event.headers);
    if (!user?.sub) return respond(401, { message: "Unauthorized." });
    if (!(await hasAdmin(user))) return respond(403, { message: "Admins only." });

    // Assemble the patch: `poses` map, or the single-pose `{name, svg}` sugar.
    let patch;
    if (body.clear === true) {
      patch = null; // wipe below
    } else if (body.poses && typeof body.poses === "object") {
      patch = body.poses;
    } else if (body.name != null) {
      patch = { [body.name]: body.svg ?? null };
    } else {
      return respond(400, { message: "Send { poses }, { name, svg }, or { clear: true }." });
    }

    await KeyValue.connect();
    const raw = await KeyValue.get(COLLECTION, to);
    const prev = raw ? JSON.parse(raw) : null;

    let next;
    if (patch === null) {
      next = { poses: {}, rev: (prev?.rev ?? 0) + 1, at: new Date().toISOString() };
    } else {
      const { poses, changed, rejected } = mergePoses(prev?.poses || {}, patch);
      if (Object.keys(poses).length > MAX_POSES) {
        await KeyValue.disconnect();
        return respond(400, { message: `Too many poses (max ${MAX_POSES}).` });
      }
      if (!changed) {
        await KeyValue.disconnect();
        return respond(200, { to, unchanged: true, rejected, ...view(prev, { withBodies: false }) });
      }
      next = { poses, rev: (prev?.rev ?? 0) + 1, at: new Date().toISOString() };
    }

    await KeyValue.set(COLLECTION, to, JSON.stringify(next));
    await KeyValue.disconnect();
    return respond(200, { to, set: true, ...view(next, { withBodies: false }) });
  }

  return respond(405, { message: "Method Not Allowed." });
}
