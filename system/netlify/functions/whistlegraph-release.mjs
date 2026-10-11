// Whistlegraph Release, 2026.10.11
// The newest Whistlegraph build testers can install, so an older copy can
// offer the update when it opens.
//
// GET  /api/whistlegraph-release   {channel, build, version, at} or 404
// POST /api/whistlegraph-release   {build, version}   Whistlegraph admins only
//      Sent by `fastlane ios distribute` once a build goes out to testers.
//      The build only ever moves forward; a lower or equal build is ignored.
import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { isWhistlegraphAdmin } from "./whistlegraph-admin-lib.mjs";

const COLLECTION = "whistlegraph-releases";
const CHANNEL = "testflight";
const BUILD = /^[1-9]\d{0,5}$/;
const VERSION = /^\d+(?:\.\d+){0,3}$/;

export function normalizeRelease(body) {
  const build = String(body?.build ?? "");
  const version = String(body?.version ?? "");
  if (!BUILD.test(build) || !VERSION.test(version)) return null;
  return { build: Number(build), version };
}

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(204, "", { "Cache-Control": "no-store" });
  try {
    if (event.httpMethod === "GET") {
      const { db } = await connect();
      const row = await db.collection(COLLECTION).findOne({ _id: CHANNEL });
      if (!row) return respond(404, { error: "No release yet" }, { "Cache-Control": "no-store" });
      return respond(200, { channel: CHANNEL, build: row.build, version: row.version, at: row.at },
        { "Cache-Control": "public, max-age=60" });
    }
    if (event.httpMethod !== "POST") return respond(405, { error: "Method not allowed" }, { "Cache-Control": "no-store" });
    const user = await authorize(event.headers);
    if (!isWhistlegraphAdmin(user)) return respond(403, { error: "Whistlegraph admins only" }, { "Cache-Control": "no-store" });
    const release = normalizeRelease(typeof event.body === "string" ? JSON.parse(event.body || "{}") : event.body);
    if (!release) return respond(400, { error: "Specify a numeric build and version" }, { "Cache-Control": "no-store" });
    const { db } = await connect();
    const releases = db.collection(COLLECTION);
    const at = new Date();
    const moved = await releases.updateOne(
      { _id: CHANNEL, build: { $lt: release.build } },
      { $set: { ...release, at } },
    );
    if (!moved.matchedCount) {
      try { await releases.insertOne({ _id: CHANNEL, ...release, at }); }
      catch (error) { if (error?.code !== 11000) throw error; } // a newer or equal build is already out
    }
    const row = await releases.findOne({ _id: CHANNEL });
    return respond(200, { channel: CHANNEL, build: row.build, version: row.version, at: row.at }, { "Cache-Control": "no-store" });
  } catch (error) {
    if (error instanceof SyntaxError) return respond(400, { error: "Invalid JSON" }, { "Cache-Control": "no-store" });
    console.error("whistlegraph-release:", error);
    return respond(503, { error: "Release record unavailable" }, { "Cache-Control": "no-store" });
  }
}
