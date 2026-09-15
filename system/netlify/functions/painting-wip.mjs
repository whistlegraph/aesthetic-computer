import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { mongoPaintingWips, pruneEmptyPaintingWips } from "../../backend/painting-wips.mjs";

const headers = { "Cache-Control": "no-store", "Access-Control-Allow-Methods": "GET, POST, OPTIONS" };
let lastCleanup = 0;

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(200, {}, headers);
  if (!["GET", "POST"].includes(event.httpMethod)) return respond(405, { error: "Method not allowed" }, headers);
  try {
    let user = null;
    if (event.headers?.authorization || event.headers?.Authorization) {
      user = await authorize(event.headers);
      if (!user) return respond(401, { error: "Sign in again to edit your painting" }, headers);
    }
    const { db } = await connect();
    const { service } = await mongoPaintingWips(db);
    if (event.httpMethod === "GET") {
      const query = event.queryStringParameters || {};
      return respond(200, await service.read(query.code, query.state === "1", user), headers);
    }
    let input;
    try { input = JSON.parse(event.body || "{}"); }
    catch { return respond(400, { error: "Invalid JSON" }, headers); }
    let result;
    if (input.action === "create") {
      if (Date.now() - lastCleanup > 60000) {
        lastCleanup = Date.now();
        await pruneEmptyPaintingWips(db);
      }
      result = await service.create(input, user);
    } else if (input.action === "save") result = await service.save(input, user);
    else if (input.action === "read") result = await service.read(input.code, Boolean(input.state), user, input.key);
    else return respond(400, { error: "Unknown painting action" }, headers);
    return respond(200, result, headers);
  } catch (error) {
    if (!error.status) console.error("Painting WIP failed:", error.message);
    return respond(error.status || 500, { error: error.status ? error.message : "Could not save the painting" }, headers);
  }
}
