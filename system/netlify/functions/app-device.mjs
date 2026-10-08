// App Device, 2026.10.08
// The AC network device registry endpoint (shared/app-devices.mjs).
//
// POST   /api/app-device  {app, deviceId, platform, event, version?, build?,
//                          model?, os?, label?, push?, topics?}
//        Authorization: Bearer <token> is optional. With it the device is bound
//        to that account (the app's Auth0 tenant); "logout" unbinds it.
// GET    /api/app-device?app=whistlegraph   the signed-in person's devices
// DELETE /api/app-device?app=…&deviceId=…   remove one of your own devices
import { createHmac, randomBytes } from "node:crypto";
import { authorize, getHandleOrEmail } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { appConfig, userKey } from "../../../shared/app-registry.mjs";
import { APP_DEVICES, OWN_DEVICE_FIELDS, deviceKey, normalizeReport, recordReport } from "../../../shared/app-devices.mjs";

const HEADERS = { "Cache-Control": "no-store" };

// Ephemeral abuse guard, as in app-open: nothing here reaches the database.
const rateSalt = randomBytes(32), rates = new Map();
function permitted(event) {
  const now = Date.now();
  const source = event.headers?.["cf-connecting-ip"] || event.headers?.["x-forwarded-for"] || "unknown";
  const key = createHmac("sha256", rateSalt).update(source).digest("hex");
  for (const [id, row] of rates) if (row.until <= now) rates.delete(id);
  let row = rates.get(key);
  if (!row) {
    if (rates.size >= 10000) return false;
    row = { count: 0, until: now + 60000 }; rates.set(key, row);
  }
  return ++row.count <= 60;
}

async function identity(event, app) {
  if (!event.headers?.authorization) return {};
  const tenant = appConfig(app)?.tenant || "aesthetic";
  const user = await authorize(event.headers, tenant);
  if (!user?.sub) throw Object.assign(new Error("That token is not valid."), { statusCode: 401 });
  let handle = null;
  if (tenant === "aesthetic") {
    const found = await getHandleOrEmail(user.sub).catch(() => null);
    if (typeof found === "string" && found.startsWith("@")) handle = found.slice(1);
  }
  return { user: userKey(app, user.sub), handle };
}

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(204, "", HEADERS);
  if (!["POST", "GET", "DELETE"].includes(event.httpMethod)) return respond(405, { error: "Method not allowed" }, HEADERS);
  if (!permitted(event)) return respond(429, { error: "Rate limit reached" }, HEADERS);
  try {
    if (event.httpMethod === "POST") {
      let body = event.body;
      if (typeof body === "string") {
        if (body.length > 4096) return respond(400, { error: "Invalid report" }, HEADERS);
        body = JSON.parse(body);
      }
      const report = normalizeReport(body);
      const who = await identity(event, report.app);
      const { db } = await connect();
      await recordReport(db.collection(APP_DEVICES), report, who);
      return respond(204, "", HEADERS);
    }
    const app = event.queryStringParameters?.app;
    if (app !== undefined && !appConfig(app)) return respond(400, { error: "Unknown app" }, HEADERS);
    const who = await identity(event, app || "aestheticcomputer");
    if (!who.user) return respond(401, { error: "Sign in to see your devices" }, HEADERS);
    const { db } = await connect();
    const devices = db.collection(APP_DEVICES);
    if (event.httpMethod === "GET") {
      const rows = await devices.find({ user: who.user, ...(app ? { app } : {}) })
        .project(OWN_DEVICE_FIELDS).sort({ lastSeenAt: -1 }).limit(100).toArray();
      return respond(200, { devices: rows }, HEADERS);
    }
    const deviceId = event.queryStringParameters?.deviceId;
    if (!app || typeof deviceId !== "string") return respond(400, { error: "Specify app and deviceId" }, HEADERS);
    const result = await devices.deleteOne({ _id: deviceKey(app, deviceId), user: who.user });
    return respond(200, { deleted: result.deletedCount }, HEADERS);
  } catch (error) {
    if (error instanceof SyntaxError) return respond(400, { error: "Invalid JSON" }, HEADERS);
    if (error.statusCode) return respond(error.statusCode, { error: error.message }, HEADERS);
    console.error("app-device:", error);
    return respond(503, { error: "Device registry unavailable" }, HEADERS);
  }
}
