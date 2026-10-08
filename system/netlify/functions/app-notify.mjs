// App Notify, 2026.10.08
// A signed-in person notifies their own devices through the AC network device
// registry (shared/push.mjs sendToTarget). It cannot reach anyone else: the
// target is always the caller's verified account, narrowed by app or device.
//
// POST /api/app-notify  {title, body?, app?, deviceId?, url?, thread?}
//      Authorization: Bearer <token>
//   → {attempted, succeeded, failed, pruned}
import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { appConfig, userKey } from "../../../shared/app-registry.mjs";
import { sendToTarget } from "../../../shared/push.mjs";

const HEADERS = { "Cache-Control": "no-store" };
const PER_HOUR = 60;
const sent = new Map(); // user → timestamps in the last hour (ephemeral)

function allowed(user, now = Date.now()) {
  const recent = (sent.get(user) || []).filter(at => now - at < 3600_000);
  if (recent.length >= PER_HOUR) return false;
  recent.push(now); sent.set(user, recent);
  if (sent.size > 10000) for (const [key, times] of sent) if (!times.some(at => now - at < 3600_000)) sent.delete(key);
  return true;
}

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(204, "", HEADERS);
  if (event.httpMethod !== "POST") return respond(405, { error: "POST required" }, HEADERS);
  let body;
  try { body = JSON.parse(event.body || "{}"); } catch { return respond(400, { error: "Invalid JSON" }, HEADERS); }
  const { title, app, deviceId, url, thread } = body;
  if (typeof title !== "string" || !title.trim() || title.length > 120) return respond(400, { error: "A title up to 120 characters is required" }, HEADERS);
  if (body.body !== undefined && (typeof body.body !== "string" || body.body.length > 1000)) return respond(400, { error: "Body must be text up to 1000 characters" }, HEADERS);
  if (app !== undefined && !appConfig(app)) return respond(400, { error: "Unknown app" }, HEADERS);
  if (deviceId !== undefined && (!app || typeof deviceId !== "string")) return respond(400, { error: "A device needs its app" }, HEADERS);
  if (url !== undefined && (typeof url !== "string" || !url.startsWith("/") || url.length > 200)) return respond(400, { error: "url must be a path" }, HEADERS);

  const tenant = appConfig(app)?.tenant || "aesthetic";
  const user = await authorize(event.headers, tenant);
  if (!user?.sub) return respond(401, { error: "Sign in to send notifications" }, HEADERS);
  const key = userKey(app || "aestheticcomputer", user.sub);
  if (!allowed(key)) return respond(429, { error: "Notification limit reached; try again later" }, HEADERS);

  try {
    const { db } = await connect();
    // A device target still has to belong to the caller.
    if (deviceId) {
      const owned = await db.collection("app-devices").countDocuments({ _id: `${app}:${deviceId}`, user: key });
      if (!owned) return respond(404, { error: "No such device on your account" }, HEADERS);
    }
    const target = deviceId ? { app, deviceId } : { user: key, ...(app ? { app } : {}) };
    const summary = await sendToTarget(db, target, {
      title: title.trim(), body: (body.body || "").trim(),
      ...(thread ? { thread: String(thread).slice(0, 64), collapse: String(thread).slice(0, 64) } : {}),
      ...(url ? { data: { url } } : {}),
    });
    return respond(200, summary, HEADERS);
  } catch (error) {
    console.error("app-notify:", error);
    return respond(503, { error: "Notifications unavailable" }, HEADERS);
  }
}
