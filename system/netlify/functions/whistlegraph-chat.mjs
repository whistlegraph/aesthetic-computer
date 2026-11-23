import { apnsConfigured, webPushConfigured, sendToDevices } from "../../../shared/push.mjs";
import { createHash } from "node:crypto";
import { authorize, handleFor } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { isWhistlegraphAdmin } from "./whistlegraph-admin-lib.mjs";

const reply = (status, body) => respond(status, body, { "Cache-Control": "no-store" });
const error = (status, message) => reply(status, { message });
const publicFields = { _id: 1, handle: 1, text: 1, when: 1 };
let indexes;
async function prepare(messages) {
  indexes ??= Promise.all([
    messages.createIndex({ when: -1, _id: -1 }),
    messages.createIndex({ author: 1, slot: 1 }, { unique: true }),
  ]).catch(err => { indexes = undefined; throw err; });
  await indexes;
}

const APP_SUBSCRIBERS = "whistlegraph-chat-app-subscribers";
async function notifyRoom(db, author, handle, text, messageId, browserDevice) {
  const users = await db.collection(APP_SUBSCRIBERS).distinct("_id", { _id: { $ne: author } });
  const devices = await db.collection("push-tokens").find({
    user: { $ne: author },
    $or: [
      { kind: "webpush", topics: "chat-whistlegraph", ...(typeof browserDevice === "string" && { deviceId: { $ne: browserDevice } }) },
      { kind: "apns", user: { $in: users } },
    ],
  }).toArray();
  await sendToDevices(db, devices, {
    title: "@" + handle + " · Whistlegraph", body: [...text].slice(0, 180).join(""),
    icon: "https://whistlegraph.org/favicon.png", thread: "whistlegraph",
    ttl: 300, expiresAt: Date.now() + 300000, data: { piece: "whistlegraph", messageId },
  }, () => {});
}

export async function handler(event) {
  const method = event.httpMethod;
  const action = event.queryStringParameters?.action || "messages";
  if (method === "OPTIONS") return reply(204, null);
  try {
    if (method === "GET" && action === "push-config") {
      return reply(200, { webKey: webPushConfigured() ? process.env.VAPID_PUBLIC_KEY : null, app: apnsConfigured() });
    }
    if (method === "GET" && action === "messages") {
      const { db } = await connect();
      const messages = await db.collection("whistlegraph-chat").find({ removed: { $ne: true } })
        .project(publicFields).sort({ when: -1, _id: -1 }).limit(100).toArray();
      return respond(200, { messages: messages.reverse() }, { "Cache-Control": "public, max-age=0, s-maxage=2" });
    }
    const user = await authorize(event.headers || {});
    if (!user?.sub) return error(401, "Sign in with Aesthetic Computer.");
    const host = isWhistlegraphAdmin(user);
    if (method === "GET" && action === "app-alerts") {
      const { db } = await connect();
      return reply(200, { enabled: !!await db.collection(APP_SUBSCRIBERS).findOne({ _id: user.sub }) });
    }
    if (method === "GET" && action === "session") {
      return reply(200, { handle: await handleFor(user.sub) || null, host });
    }
    if (method !== "POST") return error(405, "Use GET or POST.");
    let body;
    try { body = JSON.parse(event.body); } catch { return error(400, "Invalid message."); }
    if (!body || typeof body !== "object") return error(400, "Invalid message.");
    const { db } = await connect();
    if (action === "app-alerts") {
      if (typeof body.enabled !== "boolean") return error(400, "Choose on or off.");
      const subscribers = db.collection(APP_SUBSCRIBERS);
      if (body.enabled) await subscribers.updateOne({ _id: user.sub }, { $set: { updatedAt: new Date() } }, { upsert: true });
      else await subscribers.deleteOne({ _id: user.sub });
      const device = body.enabled && await db.collection("push-tokens").findOne({ user: user.sub, kind: "apns" }, { projection: { _id: 1 } });
      return reply(200, { enabled: body.enabled, hasDevice: !!device });
    }
    const messages = db.collection("whistlegraph-chat");
    const mutes = db.collection("whistlegraph-chat-mutes");

    if (action === "remove" || action === "mute") {
      if (!host) return error(403, "Only a room host can moderate.");
      if (!/^[0-9a-f]{64}$/.test(String(body.id))) return error(400, "Invalid message ID.");
      const target = await messages.findOne({ _id: body.id });
      if (!target) return error(404, "Message unavailable.");
      if (action === "remove") await messages.updateOne({ _id: body.id }, { $set: { removed: true }, $unset: { text: "" } });
      else await mutes.updateOne({ _id: target.author }, { $set: { until: new Date(Date.now() + 86400000) } }, { upsert: true });
      return reply(200, { ok: true });
    }
    if (action !== "messages") return error(404, "Unknown action.");
    const handle = await handleFor(user.sub);
    if (!handle) return error(403, "Choose an AC handle before posting.");
    if (await mutes.findOne({ _id: user.sub, until: { $gt: new Date() } })) return error(403, "You are muted in this room.");
    const text = typeof body.text === "string" && body.text.trim();
    if (!text || text.length > 1400) return error(400, "Write a message of up to 1,400 characters.");
    if (typeof body.requestId !== "string" || !/^[0-9a-f-]{36}$/i.test(body.requestId)) return error(400, "Missing message ID.");
    const _id = createHash("sha256").update(user.sub + ":" + body.requestId).digest("hex");
    const when = new Date();
    await prepare(messages);
    try {
      await messages.insertOne({ _id, handle, text, when, author: user.sub, slot: Math.floor(when.getTime() / 2000) });
    } catch (err) {
      if (err.code !== 11000) throw err;
      if (!await messages.findOne({ _id }, { projection: { _id: 1 } })) return error(429, "Give it a moment before sending again.");
      return reply(200, { id: _id });
    }
    // Lith is a persistent process. Provider latency never blocks the next message.
    notifyRoom(db, user.sub, handle, text, _id, body.browserDevice).catch(err => console.error("Whistlegraph push unavailable", err?.name));
    return reply(200, { id: _id });
  } catch (err) {
    console.error("Whistlegraph chat unavailable", err?.name);
    return error(503, "The room is temporarily unavailable. Try again.");
  }
}
