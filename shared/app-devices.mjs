// App devices — the AC network's device registry, in Mongo ("app-devices").
//
// One row per app + device: who is signed in on it, which build it runs, when
// it last opened, and (once the person allows notifications) how to reach it.
// It answers "who is on build 114, on which device" and it is the target list
// for notifications to a device, a person, or a group (topic).
//
//   _id        "<app>:<deviceId>"
//   app        a key of APPS (app-registry.mjs)
//   deviceId   stable per app install: iOS identifierForVendor, or a UUID the
//              client keeps (web, desktop)
//   user       verified Auth0 key (sotce users "sotce-" + sub), or absent
//   handle     handle at last verified report, without "@"
//   platform, version, build, model, os, label
//   firstAt, lastSeenAt, lastOpenAt, opens
//   push       { kind: "apns", token, env } | { kind: "webpush", subscription }
//   topics     group names this device receives, e.g. "testers"
//
// Rows do not expire: devices are kept until the app removes them, the push
// service reports them gone, or the account is deleted.
import { APPS, PLATFORMS, appConfig } from "./app-registry.mjs";

export const APP_DEVICES = "app-devices";
export const EVENTS = Object.freeze(["open", "seen", "login", "logout", "push"]);

const DEVICE_ID = /^[A-Za-z0-9-]{8,128}$/;
const VERSION = /^\d+(?:\.\d+){0,3}$/;
const BUILD = /^\d{1,9}$/;
const TOPIC = /^[a-z0-9][a-z0-9:-]{0,39}$/;
const APNS_TOKEN = /^[0-9a-fA-F]{32,512}$/;
const text = (value, max) => typeof value === "string" ? value.slice(0, max) : "";

function normalizePush(push, app) {
  if (push === null) return null; // permission revoked or token dropped
  if (push === undefined) return undefined;
  if (push?.kind === "apns" && appConfig(app)?.apns && APNS_TOKEN.test(push.token ?? "")) {
    return { kind: "apns", token: push.token.toLowerCase(), env: push.env === "sandbox" ? "sandbox" : "production" };
  }
  const sub = push?.subscription;
  if (push?.kind === "webpush" && appConfig(app)?.web && typeof sub?.endpoint === "string" &&
      sub.endpoint.startsWith("https://") && typeof sub.keys?.p256dh === "string" && typeof sub.keys?.auth === "string") {
    return { kind: "webpush", subscription: { endpoint: sub.endpoint, keys: { p256dh: sub.keys.p256dh, auth: sub.keys.auth } } };
  }
  throw Object.assign(new Error("Invalid push registration"), { statusCode: 400 });
}

// Validate a client report. Throws a 400-shaped error on anything malformed.
export function normalizeReport(body) {
  const fail = message => { throw Object.assign(new Error(message), { statusCode: 400 }); };
  if (!body || typeof body !== "object") fail("Invalid report");
  if (!Object.hasOwn(APPS, body.app)) fail("Unknown app");
  if (!DEVICE_ID.test(body.deviceId ?? "")) fail("Invalid deviceId");
  if (!PLATFORMS.includes(body.platform)) fail("Invalid platform");
  if (!EVENTS.includes(body.event)) fail("Invalid event");
  if (body.version !== undefined && !VERSION.test(body.version)) fail("Invalid version");
  if (body.build !== undefined && !BUILD.test(String(body.build))) fail("Invalid build");
  let topics;
  if (body.topics !== undefined) {
    if (!Array.isArray(body.topics) || body.topics.length > 16 || !body.topics.every(t => TOPIC.test(t))) fail("Invalid topics");
    topics = [...new Set(body.topics)];
  }
  return {
    app: body.app, deviceId: body.deviceId, platform: body.platform, event: body.event,
    version: body.version, build: body.build === undefined ? undefined : String(body.build),
    model: text(body.model, 40), os: text(body.os, 40), label: text(body.label, 64),
    push: normalizePush(body.push, body.app), topics,
  };
}

export const deviceKey = (app, deviceId) => `${app}:${deviceId}`;

// The Mongo update for one report. `user`/`handle` come from the verified
// token only; an unsigned report never binds or unbinds an account, except
// "logout", which unbinds this device.
export function reportUpdate(report, { user = null, handle = null, now = new Date() } = {}) {
  const set = { platform: report.platform, lastSeenAt: now };
  for (const field of ["version", "build", "model", "os", "label"]) if (report[field]) set[field] = report[field];
  if (report.topics) set.topics = report.topics;
  if (user) { set.user = user; if (handle) set.handle = handle; }
  if (report.event === "open") set.lastOpenAt = now;
  if (report.push) set.push = { ...report.push, updatedAt: now };
  const unset = {};
  if (report.event === "logout") { unset.user = ""; unset.handle = ""; }
  if (report.push === null) unset.push = "";
  return {
    $setOnInsert: { app: report.app, deviceId: report.deviceId, firstAt: now, ...(report.topics ? {} : { topics: [] }) },
    $set: set,
    ...(Object.keys(unset).length ? { $unset: unset } : {}),
    ...(report.event === "open" ? { $inc: { opens: 1 } } : {}),
  };
}

let indexed = new WeakSet();
export async function ensureIndexes(collection) {
  if (indexed.has(collection)) return;
  await Promise.all([
    collection.createIndex({ app: 1, user: 1 }),
    collection.createIndex({ app: 1, topics: 1 }),
    collection.createIndex({ app: 1, build: 1 }),
    collection.createIndex({ "push.token": 1 }, { sparse: true }),
    collection.createIndex({ user: 1 }),
  ]);
  indexed.add(collection);
}

export async function recordReport(collection, report, identity = {}) {
  await ensureIndexes(collection);
  const _id = deviceKey(report.app, report.deviceId);
  // A push token belongs to one device row; a reinstall or account switch
  // that reuses it must not leave a second row delivering to the same phone.
  if (report.push?.kind === "apns") {
    await collection.updateMany({ "push.token": report.push.token, _id: { $ne: _id } }, { $unset: { push: "" } });
  }
  const update = reportUpdate(report, identity);
  try {
    await collection.updateOne({ _id }, update, { upsert: true });
  } catch (error) {
    if (error.code !== 11000) throw error; // two first reports raced the upsert
    await collection.updateOne({ _id }, update);
  }
  return _id;
}

// Mongo filter for a notification target.
//   { app, deviceId }  one device
//   { app?, user }     every device a person is signed in on (optionally one app)
//   { app, topic }     a group
export function targetFilter(target) {
  if (target?.deviceId) {
    if (!target.app) throw new Error("A device target needs its app");
    return { _id: deviceKey(target.app, target.deviceId) };
  }
  if (target?.user) return { user: target.user, ...(target.app ? { app: target.app } : {}) };
  if (target?.topic) {
    if (!target.app || !TOPIC.test(target.topic)) throw new Error("A topic target needs its app and a valid topic");
    return { app: target.app, topics: target.topic };
  }
  throw new Error("Unknown notification target");
}

// What a person may see about their own devices.
export const OWN_DEVICE_FIELDS = Object.freeze({
  _id: 0, app: 1, deviceId: 1, platform: 1, version: 1, build: 1, model: 1, os: 1, label: 1,
  firstAt: 1, lastSeenAt: 1, lastOpenAt: 1, opens: 1, topics: 1, "push.kind": 1, "push.env": 1,
});
