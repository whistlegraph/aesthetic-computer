// push, 26.06.11
// Vendor-free push delivery for aesthetic.computer. Replaces Firebase Cloud
// Messaging with two open channels:
//
//   1. Standard Web Push — RFC 8030 (protocol), RFC 8291 (aes128gcm payload
//      encryption), RFC 8292 (VAPID) — for browsers and installed PWAs,
//      including iOS home-screen PWAs (16.4+).
//   2. Direct APNs over HTTP/2 with ES256 token auth — for the native iOS
//      app's WKWebView wrapper, which cannot use Web Push.
//
// Zero dependencies: node:crypto + node:http2 + global fetch only, so this
// module runs unchanged on lith, the session-server droplet, and nanos
// without touching any package.json.
//
// Device documents live in the Mongo `push-tokens` collection:
//   { user, deviceId, label, kind: "webpush"|"apns", platform, token,
//     subscription: { endpoint, keys: { p256dh, auth } } | null,
//     topics: ["scream", "mood", ...], updatedAt }
// Legacy FCM-era docs (no `kind` field) are ignored by every query here.
//
// Env: VAPID_PUBLIC_KEY, VAPID_PRIVATE_KEY, VAPID_SUBJECT (web push)
//      APNS_TEAM_ID, APNS_KEY_ID, APNS_KEY (base64 .p8), APNS_BUNDLE_ID (apns)

import crypto from "node:crypto";
import http2 from "node:http2";

const PUSH_COLLECTION = "push-tokens";
const ICON = "https://aesthetic.computer/api/logo.png";
const SEND_CONCURRENCY = 16;

const b64u = (buf) => Buffer.from(buf).toString("base64url");
const fromB64u = (s) => Buffer.from(s, "base64url");

// 🔌 Configuration checks — senders no-op gracefully when keys are absent
//    (e.g. local dev without the vault).

export function webPushConfigured() {
  return !!(process.env.VAPID_PUBLIC_KEY && process.env.VAPID_PRIVATE_KEY);
}

export function apnsConfigured() {
  return !!(
    process.env.APNS_TEAM_ID &&
    process.env.APNS_KEY_ID &&
    process.env.APNS_KEY
  );
}

// 🕸️ Web Push

function vapidPrivateKey() {
  const pub = fromB64u(process.env.VAPID_PUBLIC_KEY); // 65B: 0x04 || x || y
  return crypto.createPrivateKey({
    key: {
      kty: "EC",
      crv: "P-256",
      x: b64u(pub.subarray(1, 33)),
      y: b64u(pub.subarray(33, 65)),
      d: process.env.VAPID_PRIVATE_KEY,
    },
    format: "jwk",
  });
}

const vapidJwtCache = {}; // audience → { token, exp }

function vapidJWT(audience) {
  const now = Math.floor(Date.now() / 1000);
  const cached = vapidJwtCache[audience];
  if (cached && cached.exp - now > 600) return cached.token;
  const header = b64u(JSON.stringify({ typ: "JWT", alg: "ES256" }));
  const exp = now + 12 * 3600;
  const claims = b64u(
    JSON.stringify({
      aud: audience,
      exp,
      sub: process.env.VAPID_SUBJECT || "mailto:mail@aesthetic.computer",
    }),
  );
  const unsigned = header + "." + claims;
  const sig = crypto.sign("sha256", Buffer.from(unsigned), {
    key: vapidPrivateKey(),
    dsaEncoding: "ieee-p1363",
  });
  const token = unsigned + "." + b64u(sig);
  vapidJwtCache[audience] = { token, exp };
  return token;
}

// Encrypt a payload for one subscription per RFC 8291 (aes128gcm).
// `testVector` injects { asJwk, salt } so the RFC's Appendix A vector can
// verify this implementation byte-for-byte — never used in production.
export function encryptWebPush(plaintext, p256dh, auth, testVector) {
  const uaPublic = fromB64u(p256dh); // user agent's 65-byte P-256 point
  const authSecret = fromB64u(auth); // 16-byte shared auth secret

  let asPrivateKey, asPublicRaw;
  if (testVector?.asJwk) {
    asPrivateKey = crypto.createPrivateKey({
      key: testVector.asJwk,
      format: "jwk",
    });
    asPublicRaw = Buffer.concat([
      Buffer.from([4]),
      fromB64u(testVector.asJwk.x),
      fromB64u(testVector.asJwk.y),
    ]);
  } else {
    const pair = crypto.generateKeyPairSync("ec", {
      namedCurve: "prime256v1",
    });
    asPrivateKey = pair.privateKey;
    const jwk = pair.publicKey.export({ format: "jwk" });
    asPublicRaw = Buffer.concat([
      Buffer.from([4]),
      fromB64u(jwk.x),
      fromB64u(jwk.y),
    ]);
  }

  const uaPublicKey = crypto.createPublicKey({
    key: {
      kty: "EC",
      crv: "P-256",
      x: b64u(uaPublic.subarray(1, 33)),
      y: b64u(uaPublic.subarray(33, 65)),
    },
    format: "jwk",
  });

  const ecdhSecret = crypto.diffieHellman({
    privateKey: asPrivateKey,
    publicKey: uaPublicKey,
  });

  const keyInfo = Buffer.concat([
    Buffer.from("WebPush: info\0"),
    uaPublic,
    asPublicRaw,
  ]);
  const ikm = Buffer.from(
    crypto.hkdfSync("sha256", ecdhSecret, authSecret, keyInfo, 32),
  );
  const salt = testVector?.salt ?? crypto.randomBytes(16);
  const cek = Buffer.from(
    crypto.hkdfSync(
      "sha256",
      ikm,
      salt,
      Buffer.from("Content-Encoding: aes128gcm\0"),
      16,
    ),
  );
  const nonce = Buffer.from(
    crypto.hkdfSync(
      "sha256",
      ikm,
      salt,
      Buffer.from("Content-Encoding: nonce\0"),
      12,
    ),
  );

  // Single record: plaintext || 0x02 delimiter (last record marker).
  const record = Buffer.concat([Buffer.from(plaintext), Buffer.from([2])]);
  const cipher = crypto.createCipheriv("aes-128-gcm", cek, nonce);
  const ciphertext = Buffer.concat([
    cipher.update(record),
    cipher.final(),
    cipher.getAuthTag(),
  ]);

  // Body: salt(16) || record-size(4) || keyid-len(1) || as-public(65) || ct
  const header = Buffer.alloc(21);
  salt.copy(header, 0);
  header.writeUInt32BE(4096, 16);
  header.writeUInt8(65, 20);
  return Buffer.concat([header, asPublicRaw, ciphertext]);
}

async function sendWebPush(subscription, payload, { ttl = 60, urgent } = {}) {
  const endpoint = subscription?.endpoint;
  const keys = subscription?.keys;
  if (!endpoint || !keys?.p256dh || !keys?.auth) {
    return { ok: false, gone: true, detail: "malformed subscription" };
  }
  try {
    const body = encryptWebPush(payload, keys.p256dh, keys.auth);
    const jwt = vapidJWT(new URL(endpoint).origin);
    const res = await fetch(endpoint, {
      method: "POST",
      headers: {
        "Content-Encoding": "aes128gcm",
        "Content-Type": "application/octet-stream",
        TTL: String(ttl),
        Urgency: urgent ? "high" : "normal",
        Authorization: `vapid t=${jwt}, k=${process.env.VAPID_PUBLIC_KEY}`,
      },
      body,
    });
    if (res.status >= 200 && res.status < 300) return { ok: true };
    return {
      ok: false,
      gone: res.status === 404 || res.status === 410,
      status: res.status,
      detail: (await res.text().catch(() => "")).slice(0, 200),
    };
  } catch (err) {
    return { ok: false, gone: false, detail: err?.message || String(err) };
  }
}

// 🍎 APNs (direct, token-based ES256 auth — no Firebase relay)

let apnsJwtCache = null; // { token, iat }

function apnsKey() {
  const raw = process.env.APNS_KEY || "";
  // Accept either a literal PEM or base64-of-the-.p8-file.
  const pem = raw.includes("-----BEGIN")
    ? raw.replace(/\\n/g, "\n")
    : Buffer.from(raw, "base64").toString("utf8");
  return crypto.createPrivateKey(pem);
}

function apnsJWT() {
  const now = Math.floor(Date.now() / 1000);
  // APNs rejects tokens older than 1h and refreshes faster than 20min;
  // re-sign every ~50 minutes.
  if (apnsJwtCache && now - apnsJwtCache.iat < 3000) return apnsJwtCache.token;
  const header = b64u(
    JSON.stringify({ alg: "ES256", kid: process.env.APNS_KEY_ID }),
  );
  const claims = b64u(JSON.stringify({ iss: process.env.APNS_TEAM_ID, iat: now }));
  const unsigned = header + "." + claims;
  const sig = crypto.sign("sha256", Buffer.from(unsigned), {
    key: apnsKey(),
    dsaEncoding: "ieee-p1363",
  });
  apnsJwtCache = { token: unsigned + "." + b64u(sig), iat: now };
  return apnsJwtCache.token;
}


function apnsRequest(session, deviceToken, payload, headers) {
  return new Promise((resolve) => {
    const req = session.request({
      ":method": "POST",
      ":path": `/3/device/${deviceToken}`,
      "content-type": "application/json",
      ...headers,
    });
    let status = 0;
    const chunks = [];
    req.on("response", (h) => (status = h[":status"]));
    req.on("data", (c) => chunks.push(c));
    req.on("end", () => {
      let reason;
      try {
        reason = JSON.parse(Buffer.concat(chunks).toString()).reason;
      } catch {}
      resolve({
        ok: status === 200,
        gone:
          status === 410 ||
          reason === "BadDeviceToken" ||
          reason === "Unregistered" ||
          reason === "DeviceTokenNotForTopic",
        status,
        detail: reason,
      });
    });
    req.on("error", (err) =>
      resolve({ ok: false, gone: false, detail: err?.message }),
    );
    // Stream closed without a response (e.g. session timeout) — don't hang
    // the batch; double-resolve after a normal "end" is a harmless no-op.
    req.on("close", () =>
      resolve({ ok: false, gone: false, status, detail: "stream closed" }),
    );
    req.setTimeout(10_000, () => req.close());
    req.end(payload);
  });
}

// Send to many APNs tokens over a single HTTP/2 session.
// One warm HTTP/2 session per APNs host, reused across sends: a push to one
// device costs a request, not a TLS handshake. Apple keeps idle connections
// open; pings keep NATs from dropping them, and a closed or errored session is
// replaced on the next send. unref() lets short-lived processes exit.
const apnsSessions = new Map();
function apnsSession(host) {
  const live = apnsSessions.get(host);
  if (live && !live.closed && !live.destroyed) return live;
  const session = http2.connect(host);
  session.unref();
  const drop = () => { if (apnsSessions.get(host) === session) apnsSessions.delete(host); };
  session.on("error", drop);
  session.on("close", drop);
  session.on("goaway", () => { drop(); session.close(); });
  const ping = setInterval(() => {
    if (session.closed || session.destroyed) return clearInterval(ping);
    session.ping(() => {});
  }, 60_000);
  ping.unref();
  session.on("close", () => clearInterval(ping));
  apnsSessions.set(host, session);
  return session;
}

export const APNS_HOSTS = Object.freeze({
  production: process.env.APNS_HOST || "https://api.push.apple.com",
  sandbox: "https://api.sandbox.push.apple.com",
});

function apnsPayload(note) {
  const aps = {
    alert: { title: note.title, body: note.body },
    sound: "default",
    "mutable-content": 1,
  };
  if (note.urgent) aps["interruption-level"] = "time-sensitive";
  if (note.thread) aps["thread-id"] = note.thread;
  return JSON.stringify({ aps, ...(note.data || {}) });
}

// Send to many APNs tokens over the warm session for `host`, as `topic`
// (the app's bundle id). Defaults keep the AC app's existing behavior.
async function sendAPNsBatch(tokens, note, {
  topic = process.env.APNS_BUNDLE_ID || "aesthetic.computer",
  host = APNS_HOSTS.production,
} = {}) {
  if (!apnsConfigured() || tokens.length === 0) {
    return tokens.map(() => ({ ok: false, detail: "apns not configured" }));
  }
  const payload = apnsPayload(note);
  const headers = {
    authorization: `bearer ${apnsJWT()}`,
    "apns-topic": topic,
    "apns-push-type": "alert",
    "apns-priority": note.urgent === false ? "5" : "10",
    "apns-expiration": String(note.expiresAt ? Math.floor(note.expiresAt / 1000) : 0),
    ...(note.collapse ? { "apns-collapse-id": String(note.collapse).slice(0, 64) } : {}),
  };
  let session;
  try { session = apnsSession(host); }
  catch (err) { return tokens.map(() => ({ ok: false, detail: err?.message })); }
  return Promise.all(tokens.map((t) => apnsRequest(session, t, payload, headers)));
}

function webPayload(note) {
  return JSON.stringify({
    title: note.title,
    body: note.body,
    icon: note.icon || ICON,
    image: note.image,
    data: note.data || {},
  });
}

async function pool(items, limit, work) {
  const results = new Array(items.length);
  let i = 0;
  async function lane() {
    while (i < items.length) {
      const idx = i++;
      results[idx] = await work(items[idx], idx);
    }
  }
  await Promise.all(
    Array.from({ length: Math.min(limit, items.length) }, lane),
  );
  return results;
}

// 🚚 Fan-out to a list of push-token documents, pruning dead ones.
export async function sendToDevices(db, docs, note, log = console.log) {
  const webDocs = docs.filter(
    (d) => d.kind === "webpush" && d.subscription?.endpoint,
  );
  const apnsDocs = docs.filter((d) => d.kind === "apns" && d.token);

  const summary = {
    attempted: webDocs.length + apnsDocs.length,
    succeeded: 0,
    failed: 0,
    pruned: 0,
  };
  if (summary.attempted === 0) return summary;

  const goneIds = [];
  const payload = webPayload(note);

  const webResults = webPushConfigured()
    ? await pool(webDocs, SEND_CONCURRENCY, (doc) =>
        sendWebPush(doc.subscription, payload, {
          ttl: note.ttl ?? 60,
          urgent: note.urgent,
        }),
      )
    : webDocs.map(() => ({ ok: false, detail: "vapid not configured" }));

  const apnsResults = await sendAPNsBatch(
    apnsDocs.map((d) => d.token),
    note,
  );

  [...webResults, ...apnsResults].forEach((res, i) => {
    const doc = i < webResults.length ? webDocs[i] : apnsDocs[i - webResults.length];
    if (res.ok) {
      summary.succeeded++;
    } else {
      summary.failed++;
      if (res.gone) goneIds.push(doc._id);
      else if (res.detail) {
        log(`🔔 push failed (${doc.kind} ${doc.label || doc.deviceId || ""}):`, res.status || "", res.detail);
      }
    }
  });

  if (goneIds.length && db) {
    await db
      .collection(PUSH_COLLECTION)
      .deleteMany({ _id: { $in: goneIds } })
      .then(() => {
        summary.pruned = goneIds.length;
        log(`🧹 Pruned ${goneIds.length} dead push registrations.`);
      })
      .catch((err) => log("🔴 push prune failed:", err?.message));
  }

  return summary;
}

// 📣 Broadcast to every device subscribed to a topic ("scream", "mood",
//    "chat-system", ...).
export async function broadcastToTopic(db, topic, note, log = console.log) {
  const docs = await db
    .collection(PUSH_COLLECTION)
    .find({ topics: topic, kind: { $in: ["webpush", "apns"] } })
    .toArray();
  const summary = await sendToDevices(db, docs, note, log);
  log(`🔔 "${topic}" broadcast:`, JSON.stringify(summary));
  return summary;
}

// 🎯 Send to one user's devices — all of them, or a single device addressed
//    by deviceId (exact) or label (case-insensitive substring).
export async function sendToUser(db, userSub, note, { device } = {}, log = console.log) {
  let docs = await db
    .collection(PUSH_COLLECTION)
    .find({ user: userSub, kind: { $in: ["webpush", "apns"] } })
    .toArray();
  if (device) {
    const needle = String(device).toLowerCase();
    docs = docs.filter(
      (d) =>
        d.deviceId === device ||
        (d.label || "").toLowerCase().includes(needle),
    );
  }
  return sendToDevices(db, docs, note, log);
}

// 🛰️ AC network notifications: send to a device, a person, or a group in the
//    app device registry (Mongo `app-devices`, shared/app-devices.mjs).
//      target: { app, deviceId } | { user, app? } | { app, topic }
//      note:   { title, body, data?, thread?, collapse?, urgent?, expiresAt? }
//    Each device is reached through its own app's APNs topic or Web Push.
//    Registrations the push service reports gone are cleared from their row.
export async function sendToTarget(db, target, note, log = console.log) {
  const { APP_DEVICES, targetFilter } = await import("./app-devices.mjs");
  const { appConfig } = await import("./app-registry.mjs");
  const devices = db.collection(APP_DEVICES);
  const docs = await devices.find({ ...targetFilter(target), push: { $exists: true } }).toArray();
  const summary = { attempted: docs.length, succeeded: 0, failed: 0, pruned: 0 };
  if (!docs.length) return summary;

  // Group APNs rows by app topic and environment; each group is one batch.
  const groups = new Map();
  const web = [];
  for (const doc of docs) {
    if (doc.push.kind === "webpush") { web.push(doc); continue; }
    const topic = appConfig(doc.app)?.apns;
    if (doc.push.kind !== "apns" || !topic) { summary.failed++; continue; }
    const key = `${topic}|${doc.push.env}`;
    if (!groups.has(key)) groups.set(key, { topic, host: APNS_HOSTS[doc.push.env] || APNS_HOSTS.production, docs: [] });
    groups.get(key).docs.push(doc);
  }
  const gone = [];
  const tally = (doc, res) => {
    if (res.ok) return void summary.succeeded++;
    summary.failed++;
    if (res.gone) gone.push(doc._id);
    else log(`🔔 push failed (${doc.app} ${doc.handle ? "@" + doc.handle : doc.deviceId}):`, res.status || "", res.detail);
  };
  await Promise.all([
    ...[...groups.values()].map(async ({ topic, host, docs: batch }) => {
      const results = await sendAPNsBatch(batch.map((d) => d.push.token), note, { topic, host });
      results.forEach((res, i) => tally(batch[i], res));
    }),
    (async () => {
      if (!web.length) return;
      if (!webPushConfigured()) return web.forEach((d) => tally(d, { ok: false, detail: "vapid not configured" }));
      const payload = webPayload(note);
      const results = await pool(web, SEND_CONCURRENCY, (d) => sendWebPush(d.push.subscription, payload, { ttl: note.ttl ?? 60, urgent: note.urgent }));
      results.forEach((res, i) => tally(web[i], res));
    })(),
  ]);
  if (gone.length) {
    await devices.updateMany({ _id: { $in: gone } }, { $unset: { push: "" } })
      .then(() => { summary.pruned = gone.length; })
      .catch((err) => log("🔴 push prune failed:", err?.message));
  }
  return summary;
}
