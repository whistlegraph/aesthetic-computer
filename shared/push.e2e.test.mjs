// push.e2e.test, 26.06.11
// Loopback end-to-end test for shared/push.mjs: runs the real fan-out
// (sendToDevices) against a local mock Web Push endpoint and a local mock
// APNs HTTP/2 server, then verifies on the "receiving" side:
//   - Web Push: VAPID JWT signature + audience, aes128gcm decryption with the
//     subscriber's private key, payload JSON shape, prune-on-410.
//   - APNs: ES256 bearer JWT, :path device token, apns headers, aps payload.
// Run: node shared/push.e2e.test.mjs

import crypto from "node:crypto";
import http from "node:http";
import http2 from "node:http2";

// Test VAPID identity.
const vapidPair = crypto.generateKeyPairSync("ec", { namedCurve: "prime256v1" });
const vapidJwk = vapidPair.privateKey.export({ format: "jwk" });
process.env.VAPID_PUBLIC_KEY = Buffer.concat([
  Buffer.from([4]),
  Buffer.from(vapidJwk.x, "base64url"),
  Buffer.from(vapidJwk.y, "base64url"),
]).toString("base64url");
process.env.VAPID_PRIVATE_KEY = vapidJwk.d;
process.env.VAPID_SUBJECT = "mailto:test@aesthetic.computer";

// Test APNs identity (self-minted P-256 "p8").
const apnsPair = crypto.generateKeyPairSync("ec", { namedCurve: "prime256v1" });
process.env.APNS_TEAM_ID = "TESTTEAM01";
process.env.APNS_KEY_ID = "TESTKEY123";
process.env.APNS_KEY = Buffer.from(
  apnsPair.privateKey.export({ type: "pkcs8", format: "pem" }),
).toString("base64");
process.env.APNS_BUNDLE_ID = "aesthetic.computer";

let failures = 0;
function check(name, ok, detail = "") {
  console.log(`${ok ? "✅" : "❌"} ${name}${ok ? "" : " — " + detail}`);
  if (!ok) failures++;
}

function verifyJWT(jwt, publicKey) {
  const [h, c, s] = jwt.split(".");
  const ok = crypto.verify(
    "sha256",
    Buffer.from(`${h}.${c}`),
    { key: publicKey, dsaEncoding: "ieee-p1363" },
    Buffer.from(s, "base64url"),
  );
  return { ok, claims: JSON.parse(Buffer.from(c, "base64url")) };
}

// Subscriber ("browser") keys.
const uaPair = crypto.generateKeyPairSync("ec", { namedCurve: "prime256v1" });
const uaJwk = uaPair.privateKey.export({ format: "jwk" });
const uaPublicRaw = Buffer.concat([
  Buffer.from([4]),
  Buffer.from(uaJwk.x, "base64url"),
  Buffer.from(uaJwk.y, "base64url"),
]);
const uaAuth = crypto.randomBytes(16);

function decryptWebPush(body) {
  // Reverse of RFC 8291: header = salt(16) || rs(4) || idlen(1) || asPub(65)
  const salt = body.subarray(0, 16);
  const idlen = body.readUInt8(20);
  const asPublicRaw = body.subarray(21, 21 + idlen);
  const ciphertext = body.subarray(21 + idlen);
  const asPublicKey = crypto.createPublicKey({
    key: {
      kty: "EC",
      crv: "P-256",
      x: asPublicRaw.subarray(1, 33).toString("base64url"),
      y: asPublicRaw.subarray(33, 65).toString("base64url"),
    },
    format: "jwk",
  });
  const ecdh = crypto.diffieHellman({
    privateKey: uaPair.privateKey,
    publicKey: asPublicKey,
  });
  const keyInfo = Buffer.concat([
    Buffer.from("WebPush: info\0"),
    uaPublicRaw,
    asPublicRaw,
  ]);
  const ikm = Buffer.from(crypto.hkdfSync("sha256", ecdh, uaAuth, keyInfo, 32));
  const cek = Buffer.from(
    crypto.hkdfSync("sha256", ikm, salt, Buffer.from("Content-Encoding: aes128gcm\0"), 16),
  );
  const nonce = Buffer.from(
    crypto.hkdfSync("sha256", ikm, salt, Buffer.from("Content-Encoding: nonce\0"), 12),
  );
  const decipher = crypto.createDecipheriv("aes-128-gcm", cek, nonce);
  decipher.setAuthTag(ciphertext.subarray(-16));
  const record = Buffer.concat([
    decipher.update(ciphertext.subarray(0, -16)),
    decipher.final(),
  ]);
  return record.subarray(0, record.lastIndexOf(2)).toString(); // strip 0x02 pad
}

// — Mock Web Push endpoint —
const webRequests = [];
const webServer = http.createServer((req, res) => {
  const chunks = [];
  req.on("data", (c) => chunks.push(c));
  req.on("end", () => {
    webRequests.push({ headers: req.headers, body: Buffer.concat(chunks), url: req.url });
    res.statusCode = req.url === "/gone" ? 410 : 201;
    res.end();
  });
});
await new Promise((r) => webServer.listen(0, "127.0.0.1", r));
const webPort = webServer.address().port;

// — Mock APNs HTTP/2 server (h2c) —
const apnsRequests = [];
const apnsServer = http2.createServer();
apnsServer.on("stream", (stream, headers) => {
  const chunks = [];
  stream.on("data", (c) => chunks.push(c));
  stream.on("end", () => {
    apnsRequests.push({ headers, body: Buffer.concat(chunks).toString() });
    stream.respond({ ":status": 200 });
    stream.end();
  });
});
await new Promise((r) => apnsServer.listen(0, "127.0.0.1", r));
process.env.APNS_HOST = `http://127.0.0.1:${apnsServer.address().port}`;

// Import AFTER env is staged (module reads env lazily anyway, but be safe).
const { sendToDevices } = await import("./push.mjs");

const pruned = [];
const fakeDb = {
  collection: () => ({
    deleteMany: async (q) => {
      pruned.push(...q._id.$in);
      return { deletedCount: q._id.$in.length };
    },
  }),
};

const docs = [
  {
    _id: "web-ok",
    kind: "webpush",
    label: "Mock Browser",
    subscription: {
      endpoint: `http://127.0.0.1:${webPort}/ok`,
      keys: {
        p256dh: uaPublicRaw.toString("base64url"),
        auth: uaAuth.toString("base64url"),
      },
    },
  },
  {
    _id: "web-gone",
    kind: "webpush",
    label: "Dead Browser",
    subscription: {
      endpoint: `http://127.0.0.1:${webPort}/gone`,
      keys: {
        p256dh: uaPublicRaw.toString("base64url"),
        auth: uaAuth.toString("base64url"),
      },
    },
  },
  {
    _id: "ios-ok",
    kind: "apns",
    label: "Mock iPhone",
    token: "ab".repeat(32),
  },
];

const note = {
  title: "😱 Scream",
  body: "the push stack is vendor-free",
  urgent: true,
  data: { piece: "chat" },
};

const summary = await sendToDevices(fakeDb, docs, note, () => {});

check("fan-out summary counts", summary.attempted === 3 && summary.succeeded === 2 && summary.failed === 1, JSON.stringify(summary));
check("dead web subscription pruned", pruned.length === 1 && pruned[0] === "web-gone", JSON.stringify(pruned));

// Web Push assertions.
const okReq = webRequests.find((r) => r.url === "/ok");
check("web push request arrived", !!okReq);
if (okReq) {
  check("aes128gcm content encoding", okReq.headers["content-encoding"] === "aes128gcm");
  check("urgency high", okReq.headers["urgency"] === "high");
  const auth = okReq.headers["authorization"] || "";
  const m = auth.match(/^vapid t=([^,]+), k=(.+)$/);
  check("vapid authorization header shape", !!m);
  if (m) {
    const { ok, claims } = verifyJWT(m[1], vapidPair.publicKey);
    check("vapid JWT signature verifies", ok);
    check("vapid JWT audience", claims.aud === `http://127.0.0.1:${webPort}`, claims.aud);
    check("vapid k matches public key", m[2] === process.env.VAPID_PUBLIC_KEY);
  }
  const decrypted = JSON.parse(decryptWebPush(okReq.body));
  check("payload decrypts to note", decrypted.title === note.title && decrypted.body === note.body, JSON.stringify(decrypted));
  check("payload carries piece", decrypted.data?.piece === "chat");
}

// APNs assertions.
const apnsReq = apnsRequests[0];
check("apns request arrived", !!apnsReq);
if (apnsReq) {
  check("apns path carries device token", apnsReq.headers[":path"] === `/3/device/${"ab".repeat(32)}`);
  check("apns topic header", apnsReq.headers["apns-topic"] === "aesthetic.computer");
  check("apns push type alert", apnsReq.headers["apns-push-type"] === "alert");
  const bearer = (apnsReq.headers["authorization"] || "").replace("bearer ", "");
  const { ok, claims } = verifyJWT(bearer, apnsPair.publicKey);
  check("apns JWT signature verifies", ok);
  check("apns JWT issuer is team id", claims.iss === "TESTTEAM01");
  const payload = JSON.parse(apnsReq.body);
  check("aps alert title/body", payload.aps?.alert?.title === note.title && payload.aps?.alert?.body === note.body);
  check("aps time-sensitive when urgent", payload.aps?.["interruption-level"] === "time-sensitive");
  check("custom piece key at top level", payload.piece === "chat");
}

webServer.close();
apnsServer.close();
process.exit(failures ? 1 : 0);
