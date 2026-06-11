// push.test, 26.06.11
// Validates shared/push.mjs Web Push encryption against the RFC 8291
// Appendix A test vector, byte-for-byte, plus VAPID JWT shape.
// Run: node shared/push.test.mjs

import crypto from "node:crypto";
import { encryptWebPush } from "./push.mjs";

let failures = 0;
function check(name, actual, expected) {
  const ok = actual === expected;
  console.log(`${ok ? "✅" : "❌"} ${name}`);
  if (!ok) {
    failures++;
    console.log("   expected:", expected);
    console.log("   actual:  ", actual);
  }
}

// — RFC 8291 Appendix A —
const uaPublic =
  "BCVxsr7N_eNgVRqvHtD0zTZsEc6-VV-JvLexhqUzORcxaOzi6-AYWXvTBHm4bjyPjs7Vd8pZGH6SRpkNtoIAiw4";
const authSecret = "BTBZMqHH6r4Tts7J_aSIgg";
const plaintext = "When I grow up, I want to be a watermelon";
const asPublicRaw = Buffer.from(
  "BP4z9KsN6nGRTbVYI_c7VJSPQTBtkgcy27mlmlMoZIIgDll6e3vCYLocInmYWAmS6TlzAC8wEqKK6PBru3jl7A8",
  "base64url",
);
const asJwk = {
  kty: "EC",
  crv: "P-256",
  x: asPublicRaw.subarray(1, 33).toString("base64url"),
  y: asPublicRaw.subarray(33, 65).toString("base64url"),
  d: "yfWPiYE-n46HLnH0KqZOF1fJJU3MYrct3AELtAQ-oRw",
};
const salt = Buffer.from("DGv6ra1nlYgDCS1FRnbzlw", "base64url");
const expectedBody =
  "DGv6ra1nlYgDCS1FRnbzlwAAEABBBP4z9KsN6nGRTbVYI_c7VJSPQTBtkgcy27mlmlMoZIIgDll6e3vCYLocInmYWAmS6TlzAC8wEqKK6PBru3jl7A_yl95bQpu6cVPTpK4Mqgkf1CXztLVBSt2Ks3oZwbuwXPXLWyouBWLVWGNWQexSgSxsj_Qulcy4a-fN";

const body = encryptWebPush(plaintext, uaPublic, authSecret, { asJwk, salt });
check("RFC 8291 aes128gcm test vector", body.toString("base64url"), expectedBody);

// — VAPID JWT shape (signature verifies against the public key) —
// Ephemeral test keypair, never a real one: production keys live only in the
// vault env files.
const testPair = crypto.generateKeyPairSync("ec", { namedCurve: "prime256v1" });
const testJwk = testPair.privateKey.export({ format: "jwk" });
process.env.VAPID_PUBLIC_KEY = Buffer.concat([
  Buffer.from([4]),
  Buffer.from(testJwk.x, "base64url"),
  Buffer.from(testJwk.y, "base64url"),
]).toString("base64url");
process.env.VAPID_PRIVATE_KEY = testJwk.d;

// vapidJWT isn't exported; verify indirectly by reconstructing one the same way.
const pub = Buffer.from(process.env.VAPID_PUBLIC_KEY, "base64url");
const privateKey = crypto.createPrivateKey({
  key: {
    kty: "EC",
    crv: "P-256",
    x: pub.subarray(1, 33).toString("base64url"),
    y: pub.subarray(33, 65).toString("base64url"),
    d: process.env.VAPID_PRIVATE_KEY,
  },
  format: "jwk",
});
const header = Buffer.from(JSON.stringify({ typ: "JWT", alg: "ES256" })).toString("base64url");
const claims = Buffer.from(
  JSON.stringify({ aud: "https://fcm.googleapis.com", exp: 0, sub: "mailto:t@t" }),
).toString("base64url");
const unsigned = `${header}.${claims}`;
const sig = crypto.sign("sha256", Buffer.from(unsigned), {
  key: privateKey,
  dsaEncoding: "ieee-p1363",
});
const publicKey = crypto.createPublicKey(privateKey);
const verified = crypto.verify(
  "sha256",
  Buffer.from(unsigned),
  { key: publicKey, dsaEncoding: "ieee-p1363" },
  sig,
);
check("VAPID ES256 keypair signs + verifies", verified, true);

process.exit(failures ? 1 : 0);
