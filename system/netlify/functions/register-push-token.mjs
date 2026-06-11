// register-push-token, 26.04.23 → 26.06.11 (FCM → standard push migration)
// Registers a device for push delivery. Two kinds:
//   - "webpush": a standard PushSubscription from any browser / installed PWA
//   - "apns":    a raw APNs device token from the native iOS app
//
// POST /api/register-push-token
//   body: {
//     kind: "webpush" | "apns",
//     subscription?: { endpoint, keys: { p256dh, auth } },   // webpush
//     token?: string,                                        // apns (hex)
//     deviceId: string,        // stable per-install UUID (client-generated)
//     label?: string,          // human name, e.g. "iPhone" / "Chrome on macOS"
//     platform: "web" | "ios",
//     topics?: string[],       // e.g. ["scream", "mood"]
//     remove?: true,           // unregister this device
//   }
//   headers: Authorization: Bearer <Auth0 token> — OPTIONAL. With auth the
//   device is bound to the user (enables tells + per-device addressing);
//   without, it's an anonymous topic-only registration.

import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

const KNOWN_TOPICS = [
  "scream",
  "mood",
  "chat-system",
  "chat-sotce",
  "chat-clock",
];
const MAX_LABEL = 64;

export async function handler(event) {
  if (event.httpMethod !== "POST") {
    return respond(405, { message: "Method Not Allowed" });
  }

  let body;
  try {
    body = JSON.parse(event.body || "{}");
  } catch {
    return respond(400, { message: "Invalid JSON body" });
  }

  const { kind, subscription, deviceId, label, platform, remove } = body;

  // The unique key for a registration: APNs device token, or the Web Push
  // subscription endpoint (itself an unguessable capability URL).
  let token;
  if (kind === "apns") {
    token = body.token;
    if (
      typeof token !== "string" ||
      !/^[0-9a-fA-F]{32,512}$/.test(token)
    ) {
      return respond(400, { message: "Invalid APNs token" });
    }
  } else if (kind === "webpush") {
    if (
      typeof subscription?.endpoint !== "string" ||
      !subscription.endpoint.startsWith("https://") ||
      typeof subscription?.keys?.p256dh !== "string" ||
      typeof subscription?.keys?.auth !== "string"
    ) {
      return respond(400, { message: "Invalid subscription" });
    }
    token = subscription.endpoint;
  } else if (remove && typeof body.token === "string") {
    token = body.token; // legacy-shape removal (old clients on logout)
  } else {
    return respond(400, { message: "Invalid kind" });
  }

  if (!remove) {
    if (!["web", "ios"].includes(platform)) {
      return respond(400, { message: "Invalid platform" });
    }
    if (typeof deviceId !== "string" || deviceId.length < 8 || deviceId.length > 128) {
      return respond(400, { message: "Invalid deviceId" });
    }
  }

  const topics = Array.isArray(body.topics)
    ? body.topics.filter((t) => KNOWN_TOPICS.includes(t))
    : ["scream", "mood"];

  const user = await authorize(event.headers); // null is fine (anonymous)

  const database = await connect();
  try {
    const collection = database.db.collection("push-tokens");
    await collection.createIndex({ token: 1 }, { unique: true });
    await collection.createIndex({ user: 1 });
    await collection.createIndex({ topics: 1 });

    if (remove) {
      // The token/endpoint is a capability — owning it is proof enough.
      const result = await collection.deleteOne({ token });
      return respond(200, { status: "removed", deleted: result.deletedCount });
    }

    const doc = {
      kind,
      token,
      subscription: kind === "webpush" ? subscription : null,
      deviceId,
      label: typeof label === "string" ? label.slice(0, MAX_LABEL) : "",
      platform,
      topics,
      updatedAt: new Date(),
    };

    if (user?.sub) {
      // Bind to user; claim the token from any previous account on this
      // device, and drop stale rows for this device (rotated endpoints).
      doc.user = user.sub;
      await collection.deleteMany({ token, user: { $ne: user.sub } });
      await collection.deleteMany({
        user: user.sub,
        deviceId,
        token: { $ne: token },
      });
      await collection.updateOne({ token }, { $set: doc }, { upsert: true });
    } else {
      // Anonymous: never unbind an existing user from this token — only set
      // user on first insert, refresh everything else.
      await collection.deleteMany({
        user: null,
        deviceId,
        token: { $ne: token },
      });
      await collection.updateOne(
        { token },
        { $set: doc, $setOnInsert: { user: null } },
        { upsert: true },
      );
    }

    return respond(200, { status: "registered", topics });
  } catch (err) {
    console.error("🔴 register-push-token error:", err);
    return respond(500, { message: err?.message || "Server error" });
  } finally {
    await database.disconnect();
  }
}
