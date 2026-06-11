// subscribe-to-topic, 24.02.26.19.55 → 26.06.11 (FCM → standard push migration)
// Adds or removes a notification topic ("mood", "scream", "chat-system", ...)
// on an existing push registration. Topic fan-out now lives in our own Mongo
// `push-tokens` collection (see shared/push.mjs) — no Firebase.
//
// POST /api/subscribe-to-topic
//   body: { token: string, topic: string, unsubscribe?: true }
//   `token` is the registration key: the Web Push subscription endpoint, or
//   the APNs device token. Owning it is the capability — no auth needed.

import { respond } from "../../backend/http.mjs";
import { connect } from "../../backend/database.mjs";

const KNOWN_TOPICS = [
  "scream",
  "mood",
  "chat-system",
  "chat-sotce",
  "chat-clock",
];

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

  const { token, topic, unsubscribe } = body;
  if (typeof token !== "string" || !token) {
    return respond(400, { message: "Invalid token" });
  }
  if (!KNOWN_TOPICS.includes(topic)) {
    return respond(400, { message: "Unknown topic" });
  }

  const database = await connect();
  try {
    const collection = database.db.collection("push-tokens");
    const update = unsubscribe
      ? { $pull: { topics: topic } }
      : { $addToSet: { topics: topic } };
    const result = await collection.updateOne({ token }, update);
    if (result.matchedCount === 0) {
      // Includes legacy FCM clients posting old tokens — nothing to do.
      return respond(404, { message: "Registration not found" });
    }
    return respond(200, {
      status: unsubscribe ? "unsubscribed" : "subscribed",
      topic,
    });
  } catch (err) {
    console.error("🔴 subscribe-to-topic error:", err);
    return respond(500, { message: err?.message || "Server error" });
  } finally {
    await database.disconnect();
  }
}
