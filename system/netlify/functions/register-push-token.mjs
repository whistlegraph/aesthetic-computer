// register-push-token, 26.04.23
// Stores a device's push-notification token against the logged-in user so the
// backend can deliver targeted notifications (fast, per-device) instead of
// topic broadcasts. Used by the iOS app (FCM) and eventually web/android.

// POST /api/register-push-token
//   body: { token: string, platform: "ios" | "android" | "web", remove?: true }
//   headers: Authorization: Bearer <Auth0 token>
//   behavior:
//     - remove=true  → delete this (user, token) pair (logout)
//     - otherwise    → upsert { user: sub, token, platform, updatedAt }
//                      and delete any other user's binding to the same token
//                      (account-switch on the same device).

import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

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

  const { token, platform, remove } = body;
  if (typeof token !== "string" || token.length < 16 || token.length > 4096) {
    return respond(400, { message: "Invalid token" });
  }
  if (!remove && !["ios", "android", "web"].includes(platform)) {
    return respond(400, { message: "Invalid platform" });
  }

  const user = await authorize(event.headers);
  if (!user?.sub) return respond(401, { message: "Unauthorized" });

  const database = await connect();
  try {
    const collection = database.db.collection("push-tokens");
    await collection.createIndex({ user: 1, token: 1 }, { unique: true });
    await collection.createIndex({ token: 1 });

    if (remove) {
      const result = await collection.deleteOne({ user: user.sub, token });
      return respond(200, { status: "removed", deleted: result.deletedCount });
    }

    await collection.deleteMany({ token, user: { $ne: user.sub } });
    await collection.updateOne(
      { user: user.sub, token },
      { $set: { user: user.sub, token, platform, updatedAt: new Date() } },
      { upsert: true },
    );
    return respond(200, { status: "registered" });
  } catch (err) {
    console.error("🔴 register-push-token error:", err);
    return respond(500, { message: err?.message || "Server error" });
  } finally {
    await database.disconnect();
  }
}
