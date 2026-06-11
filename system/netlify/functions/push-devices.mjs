// push-devices, 26.06.11
// Lists the logged-in user's registered push devices so they can be
// individually addressed (by deviceId or label) in `tell` and `/api/push`.
//
// GET /api/push-devices
//   headers: Authorization: Bearer <Auth0 token>
//   → { devices: [{ deviceId, label, platform, kind, topics, updatedAt }] }

import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

export async function handler(event) {
  if (event.httpMethod !== "GET") {
    return respond(405, { message: "Method Not Allowed" });
  }

  const user = await authorize(event.headers);
  if (!user?.sub) return respond(401, { message: "Unauthorized" });

  const database = await connect();
  try {
    const devices = await database.db
      .collection("push-tokens")
      .find({ user: user.sub, kind: { $in: ["webpush", "apns"] } })
      .project({
        _id: 0,
        deviceId: 1,
        label: 1,
        platform: 1,
        kind: 1,
        topics: 1,
        updatedAt: 1,
      })
      .sort({ updatedAt: -1 })
      .toArray();
    return respond(200, { devices });
  } catch (err) {
    console.error("🔴 push-devices error:", err);
    return respond(500, { message: err?.message || "Server error" });
  } finally {
    await database.disconnect();
  }
}
