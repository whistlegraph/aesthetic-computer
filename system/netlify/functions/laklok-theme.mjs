// laklok-theme, 26.09.13
// Which `tema` each laklok (laer-klokken) visitor is looking at. The choice
// itself stays local to the device; this is the census. Both sisters report
// on boot and on every chip tap, so `at` reads as "last seen in the room".
//
// POST /api/laklok-theme { theme }       → upsert { sub, handle, theme, at, since }
//   headers: Authorization: Bearer <Auth0 token>
// GET  /api/laklok-theme?days=30         → { days, active, themes: {name: n}, users }
//   users seen within `days` (default 30), newest first — anyone can read it.

import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

const THEME = /^[a-z]{1,16}$/; // roster lives in the piece; the server only bounds it

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(200, {});
  if (event.httpMethod !== "GET" && event.httpMethod !== "POST") {
    return respond(405, { message: "Method Not Allowed" });
  }

  const database = await connect();
  try {
    const themes = database.db.collection("laklok-themes");

    if (event.httpMethod === "GET") {
      const days = Math.min(365, Math.max(1, parseInt(event.queryStringParameters?.days) || 30));
      const seenAfter = new Date(Date.now() - days * 86400000);
      const users = await themes
        .find({ at: { $gte: seenAfter } }, { projection: { _id: 0, handle: 1, theme: 1, at: 1, since: 1 } })
        .sort({ at: -1 })
        .toArray();
      const counts = {};
      for (const u of users) counts[u.theme] = (counts[u.theme] || 0) + 1;
      return respond(200, { days, active: users.length, themes: counts, users });
    }

    const user = await authorize(event.headers);
    if (!user?.sub) return respond(401, { message: "unauthorized" });

    let body;
    try {
      body = JSON.parse(event.body || "{}");
    } catch {
      return respond(400, { message: "Invalid JSON body" });
    }
    if (!THEME.test(body.theme || "")) return respond(400, { message: "Invalid theme" });

    const handle = (await database.db.collection("@handles").findOne({ _id: user.sub }))?.handle || null;
    const now = new Date();
    const prev = await themes.findOne({ _id: user.sub }, { projection: { theme: 1, since: 1 } });
    // `since` marks when the current theme was picked; a heartbeat with the
    // same theme keeps it, a switch resets it.
    const since = prev?.theme === body.theme && prev.since ? prev.since : now;
    await themes.updateOne(
      { _id: user.sub },
      { $set: { handle, theme: body.theme, at: now, since } },
      { upsert: true },
    );
    return respond(200, { theme: body.theme, handle, at: now });
  } catch (err) {
    console.error("🔴 laklok-theme error:", err);
    return respond(500, { message: err?.message || "Server error" });
  } finally {
    await database.disconnect();
  }
}
