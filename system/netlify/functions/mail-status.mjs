// mail-status.mjs, 2026.02.12
// API for email preferences and blast history.
// GET /api/mail-status          → public blast history + stats
// GET /api/mail-status?email=X  → subscription status (requires auth or token)
// POST /api/mail-status         → update preferences (requires auth)

import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { authorize } from "../../backend/authorization.mjs";

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(200, {});

  let database;

  try {
    database = await connect();
    const blasts = database.db.collection("email-blast-history");
    const unsubs = database.db.collection("email-blast-unsubscribes");

    // --- GET: blast history + optional subscription status ---
    if (event.httpMethod === "GET") {
      // Public: blast history summary
      const history = await blasts
        .find({})
        .sort({ when: -1 })
        .limit(50)
        .toArray();

      const totalSent = history.reduce((sum, b) => sum + (b.sent || 0), 0);
      const totalUnsubscribed = await unsubs.countDocuments();

      const result = {
        blasts: history.map((b) => ({
          id: b._id,
          when: b.when,
          subject: b.subject,
          status: b.status,
          sent: b.sent || 0,
          failed: b.failed || 0,
          totalAttempted: b.totalAttempted || 0,
          completedAt: b.completedAt,
        })),
        count: history.length,
        totalSent,
        totalUnsubscribed,
        lastBlast: history[0]?.when || null,
      };

      // If authenticated, also include their personal subscription status
      const user = await authorize(event.headers);
      if (user?.email) {
        const unsubDoc = await unsubs.findOne({
          email: user.email.toLowerCase(),
        });
        result.subscribed = !unsubDoc;
        result.email = user.email;
      }

      return respond(200, result);
    }

    // --- POST: update email preferences (authenticated) ---
    if (event.httpMethod === "POST") {
      const user = await authorize(event.headers);
      if (!user) return respond(401, { error: "unauthorized" });
      if (!user.email) return respond(400, { error: "no email on account" });

      const body = JSON.parse(event.body || "{}");
      const { action } = body;
      const email = user.email.toLowerCase().trim();

      await unsubs.createIndex({ email: 1 }, { unique: true });

      if (action === "unsubscribe") {
        try {
          await unsubs.insertOne({ email, unsubscribedAt: new Date() });
        } catch (err) {
          if (err.code !== 11000) throw err;
        }
        return respond(200, { subscribed: false });
      }

      if (action === "subscribe") {
        await unsubs.deleteOne({ email });
        return respond(200, { subscribed: true });
      }

      return respond(400, { error: "invalid action" });
    }

    return respond(405, { error: "method not allowed" });
  } finally {
    if (database) await database.disconnect();
  }
}
