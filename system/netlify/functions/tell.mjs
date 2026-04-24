// tell, 26.04.23
// Send a one-way "tell" from one AC user to another. The recipient gets a
// push notification on every registered device and the message is stored
// in the `tells` collection as their inbox.
//
// POST /api/tell
//   body: { to: "@handle", text: "message" }
//   headers: Authorization: Bearer <Auth0 token>

import {
  authorize,
  userIDFromHandleOrEmail,
  getHandleOrEmail,
} from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { filter } from "../../backend/filter.mjs";
import { shell } from "../../backend/shell.mjs";

import { initializeApp, cert, getApps } from "firebase-admin/app";
import { getMessaging } from "firebase-admin/messaging";

const MAX_TEXT_LENGTH = 500;

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

  const rawTo = typeof body.to === "string" ? body.to.trim() : "";
  const rawText = typeof body.text === "string" ? body.text : "";
  if (!rawTo) return respond(400, { message: "Missing recipient" });
  const text = filter(rawText.trim()).slice(0, MAX_TEXT_LENGTH);
  if (!text) return respond(400, { message: "Empty message" });

  const sender = await authorize(event.headers);
  if (!sender?.sub) return respond(401, { message: "Unauthorized" });

  const database = await connect();
  try {
    const recipientSub = await userIDFromHandleOrEmail(rawTo, database);
    if (!recipientSub) {
      return respond(404, { message: "Recipient not found" });
    }

    const fromHandle = await getHandleOrEmail(sender.sub);
    const toHandle = rawTo.startsWith("@") ? rawTo : `@${rawTo}`;

    const tells = database.db.collection("tells");
    await tells.createIndex({ to: 1, when: -1 });
    await tells.createIndex({ from: 1, when: -1 });

    const when = new Date();
    const insertResult = await tells.insertOne({
      to: recipientSub,
      toHandle,
      from: sender.sub,
      fromHandle,
      text,
      when,
    });

    // Look up recipient's registered push tokens.
    const pushTokens = await database.db
      .collection("push-tokens")
      .find({ user: recipientSub })
      .project({ _id: 0, token: 1 })
      .toArray();
    const tokens = pushTokens.map((d) => d.token).filter(Boolean);

    let pushSummary = { attempted: 0, succeeded: 0, failed: 0 };

    if (tokens.length > 0) {
      const { got } = await import("got");
      const serviceAccount = (
        await got(process.env.GCM_FIREBASE_CONFIG_URL, { responseType: "json" })
      ).body;
      if (getApps().length === 0) {
        initializeApp({ credential: cert(serviceAccount) });
      }

      const message = {
        notification: {
          title: `${fromHandle} told you`,
          body: text,
        },
        data: {
          kind: "tell",
          from: fromHandle || "",
          tellId: insertResult.insertedId.toString(),
        },
        apns: {
          payload: { aps: { sound: "default", "mutable-content": 1 } },
        },
      };

      try {
        const response = await getMessaging().sendEachForMulticast({
          tokens,
          ...message,
        });
        pushSummary = {
          attempted: tokens.length,
          succeeded: response.successCount,
          failed: response.failureCount,
        };

        const invalid = [];
        response.responses.forEach((resp, i) => {
          if (resp.success) return;
          const code = resp.error?.code || "";
          if (
            code === "messaging/registration-token-not-registered" ||
            code === "messaging/invalid-registration-token" ||
            code === "messaging/invalid-argument"
          ) {
            invalid.push(tokens[i]);
          } else {
            shell.log(
              `⚠️ tell push error for ${recipientSub}:`,
              code,
              resp.error?.message,
            );
          }
        });
        if (invalid.length) {
          await database.db
            .collection("push-tokens")
            .deleteMany({ token: { $in: invalid } });
          shell.log(`🧹 Pruned ${invalid.length} stale push tokens.`);
        }
      } catch (err) {
        shell.log("🔴 tell push send failed:", err?.message || err);
      }
    }

    return respond(200, {
      status: "told",
      to: toHandle,
      when,
      push: pushSummary,
    });
  } catch (err) {
    console.error("🔴 tell error:", err);
    return respond(500, { message: err?.message || "Server error" });
  } finally {
    await database.disconnect();
  }
}
