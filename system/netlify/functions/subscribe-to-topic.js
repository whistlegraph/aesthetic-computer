// subscribe-to-topic, 24.02.26.19.55
// Subscribes a user's web client to certain notification topics.
// Such as "mood", and "scream".

import { respond } from "../../backend/http.mjs";
// const dev = process.env.CONTEXT === "dev";
import { initializeApp, cert } from "firebase-admin/app"; // Firebase notifications.
import { getMessaging } from "firebase-admin/messaging";
let app;

export async function handler(event, context) {
  // console.log("😀 Subscribing...");

  if (event.httpMethod !== "POST") {
    return respond(405, { message: "Method Not Allowed" });
  }

  const body = JSON.parse(event.body);

  try {
    const { got } = await import("got");

    const serviceAccount = (
      await got(process.env.GCM_FIREBASE_CONFIG_URL, { responseType: "json" })
    ).body;

    app ||= initializeApp({ credential: cert(serviceAccount) });

    // 📯 Subscribe to a topic.
    try {
      const messaging = getMessaging();
      const response = await messaging.subscribeToTopic(
        [body.token],
        body.topic,
      );
      console.log(`🟢 Successfully subscribed to topic: ${body.topic}`, response);
      return respond(200, { status: "subscribed", topic: body.topic });
    } catch (err) {
      console.log("🔴 Error subscribing to topic:", err);
      throw new Error(err);
    }

    // 🔔 Send a notification.
    // getMessaging()
    //   .send({
    //     notification: { title: `${handle}'s mood is`, body: `${mood}` },
    //     topic: "mood",
    //   })
    //   .then((response) => {
    //     console.log("☎️  Successfully sent notification:", response);
    //   })
    //   .catch((error) => {
    //     console.log("📵  Error sending notification:", error);
    //   });
  } catch (error) {
    console.log("🔴 Error:", error);
    return respond(500, { status: error });
  }
}
