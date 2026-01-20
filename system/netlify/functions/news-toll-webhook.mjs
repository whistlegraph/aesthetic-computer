// News Toll Webhook, 2026.01.20
// 🧌 Stripe webhook to publish posts after troll toll payment
// Endpoint: POST /api/news/toll-webhook

import Stripe from "stripe";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

const dev = process.env.CONTEXT === "dev";

export async function handler(event, context) {
  if (event.httpMethod !== "POST") {
    return respond(405, { error: "Method not allowed" });
  }

  const stripeKey = dev
    ? process.env.STRIPE_API_TEST_PRIV_KEY
    : process.env.STRIPE_API_PRIV_KEY;

  const webhookSecret = dev
    ? process.env.STRIPE_WEBHOOK_SECRET_TEST
    : process.env.STRIPE_WEBHOOK_SECRET;

  if (!stripeKey) {
    return respond(500, { error: "Stripe not configured" });
  }

  const stripe = Stripe(stripeKey);

  try {
    // Verify webhook signature
    const sig = event.headers["stripe-signature"];
    let stripeEvent;

    if (webhookSecret && sig) {
      try {
        stripeEvent = stripe.webhooks.constructEvent(
          event.body,
          sig,
          webhookSecret
        );
      } catch (err) {
        console.error("Webhook signature verification failed:", err.message);
        return respond(400, { error: "Webhook signature verification failed" });
      }
    } else {
      // In dev without webhook secret, parse directly (less secure)
      stripeEvent = JSON.parse(event.body);
    }

    // Handle checkout.session.completed event
    if (stripeEvent.type === "checkout.session.completed") {
      const session = stripeEvent.data.object;
      const metadata = session.metadata || {};

      // Only handle news toll payments
      if (metadata.type !== "news-toll") {
        return respond(200, { received: true, skipped: true });
      }

      const postCode = metadata.postCode;
      const userSub = metadata.userSub;

      if (!postCode) {
        console.error("No postCode in toll webhook metadata");
        return respond(200, { received: true, error: "No postCode" });
      }

      // Update post status to live
      const database = await connect();
      const posts = database.db.collection("news-posts");

      const result = await posts.updateOne(
        { code: postCode, user: userSub, status: "pending-toll" },
        { 
          $set: { 
            status: "live", 
            tollPaid: true,
            tollPaidAt: new Date(),
            stripeSessionId: session.id,
          } 
        }
      );

      await database.disconnect();

      if (result.modifiedCount === 0) {
        console.error("Post not found or already published:", postCode);
        return respond(200, { received: true, warning: "Post not found or already live" });
      }

      console.log(`🧌 Troll toll paid! Post ${postCode} is now live.`);
      return respond(200, { received: true, published: postCode });
    }

    return respond(200, { received: true });
  } catch (error) {
    console.error("Toll webhook error:", error);
    return respond(500, { error: error.message });
  }
}
