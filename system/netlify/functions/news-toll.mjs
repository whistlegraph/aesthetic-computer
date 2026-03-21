// News Troll Toll, 2026.01.20
// 🧌 Stripe checkout + webhook for posting external links on Aesthetic News
// Uses existing STRIPE_ENDPOINT_DEV_SECRET pattern from mug.js
// Endpoint: POST /api/news/toll (checkout OR webhook)

import Stripe from "stripe";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { authorize } from "../../backend/authorization.mjs";

const dev = process.env.CONTEXT === "dev";
const TOLL_AMOUNT_CENTS = 200; // $2.00

export async function handler(event, context) {
  // Handle CORS preflight
  if (event.httpMethod === "OPTIONS") {
    return respond(200, {});
  }

  if (event.httpMethod !== "POST") {
    return respond(405, { error: "Method not allowed" });
  }

  const stripeKey = dev
    ? process.env.STRIPE_API_TEST_PRIV_KEY
    : process.env.STRIPE_API_PRIV_KEY;

  if (!stripeKey) {
    return respond(500, { error: "Stripe not configured" });
  }

  const stripe = Stripe(stripeKey);

  // Check if this is a webhook request (has stripe-signature header)
  const sig = event.headers["stripe-signature"];
  if (sig) {
    // 🔔 WEBHOOK: Handle Stripe event
    const secret = dev
      ? process.env.STRIPE_ENDPOINT_DEV_SECRET
      : process.env.STRIPE_ENDPOINT_SECRET;
    
    let hookEvent;
    try {
      hookEvent = stripe.webhooks.constructEvent(event.body, sig, secret);
    } catch (err) {
      console.error("🧌 Webhook signature failed:", err.message);
      return respond(400, { error: `Webhook Error: ${err.message}` });
    }

    if (hookEvent.type === "checkout.session.completed") {
      const session = hookEvent.data.object;
      const metadata = session.metadata || {};

      // Only handle news toll payments
      if (metadata.type !== "news-toll") {
        return respond(200, { received: true, skipped: true });
      }

      const postCode = metadata.postCode;
      const userSub = metadata.userSub;

      if (!postCode) {
        console.error("🧌 No postCode in webhook metadata");
        return respond(200, { received: true, error: "No postCode" });
      }

      // Update post status to live
      const database = await connect();
      const posts = database.db.collection("news-posts");
      const votes = database.db.collection("news-votes");

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

      // Also create the initial upvote now that post is live
      if (result.modifiedCount > 0) {
        try {
          await votes.insertOne({
            itemType: "post",
            itemId: postCode,
            user: userSub,
            when: new Date(),
          });
        } catch (e) {
          // Ignore duplicate vote errors
        }
      }

      await database.disconnect();

      if (result.modifiedCount === 0) {
        console.error("🧌 Post not found or already live:", postCode);
        return respond(200, { received: true, warning: "Not found or already live" });
      }

      console.log(`🧌 Troll toll paid! Post ${postCode} is now live.`);
      return respond(200, { received: true, published: postCode });
    }

    return respond(200, { received: true });
  }

  // 💳 CHECKOUT: Create Stripe checkout session
  try {
    const user = await authorize(event.headers || {});
    if (!user) {
      return respond(401, { error: "Unauthorized" });
    }

    const body = JSON.parse(event.body || "{}");
    const { postCode } = body;

    if (!postCode) {
      return respond(400, { error: "Missing postCode" });
    }

    // Verify the post exists and is pending-toll
    const database = await connect();
    const posts = database.db.collection("news-posts");
    const post = await posts.findOne({ code: postCode, user: user.sub });

    if (!post) {
      await database.disconnect();
      return respond(404, { error: "Post not found" });
    }

    if (post.status !== "pending-toll") {
      await database.disconnect();
      return respond(400, { error: "Post is not pending payment" });
    }

    await database.disconnect();

    // Create Stripe checkout session
    const baseUrl = dev 
      ? `https://${event.headers.host}`
      : "https://news.aesthetic.computer";
    
    const session = await stripe.checkout.sessions.create({
      mode: "payment",
      success_url: `${baseUrl}/${postCode}?toll=paid`,
      cancel_url: `${baseUrl}/report?toll=cancelled&code=${postCode}`,
      line_items: [
        {
          price_data: {
            currency: "usd",
            product_data: {
              name: "🧌 Troll Toll",
              description: "Post an external link on Aesthetic News",
              images: ["https://aesthetic.computer/aesthetic.computer/logo/aesthetic-icon-512.png"],
            },
            unit_amount: TOLL_AMOUNT_CENTS,
          },
          quantity: 1,
        },
      ],
      metadata: {
        type: "news-toll",
        postCode: postCode,
        userSub: user.sub,
      },
    });

    return respond(200, { url: session.url, sessionId: session.id });
  } catch (error) {
    console.error("🧌 Toll checkout error:", error);
    return respond(500, { error: error.message });
  }
}
