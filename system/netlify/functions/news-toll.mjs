// News Troll Toll, 2026.01.20
// 🧌 Stripe checkout for posting external links on Aesthetic News
// Endpoint: POST /api/news/toll

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

  try {
    // Authorize user
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
      success_url: `${baseUrl}/item/${postCode}?toll=paid`,
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
    console.error("Troll toll checkout error:", error);
    return respond(500, { error: error.message });
  }
}
