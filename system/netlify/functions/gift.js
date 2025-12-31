// Gift, 25.12.30
// 🎁 Creates Stripe checkout sessions for gifting/supporting AC
// Endpoint: /api/gift

import Stripe from "stripe";
import { respond } from "../../backend/http.mjs";

const dev = process.env.CONTEXT === "dev";

export async function handler(event, context) {
  // Handle CORS preflight
  if (event.httpMethod === "OPTIONS") {
    return respond(200, {});
  }

  // Only allow POST
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
    const body = JSON.parse(event.body || "{}");
    const amountCents = parseInt(body.amount) || 2500; // Default $25

    // Minimum $1, maximum $10,000
    if (amountCents < 100 || amountCents > 1000000) {
      return respond(400, { error: "Invalid amount (min $1, max $10,000)" });
    }

    const amountDollars = (amountCents / 100).toFixed(2);

    // Create checkout session
    const session = await stripe.checkout.sessions.create({
      payment_method_types: ["card"],
      line_items: [
        {
          price_data: {
            currency: "usd",
            product_data: {
              name: `Gift to Aesthetic Computer`,
              description: `Thank you for supporting creative coding! ($${amountDollars})`,
              images: ["https://aesthetic.computer/aesthetic.computer/icon/512x512.png"],
            },
            unit_amount: amountCents,
          },
          quantity: 1,
        },
      ],
      mode: "payment",
      success_url: `${getBaseUrl(event)}/gift.aesthetic.computer/thanks.html?amount=${amountDollars}`,
      cancel_url: `${getBaseUrl(event)}/gift.aesthetic.computer/`,
      submit_type: "donate",
      billing_address_collection: "auto",
      metadata: {
        type: "gift",
        amount: amountDollars,
      },
    });

    return respond(200, { url: session.url, sessionId: session.id });
  } catch (error) {
    console.error("Gift checkout error:", error);
    return respond(500, { error: error.message });
  }
}

function getBaseUrl(event) {
  if (dev) {
    return `https://${event.headers.host}`;
  }
  return "https://aesthetic.computer";
}
