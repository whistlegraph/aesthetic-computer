// Give, 25.12.30
// 🎁 Creates Stripe checkout sessions for giving/supporting AC
// Endpoint: /api/give

import Stripe from "stripe";
import { respond } from "../../backend/http.mjs";

const dev = process.env.CONTEXT === "dev";

// Supported currencies and their configs
const currencies = {
  usd: { symbol: "$", min: 100, max: 100000 },
  dkk: { symbol: "kr", min: 500, max: 700000 },
};

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
    ? process.env.STRIPE_API_KEY
    : process.env.STRIPE_API_KEY_LIVE;

  if (!stripeKey) {
    return respond(500, { error: "Stripe not configured" });
  }

  const stripe = Stripe(stripeKey);

  try {
    const body = JSON.parse(event.body || "{}");
    const currency = (body.currency || "usd").toLowerCase();
    const currencyConfig = currencies[currency] || currencies.usd;
    const amountCents = parseInt(body.amount) || 2500;
    const recurring = body.recurring === true;

    // Validate amount for currency
    if (amountCents < currencyConfig.min || amountCents > currencyConfig.max) {
      return respond(400, { error: `Invalid amount for ${currency.toUpperCase()}` });
    }

    const amountDisplay = (amountCents / 100).toFixed(currency === 'dkk' ? 0 : 2);
    const displayStr = currency === 'dkk' ? `${amountDisplay} kr` : `$${amountDisplay}`;

    // Build session config
    const sessionConfig = {
      success_url: `${getBaseUrl(event)}/give.aesthetic.computer/thanks.html?amount=${amountDisplay}&currency=${currency}${recurring ? '&recurring=true' : ''}`,
      cancel_url: `${getBaseUrl(event)}/give.aesthetic.computer/`,
      billing_address_collection: "auto",
      metadata: {
        type: recurring ? "subscription" : "gift",
        amount: amountDisplay,
        currency: currency,
      },
    };

    if (recurring) {
      // Monthly subscription
      sessionConfig.mode = "subscription";
      sessionConfig.line_items = [
        {
          price_data: {
            currency: currency,
            product_data: {
              name: `🎁 Monthly`,
              description: currency === 'dkk' 
                ? `${displayStr}/måned til Aesthetic Computer ✨`
                : `${displayStr}/month to Aesthetic Computer ✨`,
              images: ["https://aesthetic.computer/aesthetic.computer/icon/512x512.png"],
            },
            unit_amount: amountCents,
            recurring: {
              interval: "month",
            },
          },
          quantity: 1,
        },
      ];
    } else {
      // One-time payment
      sessionConfig.mode = "payment";
      sessionConfig.submit_type = "donate";
      sessionConfig.line_items = [
        {
          price_data: {
            currency: currency,
            product_data: {
              name: `🎁 Give`,
              description: currency === 'dkk' 
                ? `${displayStr} til Aesthetic Computer — tak! 💖`
                : `${displayStr} to Aesthetic Computer — thank you! 💖`,
              images: ["https://aesthetic.computer/aesthetic.computer/icon/512x512.png"],
            },
            unit_amount: amountCents,
          },
          quantity: 1,
        },
      ];
    }

    const session = await stripe.checkout.sessions.create(sessionConfig);

    return respond(200, { url: session.url, sessionId: session.id });
  } catch (error) {
    console.error("Give checkout error:", error);
    return respond(500, { error: error.message });
  }
}

function getBaseUrl(event) {
  if (dev) {
    return `https://${event.headers.host}`;
  }
  return "https://aesthetic.computer";
}
