// Give, 25.12.30
// 🎁 Creates Stripe checkout sessions for giving/supporting AC
// Endpoint: /api/give

import Stripe from "stripe";
import { respond } from "../../backend/http.mjs";
import { authorize } from "../../backend/authorization.mjs";
import { hasMonthlySupport } from "../../backend/monthly-support.mjs";

const dev = process.env.CONTEXT === "dev";

// Supported currencies and their configs
const currencies = {
  usd: { symbol: "$", min: 100, max: 250000 },
  dkk: { symbol: "kr", min: 500, max: 1750000 },
};

export function createGiveHandler({ authenticate = authorize, support = hasMonthlySupport } = {}) {
return async function handler(event, context) {
  // Handle CORS preflight
  if (event.httpMethod === "OPTIONS") {
    return respond(200, {});
  }

  // Only allow POST
  if (event.httpMethod !== "POST") {
    return respond(405, { error: "Method not allowed" });
  }

  // Use the correct Netlify env var names
  const stripeKey = dev
    ? process.env.STRIPE_API_TEST_PRIV_KEY
    : process.env.STRIPE_API_PRIV_KEY;

  if (!stripeKey) {
    return respond(500, { error: "Stripe not configured" });
  }

  const stripe = new Stripe(stripeKey);

  try {
    const body = JSON.parse(event.body || "{}");
    const currency = (body.currency || "usd").toLowerCase();
    const currencyConfig = currencies[currency] || currencies.usd;
    const amountCents = parseInt(body.amount) || 2500;
    const recurring = body.recurring === true;
    let email = body.email; // Optional on ordinary giving; verified for perks.
    const attribution = ["homepage", "give-piece", "signup", "handle-colors"].includes(body.source) ? { source: body.source } : {};
    const colors = ["signup", "handle-colors"].includes(attribution.source);

    // Validate amount for currency
    if (amountCents < currencyConfig.min || amountCents > currencyConfig.max) {
      return respond(400, { error: `Invalid amount for ${currency.toUpperCase()}` });
    }

    if (colors) {
      const user = await authenticate(event.headers || {}, "aesthetic");
      if (!user?.email_verified || !user.email) return respond(401, { error: "Sign in to connect support to your handle" });
      email = user.email.trim().toLowerCase();
      if (recurring && await support(user, { stripe }))
        return respond(409, { code: "already_supporting", error: "Monthly support is already active" });
    }

    const amountDisplay = (amountCents / 100).toFixed(currency === 'dkk' ? 0 : 2);
    const displayStr = currency === 'dkk' ? `${amountDisplay} kr` : `$${amountDisplay}`;

    // Build session config
    const giveBaseUrl = dev ? `https://${event.headers.host}` : "https://give.aesthetic.computer";
    const pieceReturn = new URL("https://aesthetic.computer/give");
    pieceReturn.search = new URLSearchParams({ amount: (amountCents / 100).toFixed(2), currency,
      frequency: recurring ? "monthly" : "once", ...(attribution.source ? { source: attribution.source } : {}) }).toString();
    const colorReturn = "https://aesthetic.computer/handle";
    const cancelUrl = colors ? colorReturn : body.surface === "piece" ? pieceReturn.href
      : `${giveBaseUrl}/${attribution.source ? `?source=${attribution.source}` : ""}`;
    const sessionConfig = {
      success_url: colors ? `${colorReturn}?support=return` : body.surface === "piece" ? `${pieceReturn.href}&thanks=1`
        : `${giveBaseUrl}/thanks.html?amount=${amountDisplay}&currency=${currency}${recurring ? '&recurring=true' : ''}`,
      cancel_url: cancelUrl,
      billing_address_collection: "auto",
      // Prefill email if user is logged in (makes checkout easier & ties to account)
      ...(email && { customer_email: email }),
      metadata: {
        type: recurring ? "subscription" : "gift",
        ...attribution,
        amount: amountDisplay,
        currency: currency,
      },
      // Custom fields for optional note (works with both payment and subscription modes)
      custom_fields: [
        {
          key: "note",
          label: { type: "custom", custom: "Add a note (optional)" },
          type: "text",
          optional: true,
        },
      ],
    };

    if (recurring) {
      // Monthly subscription
      sessionConfig.mode = "subscription";
      sessionConfig.subscription_data = { metadata: { ...attribution, ac_support: "monthly" } };
      sessionConfig.line_items = [
        {
          price_data: {
            currency: currency,
            product_data: {
              name: `Give`,
              description: `to Aesthetic Computer`,
              metadata: { ac_support: "monthly" },
              images: [`https://aesthetic.computer/api/give-image?amount=${amountDisplay}&currency=${currency}&recurring=true`],
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
      if (attribution.source) sessionConfig.payment_intent_data = { metadata: attribution };
      // Note: Stripe submit_type only allows: auto, pay, book, donate - no "give" option
      // Using "pay" instead of "donate" for a more neutral label
      sessionConfig.line_items = [
        {
          price_data: {
            currency: currency,
            product_data: {
              name: `Give`,
              description: `to Aesthetic Computer`,
              images: [`https://aesthetic.computer/api/give-image?amount=${amountDisplay}&currency=${currency}`],
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
};
}

export const handler = createGiveHandler();

function getBaseUrl(event) {
  if (dev) {
    return `https://${event.headers.host}`;
  }
  return "https://aesthetic.computer";
}
