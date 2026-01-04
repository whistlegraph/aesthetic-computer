// Give Portal, 26.01.04
// 🚪 Creates Stripe Customer Portal sessions for managing subscriptions
// Endpoint: /api/give-portal

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
    const email = body.email;

    if (!email) {
      return respond(400, { error: "Email required to find your subscriptions" });
    }

    // Find customers by email
    const customers = await stripe.customers.list({
      email: email,
      limit: 1,
    });

    if (!customers.data.length) {
      return respond(404, { error: "No subscription found for this email. Try the email you used when giving." });
    }

    const customer = customers.data[0];

    // Create portal session
    const giveBaseUrl = dev ? `https://${event.headers.host}` : "https://give.aesthetic.computer";
    const session = await stripe.billingPortal.sessions.create({
      customer: customer.id,
      return_url: giveBaseUrl,
    });

    return respond(200, { url: session.url });
  } catch (error) {
    console.error("Give portal error:", error);
    return respond(500, { error: "Could not create portal session" });
  }
}
