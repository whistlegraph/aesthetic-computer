// Give Portal, 26.01.04
// 🚪 Creates Stripe Customer Portal sessions for managing subscriptions
// Endpoint: /api/give-portal

import Stripe from "stripe";
import { respond } from "../../backend/http.mjs";
import { authorize } from "../../backend/authorization.mjs";

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

  // Require authenticated user session; do not allow arbitrary email lookup.
  const authHeader = event.headers?.authorization || event.headers?.Authorization;
  const user = await authorize({ authorization: authHeader });
  if (!user) {
    return respond(401, { error: "Unauthorized" });
  }

  const authenticatedEmail = user.email?.trim().toLowerCase();
  if (!authenticatedEmail) {
    return respond(400, { error: "Could not determine account email from session" });
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
    const requestedEmail = body.email?.trim().toLowerCase();

    // Optional client-sent email must match authenticated identity.
    if (requestedEmail && requestedEmail !== authenticatedEmail) {
      return respond(403, { error: "Email does not match authenticated account" });
    }

    // Find customers by email
    const customers = await stripe.customers.list({
      email: authenticatedEmail,
      limit: 1,
    });

    if (!customers.data.length) {
      return respond(404, { error: "No subscription found for your account email." });
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
