import Stripe from "stripe";
import { respond } from "./http.mjs";

// Match only the verified caller's billing email. Nothing supplied in a request
// body can grant an entitlement. Legacy Give products predate our metadata.
export async function hasMonthlySupport(user, { stripe } = {}) {
  if (!user?.email_verified || typeof user.email !== "string") return false;
  const email = user.email.trim().toLowerCase();
  if (!email) return false;
  if (!stripe) {
    const key = process.env.CONTEXT === "dev" ? process.env.STRIPE_API_TEST_PRIV_KEY : process.env.STRIPE_API_PRIV_KEY;
    if (!key) throw new Error("Support lookup unavailable");
    stripe = new Stripe(key, { timeout: 5000, maxNetworkRetries: 0 });
  }
  const products = new Map();
  for await (const customer of stripe.customers.list({ email, limit: 100 })) {
    for await (const subscription of stripe.subscriptions.list({ customer: customer.id, status: "active", limit: 100 })) {
      if (subscription.status !== "active") continue;
      for (const item of subscription.items.data) {
        const price = item.price;
        if (price?.recurring?.interval !== "month" || price.recurring.interval_count !== 1) continue;
        // No tier or minimum gift is required for colors.
        const id = typeof price.product === "string" ? price.product : price.product?.id;
        if (!id) continue;
        if (!products.has(id)) products.set(id, typeof price.product === "object"
          ? price.product : await stripe.products.retrieve(id));
        const product = products.get(id);
        if (!product.deleted && (product.metadata?.ac_support === "monthly" || product.name === "Give")) return true;
      }
    }
  }
  return false;
}

export function createGiveStatusHandler({ authorize, support = hasMonthlySupport }) {
  return async event => {
    const reply = (code, data) => respond(code, data, { "Cache-Control": "no-store" });
    if (event.httpMethod === "OPTIONS") return reply(204, "");
    if (event.httpMethod !== "GET") return reply(405, { message: "Method not allowed" });
    try {
      const user = await authorize(event.headers || {}, "aesthetic");
      if (!user?.sub || !user.email_verified) return reply(401, { message: "Verified account required" });
      return reply(200, { monthlySupporter: await support(user) });
    } catch { return reply(503, { message: "Support check unavailable" }); }
  };
}
