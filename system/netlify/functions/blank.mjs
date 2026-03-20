// Blank, 26.03.20
// AC Blank — AC Native Laptop checkout
// Endpoint: /api/blank

// GET:  Returns product info (pricing, stock)
// POST: Creates Stripe checkout session (?new=true) or handles webhook

// Testing:
// stripe listen --forward-to "https://localhost:8888/api/blank"

import Stripe from "stripe";
import { respond } from "../../backend/http.mjs";
import { email } from "../../backend/email.mjs";
import { authorize } from "../../backend/authorization.mjs";

const dev = process.env.CONTEXT === "dev";
const stripeKey = dev
  ? process.env.STRIPE_API_TEST_PRIV_KEY
  : process.env.STRIPE_API_PRIV_KEY;

// Pricing (cents)
const pricing = {
  usd: { min: 9600, suggested: 12800, max: 51200 },
  dkk: { min: 67200, suggested: 89600, max: 358400 },
};

// Shipping options
const shippingOptions = [
  {
    shipping_rate_data: {
      type: "fixed_amount",
      fixed_amount: { amount: 0, currency: "usd" },
      display_name: "Hand delivery",
      delivery_estimate: {
        minimum: { unit: "business_day", value: 1 },
        maximum: { unit: "business_day", value: 14 },
      },
    },
  },
  {
    shipping_rate_data: {
      type: "fixed_amount",
      fixed_amount: { amount: 1200, currency: "usd" },
      display_name: "USPS Priority Mail",
      delivery_estimate: {
        minimum: { unit: "business_day", value: 2 },
        maximum: { unit: "business_day", value: 5 },
      },
    },
  },
  {
    shipping_rate_data: {
      type: "fixed_amount",
      fixed_amount: { amount: 2500, currency: "usd" },
      display_name: "International (DK / EU)",
      delivery_estimate: {
        minimum: { unit: "business_day", value: 7 },
        maximum: { unit: "business_day", value: 21 },
      },
    },
  },
];

// DKK shipping options (same structure, different currency)
const shippingOptionsDKK = [
  {
    shipping_rate_data: {
      type: "fixed_amount",
      fixed_amount: { amount: 0, currency: "dkk" },
      display_name: "Hand delivery",
      delivery_estimate: {
        minimum: { unit: "business_day", value: 1 },
        maximum: { unit: "business_day", value: 14 },
      },
    },
  },
  {
    shipping_rate_data: {
      type: "fixed_amount",
      fixed_amount: { amount: 8400, currency: "dkk" },
      display_name: "PostNord / International",
      delivery_estimate: {
        minimum: { unit: "business_day", value: 5 },
        maximum: { unit: "business_day", value: 14 },
      },
    },
  },
];

function tierFromAmount(amount) {
  if (amount >= 51200) return "tutorial";
  if (amount > 9600) return "support";
  return "blank";
}

export async function handler(event) {
  // CORS preflight
  if (event.httpMethod === "OPTIONS") {
    return respond(200, {});
  }

  // GET: Product info
  if (event.httpMethod === "GET") {
    return respond(200, {
      product: "AC Blank",
      model: "Lenovo ThinkPad Yoga 11e (Gen 4/5)",
      description:
        "A surplus laptop running AC Native OS — a pared-down creative computing instrument with only stable, permanent commands. Like a blank tape waiting to be filled.",
      pricing,
      tiers: {
        blank: { label: "AC Blank", description: "Laptop + AC Native OS" },
        support: {
          label: "AC Blank + Support",
          description: "Supports AC development",
        },
        tutorial: {
          label: "AC Blank + Tutorial",
          description: "In-person meeting & tutorial in LA",
        },
      },
    });
  }

  if (event.httpMethod !== "POST") {
    return respond(405, { error: "Method not allowed" });
  }

  if (!stripeKey) {
    return respond(500, { error: "Stripe not configured" });
  }

  const stripe = new Stripe(stripeKey);

  // 1. Create checkout session
  if (event.queryStringParameters?.new === "true") {
    try {
      const body = JSON.parse(event.body || "{}");
      const currency = (body.currency || "usd").toLowerCase();
      const currencyConfig = pricing[currency];

      if (!currencyConfig) {
        return respond(400, { error: `Unsupported currency: ${currency}` });
      }

      const amount = parseInt(body.amount) || currencyConfig.suggested;

      if (amount < currencyConfig.min || amount > currencyConfig.max) {
        return respond(400, {
          error: `Amount must be between ${currencyConfig.min} and ${currencyConfig.max} for ${currency.toUpperCase()}`,
        });
      }

      const tier = tierFromAmount(amount);
      const domain = dev
        ? `https://${event.headers.host}`
        : "https://aesthetic.computer";

      const amountDisplay =
        currency === "dkk"
          ? `${(amount / 100).toFixed(0)} kr`
          : `$${(amount / 100).toFixed(2)}`;

      let productName = "AC Blank";
      if (tier === "tutorial") productName += " + Tutorial";

      const sessionConfig = {
        line_items: [
          {
            price_data: {
              currency,
              product_data: {
                name: productName,
                description:
                  tier === "tutorial"
                    ? "AC Native Laptop + in-person tutorial in Los Angeles"
                    : "Surplus laptop running AC Native OS",
              },
              unit_amount: amount,
            },
            quantity: 1,
          },
        ],
        metadata: {
          type: "blank",
          tier,
          model: "yoga-11e",
          amount: amountDisplay,
          currency,
        },
        mode: "payment",
        shipping_address_collection: { allowed_countries: ["US", "DK"] },
        shipping_options:
          currency === "dkk" ? shippingOptionsDKK : shippingOptions,
        success_url: `${domain}/blank~thanks`,
        cancel_url: `${domain}/blank`,
        automatic_tax: { enabled: true },
        custom_text: {
          submit: {
            message:
              tier === "tutorial"
                ? "We'll reach out to schedule your in-person session in LA."
                : "We'll flash your Blank with AC Native OS and ship it your way.",
          },
        },
        custom_fields: [
          {
            key: "note",
            label: { type: "custom", custom: "Add a note (optional)" },
            type: "text",
            optional: true,
          },
        ],
      };

      // Prefill email if logged in
      const user = await authorize(event.headers);
      if (user?.email) sessionConfig.customer_email = user.email;

      const session = await stripe.checkout.sessions.create(sessionConfig);
      return respond(200, { location: session.url, sessionId: session.id });
    } catch (error) {
      console.error("Blank checkout error:", error);
      return respond(500, { error: error.message });
    }
  }

  // 2. Webhook handler
  const sig = event.headers["stripe-signature"];
  if (!sig) {
    return respond(400, { error: "Missing stripe-signature header" });
  }

  const secret = dev
    ? process.env.STRIPE_ENDPOINT_DEV_SECRET
    : process.env.STRIPE_ENDPOINT_BLANK_SECRET;

  let hookEvent;
  try {
    hookEvent = stripe.webhooks.constructEvent(event.body, sig, secret);
  } catch (err) {
    return respond(400, { message: `Webhook Error: ${err.message}` });
  }

  if (hookEvent.type === "checkout.session.completed") {
    const session = await stripe.checkout.sessions.retrieve(
      hookEvent.data.object.id,
      { expand: ["line_items", "shipping_details", "customer_details"] },
    );

    if (session.payment_status !== "paid") {
      return respond(500, { message: "Payment not completed" });
    }

    const metadata = hookEvent.data.object.metadata;
    const tier = metadata.tier || "blank";
    const customerEmail = session.customer_details?.email;
    const customerName = session.shipping_details?.name || "Friend";
    const note =
      session.custom_fields?.find((f) => f.key === "note")?.text?.value || null;

    console.log(
      `✅ Blank order: ${tier} tier, ${metadata.amount}, ${customerEmail}`,
    );

    // Store order in MongoDB
    try {
      const { connect } = await import("../../backend/database.mjs");
      const database = await connect();
      const orders = database.db.collection("blank-orders");

      await orders.insertOne({
        stripeSessionId: session.id,
        paymentIntentId: session.payment_intent,
        customer: {
          email: customerEmail,
          name: customerName,
          address: session.shipping_details?.address || null,
        },
        product: {
          model: metadata.model,
          tier,
          amount: parseInt(metadata.amount) || 0,
          currency: metadata.currency,
        },
        note,
        status: "paid",
        trackingNumber: null,
        createdAt: new Date(),
        updatedAt: new Date(),
      });
    } catch (dbErr) {
      console.error("Failed to store blank order:", dbErr);
      // Don't fail the webhook — order is paid, we'll handle manually
    }

    // Send confirmation email to buyer
    const tierLabel =
      tier === "tutorial"
        ? "AC Blank + Tutorial"
        : tier === "support"
          ? "AC Blank (thank you for your support!)"
          : "AC Blank";

    await email({
      to: customerEmail,
      subject: "your blank is coming!",
      html: `
        <h2>${tierLabel}</h2>
        <p>hi ${customerName},</p>
        <p>thank you for your order! we'll flash your blank with AC Native OS and get it to you soon.</p>
        ${tier === "tutorial" ? "<p><b>we'll reach out separately to schedule your in-person tutorial session in los angeles.</b></p>" : ""}
        ${note ? `<p>your note: <em>${note}</em></p>` : ""}
        <p>
          <b><a href="https://aesthetic.computer">aesthetic.computer</a></b><br>
          <code>${session.payment_intent?.replace("pi_", "") || session.id}</code>
        </p>
      `,
    });

    // Notify us internally
    await email({
      to: "mail@aesthetic.computer",
      subject: `new blank order! (${tier}) — ${metadata.amount}`,
      html: `
        <h2>New Blank Order</h2>
        <p><b>Tier:</b> ${tier}</p>
        <p><b>Amount:</b> ${metadata.amount}</p>
        <p><b>Customer:</b> ${customerName} (${customerEmail})</p>
        <p><b>Shipping:</b><br>
          ${session.shipping_details?.address?.line1 || "Hand delivery"}<br>
          ${session.shipping_details?.address?.line2 ? session.shipping_details.address.line2 + "<br>" : ""}
          ${session.shipping_details?.address?.city || ""}, ${session.shipping_details?.address?.state || ""} ${session.shipping_details?.address?.postal_code || ""}<br>
          ${session.shipping_details?.address?.country || ""}
        </p>
        ${note ? `<p><b>Note:</b> ${note}</p>` : ""}
        <p><b>Stripe:</b> ${session.payment_intent}</p>
      `,
    });

    return respond(200, { received: true, tier });
  }

  return respond(400, { message: `Unhandled event: ${hookEvent.type}` });
}
