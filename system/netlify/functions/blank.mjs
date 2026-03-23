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
import { connect } from "../../backend/database.mjs";

const dev = process.env.CONTEXT === "dev";

// Cache secrets from MongoDB
let cachedSecrets = null;
async function getSecrets() {
  if (cachedSecrets) return cachedSecrets;
  const { db } = await connect();
  const secrets = await db.collection("secrets").findOne({ _id: "blank" });
  if (!secrets) throw new Error("Blank secrets not found in database");
  cachedSecrets = secrets;
  return cachedSecrets;
}
const stripeKey = dev
  ? process.env.STRIPE_API_TEST_PRIV_KEY
  : process.env.STRIPE_API_PRIV_KEY;

// Pricing (cents)
const AMOUNT = 12800; // $128

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
      fixed_amount: { amount: 1600, currency: "usd" },
      display_name: "USPS Priority Mail",
      delivery_estimate: {
        minimum: { unit: "business_day", value: 2 },
        maximum: { unit: "business_day", value: 5 },
      },
    },
  },
];


export async function handler(event) {
  // CORS preflight
  if (event.httpMethod === "OPTIONS") {
    return respond(200, {});
  }

  // GET: Product info
  if (event.httpMethod === "GET") {
    return respond(200, {
      product: "AC Blank",
      model: "Lenovo ThinkPad 11e Yoga Gen 6",
      description:
        "Buy a @jeffrey approved Thinkpad 11e Yoga Gen 6 (refurbished / used) pre-flashed with AC Native today! Comes decorated with recovery USB. (No USB C Charger Included)",
      amount: AMOUNT,
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
      const domain = dev
        ? `https://${event.headers.host}`
        : "https://aesthetic.computer";

      const sessionConfig = {
        line_items: [
          {
            price_data: {
              currency: "usd",
              product_data: {
                name: "AC Blank 1",
                description:
                  "A @jeffrey approved, refurbished Thinkpad 11e Yoga Gen 6 pre-flashed with AC Native OS and Live USB recovery stick.",
                images: [
                  "https://p3-ofp.static.pub/fes/cms/2022/03/28/4wfdaky6aue1z6x5xmxkl9ms8gdpmz225363.png",
                ],
              },
              unit_amount: AMOUNT,
            },
            quantity: 1,
          },
        ],
        metadata: {
          type: "blank",
          model: "yoga-11e-gen6",
          amount: `$${(AMOUNT / 100).toFixed(0)}`,
          currency: "usd",
        },
        mode: "payment",
        shipping_address_collection: { allowed_countries: ["US"] },
        shipping_options: shippingOptions,
        success_url: `${domain}/blank~thanks`,
        cancel_url: `${domain}/blank`,
        automatic_tax: { enabled: true },
        custom_text: {
          submit: {
            message:
              "We'll flash your Blank with AC Native OS and ship it your way.",
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

  let secret;
  try {
    const secrets = await getSecrets();
    secret = dev
      ? process.env.STRIPE_ENDPOINT_DEV_SECRET
      : secrets.stripeWebhookSecret;
  } catch (err) {
    return respond(500, { message: `Secrets Error: ${err.message}` });
  }

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
    const customerEmail = session.customer_details?.email;
    const customerName = session.shipping_details?.name || "Friend";
    const note =
      session.custom_fields?.find((f) => f.key === "note")?.text?.value || null;

    console.log(
      `✅ Blank order: ${metadata.amount}, ${customerEmail}`,
    );

    // Store order in MongoDB
    try {
      const { db } = await connect();
      const orders = db.collection("blank-orders");

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
          amount: AMOUNT,
          currency: "usd",
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
    await email({
      to: customerEmail,
      subject: "your blank is coming!",
      html: `
        <h2>AC Blank</h2>
        <p>hi ${customerName},</p>
        <p>thank you for your order! we'll flash your blank with AC Native OS and get it to you soon.</p>
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
      subject: `new blank order! — ${metadata.amount}`,
      html: `
        <h2>New Blank Order</h2>
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

    return respond(200, { received: true });
  }

  return respond(400, { message: `Unhandled event: ${hookEvent.type}` });
}
