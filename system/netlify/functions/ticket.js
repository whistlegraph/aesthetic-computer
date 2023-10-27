// Ticker, 23.10.26.19.26
// This file is for supporting stripe pop-up checkout functionality across AC.

import Stripe from "stripe";
import { respond } from "../../backend/http.mjs";
import { email } from "../../backend/email.mjs";
const dev = process.env.CONTEXT === "dev";

// 💲 A utility function to calculate the order amount
const calculateOrderAmount = (items) => {
  console.log("💲 Items:", items);
  if (items[0].id === "botce") return 1400; // Assume just one item for now...
  return 0; // Free!
};

export async function handler(event, context) {
  // Only allow POST
  if (event.httpMethod !== "POST")
    return respond(405, { message: "Method Not Allowed" });

  // Get a payment intent / start a new sale.
  if (event.queryStringParameters.new === "true") {
    const data = JSON.parse(event.body); // Parse the body contents into an object
    const { items, from } = data; // Get the items from the body

    let key;
    if (from === "sotce") {
      key = dev
        ? process.env.SOTCE_STRIPE_API_TEST_PRIV_KEY
        : process.env.SOTCE_STRIPE_API_PRIV_KEY;
    } else {
      key = dev
        ? process.env.STRIPE_API_TEST_PRIV_KEY
        : process.env.STRIPE_API_PRIV_KEY;
    }

    const stripe = Stripe(key);

    try {
      // Create a PaymentIntent with the order amount and currency
      const paymentIntent = await stripe.paymentIntents.create({
        amount: calculateOrderAmount(items),
        currency: "usd",
        automatic_payment_methods: { enabled: true },
      });

      return respond(200, { clientSecret: paymentIntent.client_secret });
    } catch (error) {
      return respond(400, { error: error.message });
    }
  } else {
    // ↪️ Receive webhook events...
    // ✅ payment_intent.succeeded

    let prodSecret, devSecret, key;
    // TODO: How to know here if sotce or not?
    const fromSotce = event.path.endsWith("sotce");
    if (fromSotce) {
      prodSecret = process.env.SOTCE_STRIPE_ENDPOINT_SECRET;
      devSecret = process.env.SOTCE_STRIPE_ENDPOINT_DEV_SECRET;
      key = dev
        ? process.env.SOTCE_STRIPE_API_TEST_PRIV_KEY
        : process.env.SOTCE_STRIPE_API_PRIV_KEY;
    } else {
      prodSecret = process.env.STRIPE_ENDPOINT_SECRET;
      devSecret = process.env.STRIPE_ENDPOINT_DEV_SECRET;
      key = dev
        ? process.env.STRIPE_API_TEST_PRIV_KEY
        : process.env.STRIPE_API_PRIV_KEY;
    }

    const stripe = Stripe(key);

    const sig = event.headers["stripe-signature"];
    const secret = dev ? devSecret : prodSecret;
    let hookEvent;

    console.log("Event:", event, context);
    console.log("Path:", event.path);
    console.log("Sig:", sig);

    try {
      hookEvent = stripe.webhooks.constructEvent(event.body, sig, secret);
    } catch (err) {
      const msg = { message: `Webhook Error: ${err.message}` };
      console.log(msg);
      return respond(400, msg);
    }

    // console.log(hookEvent.type);

    if (hookEvent.type === "charge.succeeded") {
      console.log("😃 Charge succeeeded!");

      console.log("Hook Event:", hookEvent);

      const emailOptions = {
        to: "hellosotce@gmail.com",
        subject: "🪷 hello",
        html: `
          <img src="#" width="250">
          <p>you can use this link 10 times: <a href="#">botce</a></p>
          <b><a href="https://sotce.com">sotce.com</a></b>
          <br>
          <code>hello ;)</code>
        `,
      };

      if (fromSotce) {
        emailOptions.auth = {
          user: process.env.SOTCE_SMTP_USER,
          pass: process.env.SOTCE_SMTP_PASS,
        };
      }

      const emailSent = await email(emailOptions);

      console.log("Email:", emailSent);
    }

    return respond(200, { message: "Webhook..." });
  }
}
