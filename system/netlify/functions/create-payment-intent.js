// Create Payement Intent, 23.10.26.19.26
// This file is for supporting stripe pop-up checkout functionality across AC.

import stripePackage from "stripe";
import { respond } from "../../backend/http.mjs";
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

  const stripe = stripePackage(key);

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
}
