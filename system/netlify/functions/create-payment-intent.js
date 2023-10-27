// Create Payement Intent, 23.10.26.19.26
// This file is for supporting stripe pop-up checkout functionality across AC.

import stripePackage from "stripe";
const stripe = stripePackage(process.env.STRIPE_API_TEST_KEY);
// const stripe = stripePackage(process.env.STRIPE_API_PRIV_KEY);
import { respond } from "../../backend/http.mjs";

// 💲 A utility function to calculate the order amount
const calculateOrderAmount = (items) => {
  // Replace this constant with a calculation of the order's amount
  // Calculate the order total on the server to prevent
  // people from directly manipulating the amount on the client
  return 1400;
};

export async function handler(event, context) {
  // Only allow POST
  if (event.httpMethod !== "POST")
    return respond(405, { message: "Method Not Allowed" });

  const data = JSON.parse(event.body); // Parse the body contents into an object
  const { items } = data; // Get the items from the body

  try {
    // Create a PaymentIntent with the order amount and currency
    const paymentIntent = await stripe.paymentIntents.create({
      amount: calculateOrderAmount(items),
      currency: "usd",
      automatic_payment_methods: {
        enabled: true,
      },
    });

    return respond(200, { clientSecret: paymentIntent.client_secret });
  } catch (error) {
    return respond(400, { error: error.message });
  }
}
