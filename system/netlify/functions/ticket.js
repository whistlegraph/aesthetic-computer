// Ticket, 23.10.26.19.26
// This file is for supporting stripe pop-up checkout functionality across AC.

import Stripe from "stripe";
import { respond } from "../../backend/http.mjs";
import { email } from "../../backend/email.mjs";
import { connect } from "../../backend/database.mjs";
const dev = process.env.CONTEXT === "dev";
const botcePiece = "wPyYL4osf6Cw0pkHn_E5I-botce";

// 💲 A utility function to calculate the order amount
const calculateOrderAmount = (items) => {
  console.log("💲 Items:", items);
  if (items[0].id === "botce") return 1400; // Assume just one item for now...
  return 0; // Free!
};

export async function handler(event, context) {
  const database = await connect(); // 📕 Database
  const collection = database.db.collection("tickets");

  if (event.httpMethod === "GET") {
    // Confirm a previously set payment by checking for a ticket
    // that matches the payment intent id.
    if (event.queryStringParameters.check === "true") {
      const ticket = await collection.findOne({
        pid: event.queryStringParameters.pid,
      });
      console.log(
        "🎟️ Ticket from payment id:",
        ticket,
        event.queryStringParameters.pid,
      );
      if (ticket) {
        return respond(200, { ticketed: true, ticket, piece: botcePiece });
      } else {
        return respond(200, { ticketed: false });
      }
    }

    // Or grab a ticket.
    const key = event.path.split("/").pop();
    let ticket = await collection.findOne({ key });

    if (!ticket) {
      console.log("🚫🎟️ No ticket found...");
      return respond(401, { message: "No ticket found. 😢" });
    }

    if (ticket.uses === 0) {
      return respond(403, { message: "Ticket expired. 🎟️" });
    }

    // Don't decrement the ticket counter if one was already found.
    if (event.queryStringParameters.found !== "true") {
      await collection.updateOne({ key }, { $inc: { uses: -1 } }); // Dec uses.
      ticket = await collection.findOne({ key });
    }

    console.log("🎟️ Ticket:", ticket);
    await database.disconnect();

    const body = { ticket };

    if (ticket.for === "botce") {
      // Include the paid `botce` prompt.

      // TODO: How could this data eventually be pulled live from
      //       a source that anyone could easily edit, like
      //       a google spreadsheet.
      //     - And some keys stored in the ENV variables.

      // body.botce = { before: "Hello...", after: "Goodbye..." };
      body.botce = { piece: botcePiece };
    }

    return respond(200, body);
  }

  // Only allow POST from here on.
  if (event.httpMethod !== "POST")
    return respond(405, { message: "Method Not Allowed" });

  // Get a payment intent / start a new sale.
  if (event.queryStringParameters.new === "true") {
    const data = JSON.parse(event.body); // Parse the body contents into an object
    const { items, from } = data; // Get the items from the body

    let key;
    const envKey = from === "sotce" ? "SOTCE_" : "";
    key = dev
      ? process.env[`${envKey}STRIPE_API_TEST_PRIV_KEY`]
      : process.env[`${envKey}STRIPE_API_TEST_PRIV_KEY`];
    //: process.env[`${envKey}STRIPE_API_PRIV_KEY`];

    const stripe = Stripe(key);

    try {
      // Create a PaymentIntent with the order amount and currency
      const paymentIntent = await stripe.paymentIntents.create({
        amount: calculateOrderAmount(items),
        currency: "usd",
        automatic_payment_methods: { enabled: true },
      });

      // console.log("Payment intent:", paymentIntent);

      return respond(200, { clientSecret: paymentIntent.client_secret });
    } catch (error) {
      return respond(400, { error: error.message });
    }
  } else {
    // ↪️ Receive webhook events...
    // ✅ charge.succeeded
    // ✅ payment_intent.succeeded

    let prodSecret, devSecret, key;
    // TODO: How to know here if sotce or not?
    const fromSotce = event.path.endsWith("sotce");
    const prefix = fromSotce ? "SOTCE_" : "";
    prodSecret = process.env[`${prefix}STRIPE_ENDPOINT_SECRET`];
    devSecret = process.env[`${prefix}STRIPE_ENDPOINT_DEV_SECRET`];
    key = dev
      ? process.env[`${prefix}STRIPE_API_TEST_PRIV_KEY`]
      : process.env[`${prefix}STRIPE_API_TEST_PRIV_KEY`];
    //: process.env[`${prefix}STRIPE_API_PRIV_KEY`];

    const stripe = Stripe(key);

    const sig = event.headers["stripe-signature"];
    // const secret = dev ? devSecret : prodSecret;
    const secret = prodSecret;
    let hookEvent;

    try {
      hookEvent = stripe.webhooks.constructEvent(event.body, sig, secret);
    } catch (err) {
      const msg = { message: `Webhook Error: ${err.message}` };
      // console.log(msg);
      return respond(400, msg);
    }

    console.log("Hook:", hookEvent.type);

    if (hookEvent.type === "charge.succeeded") {
      console.log("😃 Charge succeeeded!");
      const emailAddress = hookEvent.data.object.receipt_email;

      // Create an expiring link via a "tickets" collection in the db.
      const database = await connect(); // 📕 Database
      const collection = database.db.collection("tickets");
      const { nanoid } = await import("nanoid");
      const ticketKey = nanoid();
      await collection.insertOne({
        key: ticketKey,
        for: fromSotce ? "botce" : "aesthetic",
        email: emailAddress,
        uses: 10,
        pid: hookEvent.data.object.payment_intent,
      });
      await database.disconnect();

      const link = `https://${
        dev ? "localhost:8888/botce" : "botce.ac"
      }?ticket=${ticketKey}`;

      const emailOptions = {
        to: hookEvent.data.object.receipt_email,
        subject: "🪷 visit me again?",
        html: `
          <p>This email contains three tickets for <code>botce</code>. Each click opens a chat for 24 hours.
          <br>
          <br>
          <a href="${link}">Click here to visit with botce.</a></p>
          <b><a href="https://sotce.com">sotce</a></b>
        `,
      };

      if (fromSotce) {
        emailOptions.auth = {
          user: process.env.SOTCE_SMTP_USER,
          pass: process.env.SOTCE_SMTP_PASS,
        };
      }

      const emailSent = await email(emailOptions);
      console.log("📧 Email sent:", emailSent);
    }

    return respond(200, { message: `Webhook: ${hookEvent.type}` });
  }
}
