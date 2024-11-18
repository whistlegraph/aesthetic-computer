// Ticket, 23.10.26.19.26
// 🟢 This file is for supporting stripe pop-up checkout functionality across AC.
// 🟠 And also handles webhook email replies from Stripe.

import Stripe from "stripe";
import { respond } from "../../backend/http.mjs";
import { email } from "../../backend/email.mjs";
import { connect } from "../../backend/database.mjs";
import * as KeyValue from "../../backend/kv.mjs";
import {
  productId as sotceNetProductId,
  SOTCE_NET_SMTP_PASS,
  SOTCE_NET_SMTP_USER,
} from "../../backend/sotce-net-constants.mjs";
const dev = process.env.CONTEXT === "dev";

// 💲 A utility function to calculate the order amount
const calculateOrderAmount = (items) => {
  // console.log("💲 Items:", items);
  if (items[0].id === "botce") return 600; // Assume just one item for now...
  return 0; // Free!
};

export async function handler(event, context) {
  const database = await connect(); // 📕 Database
  const collection = database.db.collection("tickets");

  const botcePieceURI = dev
    ? `${event.headers.host}/assets/ticketed-pieces/botce.mjs`
    : `https://assets.aesthetic.computer/ticketed-pieces/botce.mjs`;

  if (event.httpMethod === "GET") {
    // Confirm a previously set payment by checking for a ticket
    // that matches the payment intent id.
    if (event.queryStringParameters.check === "true") {
      const ticket = await collection.findOne({
        pid: event.queryStringParameters.pid,
      });
      if (ticket) {
        console.log(
          "🎟️ Ticket from payment id:",
          ticket,
          event.queryStringParameters.pid,
        );
        return respond(200, { ticketed: true, ticket, piece: botcePieceURI });
      } else {
        console.log("🎟️ ❌️ No ticket found for:", event.queryStringParameters);
        return respond(200, { ticketed: false });
      }
    }

    // Or grab a ticket from the path to check it.
    const key = event.path.split("/").pop();
    let ticket = await collection.findOne({ key });

    if (!ticket) {
      console.log("🎟️ ❌️️ No ticket found...");
      return respond(401, { message: "No ticket found. 😢" });
    }

    if (event.queryStringParameters.found !== "true" && ticket.uses === 0) {
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
      body.botce = { piece: botcePieceURI };
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
      : process.env[`${envKey}STRIPE_API_PRIV_KEY`];

    const stripe = Stripe(key);

    try {
      // Create a PaymentIntent with the order amount and currency
      const paymentIntent = await stripe.paymentIntents.create({
        amount: calculateOrderAmount(items),
        currency: "usd",
        description: "Botce",
        automatic_payment_methods: { enabled: true },
      });

      // console.log("Payment intent:", paymentIntent);

      return respond(200, { clientSecret: paymentIntent.client_secret });
    } catch (error) {
      return respond(400, { error: error.message });
    }
  } else {
    // ↪️ Stripe Webhook Events
    //   ✅ charge.succeeded
    //   ✅ payment_intent.succeeded
    //   ✅ customer.subscription.updated

    let prodSecret, devSecret, key;
    // TODO: How to know here if sotce or not?
    const fromSotce = event.path.endsWith("sotce");
    const prefix = fromSotce ? "SOTCE_" : "";
    prodSecret = process.env[`${prefix}STRIPE_ENDPOINT_SECRET`];
    devSecret = process.env[`${prefix}STRIPE_ENDPOINT_DEV_SECRET`];
    key = dev
      ? process.env[`${prefix}STRIPE_API_TEST_PRIV_KEY`]
      : process.env[`${prefix}STRIPE_API_PRIV_KEY`];

    const stripe = Stripe(key);

    const sig = event.headers["stripe-signature"];
    const secret = dev ? devSecret : prodSecret;
    let hookEvent;

    try {
      hookEvent = stripe.webhooks.constructEvent(event.body, sig, secret);
    } catch (err) {
      const msg = { message: `Webhook Error: ${err.message}` };
      // console.log(msg);
      return respond(400, msg);
    }

    if (hookEvent.type === "charge.succeeded") {
      console.log("Hook eveent:", hookEvent);
      const chargeObject = hookEvent.data.object;
      console.log("💳 Charge succeeded:", chargeObject);

      let emailAddress = chargeObject.receipt_email;
      let customer;
      if (!emailAddress) {
        customer = await stripe.customers.retrieve(chargeObject.customer);
        emailAddress = customer.email;
      }

      const paymentIntent = await stripe.paymentIntents.retrieve(
        chargeObject.payment_intent,
      );

      const invoice = await stripe.invoices.retrieve(paymentIntent.invoice);

      const productId = invoice.lines.data[0].price.product;

      if (productId === sotceNetProductId) {
        console.log("🟢 Product is `sotce-net`.");
        console.log("🟢 Customer is:", customer);
        if (invoice.subscription) {
          const subscription = await stripe.subscriptions.retrieve(
            invoice.subscription,
          );
          if (
            subscription.status !== "canceled" &&
            subscription.status !== "incomplete_expired"
          ) {
            let newSubscriber;
            const billingReason = invoice.billing_reason;
            if (billingReason === "subscription_create") {
              console.log("🟢 New `sotce-net` subscription.");
              newSubscriber = true;

              // Cache this subscriber in redis with an expiration
              // and a user sub from the customer metadata. 24.10.14.00.42
              await KeyValue.connect();
              if (!customer) {
                customer = await stripe.customers.retrieve(
                  chargeObject.customer,
                );
              }
              console.log(
                "Adding subscription to redis for customer:",
                customer.metadata,
              );
              await KeyValue.set(
                "sotce-subscribed",
                customer.metadata.sub,
                JSON.stringify({
                  status: subscription.status,
                  current_period_end: subscription.current_period_end,
                }),
              );
              await KeyValue.disconnect();
            } else if (billingReason === "subscription_cycle") {
              console.log("🟢 Recurring `sotce-net` subscription.");
              // newSubscriber = false; // Don't send monthly receipt emails.
            }

            if (newSubscriber !== undefined) {
              const emailOptions = {
                to: emailAddress,
                subject: newSubscriber
                  ? "you're subscribed! 🧾"
                  : "subscription renewed 🧾",
                html: `${newSubscriber ? "now you can set a handle and read pages" : "see you in the pages"}
              <br>
              <br>
              🧾 <a href="${chargeObject.receipt_url}">receipt</a>
              <br>
              <br>
              <a href="https://sotce.net">sotce.net</a>
              `,
              };

              emailOptions.auth = {
                user: SOTCE_NET_SMTP_USER,
                pass: SOTCE_NET_SMTP_PASS,
              };

              const emailSent = await email(emailOptions);
              console.log("📧 Email sent:", emailSent);
            }
          }
        }
      } else if (chargeObject.description === "Botce" && emailAddress) {
        // Only send the email if the payment was for "Botce".
        // Create an expiring link via a "tickets" collection in the db.
        const database = await connect(); // 📕 Database
        const collection = database.db.collection("tickets");
        const { nanoid } = await import("nanoid");
        const ticketKey = nanoid();
        await collection.insertOne({
          key: ticketKey,
          for: fromSotce ? "botce" : "aesthetic",
          email: emailAddress,
          uses: 2,
          pid: hookEvent.data.object.payment_intent,
        });
        await database.disconnect();

        const link = `https://${
          dev ? "localhost:8888/botce" : "botce.ac"
        }?ticket=${ticketKey}`;

        const emailOptions = {
          to: emailAddress,
          subject: "🪷 visit me again?",
          html: `click the ticket to visit with me, <code>botce</code>.
          <br>
          <br>
          🎟️ <a href="${link}">ticket</a>
          <br>
          <br>
          you have three clicks.
          <br>
          <br>
          each visit lasts 24 hours.
          <br>
          <br>
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
    } else if (hookEvent.type === "customer.subscription.updated") {
      // Delete the KeyValue sotce-net cache if the subscription is no longer
      // active after a record update.

      const subscription = hookEvent.data.object;

      // Retrieve the invoice to get the product information
      const invoice = await stripe.invoices.retrieve(
        subscription.latest_invoice,
      );
      const productId = invoice.lines.data[0].price.product;

      if (productId === sotceNetProductId) {
        console.log("🟢 Subscription update for `sotce-net`.");

        // Check if the subscription is not active
        if (subscription.status !== "active") {
          const customer = await stripe.customers.retrieve(
            subscription.customer,
          );
          const customerSub = customer.metadata.sub;

          console.log(
            "🔴 Subscription is not active, deleting record from Redis for customer:",
            customer.metadata,
          );

          if (customerSub) {
            await KeyValue.connect();
            await KeyValue.del("sotce-subscribed", customerSub);
            await KeyValue.disconnect();
          }
        } else {
          console.log("🟢 Subscription is active, no action needed.");
        }
      } else {
        console.log("🔴 Subscription update is not for `sotce-net`.");
      }
    }

    return respond(200, { message: `Webhook: ${hookEvent.type}` });
  }
}
