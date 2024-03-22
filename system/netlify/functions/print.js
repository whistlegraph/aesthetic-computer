// Print, 23.08.25.14.37
// Order stickers or get mockup imagery from a POD API (Printful)
// Dashboard: https://www.printful.com/dashboard

// GET: Returns an image mockup url for the print.
//      Usage: `/print?pixels=imageUrl`
// POST: Takes user info like an address and creates an order in the system,
//       running off of a Stripe webhook.

// Testing:
// Prod example URL:  https://aesthetic.computer/api/print?pixels=https://aesthetic.computer/api/pixel/1650x1650/art/Lw2OYs0H
// Local example URL: https://aesthetic.local:8888/api/print?pixels=https://aesthetic.computer/api/pixel/1650x1650/art/Lw2OYs0H
// Or...              https://aesthetic.local:8888/api/print?pixels=https://aesthetic.computer/api/pixel/1650x1650/@jeffrey/painting/2023.8.24.16.21.09.123.png

// Installing the Stripe CLI: https://stripe.com/docs/stripe-cli#install
// ⚠️ For testing webhooks: `stripe listen --forward-to stripe listen --forward-to "https://localhost:8888/api/print"

/* #region 🏁 TODO 
  - [️🔴] How can I route tracking numbers of shipped orders / get the status
       or listen in via Printful's API? 
       (Do I have to run a cron-task or listen for a webhook?)
       - [] https://chat.openai.com/c/8ecc3d00-fe14-4f89-ba8f-b7d0c558591b
       - Write a CURL script / request to enable the https://developers.printful.com/docs/#operation/packageShipped webhook.
       - [] https://developers.printful.com/docs/#operation/getWebhooks
       - [] https://www.printful.com/api/webhook-simulator
  + Later
  - [] Email should link to a sticker feed of some kind?
      (Don't want your sticker included? Reply to this email to opt-out.)
      (Paintings that have been printed get special copies in S3.)
  + Done
  - [x] Why are there black bars in the sticker mockups / orders?
    - [x] Check the official mockup api against the image I'm sending in?
  - [x] Hook up a real production order! Make it actually work?!
  - [x️] Make a dynamic logo endpoint that always returns a different graphic:
        "https://assets.aesthetic.computer/images/favicon.png"
  - [x] Make sure to refund the user if the Printful order can't created.
  - [x‍] How can I associate the Stripe order ID with the printful order
        just in case of issues that need to be manually addressed?
  - [x] Retrieve the mockup image for a successful order and show it to
        the user either on the success screen or in the email they receive.
  - [x] Image mockup generation is kind of slow... maybe I could use /pixel
       to do it on my own server and just have "essentially" the same kind
       of image?
       (Also it may be rate-limited)
  - [x] Add branding: https://stripe.com/docs/payments/checkout/customization
  - [x] Customize stripe: https://dashboard.stripe.com/settings/branding
  - [x] Send user a stripe receipt email. (How does this work in testing?)
  - [x] Replace printful compositor with my own from `api/pixel`.
  - [x] Get mockup images working and looking good for different
       resolutions.
    - [x] Test a painting that is at a different resolution / try
         a cropped image.
  - [x] Send user a confirmation email from mail@aesthetic.computer
       upon a successful Printful order fulfillment.
       - [x] Email should embed the mockup image of the sticker, or 
             at the very least, include a link?
  - [x] Integrate into a `print` command and the [Print] button on a painting page.
  - [x] Show a `success` or `failure` screen to the user after they
        attempt to checkout.
  - [x] Write a `print` command that will print the current painting.
  - [x] And also wire up the `Print` button from a painting page to that command.
  - [x] Adjust the POST requests with a `hook` flag for normal order creation or
       responding to the webhook.
  - [x] Run some test orders.
  - [x] Add stripe checkout. 
  - [x] There needs to be a way to get and/or store user address information or
        have the user fill out a payment form and validate their address that
        way...
  - [x] Add some funds ($150) to my Printful wallet.
  - [x] Find a library that can decode and rescale the PNG.
  - [x] Should this eventually go in a separate API call?
  - [x] Nearest-neighbor stretch the painting to fit 1650x1650
      - [x] And place it on a URL.
      - https://aesthetic.local:8888/api/pixel/1024x1024/art/Lw2OYs0H
#endregion */

import { respond } from "../../backend/http.mjs";
import { email } from "../../backend/email.mjs";
import Stripe from "stripe";
const dev = process.env.CONTEXT === "dev";
const printfulKey = process.env.PRINTFUL_API_TOKEN;
const stripeKey = dev
  ? process.env.STRIPE_API_TEST_PRIV_KEY
  : process.env.STRIPE_API_PRIV_KEY; // Uncomment for real orders.

import { authorize } from "../../backend/authorization.mjs";
import { logoUrl } from "../../backend/logo.mjs";

export async function handler(event, context) {
  const { got } = await import("got");
  // const { nanoid } = await import("nanoid");
  const API = "https://api.printful.com";
  const headers = {
    Authorization: "Bearer " + printfulKey,
    "Content-Type": "application/json",
  };

  // Lock these requests to a particular hosts. // Not necessary? 23.09.06.00.06
  // if (event.headers["host"] !== "aesthetic.computer" && !dev) {
  //   return respond(400, {
  //     message: "Bad request.",
  //     host: event.headers["host"],
  //   });
  // }

  const id = 358; // Kiss-cut Sticker
  const variant = 10165; // 5.5" Square
  const width = 1650; // Print resolution.
  const height = 1650;

  // 🅰️ GET: Generate a mockup image.
  if (event.httpMethod === "GET") {
    // 🔸 List all products.
    // (Only really needed while developing for now. 23.08.25.18.42)
    // const products = await got.get(`${API_URL}/products?category_id=202`, {
    // headers,
    // });
    // return respond(200, { product: JSON.parse(products.body) });
    // 🔍
    // 202 - Stickers `category_id`.
    // 358 - Kiss-cut sticker product `id`.
    // const id = 358;
    // 🔸 Get information for a single product.
    // (Which includes its availailbility status by region, per variant.)
    // TODO: Make sure it's stock before purchasing? 23.08.25.18.50
    // const product = await got.get(`${API}/products/${id}`, { headers });
    // return respond(200, { product: JSON.parse(product.body) });
    // 🔍
    // ??? - Kiss-cut sticker 5.5" square `variant_id`.
    // const variant = 10165;
    // 🔸 Get print constraints for a given product.
    // const printfiles = await got.get(
    //   `${API}/mockup-generator/printfiles/${id}`,
    //   { headers }
    // );
    // return respond(200, { product: JSON.parse(printfiles.body) });
    // 🔸 Generate a mockup image.
    // return respond(200, { message: mockupUrl });
    return respond(405, { message: "No GET Method allowed. 😃" });
  } else if (event.httpMethod === "POST") {
    const stripe = new Stripe(stripeKey);

    // 1. 🔵 Begin a new print order.
    if (event.queryStringParameters.new === "true") {
      let mockupUrl = null; // Generated by printful below, before each order.

      // Parse the input slug or url and generate a mockup image if possible.
      let imageUrl;
      // console.log("Pixels:", event.queryStringParameters.pixels);
      if (!event.queryStringParameters.pixels.startsWith("https://")) {
        imageUrl = `https://aesthetic.computer/api/pixel/1650:contain/${event.queryStringParameters.pixels}`;
        mockupUrl = imageUrl
          .replace("contain", "sticker")
          .replace("1650", "640");
      } else {
        imageUrl = event.queryStringParameters.pixels;
        mockupUrl = imageUrl; // Don't generate a mockup yet for a custom url. 23.09.05.02.34
        return respond(500, {
          message: "No external image URLs allowed.",
          imageUrl,
        });
      }

      // console.log("imageURL:", imageUrl);

      /*
      try {
        // 🔸 Generate a mockup image via printful.
        const task = await got.post(
          `${API}/mockup-generator/create-task/${id}`,
          {
            headers: headers,
            json: {
              variant_ids: [variant],
              files: [
                {
                  placement: "default", // 185
                  image_url: imageUrl,
                  position: {
                    area_width: 1650, // Constant for this variant.
                    area_height: 1650,
                    width,
                    height,
                    top: 0,
                    left: 0,
                  },
                },
              ], // See also: https://developers.printful.com/docs/#operation/createGeneratorTask
              format: "png",
            },
          },
        );

        const taskResult = JSON.parse(task.body)?.result;

        const maxAttempts = 12;
        let attempt = 0;
        while (attempt < maxAttempts) {
          const statusResponse = await got.get(
            `${API}/mockup-generator/task/?task_key=${taskResult?.task_key}`,
            { headers },
          );
          const statusResult = JSON.parse(statusResponse.body);

          if (statusResult.result.status === "completed") {
            console.log("Result:", statusResult);
            mockupUrl = statusResult.result.mockups[0].mockup_url;
            break;
          } else {
            await new Promise((res) => setTimeout(res, 2000)); // wait 2 seconds.
            attempt += 1;
          }
        }

        if (!mockupUrl) {
          return respond(500, {
            message: "Timed out waiting for mockup generation.",
          });
        }
      } catch (error) {
        return respond(500, { message: error.message });
      }
      */

      // 🅱️ POST: Place an order.
      const post = JSON.parse(event.body);

      const domain = dev
        ? "https://localhost:8888"
        : "https://aesthetic.computer";

      // Pricing details.
      const quantity = post.quantity || 1; // Set quantity from post data.
      const unitAmount = 400 * quantity; // Kiss-cut sticker price in cents
      //                 ❓ Go down a bit in price for higher quantities?
      const shippingAndProcessing = 500; // Shipping & Processing in cents
      const paymentProcessorFees = 100; // Payment processor fees in cents
      const subtotal =
        unitAmount + shippingAndProcessing + paymentProcessorFees;
      // const stripeFee = Math.round(subtotal * 0.03 + 30); // Stripe takes 3% + 30 cents
      const finalAmount = subtotal; // + stripeFee;

      // TODO: Add quantity to product name if there is more than 1.
      const productName = 'Painting Sticker 5.5"';
      let name = productName;
      if (quantity > 1) name += " (" + quantity + " copies)";

      const stripeCheckout = {
        line_items: [
          {
            price_data: {
              currency: "usd",
              product_data: { name, images: [mockupUrl] },
              unit_amount: finalAmount, // Amount in cents
            },
            quantity: 1,
          },
        ],
        metadata: {
          productName, // Pass product name to Printful.
          quantity, // Pass actual quantity to Printful.
          mockupUrl, // Send the mockup url for a reply email.
          imageUrl, // TODO: Eventually this might need to include
          //                 images<->product associations for multiple items.
        },
        mode: "payment",
        shipping_address_collection: { allowed_countries: ["US"] },
        success_url: `${domain}/${post.slug}?notice=success`, // Pick these states up in the piece.
        cancel_url: `${domain}/${post.slug}?notice=cancel`,
        automatic_tax: { enabled: true },
        custom_text: {
          submit: {
            message: `📥 Check your email and expect your sticker${
              quantity > 1 ? "s" : ""
            } to arrive soon!`,
          },
        },
      };

      // 🤹 Add `customer_email` to the checkout if the user is logged in.
      const user = await authorize(event.headers);
      if (user?.email) stripeCheckout.customer_email = user.email;

      const session = await stripe.checkout.sessions.create(stripeCheckout);
      return respond(200, { location: session.url });
    } else {
      // 2. 🔵 Or respond to an existing one.
      // ↪️ Receive webhook events...
      // ✅ checkout.session.completed
      //    charge.succeeded
      //    payment_intent.succeeded
      //    payment_intent.created

      const sig = event.headers["stripe-signature"];
      const secret = dev
        ? process.env.STRIPE_ENDPOINT_DEV_SECRET 
        : process.env.STRIPE_ENDPOINT_SECRET;
      let hookEvent;

      try {
        hookEvent = stripe.webhooks.constructEvent(event.body, sig, secret);
      } catch (err) {
        const msg = { message: `Webhook Error: ${err.message}` };
        // console.log(msg);
        return respond(400, msg);
      }

      // Handle the `checkout.session.completed` webhook.
      if (hookEvent.type === "checkout.session.completed") {
        const session = await stripe.checkout.sessions.retrieve(
          hookEvent.data.object.id,
          { expand: ["line_items", "shipping_details", "customer_details"] },
        ); // Retrieve session expanding certain fields.
        // See also: https://stripe.com/docs/api/checkout/sessions/object

        // console.log("🤑 Stripe order:", session);

        // 💁 If this fails after a successful payment, then it needs to be
        // manually triggerable for already paid orders or a refund needs
        // to be triggered automagically. 23.08.29.17.50

        if (session.payment_status === "paid") {
          console.log("✅ Paid!");
          const productName = hookEvent.data.object.metadata.productName;
          const quantity = hookEvent.data.object.metadata.quantity;
          const itemImage = hookEvent.data.object.metadata.imageUrl;
          const mockupUrl = hookEvent.data.object.metadata.mockupUrl;
          // console.log("Metadata image:", itemImage);
          // console.log("Shipping details:", session.shipping_details);
          // console.log("Customer:", session.customer_details);

          // 📔 Prinful Order Docs: https://developers.printful.com/docs/#operation/createOrder

          // 🖨️ Run the printful order, transferring over the shipping data.
          const shipping = session.shipping_details;
          const recipient = {
            name: shipping.name,
            address1: shipping.address.line1,
            city: shipping.address.city,
            state_code: shipping.address.state,
            country_code: shipping.address.country,
            zip: shipping.address.postal_code,
            email: session.customer_details.email,
          };
          if (shipping.address.line2)
            recipient.address2 = shipping.address.line2;

          const order = {
            external_id: session.payment_intent, // Store this "Stripe" checkout session id.
            recipient,
            items: [
              {
                name: productName,
                variant_id: variant,
                quantity,
                files: [
                  {
                    url: itemImage,
                    position: {
                      area_width: 1650, // Constant for this variant.
                      area_height: 1650,
                      width,
                      height,
                      top: 0,
                      left: 0,
                    },
                  },
                ],
              },
            ],
            packing_slip: {
              email: "mail@aesthetic.computer",
              message: "Your pictures belong on this earth. - @jeffrey",
              logo_url: logoUrl(),
              store_name: "aesthetic.computer",
            },
          };

          // console.log("Making printful order:", order);

          try {
            // 🔸 Place an order.
            //    (Auto-confirm in production.)
            const orderResponse = await got.post(
              `${API}/orders?confirm=${dev ? false : true}`,
              { headers, json: order },
            );

            const orderResult = JSON.parse(orderResponse.body);

            if (orderResult && orderResult.code === 200 && orderResult.result) {
              // console.log("😃 Order sent!", orderResult);
              const emailSent = await email({
                to: session.customer_details.email,
                subject:
                  quantity > 1
                    ? "your stickers are coming! 🫠"
                    : "your sticker is coming! 🫠",
                html: `
                <img src="${mockupUrl}" width="250">
                <p>and we appreciate your order</p>
                <b><a href="https://aesthetic.computer">aesthetic.computer</a></b>
                <br>
                <code>${session.payment_intent.replace("pi_", "")}</code>
                `,
              });

              return respond(200, { order: orderResult.result, emailSent });
            } else {
              throw new Error("Error processing order.");
            }
          } catch (error) {
            console.log("Printful order failed, refunding the customer...");
            let refunded = false;

            try {
              await stripe.refunds.create({
                payment_intent: session.payment_intent,
              });
              refunded = true;
              console.log("✅ Refund successful!");
            } catch (refundError) {
              console.error("🚫 Refund error:", refundError.message);
            }

            await email({
              to: session.customer_details.email,
              subject: "oh no! 🚫",
              html: `
                <img src="${mockupUrl}" width="250">
                <p>
                  your sticker order has failed due to an error,<br>
                  ${
                    refunded
                      ? "and you have been refunded."
                      : "reply here to be refunded."
                  } 
                </p>
                <p>deeply sorry about that!</p>
                <b><a href="https://aesthetic.computer">aesthetic.computer</a></b>
                <br>
                <code>${session.payment_intent.replace("pi_", "")}</code>
                `,
            });

            const msg = { message: error.message + " - Refund attempted." };
            return respond(500, msg);
          }
        } else {
          // Stripe order was unpaid.
          return respond(500, { message: "Order payment failed." });
        }
      } else {
        return respond(400, {
          message: `Unhandled webhook event: ${hookEvent.type}`,
        });
      }
    }
  } else {
    return respond(405, { message: "Method Not Allowed" });
  }
}
