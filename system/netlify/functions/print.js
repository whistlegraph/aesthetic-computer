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
// Or...              https://aesthetic.local:8888/api/print?pixels=https://aesthetic.local:8888/api/pixel/1650x1650/@jeffrey/painting/2023.8.24.16.21.09.123.png

// Installing the Stripe CLI: https://stripe.com/docs/stripe-cli#install
// For testing webhooks: `stripe listen --forward-to stripe listen --forward-to "https://localhost:8888/api/print"

/* #region 🏁 TODO 
  - [-] Adjust the POST requests with a `hook` flag for normal order creation or
       responding to the webhook.
  - [] Add branding: https://stripe.com/docs/payments/checkout/customization
  - [] Get mockup images working and looking good for different
       resolutions.
    - [] Test a painting that is at a different resolution / try
         a cropped image.
  - [] Run some test orders.
  - [] Create a REAL order!
  - [] Handle errors or automatic refunds if Printful fails?
  + Later
  - [] Generate multiple items / stickers per order. (Multi-pack) 
  + Done
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
import Stripe from "stripe";
const dev = process.env.CONTEXT === "dev";
const printfulKey = process.env.PRINTFUL_API_TOKEN;
const stripeKey = process.env.STRIPE_API_TEST_KEY; // process.env.STRIPE_API_PRIV_KEY; // Uncomment for real orders.
const productName = "aesthetic.computer Painting Sticker";

export async function handler(event, context) {
  const { got } = await import("got");
  const API = "https://api.printful.com";
  const headers = {
    Authorization: "Bearer " + printfulKey,
    "Content-Type": "application/json",
  };

  if (event.headers["host"] !== "aesthetic.computer" && !dev) {
    return respond(400, { message: "Bad request." });
  }

  const id = 358;
  const variant = 10165;
  const imageUrl = event.queryStringParameters.pixels;
  const width = 1650;
  const height = 1650;

  // 🅰️ GET: Generate a mockup image.
  if (event.httpMethod === "GET") {
    // TODO: Eventually move this down into a "POST" request.
    const stripe = new Stripe(stripeKey);
    const domain = dev
      ? "https://localhost:8888"
      : "https://aesthetic.computer";

    const stripeCheckout = {
      line_items: [
        {
          price_data: {
            currency: "usd",
            product_data: {
              name: productName,
              images: [imageUrl],
            },
            unit_amount: 1000, // Amount in cents
          },
          quantity: 1,
        },
      ],
      metadata: {
        imageUrl, // TODO: Eventually this might need to include images<->product associations for multiple items.
      },
      mode: "payment",
      shipping_address_collection: { allowed_countries: ["US"] },
      success_url: `${domain}/success.html`,
      cancel_url: `${domain}/cancel.html`,
      automatic_tax: { enabled: true },
    };

    // TODO: 🔥 Add `customer_email` to the above if the user is currently logged in?

    const session = await stripe.checkout.sessions.create(stripeCheckout);

    return respond(303, undefined, { Location: session.url });

    try {
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
      const task = await got.post(`${API}/mockup-generator/create-task/${id}`, {
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
      });

      const taskResult = JSON.parse(task.body)?.result;

      let mockupUrl = null;
      const maxAttempts = 12;
      let attempt = 0;
      while (attempt < maxAttempts) {
        const statusResponse = await got.get(
          `${API}/mockup-generator/task/?task_key=${taskResult?.task_key}`,
          { headers }
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

      if (mockupUrl) {
        return respond(200, { sticker: mockupUrl });
      } else {
        return respond(500, {
          message: "Timed out waiting for mockup generation.",
        });
      }
    } catch (error) {
      return respond(500, { message: error.message });
    }
  } else if (event.httpMethod === "POST") {
    // 🅱️ POST: Place an order.

    // ↪️ Receive webhook events...
    // ✅ checkout.session.completed
    //    charge.succeeded
    //    payment_intent.succeeded
    //    payment_intent.created

    const stripe = new Stripe(stripeKey);
    const sig = event.headers["stripe-signature"];
    const secret = dev
      ? "whsec_181bbe48c808731616de39c5c75fc670c83c6ef554409b8c770170b718fbd3b0"
      : process.env.STRIPE_ENDPOINT_SECRET;
    let hookEvent;

    try {
      hookEvent = stripe.webhooks.constructEvent(event.body, sig, secret);
    } catch (err) {
      const msg = { message: `Webhook Error: ${err.message}` };
      console.log(msg);
      return respond(400, msg);
    }

    // Handle the `checkout.session.completed` event
    if (hookEvent.type === "checkout.session.completed") {
      // Retrieve the session. If you require line items in the response, you may include them by expanding line_items.
      const session = await stripe.checkout.sessions.retrieve(
        hookEvent.data.object.id,
        { expand: ["line_items", "shipping_details", "customer_details"] }
      );
      console.log("🤑 Stripe order:", session);

      console.log("Line items:", session.line_items);

      const itemImage = hookEvent.data.object.metadata.imageUrl;
      console.log("Image:", itemImage);
      // console.log("Shipping details:", session.shipping_details);
      // console.log("Customer:", session.customer_details);

      // TODO: If this fails after a successful payment, then it needs to be
      // manually triggerable for already paid orders or a refund needs
      // to be triggered automagically. 23.08.29.17.50

      if (session.payment_status === "paid") {
        console.log("✅ Paid!");

        // 🖨️ Run the printful order.
        const shipping = session.shipping_details;
        const recipient = {
          name: shipping.name,
          address1: shipping.address.line1,
          city: shipping.address.city,
          state_code: shipping.address.state,
          country_code: shipping.address.country,
          zip: shipping.address.postal_code,
        };
        if (shipping.address.line2) recipient.address2 = shipping.address.line2;

        const order = {
          recipient,
          items: [
            {
              name: productName,
              variant_id: variant,
              quantity: 1,
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
            message: "Stickers belong in special spots. - @jeffrey",
            logo_url: "https://assets.aesthetic.computer/images/favicon.png",
            store_name: "aesthetic.computer",
          },
        };

        console.log("Making printful order:", order);

        try {
          // 🔸 Place an order.
          // See also: https://developers.printful.com/docs/#operation/createOrder
          // TODO: Auto-confirm orders.
          const orderResponse = await got.post(`${API}/orders?confirm=false`, {
            headers,
            json: order,
          });

          const orderResult = JSON.parse(orderResponse.body);

          if (orderResult && orderResult.result) {
            console.log("😃 Order sent!", orderResult);
            // TODO: How to include image metadata?

            return respond(200, { order: orderResult.result });
          } else {
            const msg = { message: "Error processing order." };
            console.log(msg);
            return respond(500, msg);
          }
        } catch (error) {
          const msg = { message: error.message };
          console.log(msg);
          return respond(500, msg);
        }
      } else {
        // Stripe order was unpaid.
        return respond(500, { message: "Order payment failed." });
      }
    } else {
      return respond(400, { message: "Incorrect webhook event." });
    }
  } else {
    return respond(405, { message: "Method Not Allowed" });
  }
}
