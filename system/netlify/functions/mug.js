// Mug, 24.12.19
// Order ceramic mugs from Printful POD API
// Dashboard: https://www.printful.com/dashboard

// Testing:
// Local: https://localhost:8888/api/mug?new=true&pixels=CODE.png&color=white
// Prod:  https://aesthetic.computer/api/mug?new=true&pixels=CODE.png&color=blue

// Webhook: stripe listen --forward-to "https://localhost:8888/api/mug"

/* #region 🏁 TODO 
  - [] Add 15oz size option
  - [] Test all color variants
  - [] Add tracking number support
#endregion */

import { respond } from "../../backend/http.mjs";
import { email } from "../../backend/email.mjs";
import { findOrCreateProduct, updateProduct, cacheWebpPreview, incrementProductStat } from "../../backend/products.mjs";
import Stripe from "stripe";
const dev = process.env.CONTEXT === "dev";
const printfulKey = process.env.PRINTFUL_API_TOKEN;
const stripeKey = dev
  ? process.env.STRIPE_API_TEST_PRIV_KEY
  : process.env.STRIPE_API_PRIV_KEY;

import { authorize } from "../../backend/authorization.mjs";
import { logoUrl } from "../../backend/logo.mjs";

// Printful Ceramic Mug with Color Inside - Product ID 403
// 11oz variants by color
const MUG_PRODUCT_ID = 403;
const MUG_VARIANTS = {
  black: 11051,
  blue: 11050,
  orange: 12579,
  pink: 12578,
  red: 11049,
  yellow: 11048,
  darkblue: 17362,
  darkgreen: 17359,
  green: 17361,
};

// Also offer the simple white glossy mug (Product ID 19)
const WHITE_MUG_PRODUCT_ID = 19;
const WHITE_MUG_VARIANT = 1320; // 11oz white

// Print area dimensions for 11oz mug wrap
const MUG_WIDTH = 2700;
const MUG_HEIGHT = 1050;

// CORS headers for cross-origin requests
const corsHeaders = {
  "Access-Control-Allow-Origin": "*",
  "Access-Control-Allow-Methods": "GET, POST, OPTIONS",
  "Access-Control-Allow-Headers": "Content-Type, Authorization",
};

export async function handler(event, context) {
  // Handle OPTIONS preflight requests
  if (event.httpMethod === "OPTIONS") {
    return {
      statusCode: 204,
      headers: corsHeaders,
      body: "",
    };
  }

  const { got } = await import("got");
  const API = "https://api.printful.com";
  const headers = {
    Authorization: "Bearer " + printfulKey,
    "Content-Type": "application/json",
  };

  // 🅰️ GET: Return available colors OR product by code
  if (event.httpMethod === "GET") {
    // If ?code=PRODUCTCODE, return that specific mug's metadata
    // Supports both +CODE (with prefix) and CODE (without)
    let productCode = event.queryStringParameters?.code;
    if (productCode) {
      // Strip + prefix if present (print codes use + like # for paintings)
      productCode = productCode.replace(/^\+/, "");
      
      const { getProduct } = await import("../../backend/products.mjs");
      const product = await getProduct(productCode);
      if (product) {
        return respond(200, {
          code: "+" + product.code, // Include + prefix in response
          rawCode: product.code,    // Also provide raw code for internal use
          sourceCode: product.source?.code,
          via: product.source?.via,
          color: product.variant,
          preview: product.preview,
          createdAt: product.createdAt,
        });
      } else {
        return respond(404, { error: "Mug not found" });
      }
    }
    
    return respond(200, { 
      colors: ["white", ...Object.keys(MUG_VARIANTS)],
      product: "Ceramic Mug 11oz",
      dimensions: { width: MUG_WIDTH, height: MUG_HEIGHT }
    });
  } else if (event.httpMethod === "POST") {
    const stripe = new Stripe(stripeKey);

    // 1. 🔵 Begin a new mug order.
    if (event.queryStringParameters.new === "true") {
      // Get color from query params (default: white)
      const color = (event.queryStringParameters.color || "white").toLowerCase();
      
      // Extract painting code from pixels param (e.g., "9vg.png" -> "9vg")
      const pixelsParam = event.queryStringParameters.pixels;
      const sourceCode = pixelsParam.replace(/\.png$/i, "");
      
      // Get KidLisp source code if provided (e.g., "cow" for $cow from kidlisp.com)
      const viaCode = event.queryStringParameters.via;
      
      // 📦 Check for cached product first
      let productRecord = null;
      let cachedWebpUrl = null;
      try {
        const source = { type: "painting", code: sourceCode };
        if (viaCode) source.via = viaCode; // Track KidLisp source
        
        const { product: existingProduct, isNew } = await findOrCreateProduct({
          source,
          product: "mug",
          variant: color,
        });
        productRecord = existingProduct;
        
        // If we have a cached WebP, use it directly
        if (!isNew && existingProduct.preview) {
          cachedWebpUrl = existingProduct.preview;
          console.log(`☕ Using cached WebP: ${cachedWebpUrl}`);
        }
      } catch (e) {
        console.warn("☕ Product cache lookup failed:", e.message);
      }
      
      // Determine product ID and variant based on color
      let productId, variant;
      if (color === "white") {
        productId = WHITE_MUG_PRODUCT_ID;
        variant = WHITE_MUG_VARIANT;
      } else {
        productId = MUG_PRODUCT_ID;
        variant = MUG_VARIANTS[color];
      }
      
      if (!variant) {
        return respond(400, { 
          message: `Invalid color: ${color}. Available: white, ${Object.keys(MUG_VARIANTS).join(", ")}` 
        });
      }

      let mockupUrl = null;
      let allMockupUrls = []; // All mockup views for Stripe carousel
      let imageUrl;

      // Parse the input slug or url
      // Note: apiDomain for client-side fallback images (can be localhost in dev)
      // printfulDomain MUST be production URL - Printful can't access localhost
      const apiDomain = dev ? "https://localhost:8888" : "https://aesthetic.computer";
      const printfulDomain = "https://aesthetic.computer"; // Always production for Printful
      if (!event.queryStringParameters.pixels.startsWith("https://")) {
        // Use /api/pixel to resize painting for mug wrap dimensions
        // Use contain-clear to preserve transparency (mug color shows through)
        // Printful needs a publicly accessible URL
        
        // 🔲 Build QR code + KidLisp label params for print file
        // These get baked into the flat image that's printed on the physical mug
        let printParams = [];
        if (productRecord?.code) {
          // QR code links to mug~+productCode
          const qrSlug = `mug~+${productRecord.code}`;
          printParams.push(`qr=${encodeURIComponent(qrSlug)}`);
          printParams.push(`qrpos=bottom-right`);
        }
        if (viaCode) {
          // KidLisp.com label rotated on the side
          printParams.push(`via=${encodeURIComponent(viaCode)}`);
        }
        const queryString = printParams.length > 0 ? `?${printParams.join("&")}` : "";
        
        imageUrl = `${printfulDomain}/api/pixel/${MUG_WIDTH}x${MUG_HEIGHT}:contain-clear/${event.queryStringParameters.pixels}${queryString}`;
      } else {
        return respond(500, {
          message: "No external image URLs allowed.",
        });
      }

      // 🔸 Generate a mockup image via Printful's mockup generator API
      try {
        const task = await got.post(
          `${API}/mockup-generator/create-task/${productId}`,
          {
            headers,
            json: {
              variant_ids: [variant],
              files: [
                {
                  placement: "default",
                  image_url: imageUrl,
                  position: {
                    area_width: MUG_WIDTH,
                    area_height: MUG_HEIGHT,
                    width: MUG_WIDTH,
                    height: MUG_HEIGHT,
                    top: 0,
                    left: 0,
                  },
                },
              ],
              format: "png",
            },
          },
        );

        const taskResult = JSON.parse(task.body)?.result;

        // Poll for mockup completion (max 24 seconds)
        const maxAttempts = 12;
        let attempt = 0;
        
        while (attempt < maxAttempts) {
          const statusResponse = await got.get(
            `${API}/mockup-generator/task/?task_key=${taskResult?.task_key}`,
            { headers },
          );
          const statusResult = JSON.parse(statusResponse.body);

          if (statusResult.result.status === "completed") {
            const mainMockup = statusResult.result.mockups[0];
            console.log("☕ Mockup generated:", mainMockup?.mockup_url);
            
            // Collect mockup URLs - main is typically "right" view (handle on right)
            const rightView = mainMockup?.mockup_url;
            
            // Find front and left views from extras
            let frontView = null;
            let leftView = null;
            
            if (mainMockup?.extra && Array.isArray(mainMockup.extra)) {
              for (const extraView of mainMockup.extra) {
                if (extraView.url) {
                  const title = (extraView.title || "").toLowerCase();
                  if (title.includes("front")) {
                    frontView = extraView.url;
                  } else if (title.includes("left")) {
                    leftView = extraView.url;
                  }
                }
              }
            }
            
            // Build 4-frame array for smooth spinning loop:
            // front → right → front → left → (loops back to front)
            // Front first so thumbnail/preview shows the main design
            mockupUrl = frontView || rightView;
            if (rightView && frontView && leftView) {
              allMockupUrls = [frontView, rightView, frontView, leftView];
              console.log("☕ Built 4-frame loop: front → right → front → left");
            } else if (rightView && frontView) {
              allMockupUrls = [frontView, rightView];
              console.log("☕ Built 2-frame loop: front → right");
            } else {
              allMockupUrls = [frontView || rightView];
              console.log("☕ Single frame only");
            }
            
            console.log("☕ Generated", allMockupUrls.length, "mockup frames");
            console.log("☕ MOCKUP URLs:", JSON.stringify(allMockupUrls));
            // Save URLs for testing
            try {
              const fs = await import("fs");
              fs.writeFileSync("/tmp/mug-mockup-urls.json", JSON.stringify(allMockupUrls, null, 2));
            } catch (e) { /* ignore */ }
            break;
          } else if (statusResult.result.status === "failed") {
            console.error("☕ Mockup generation failed:", statusResult);
            break;
          } else {
            await new Promise((res) => setTimeout(res, 2000)); // wait 2 seconds
            attempt += 1;
          }
        }

        if (!mockupUrl) {
          // Fallback to simple scaled image if mockup generation fails/times out
          console.warn("☕ Mockup generation timed out, using fallback");
          mockupUrl = `${apiDomain}/api/pixel/400x150:contain/${event.queryStringParameters.pixels}`;
        }
      } catch (error) {
        console.error("☕ Mockup generation error:", error.message);
        // Fallback to simple scaled image
        mockupUrl = `${apiDomain}/api/pixel/400x150:contain/${event.queryStringParameters.pixels}`;
      }

      // 🔍 Preview mode: return mockup URLs without creating Stripe checkout
      // Usage: /api/mug?new=true&pixels=CODE.png&color=blue&preview=true
      if (event.queryStringParameters.preview === "true") {
        const webpDomain = dev ? "https://localhost:8888" : "https://aesthetic.computer";
        const size = parseInt(event.queryStringParameters.size || "600");
        let animatedWebpUrl = cachedWebpUrl; // Use cached if available
        
        if (!animatedWebpUrl && allMockupUrls.length > 1) {
          const encodedUrls = allMockupUrls.slice(0, 4).map(u => encodeURIComponent(u)).join(",");
          
          // 📽️ Preview animation - no QR code needed here since QR is baked into the print file
          // The KidLisp label is still useful for attribution in the preview
          let webpParams = `urls=${encodedUrls}&delay=800&size=${size}`;
          if (viaCode) {
            webpParams += `&via=${encodeURIComponent(viaCode)}`;
          }
          animatedWebpUrl = `${webpDomain}/api/mockup-webp?${webpParams}`;
          
          // 📦 Cache the WebP preview to S3 (async, don't wait)
          if (productRecord && event.queryStringParameters.cache !== "false") {
            (async () => {
              try {
                const { default: got } = await import("got");
                const response = await got(animatedWebpUrl, { 
                  responseType: "buffer",
                  https: { rejectUnauthorized: !dev },
                  timeout: { request: 30000 },
                });
                const s3Url = await cacheWebpPreview(productRecord.code, response.body);
                // Also save printful data
                await updateProduct(productRecord.code, {
                  "printful.productId": productId,
                  "printful.variantId": variant,
                });
                console.log(`☕ Cached WebP to S3: ${s3Url}`);
              } catch (e) {
                console.warn("☕ Failed to cache WebP:", e.message);
              }
            })();
          }
        }
        
        return respond(200, {
          mockupUrl,
          allMockupUrls,
          animatedWebpUrl,
          imageUrl,
          color,
          variant,
          productCode: productRecord?.code ? "+" + productRecord.code : null,
          rawProductCode: productRecord?.code,
          cached: !!cachedWebpUrl,
        });
      }

      const post = JSON.parse(event.body);
      const domain = dev
        ? "https://localhost:8888"
        : "https://aesthetic.computer";

      // Pricing for ceramic mug - competitive at $18 total
      const quantity = post.quantity || 1;
      const unitAmount = 1800 * quantity; // $18.00 total per mug (all-inclusive)
      const finalAmount = unitAmount;

      // Branded product name: "red mug of oyn · 11oz Ceramic" (no # for anonymous codes)
      // Or with KidLisp source: "red mug of oyn in $bop · 11oz Ceramic"
      // Hashed painting codes (user paintings) still get # prefix
      // Color stays lowercase, code stays original case (case-sensitive!)
      // viaCode already declared above
      // Anonymous codes are typically 8+ chars (e.g., CPUQnQi2), user painting codes are shorter (e.g., oyn)
      const isAnonymousCode = sourceCode.length >= 8;
      const codePrefix = isAnonymousCode ? '' : '#';
      let productName = `${color} mug of ${codePrefix}${sourceCode}`;
      if (viaCode) {
        productName += ` in $${viaCode}`;
      }
      productName += ` · 11oz Ceramic`;
      let name = productName;
      if (quantity > 1) name += " (" + quantity + " copies)";

      // Create animated WebP from mockup views for Stripe preview
      // WebP has better quality + transparency, smaller file size than GIF
      // Always use production URL so Stripe can access it
      let productImages = [mockupUrl];
      if (allMockupUrls.length > 1) {
        // Build animated WebP URL from mockup views (always use production domain)
        const webpDomain = "https://aesthetic.computer";
        const encodedUrls = allMockupUrls.slice(0, 4).map(u => encodeURIComponent(u)).join(",");
        
        // 📽️ Stripe preview image - no QR overlay needed since QR is baked into print file
        // KidLisp label is still useful for attribution
        let webpParams = `urls=${encodedUrls}&delay=800&size=600`;
        if (viaCode) {
          webpParams += `&via=${encodeURIComponent(viaCode)}`;
        }
        const animatedWebpUrl = `${webpDomain}/api/mockup-webp?${webpParams}`;
        productImages = [animatedWebpUrl];
        console.log("☕ Animated WebP URL:", animatedWebpUrl);
      }

      const stripeCheckout = {
        line_items: [
          {
            price_data: {
              currency: "usd",
              product_data: { name, images: productImages },
              unit_amount: finalAmount,
            },
            quantity: 1,
          },
        ],
        metadata: {
          productName,
          productType: "mug",
          productId: productId.toString(),
          color,
          variant: variant.toString(),
          quantity: quantity.toString(),
          mockupUrl,
          imageUrl,
        },
        mode: "payment",
        shipping_address_collection: { allowed_countries: ["US"] },
        success_url: `${domain}/${post.slug}?notice=success`,
        cancel_url: `${domain}/${post.slug}?notice=cancel`,
        automatic_tax: { enabled: true },
        custom_text: {
          submit: {
            message: `☕ Check your email and expect your mug${
              quantity > 1 ? "s" : ""
            } to arrive soon!`,
          },
        },
      };

      // Add customer_email if user is logged in
      const user = await authorize(event.headers);
      if (user?.email) stripeCheckout.customer_email = user.email;

      const session = await stripe.checkout.sessions.create(stripeCheckout);
      return respond(200, { location: session.url });
    } else {
      // 2. 🔵 Respond to webhook
      const sig = event.headers["stripe-signature"];
      const secret = dev
        ? process.env.STRIPE_ENDPOINT_DEV_SECRET
        : process.env.STRIPE_ENDPOINT_MUG_SECRET; // Need to set up separate webhook for mug
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

        if (session.payment_status === "paid") {
          console.log("✅ Mug order paid!");
          const metadata = hookEvent.data.object.metadata;
          const productName = metadata.productName;
          const quantity = parseInt(metadata.quantity) || 1;
          const variant = parseInt(metadata.variant);
          const itemImage = metadata.imageUrl;
          const mockupUrl = metadata.mockupUrl;

          // Build Printful order
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
          if (shipping.address.line2) recipient.address2 = shipping.address.line2;

          const order = {
            external_id: session.payment_intent,
            recipient,
            items: [
              {
                name: productName,
                variant_id: variant,
                quantity,
                files: [
                  {
                    type: "default", // Full wrap placement
                    url: itemImage,
                    position: {
                      area_width: MUG_WIDTH,
                      area_height: MUG_HEIGHT,
                      width: MUG_WIDTH,
                      height: MUG_HEIGHT,
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

          try {
            const orderResponse = await got.post(
              `${API}/orders?confirm=${dev ? false : true}`,
              { headers, json: order },
            );

            const orderResult = JSON.parse(orderResponse.body);

            if (orderResult && orderResult.code === 200 && orderResult.result) {
              const emailSent = await email({
                to: session.customer_details.email,
                subject:
                  quantity > 1
                    ? "your mugs are coming! ☕"
                    : "your mug is coming! ☕",
                html: `
                <img src="${mockupUrl}" width="250">
                <p>thank you for your order!</p>
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
            console.log("Printful order failed, refunding customer...");
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
                  your mug order has failed due to an error,<br>
                  ${refunded ? "and you have been refunded." : "reply here to be refunded."}
                </p>
                <p>deeply sorry about that!</p>
                <b><a href="https://aesthetic.computer">aesthetic.computer</a></b>
                <br>
                <code>${session.payment_intent.replace("pi_", "")}</code>
                `,
            });

            return respond(500, { message: error.message + " - Refund attempted." });
          }
        } else {
          return respond(500, { message: "Order payment failed." });
        }
      } else {
        return respond(400, { message: `Unhandled webhook event: ${hookEvent.type}` });
      }
    }
  } else {
    return respond(405, { message: "Method Not Allowed" });
  }
}
