// Shop Draft Order API, 26.05.16
// Create a Shopify draft order with a sliding-scale price.
// Used by the storefront theme on whitelisted products (e.g. "1 hour of
// remote help") to charge the price the customer chose with a slider.
//
// Credentials are pulled from MongoDB the same way as ../shop.mjs.

import { respond } from "../../backend/http.mjs";
import { connect } from "../../backend/database.mjs";

// Products that accept sliding-scale pricing, with per-handle bounds.
// Anything outside this list is rejected — no charging $1 for a $500 bike.
const SLIDING_SCALE = {
  "1-hour-of-remote-help-from-jeffrey": {
    title: "1 hour of remote help from jeffrey",
    minPrice: 25,
    maxPrice: 500,
  },
};

let cachedCredentials = null;

async function getShopifyCredentials() {
  if (cachedCredentials) return cachedCredentials;
  const { db } = await connect();
  const secrets = await db.collection("secrets").findOne({ _id: "shopify" });
  if (!secrets) throw new Error("Shopify credentials not found in database");
  cachedCredentials = {
    domain: secrets.storeDomain,
    token: secrets.adminAccessToken,
  };
  return cachedCredentials;
}

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(200, {});
  if (event.httpMethod !== "POST") {
    return respond(405, { message: "Method Not Allowed" });
  }

  let body;
  try {
    body = JSON.parse(event.body || "{}");
  } catch {
    return respond(400, { message: "Invalid JSON body" });
  }

  const { productHandle, price } = body;

  const config = SLIDING_SCALE[productHandle];
  if (!config) {
    return respond(400, {
      message: "Product is not on the sliding-scale allowlist",
    });
  }

  const priceNum = Number(price);
  if (!Number.isFinite(priceNum)) {
    return respond(400, { message: "Price must be a number" });
  }
  if (priceNum < config.minPrice || priceNum > config.maxPrice) {
    return respond(400, {
      message: `Price must be between $${config.minPrice} and $${config.maxPrice}`,
    });
  }

  try {
    const { domain, token } = await getShopifyCredentials();

    const draftRes = await fetch(
      `https://${domain}/admin/api/2024-10/draft_orders.json`,
      {
        method: "POST",
        headers: {
          "X-Shopify-Access-Token": token,
          "Content-Type": "application/json",
        },
        body: JSON.stringify({
          draft_order: {
            line_items: [
              {
                title: config.title,
                price: priceNum.toFixed(2),
                quantity: 1,
                requires_shipping: false,
                taxable: true,
              },
            ],
            note: `Sliding-scale via ${productHandle}`,
            tags: "sliding-scale",
            use_customer_default_address: false,
          },
        }),
      },
    );

    if (!draftRes.ok) {
      const errText = await draftRes.text();
      console.error("Shopify draft_orders error:", draftRes.status, errText);
      return respond(502, {
        message: "Could not create draft order",
        detail: errText,
      });
    }

    const data = await draftRes.json();
    const invoiceUrl = data.draft_order?.invoice_url;
    if (!invoiceUrl) {
      return respond(502, {
        message: "Draft order created but no invoice URL returned",
      });
    }

    return respond(200, {
      invoiceUrl,
      draftOrderId: data.draft_order.id,
      price: priceNum,
    });
  } catch (err) {
    console.error("shop-draft-order failed:", err);
    return respond(500, { message: err.message || String(err) });
  }
}
