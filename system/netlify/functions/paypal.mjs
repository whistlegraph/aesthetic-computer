// PayPal, 25.01.13
// 💰 Creates PayPal checkout orders for giving/supporting AC
// Endpoint: /api/paypal

import { respond } from "../../backend/http.mjs";
import { connect } from "../../backend/database.mjs";

const dev = process.env.CONTEXT === "dev";

// PayPal API URLs
const PAYPAL_API_URL = dev 
  ? "https://api-m.sandbox.paypal.com"
  : "https://api-m.paypal.com";

// Supported currencies
const currencies = {
  usd: { symbol: "$", min: 1, max: 2500 },
  eur: { symbol: "€", min: 1, max: 2500 },
  gbp: { symbol: "£", min: 1, max: 2500 },
};

// Cache credentials in memory for warm function invocations
let cachedCredentials = null;

async function getPayPalCredentials() {
  if (cachedCredentials) return cachedCredentials;
  
  // First try MongoDB secrets
  try {
    const { db } = await connect();
    const secrets = await db.collection("secrets").findOne({ _id: "paypal" });
    
    if (secrets) {
      cachedCredentials = {
        clientId: secrets.clientId,
        clientSecret: secrets.clientSecret,
      };
      return cachedCredentials;
    }
  } catch (e) {
    console.log("MongoDB not available, falling back to env vars");
  }
  
  // Fallback to environment variables
  const clientId = process.env.PAYPAL_CLIENT_ID;
  const clientSecret = process.env.PAYPAL_CLIENT_SECRET;
  
  if (!clientId || !clientSecret) {
    throw new Error("PayPal credentials not configured");
  }
  
  cachedCredentials = { clientId, clientSecret };
  return cachedCredentials;
}

// Get OAuth 2.0 access token
async function getAccessToken() {
  const { clientId, clientSecret } = await getPayPalCredentials();
  
  const response = await fetch(`${PAYPAL_API_URL}/v1/oauth2/token`, {
    method: "POST",
    headers: {
      Authorization: `Basic ${Buffer.from(`${clientId}:${clientSecret}`).toString("base64")}`,
      "Content-Type": "application/x-www-form-urlencoded",
    },
    body: "grant_type=client_credentials",
  });
  
  if (!response.ok) {
    const error = await response.text();
    throw new Error(`PayPal auth failed: ${error}`);
  }
  
  const data = await response.json();
  return data.access_token;
}

// Create a PayPal order
async function createOrder(amount, currency, returnUrl, cancelUrl) {
  const accessToken = await getAccessToken();
  
  const response = await fetch(`${PAYPAL_API_URL}/v2/checkout/orders`, {
    method: "POST",
    headers: {
      Authorization: `Bearer ${accessToken}`,
      "Content-Type": "application/json",
    },
    body: JSON.stringify({
      intent: "CAPTURE",
      purchase_units: [{
        amount: {
          currency_code: currency.toUpperCase(),
          value: amount.toFixed(2),
        },
        description: "Give to Aesthetic Computer",
      }],
      application_context: {
        brand_name: "Aesthetic Computer",
        landing_page: "NO_PREFERENCE",
        user_action: "PAY_NOW",
        return_url: returnUrl,
        cancel_url: cancelUrl,
      },
    }),
  });
  
  if (!response.ok) {
    const error = await response.text();
    throw new Error(`PayPal create order failed: ${error}`);
  }
  
  return response.json();
}

// Capture a PayPal order (after user approval)
async function captureOrder(orderId) {
  const accessToken = await getAccessToken();
  
  const response = await fetch(`${PAYPAL_API_URL}/v2/checkout/orders/${orderId}/capture`, {
    method: "POST",
    headers: {
      Authorization: `Bearer ${accessToken}`,
      "Content-Type": "application/json",
    },
  });
  
  if (!response.ok) {
    const error = await response.text();
    throw new Error(`PayPal capture failed: ${error}`);
  }
  
  return response.json();
}

// Get order details
async function getOrder(orderId) {
  const accessToken = await getAccessToken();
  
  const response = await fetch(`${PAYPAL_API_URL}/v2/checkout/orders/${orderId}`, {
    method: "GET",
    headers: {
      Authorization: `Bearer ${accessToken}`,
      "Content-Type": "application/json",
    },
  });
  
  if (!response.ok) {
    const error = await response.text();
    throw new Error(`PayPal get order failed: ${error}`);
  }
  
  return response.json();
}

export async function handler(event, context) {
  // Handle CORS preflight
  if (event.httpMethod === "OPTIONS") {
    return respond(200, {});
  }

  const path = event.path.replace(/^\/api\/paypal\/?/, "");

  try {
    // POST /api/paypal - Create order
    if (event.httpMethod === "POST" && !path) {
      const body = JSON.parse(event.body || "{}");
      const currency = (body.currency || "usd").toLowerCase();
      const currencyConfig = currencies[currency] || currencies.usd;
      const amount = parseFloat(body.amount) || 25;

      // Validate amount
      if (amount < currencyConfig.min || amount > currencyConfig.max) {
        return respond(400, { 
          error: `Amount must be between ${currencyConfig.min} and ${currencyConfig.max} ${currency.toUpperCase()}` 
        });
      }

      const giveBaseUrl = dev 
        ? `https://${event.headers.host}` 
        : "https://give.aesthetic.computer";
      
      const order = await createOrder(
        amount,
        currency,
        `${giveBaseUrl}/?paypal_success=1`,
        `${giveBaseUrl}/?paypal_cancel=1`
      );

      // Find the approval URL
      const approvalUrl = order.links?.find(l => l.rel === "approve")?.href;

      return respond(200, {
        orderId: order.id,
        status: order.status,
        approvalUrl,
      });
    }

    // POST /api/paypal/capture - Capture approved order
    if (event.httpMethod === "POST" && path === "capture") {
      const body = JSON.parse(event.body || "{}");
      const orderId = body.orderId;

      if (!orderId) {
        return respond(400, { error: "orderId required" });
      }

      const result = await captureOrder(orderId);
      
      return respond(200, {
        status: result.status,
        payer: result.payer,
        purchase_units: result.purchase_units,
      });
    }

    // GET /api/paypal/order/:id - Get order details
    if (event.httpMethod === "GET" && path.startsWith("order/")) {
      const orderId = path.replace("order/", "");
      
      if (!orderId) {
        return respond(400, { error: "orderId required" });
      }

      const order = await getOrder(orderId);
      
      return respond(200, {
        id: order.id,
        status: order.status,
        amount: order.purchase_units?.[0]?.amount,
      });
    }

    return respond(404, { error: "Not found" });
  } catch (error) {
    console.error("PayPal error:", error);
    return respond(500, { error: error.message });
  }
}
