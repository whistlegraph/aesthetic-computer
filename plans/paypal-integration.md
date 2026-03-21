# PayPal Integration Plan

## Overview
Integrate PayPal as a payment option on the give page (give.aesthetic.computer) alongside existing USD (Stripe), DKK, and Crypto options.

## Account Details
- **PayPal Account**: mail@aesthetic.computer
- **API Environment**: Production (api-m.paypal.com)

## Credentials Storage
| Location | Purpose |
|----------|---------|
| `aesthetic-computer-vault/.env` | Local development, secure storage |
| MongoDB secrets collection | Netlify functions runtime access |

### Environment Variables
```
PAYPAL_CLIENT_ID=<stored in vault>
PAYPAL_CLIENT_SECRET=<stored in vault>
PAYPAL_API_URL=https://api-m.paypal.com
```

## API Architecture

### PayPal Orders API v2
- **Auth**: OAuth 2.0 (Client ID + Secret → Bearer Token)
- **Create Order**: `POST /v2/checkout/orders`
- **Capture Payment**: `POST /v2/checkout/orders/{id}/capture`

### Flow
```
1. User selects amount on give page
2. Frontend calls /api/paypal (Netlify function)
3. paypal.mjs creates order via PayPal API
4. Returns approval URL
5. User redirected to PayPal to approve
6. PayPal redirects back to give.aesthetic.computer?paypal_success=1
7. Frontend calls /api/paypal/capture with order ID
8. paypal.mjs captures payment
```

## Implementation Checklist

### ✅ Completed
- [x] PayPal tab in currency selector
- [x] PayPal section UI (header, button, email)
- [x] PayPal.me direct link button
- [x] Copyable email address
- [x] Multi-language support (EN, DA, DE, ES, ZH)
- [x] PayPal QR code integration
- [x] CSS styling (blue theme #0070ba)
- [x] Give button "PP" cycling
- [x] Credentials stored in vault

### 🔄 In Progress
- [ ] paypal.mjs Netlify endpoint
- [ ] MongoDB secrets sync

### ❌ Not Started
- [ ] Custom amount input (like Stripe)
- [ ] Payment capture flow
- [ ] Success/failure handling on redirect
- [ ] Transaction logging

## Netlify Function: paypal.mjs

### Endpoints
```javascript
// Create order
POST /api/paypal
Body: { amount: "10.00", currency: "USD" }
Returns: { id: "ORDER_ID", approval_url: "https://paypal.com/..." }

// Capture payment (optional, if using redirect flow)
POST /api/paypal/capture
Body: { order_id: "ORDER_ID" }
Returns: { status: "COMPLETED", ... }
```

### Implementation Pattern
```javascript
// Follow give.js pattern
import { MongoClient } from "mongodb";

const getSecrets = async () => {
  const client = await MongoClient.connect(process.env.MONGODB_URI);
  const db = client.db("aesthetic");
  return db.collection("secrets").findOne({ name: "paypal" });
};

const getAccessToken = async (clientId, clientSecret) => {
  const response = await fetch("https://api-m.paypal.com/v1/oauth2/token", {
    method: "POST",
    headers: {
      Authorization: `Basic ${Buffer.from(`${clientId}:${clientSecret}`).toString("base64")}`,
      "Content-Type": "application/x-www-form-urlencoded",
    },
    body: "grant_type=client_credentials",
  });
  const data = await response.json();
  return data.access_token;
};

const createOrder = async (accessToken, amount, currency = "USD") => {
  const response = await fetch("https://api-m.paypal.com/v2/checkout/orders", {
    method: "POST",
    headers: {
      Authorization: `Bearer ${accessToken}`,
      "Content-Type": "application/json",
    },
    body: JSON.stringify({
      intent: "CAPTURE",
      purchase_units: [{
        amount: {
          currency_code: currency,
          value: amount,
        },
        description: "Aesthetic Computer Contribution",
      }],
      application_context: {
        return_url: "https://give.aesthetic.computer?paypal_success=1",
        cancel_url: "https://give.aesthetic.computer?paypal_cancel=1",
        brand_name: "Aesthetic Computer",
        user_action: "PAY_NOW",
      },
    }),
  });
  return response.json();
};
```

## Limitations
- **No Monthly Subscriptions**: Orders API is one-time only
  - Would need PayPal Subscriptions API for recurring
  - User acknowledged this is acceptable
- **Currency Support**: USD, EUR, GBP, etc. (no DKK via PayPal API)

## QR Code
- **Asset URL**: `https://assets.aesthetic.computer/images/paypal-qrcode.png`
- **Links to**: PayPal.me/aestheticcomputer
- **Purpose**: Quick mobile scanning for donations

## Security Notes
1. Never commit credentials to main repo
2. Use vault for local dev
3. Use MongoDB secrets for production
4. Rotate credentials periodically
5. Monitor for unauthorized transactions

## Testing
1. Use PayPal Sandbox for development testing
2. Create sandbox accounts at developer.paypal.com
3. Test with small amounts ($0.01) in production

## Related Files
- `system/public/give.aesthetic.computer/index.html` - Give page UI
- `system/netlify/functions/give.js` - Stripe endpoint (reference)
- `system/netlify/functions/paypal.mjs` - PayPal endpoint (to create)
- `aesthetic-computer-vault/.env` - Credentials storage
