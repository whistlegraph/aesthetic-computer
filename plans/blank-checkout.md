# Plan: `blank` — AC Native Laptop Product Page & Checkout

## What is a Blank?

A **Blank** is a surplus/refurbished Lenovo ThinkPad Yoga 11e (Gen 4 or Gen 5) running AC Native OS — a pared-down creative computing environment with only stable, permanent commands. Like a blank tape or blank disc, it's an empty medium waiting to be filled.

**Target price:** ~$128 (covers hardware sourcing at ~$30–75/unit + AC OS flashing + shipping)

---

## Architecture

### Existing Patterns to Follow

The `mug.js` and `print.js` functions establish a clean Stripe checkout pattern:

```
piece (blank.mjs)  →  POST /api/blank?new=true  →  Stripe checkout session
                                                         ↓
                   ←  redirect to Stripe hosted checkout
                                                         ↓
webhook (POST /api/blank with stripe-signature)  →  order confirmation + email
```

**Key pattern from mug/print:**
- `POST ?new=true` → creates Stripe checkout session, returns `session.url`
- `POST` with `stripe-signature` header → webhook handler for `checkout.session.completed`
- Uses `respond()` from `backend/http.mjs`
- Uses `email()` from `backend/email.mjs`
- Uses `authorize()` from `backend/authorization.mjs` (optional, for prefilling email)
- Dev/prod key switching: `dev ? STRIPE_API_TEST_PRIV_KEY : STRIPE_API_PRIV_KEY`

### Differences from Mug/Print

| | Mug/Print | Blank |
|---|---|---|
| Fulfillment | Printful POD (automated) | Manual (flash OS, ship laptop) |
| Inventory | Unlimited (on-demand) | Limited stock |
| Product image | Generated mockup | Static photo(s) of the laptop |
| Shipping | Printful handles | We handle (USPS/UPS) |
| Price | $10–$18 | ~$128 |

Because fulfillment is manual, the webhook just needs to:
1. Record the order in MongoDB
2. Send confirmation email to buyer
3. Notify us (email or webhook to Slack/Discord) that an order came in

---

## Files to Create / Modify

### 1. `system/netlify/functions/blank.mjs` — API endpoint

New serverless function following the mug/print pattern:

```
GET  /api/blank          → product info (price, stock count, description)
POST /api/blank?new=true → create Stripe checkout session
POST /api/blank          → Stripe webhook (order confirmation)
```

**Checkout session config:**
- `mode: "payment"`
- `shipping_address_collection: { allowed_countries: ["US"] }` (expand later)
- Product: "The Blank — AC Native Laptop"
- Price: configured in code or via Stripe product ID
- `metadata: { type: "blank", model: "yoga-11e-gen5" }`
- Success URL: `aesthetic.computer/blank:thanks`
- Cancel URL: `aesthetic.computer/blank`

**Webhook handler:**
- Verify signature with `STRIPE_ENDPOINT_BLANK_SECRET`
- Store order in MongoDB `orders` collection (or `blank-orders`)
- Send buyer confirmation email
- Send internal notification email to us

### 2. `system/public/aesthetic.computer/disks/blank.mjs` — The piece

Replace the current WebGPU stress test with the Blank product page.

**Piece behavior:**
- `boot` — fetch product info from `/api/blank`, set up Buy button
- `paint` — render product page:
  - Product image/photo of the Blank laptop
  - "The Blank" title
  - Description text (what it is, what it runs, what comes with it)
  - Price
  - Stock indicator (if limited)
  - **BUY** button
- `act` — handle Buy button click → `POST /api/blank?new=true` → redirect to Stripe
- Optional: `preview` for link unfurling / social sharing

**UI approach:**
- Keep it minimal — the page should feel like the product itself
- Could start with just text + a buy button (very blank)
- Photo(s) can be loaded from DO Spaces CDN

### 3. Environment Variables (netlify.toml / .env)

```
STRIPE_ENDPOINT_BLANK_SECRET=whsec_...   # Webhook signing secret for /api/blank
```

No new Stripe keys needed — reuses existing `STRIPE_API_PRIV_KEY` / `STRIPE_API_TEST_PRIV_KEY`.

### 4. Stripe Dashboard Setup

- Create a new Product: "The Blank — AC Native Laptop"
- Create a Price: $128.00 USD (one-time)
- Register webhook endpoint: `https://aesthetic.computer/api/blank`
  - Events: `checkout.session.completed`
- Note the webhook signing secret → `STRIPE_ENDPOINT_BLANK_SECRET`

### 5. MongoDB Schema

```javascript
// Collection: blank-orders
{
  _id: ObjectId,
  stripeSessionId: string,
  paymentIntentId: string,
  customer: {
    email: string,
    name: string,
    address: { /* shipping address from Stripe */ }
  },
  product: {
    model: "yoga-11e-gen5",  // or "yoga-11e-gen4"
    price: 12800,            // cents
  },
  status: "paid" | "preparing" | "shipped" | "delivered",
  trackingNumber: string | null,
  notes: string | null,
  createdAt: Date,
  updatedAt: Date,
}
```

---

## Implementation Order

1. **Stripe Dashboard** — Create product + price + webhook endpoint
2. **`blank.mjs` function** — API endpoint (checkout session + webhook)
3. **`blank.mjs` piece** — Product page with Buy button
4. **Test locally** — `stripe listen --forward-to "https://localhost:8888/api/blank"`
5. **Deploy + verify** — Test with Stripe test mode end-to-end
6. **Go live** — Switch to production keys, source first batch of laptops

---

## Pricing: Sliding Scale

Follow the `give.js` pattern — let the buyer choose what to pay within a range.

```javascript
const pricing = {
  usd: { min: 9600, suggested: 12800, max: 51200 },   // $96–$512, suggested $128
  dkk: { min: 67200, suggested: 89600, max: 358400 },  // ~672–3584 kr, suggested ~896 kr
};
```

**Tiers:**

| Amount | What you get |
|--------|-------------|
| **$96** (minimum) | The Blank — laptop + AC Native OS |
| **$128** (suggested) | The Blank + supports AC development |
| **$512** (max) | The Blank + in-person meeting/tutorial in LA |

- **$96** — covers hardware sourcing + OS flashing + shipping
- **$128** — suggested default, surplus supports ongoing AC work
- **$512** — includes an in-person session in Los Angeles: setup walkthrough, tutorial on making pieces, Q&A with Jeffrey
- The piece UI shows a slider (or discrete tier buttons) to choose
- $512 tier triggers an additional email/scheduling flow for the in-person meeting
- Stripe `price_data.unit_amount` is set dynamically (same as `give.js`)
- Metadata tracks the tier: `{ tier: "blank" | "blank+support" | "blank+tutorial" }`

## Shipping & Delivery Options

**Shipping options in Stripe checkout:**

| Option | Countries | Cost |
|--------|-----------|------|
| Hand delivery | US (local) / Denmark | Free |
| USPS Priority Mail | US | ~$10–15 (flat rate padded envelope) |
| International (PostNord / USPS) | DK + others | ~$20–30 |

- `shipping_address_collection.allowed_countries: ["US", "DK"]` to start
- `shipping_options` array in checkout session config for delivery method selection
- Hand delivery adds a note field: "We'll reach out to arrange pickup/delivery"
- Can expand countries later as demand shows up

**Hand delivery flow:**
- Buyer selects "Hand delivery (free)" at checkout
- We get notified, reach out via email to arrange
- Perfect for workshops, events, or local community

## Currencies

Reuse the `give.js` dual-currency pattern:

```javascript
const currencies = {
  usd: { symbol: "$", min: 9600, max: 51200, suggested: 12800 },
  dkk: { symbol: "kr", min: 67200, max: 358400, suggested: 89600 },
};
```

The piece auto-detects or lets the buyer toggle USD/DKK.

---

## Open Questions

- [ ] How many units for the first batch?
- [ ] Product photography — shoot the actual flashed laptop?
- [ ] Should `blank:thanks` be a separate piece or a param/state of `blank.mjs`?
- [ ] Inventory tracking — manual count in env var, or MongoDB stock field?
- [ ] Should there be a waitlist if out of stock?
- [ ] Exact sliding scale range — what's the true cost floor per unit?
