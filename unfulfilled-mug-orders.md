# Unfulfilled Mug Orders

## Olivia Sastry — needs follow-up

- **Email:** oliviasastry@gmail.com
- **Date:** Feb 8, 2026
- **Amount:** $18.00 (paid)
- **Stripe PI:** `pi_3SnYRTD01uz279HJ03OPAJ8V`
- **Shipping address:** NONE — not collected
- **Printful:** never submitted (no webhook was configured)

### Action needed

Email Olivia to get her shipping address, then manually submit to Printful:

```bash
curl -s -X POST "https://api.printful.com/orders?confirm=true" \
  -H "Authorization: Bearer $PRINTFUL_API_TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "external_id": "pi_3SnYRTD01uz279HJ03OPAJ8V",
    "recipient": {
      "name": "NAME",
      "address1": "ADDRESS",
      "city": "CITY",
      "state_code": "ST",
      "country_code": "US",
      "zip": "ZIP",
      "email": "oliviasastry@gmail.com"
    },
    "items": [{
      "name": "PRODUCT NAME (check Stripe order_reference prod_Tl4XQCZ51gpJF0)",
      "variant_id": VARIANT_ID,
      "quantity": 1,
      "files": [{
        "type": "default",
        "url": "IMAGE_URL",
        "position": { "area_width": 2700, "area_height": 1050, "width": 2700, "height": 1050, "top": 0, "left": 0 }
      }]
    }],
    "packing_slip": {
      "email": "mail@aesthetic.computer",
      "message": "Your pictures belong on this earth. - @jeffrey",
      "logo_url": "https://pals-aesthetic-computer.sfo3.cdn.digitaloceanspaces.com/painting-2023.8.01.17.07.png",
      "store_name": "aesthetic.computer"
    }
  }'
```

Alternative: refund via `stripe.refunds.create({ payment_intent: "pi_3SnYRTD01uz279HJ03OPAJ8V" })`

---

## James Colby — RESOLVED

- **Email:** j.r.colby@gmail.com
- **Date:** Mar 2, 2026
- **Amount:** $18.00
- **Product:** yellow mug of dgEs2UZQ in $bop (11oz Ceramic, variant 11048)
- **Printful order:** #152298666 (pending, submitted 2026-03-31)
- **Confirmation email:** sent 2026-03-31
- **Ship to:** 1109 Oxley Street, South Pasadena, CA 91030

---

## Root cause

No Stripe webhook was configured for `https://aesthetic.computer/api/mug`. Fixed 2026-03-31:
- Webhook `we_1THG4uD01uz279HJuOzzLwSC` created
- Secret: already in vault `lith/.env` as `STRIPE_ENDPOINT_MUG_SECRET`
- **Needs deploy to lith** for future orders to work
