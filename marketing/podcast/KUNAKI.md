# Kunaki cassette production

Kunaki separates product creation from fulfillment:

1. Generate a cassette kit locally.
2. Create the cassette in Kunaki's browser uploader and inspect its virtual proof.
3. Record the resulting 10-character product ID.
4. Use the API client for shipping quotes, test orders, live orders, and tracking.

```sh
npm run podcast:cassette-kit -- --slug physical-mail --title "A Record in the Mail"
npm run podcast:kunaki -- specs
npm run podcast:kunaki -- shipping shipping.json
KUNAKI_USER_ID=... KUNAKI_PASSWORD=... npm run podcast:kunaki -- order order.json
```

An order defaults to Kunaki's `Test` mode. Live order construction additionally
requires `KUNAKI_ALLOW_LIVE=1`; this prevents an accidental manufacturing order.
Do not commit credentials or recipients' addresses.

Example shipping input:

```json
{
  "country": "United States",
  "stateProvince": "CA",
  "postalCode": "90012",
  "items": [{ "productId": "PX0012345", "quantity": 1 }]
}
```

Example order input adds `recipient` and the exact `shippingDescription` returned
by the quote. Product creation cannot be automated by Kunaki's fulfillment API.

Current cassette artwork requirements are encoded in `lib/kunaki.mjs`: JPEG at
300 DPI, 1200×1110 for the J-card, and 1062×496 for each side label. Kunaki says
bleed is unnecessary but recommends keeping text and lines clear of the edges.
