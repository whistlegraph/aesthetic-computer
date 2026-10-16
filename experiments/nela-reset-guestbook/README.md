# Guest BOOk

A guestbook proposal for [Echo's Reset In Peace page](https://nela.computer/reset/).

Run with Node 22 or later; no package install or build:

```sh
npm start
```

Open http://127.0.0.1:4327/reset/. `npm test` checks storage and request handling.

The browser addition is `guestbook.js`, `guestbook.css`, one small SVG, a local
font, and the commented section in `public/reset/index.html`. It reuses the
original purple tile, body type, and skeleton cursor. The original `site.css`
is unchanged. Sign uses [UnifrakturMaguntia](https://fonts.google.com/specimen/UnifrakturMaguntia)
by j. 'mach' wust / Peter Wiegel; its SIL Open Font License is in `assets/`.
The OO pupils follow the mouse in pixel steps and rest with reduced motion.
Messages are optional; a name alone can sign. “I'll be there” adds an RSVP.
Signatures show their age, updated each minute; hover for the exact timestamp.
The prompt invites a hardware goodbye or a repair, reuse, or recycling pledge.
Four bytes derived from the signature ID choose its ghost's shape, face, color,
and movement. A 12 × 14 SVG grid and stepped CSS keep the pixels crisp while
floating, waving, and blinking, with reduced-motion
support; no dependencies, animation loop, or additional image downloads.

`upstream/` is a snapshot of the publicly served page and its linked files,
retrieved October 14, 2026; it is not a checkout of the site's source repository.
`upstream-manifest.json` records original URLs and hashes. All original art and
page copy remain attributed to NELA Computer Club / Echo.

The reference server keeps signatures in `data/guestbook.json`. It accepts
`GET` and `POST` at `/reset/api/guestbook`. Writes are serialized and replace the
file atomically. Retries with the same entry ID do not create duplicates. Guest
text is rendered as text, never HTML. A hidden field, length limits, origin
checks, and five signatures per ten minutes per address deter basic spam.
Addresses are kept only in memory. This is not authenticated identity or robust
bot protection; there is no moderation dashboard.

For deployment, run one Node process behind the existing HTTPS reverse proxy,
route `/reset/api/guestbook` to it, and serve `public/reset/` at `/reset/`.
Set `PUBLIC_ORIGIN=https://nela.computer` and keep `GUESTBOOK_DATA` on persistent
storage. `HOST` and `PORT` are optional. Set `TRUST_PROXY=1` only when the proxy
overwrites `X-Real-IP` and direct access is blocked; otherwise leave it off.
Back up the JSON file. To remove an entry, stop the process, edit that backup
into the data file, and restart. Existing malformed data fails startup instead
of being replaced. The book is capped at 5,000 entries.

Part of [Aesthetic Computer's NELA contributions](../../docs/nela-contributions.md).
This is a proposal, not a live deployment. Local proposal recordings and message
receipts are excluded from the source repository.
