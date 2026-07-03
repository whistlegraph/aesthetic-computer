# Domains

Buy and wire up vanity domains for AC pieces from the command line, via
[Porkbun](https://porkbun.com)'s API.

A vanity domain (e.g. `notepat.com`, `laklok.com`) is just two things:

1. **DNS** pointing the apex at lith — `A → 209.38.133.33`
2. **A host block in `lith/Caddyfile`** that rewrites `/` to the piece and
   reverse-proxies the rest to `localhost:8888`

This tool owns buying the domain and hands you the Caddy block for the rest.

## Setup (one time)

1. **Account + API access.** Log into Porkbun → **Account → API Access** →
   create an **API Key** (`pk1_…`) and **Secret Key** (`sk1_…`). Enable
   API access, and set the account's **default WHOIS contact** — registration
   borrows it, so buys fail without it.
2. **Store the keys** in the vault so every machine picks them up:

   ```
   PORKBUN_API_KEY=pk1_…
   PORKBUN_SECRET_API_KEY=sk1_…
   ```

   Add those lines to `aesthetic-computer-vault/.env`. The client reads the
   environment first, then falls back to that file.
3. **Verify:** `npm run domain ping` → should print your IP.

## Commands

```bash
npm run domain ping                    # test the keys
npm run domain price .games .com .ac   # register/renew/transfer prices per TLD
npm run domain check nom.games a.com   # availability + price for full domains
npm run domain buy nom.games           # register (prompts to confirm price)
npm run domain buy nom.games --yes     # register without the prompt
npm run domain caddy nom.games nom     # print the Caddyfile block for a domain→piece
```

`buy` re-checks availability and price at purchase time; Porkbun aborts if the
price moved since your `check`.

## After buying

1. Point DNS at lith. Either move the domain onto the Cloudflare account and add
   an `A` record → `209.38.133.33`, or set Porkbun DNS directly.
2. Add the host block (`npm run domain caddy <domain> <piece>` prints it) to
   `lith/Caddyfile`, and add the domain to the `@mainspa` host list.
3. Deploy: `fish lith/deploy.fish`.

## Roadmap

- Fold Cloudflare DNS creation + the Caddy edit + redeploy into a single
  `npm run domain add <domain> <piece>` once a CF API token is wired in.
