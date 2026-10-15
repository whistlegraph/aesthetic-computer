# Domains

Buy and wire up vanity domains for AC pieces from the command line, via
[Porkbun](https://porkbun.com) (the registrar) and Cloudflare (the DNS).

A vanity domain (e.g. `notepat.com`, `laklok.com`) is just two things:

1. **DNS** pointing the host at lith — `A → 209.38.133.33`
2. **A host block in `lith/Caddyfile`** that rewrites `/` to the piece and
   reverse-proxies the rest to `localhost:8888`

The registrar is only needed twice per domain: once to buy it, once to hand its
nameservers to Cloudflare. After that a new host is one API call.

## Setup (one time)

1. **Porkbun API access.** Log into Porkbun → **Account → API Access** → create
   an **API Key** (`pk1_…`) and **Secret Key** (`sk1_…`). Two things bite here:
   API access must also be **enabled per-domain** on the domain's own page, and
   the account needs a **default WHOIS contact** — registration borrows it, so
   buys fail without one.
2. **Store the keys** in `aesthetic-computer-vault/.env`:

   ```
   PORKBUN_API_KEY=pk1_…
   PORKBUN_SECRET_API_KEY=sk1_…
   ```

3. **Cloudflare** accepts `CLOUDFLARE_API_TOKEN` or the existing
   `CLOUDFLARE_EMAIL` + `CLOUDFLARE_API_KEY` pair. Both clients automatically
   resolve credentials through `toolchain/vault/credentials.mjs`.
4. **Verify:** `npm run domain ping` → prints your IP.

## MCP and vault discovery

Install only the domain daemon without restarting other MCPs:

```bash
bash toolchain/mcp/install-daemons.sh domains
```

This registers `domains` in Claude and Codex and serves it on loopback port
7782. A client may need to reconnect or start a new session to discover the
tools. Without launchd, run `node toolchain/domains/domains-mcp.mjs` over stdio.

- `vault_status` reports registered credential availability and source, never values; `domains_vault_status` limits this to Cloudflare and Porkbun.
- `domains_inspect` reads Cloudflare records, public delegation, and .com/.net registrar metadata.
- `domains_dns_set` previews one A/AAAA/CNAME change; `apply: true` writes it.
- `domains_porkbun_nameservers` reads delegation for a Porkbun-held domain.

Credentials are loaded inside the server on each call, so rotations take effect
without copying keys into MCP configuration or conversations. Process environment
credentials take precedence. `AC_VAULT` explicitly selects a vault; otherwise the
resolver checks the checkout's nested vault, `vault` alias, sibling vault, then
the home-directory vault. Explicit overrides never fall through to another vault.

Cloudflare checks `cloudflare.env`, `cloudflare/.env`, `feed/.env`, `lith/.env`,
the root `.env`, and the historical deploy env files listed in the resolver.
Porkbun checks `porkbun.env`, `porkbun/.env`, and the root `.env`. Credential pairs
come from one source; an incomplete process-environment override is reported
instead of mixing accounts. Encrypted-only files report `locked`: these tools
do not launch pinentry or decrypt/export the vault. Client folders are not scanned.

The shared resolver also serves Frame and Illy (OpenAI/fal), AC web search (Exa),
Slack, Instagram's four named accounts, X's two named accounts, Bluesky, and
DMGify's Apple notarization credentials. Social discovery and provider calls use
the same account registry. Slack's launcher parses dotenv as data instead of
sourcing it as shell code; only registered credentials and its existing message
tool setting enter the child process. Account pairs cannot be assembled across
different files or partial environment overrides.

Analytics uses the same vault root for its Lith SSH and encrypted App Store key
paths. Fleet, Memory, and Papers share root resolution too. Mail's remote
credentials, AC session tokens, browser/OS permissions, and external OAuth
connections keep their existing authentication mechanisms.

For new credential-backed MCPs, add an explicit service entry to the shared
resolver and use `requireCredentials(service)` inside the provider client.
Return `credentialStatus(service)` for discovery. Never serialize `.values`, add
arbitrary secret-read tools, or accept caller-selected authenticated URLs.

An active Cloudflare zone does **not** establish that the registrar delegates to
it. Compare assigned nameservers with public DNS and registry metadata. Updating
NS records inside the zone cannot repair parent delegation at another registrar.

Validation: `node --test toolchain/vault/*.test.mjs toolchain/domains/domains.test.mjs toolchain/mcp/ac-web-search/search.test.mjs`.

## Commands

```bash
npm run domain ping                    # test the registrar keys
npm run domain price .games .com .ac   # register/renew/transfer prices per TLD
npm run domain check nom.games a.com   # availability + price
npm run domain buy nom.games           # register (prompts to confirm price)
npm run domain caddy nom.games nom     # print the Caddyfile block

npm run domain cf adopt nom.games      # create the Cloudflare zone (+ @ and www)
npm run domain cf adopt nom.games midi # …and extra hosts while you're there
npm run domain cf add nom.games api    # add one host to an existing zone
npm run domain cf list nom.games       # show the zone's records

npm run domain ns nom.games            # registrar's current nameservers
npm run domain ns nom.games a.ns b.ns  # repoint them (prompts)
npm run domain dns nom.games           # registrar-side records, if still there
npm run domain dns nom.games add A midi 209.38.133.33
```

## Moving a domain onto Cloudflare

```bash
npm run domain cf adopt oskiewar.com midi   # prints the two nameservers
npm run domain ns oskiewar.com cloe.ns.cloudflare.com rob.ns.cloudflare.com
```

Adopting is inert — Cloudflare will happily serve a zone nobody is pointed at,
so you can verify it answers correctly *before* cutting over:

```bash
dig +short @cloe.ns.cloudflare.com midi.oskiewar.com A
```

Then repoint the nameservers. Check the old zone first (`npm run domain dns
<domain>`) and carry over anything that is not just `@` and `www` — an MX or a
verification TXT left behind is how mail and domain ownership quietly break.

## Adding a subdomain to a domain already on Cloudflare

```bash
npm run domain cf add oskiewar.com midi
```

Then add the host to its block in `lith/Caddyfile` and `fish lith/deploy.fish`.
Caddy obtains the certificate itself on first request.

**Order matters.** If the Caddyfile learns a hostname before DNS exists, Caddy
fails the ACME challenge (`NXDOMAIN looking up A for …`), backs off for five
minutes, and falls back to Let's Encrypt *staging* to protect the production
rate limit. TLS then answers with an internal-error alert and no certificate,
which reads like a broken cert but is really a missing record. Create the DNS
first; if you did it the other way round, `systemctl restart caddy` on lith
clears the backoff and the certificate lands in about fifteen seconds.

## Why records are DNS-only

`cf adopt` and `cf add` create records grey-cloud (unproxied) on purpose.
Proxying puts Cloudflare's certificate in front of lith, which forces that
host's Caddy block off its Let's Encrypt issuer and onto the origin-certificate
pattern used by the `:443` block. That is a deliberate migration with its own
testing, never a side effect of adding a domain. Turn it on per host once you
actually want the CDN in front.

## Roadmap

- One `npm run domain add <domain> <piece>` that buys, adopts, adds the host,
  writes the Caddy block, and redeploys.
- Read the Caddyfile to check a host block exists before promising the
  certificate will issue.
