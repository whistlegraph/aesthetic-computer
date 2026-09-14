# Cloudflare DNS Status

Last audited: 2026-03-30

This file reflects the live Cloudflare state after the lith cutover.
The primary frontend origin is `209.38.133.33`.

## Frontend Records On lith

- `aesthetic.computer` zone: `aesthetic.computer`, `api`, `bills`, `data`, `feed`, `give`, `keeps`, `l5`, `news`, `p5`, `pals`, `papers`, `processing`, `sitemap`, `www`
  - `data` (A → 209.38.133.33, proxied) added 2026-06-29 — Linked Open Data / CIDOC CRM endpoint (`crm.mjs`); needs lith deploy to serve.
- `false.work` zone: `builds.false.work`
- `jas.life` zone: `jas.life`
- `justanothersystem.org` zone: `justanothersystem.org`, `www`
- `kidlisp.com` zone: `kidlisp.com`, `www`, `buy`, `calm`, `device`, `keep`, `keeps`, `learn`, `pj`, `top`
- `notepat.com` zone: `notepat.com`, `www`
- `prompt.ac` zone: `prompt.ac`, `api`, `l5`, `p5`, `papers`, `processing`, `sitemap`
  - `www` MISSING as of 2026-07-06 — falls into the `*.prompt.ac → 100::` Worker wildcard and 522s. Fix: add `www` A → 209.38.133.33 (proxied); Caddyfile already has the www→apex redirect.
- `quiltnet.org` zone: `quiltnet.org`, `www`
- `sotce.net` zone: `sotce.net`, `www`

## Other Live Exceptions

- `aesthetic.computer` keeps its non-lith service records for PDS, session, silo, oven, spaces/CDN, Auth0, Shopify, Stripe, and Cloudflare Worker endpoints.
- `prompt.ac` keeps `*.prompt.ac -> 100::` for the Worker wildcard.
- `prompt.ac` zone ruleset "prompt.ac bare-domain auto-redirect" (dynamic
  redirect, ruleset 99ad241726fe48ee9db29ba97f11ad4d) was DISABLED 2026-09-01:
  it 301'd every non-root path to aesthetic.computer at the edge, which now
  belongs to lith's resolver (server.mjs serves the HTML prompt shell for
  dotless piece paths, 302s live wg codes, 301s dotted paths). Re-enable it
  only if the shell has to be pulled — it supersedes the origin's routing.
- `sotce.net` keeps `chat.sotce.net -> 157.245.134.225` plus its mail/Auth0 records.

## Netlify Status

- No production records in the audited zones point to `aesthetic-computer.netlify.app`.
- No production records in the audited zones point to `75.2.60.5`.
- Legacy `duckweedtri.aesthetic.computer` and `duckweedtri.prompt.ac` were stale Netlify 404s during the audit and should resolve through lith-managed redirects instead.

## Amail Inbound (added 2026-09-13)

- `inbound.aesthetic.computer` A → 209.38.133.33, **DNS-only** (Cloudflare does
  not proxy SMTP). This is the "route to host" target of the Google Workspace
  default-routing rule that catches unknown `@aesthetic.computer` addresses;
  `lith/mail-inbound.mjs` (lith-mail.service, :25) files them as Amail.
  Caddy serves the same name over HTTPS only to hold its STARTTLS certificate.
- The apex MX stays on Google (`aspmx.l.google.com` et al.) — one human
  mailbox, `mail@aesthetic.computer`, lives there.
- Mail authentication for the apex (added 2026-09-14 — there was none before,
  so every `mail@aesthetic.computer` send had been unauthenticated):
  `aesthetic.computer TXT "v=spf1 include:_spf.google.com ~all"` and
  `google._domainkey.aesthetic.computer TXT "v=DKIM1; …"` (key generated in
  Admin → Gmail → Authenticate email; status "Authenticating email with DKIM").
  DMARC stays `p=none` for now.
