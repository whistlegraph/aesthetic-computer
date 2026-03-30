# Cloudflare DNS Status

Last audited: 2026-03-30

This file reflects the live Cloudflare state after the lith cutover.
The primary frontend origin is `209.38.133.33`.

## Frontend Records On lith

- `aesthetic.computer` zone: `aesthetic.computer`, `api`, `bills`, `give`, `keeps`, `l5`, `news`, `p5`, `pals`, `papers`, `processing`, `sitemap`, `www`
- `false.work` zone: `builds.false.work`
- `jas.life` zone: `jas.life`
- `justanothersystem.org` zone: `justanothersystem.org`, `www`
- `kidlisp.com` zone: `kidlisp.com`, `www`, `buy`, `calm`, `device`, `keep`, `keeps`, `learn`, `pj`, `top`
- `notepat.com` zone: `notepat.com`, `www`
- `prompt.ac` zone: `prompt.ac`, `api`, `l5`, `p5`, `papers`, `processing`, `sitemap`
- `sotce.net` zone: `sotce.net`, `www`

## Other Live Exceptions

- `aesthetic.computer` keeps its non-lith service records for PDS, session, silo, oven, spaces/CDN, Auth0, Shopify, Stripe, and Cloudflare Worker endpoints.
- `prompt.ac` keeps `*.prompt.ac -> 100::` for the Worker wildcard.
- `sotce.net` keeps `chat.sotce.net -> 157.245.134.225` plus its mail/Auth0 records.

## Netlify Status

- No production records in the audited zones point to `aesthetic-computer.netlify.app`.
- No production records in the audited zones point to `75.2.60.5`.
- Legacy `duckweedtri.aesthetic.computer` and `duckweedtri.prompt.ac` were stale Netlify 404s during the audit and should resolve through lith-managed redirects instead.
