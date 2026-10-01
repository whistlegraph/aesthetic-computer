# Public machine visitors

The stack already publishes `llms.txt`, the Whistlegraph Markdown/JSON index,
`/docs.json`, `/api?format=json`, `/mcp`, Platter manifests, release manifests,
CIDOC CRM/VoID, and product catalogs. The machine directory links those existing
resources; it does not introduce a new agent protocol.

`utilities/machine-properties.mjs` curates 40 public properties and 63 hostnames.
`utilities/generate-machine-manifests.mjs` emits product-specific text files,
`network.json`, the canonical AC `sitemap.xml`, and the `machine_discovery`
snippet in `lith/Caddyfile`. Each covered site sends an HTTP `Link` to `/llms.txt`.
Exact host/path matching prevents SPA fallbacks and application redirects from
turning a text request into HTML. Caddy's existing `robots.txt` routes and
Whistlegraph's terms remain unchanged.

AC's existing piece generator now keeps `/llms.txt` short and emits the complete
catalog at `/llms-full.txt`. The short map points to the real `/mcp` endpoint;
the former `/.well-known/mcp.json` link returned 404. It links the supported HTTP
API separately from the runtime API and historical backend source directory.

## Update and verify

```sh
node utilities/generate-llms-txt.mjs
node utilities/generate-machine-manifests.mjs
node utilities/generate-llms-txt.mjs --check
node utilities/generate-machine-manifests.mjs --check
caddy adapt --adapter caddyfile --config lith/Caddyfile >/tmp/ac-caddy.json
node utilities/generate-machine-manifests.mjs --probe
```

The check fails when a new explicit Caddy hostname is neither covered nor
classified. The HTTPS probe checks actual text/JSON bodies, discovery headers,
redirect destinations and linked resources. HTTP 200 alone is insufficient:
KidLisp, Menu Band, No Paint and Papers previously returned application HTML for
`/llms.txt`. The probe is unauthenticated and GET-only; it never invokes a
publishing, payment, account or administrative operation.

Deploy through the normal Lith main deployment, including the Caddy reload.
There is no background service or build dependency. Regenerate the Aesel entry
when a new release channel is actually published; link a current release
manifest instead of freezing a filename or version in this directory.

## Scope and remaining host work

This is the current public Lith application stack, not a registrar inventory.
Each explicit host in Caddy has a coverage or exclusion decision. Redirect-only
aliases keep their canonical destination. Client properties (`false.work`,
`danzballet.studio`, `gym.anthonyzollo.com`) and authenticated infrastructure are
outside this change. The IPFS gateway retains its own protocol behavior.

The 2026-10-01 public audit found and repaired three pre-existing serving gaps:

- `www.prompt.ac` lacked an explicit DNS record and inherited the `100::`
  wildcard, producing Cloudflare 522. A proxied A override now points at Lith.
- `rdp.jas.life` inherited a Vercel wildcard and returned DEPLOYMENT_NOT_FOUND.
  A proxied A override now reaches its already-published Lith painting gallery.
- `www.mime.ac` selected the catch-all origin certificate. Its own TLS policy
  now selects the managed certificate, matching the existing apex pattern.

Both DNS changes add only the exact hostname, preserving wildcard records and
all other hosts. Removing those explicit records restores the prior behavior.
The public directory contains no DNS credentials or control-plane identifiers.

The local network intercepts `sotce.net`: local DNS returns a documentation
address and the certificate is issued by the UniFi SSL Certificate Authority.
Public DNS returns Cloudflare addresses; normal HTTPS from Lith verifies Sotce's
manifest successfully. Run the probe from an unaffected network; never disable
certificate validation.

`wipppps.world` currently reaches an external site, despite legacy Caddy rules.
Other separately hosted properties (Shopify shop/gift, status/grab workers,
release/asset object storage, AT Protocol PDS and knot, and legacy jas.life or
Whistlegraph archive hosts) retain their own serving and protocol contracts.
They are not silently claimed as deployed by a Lith change. The directory links
existing public releases and canonical source without changing those hosts.

## What this enables

An assistant can find the right product, fetch current release metadata, learn
the piece API, cite a paper or music record, and locate public artwork source
without booting every canvas or downloading a full HTML application. The short
AC map lets it fetch the full piece catalog only when needed. Whistlegraph's
existing attribution and paid machine-access terms remain discoverable.

These files do not guarantee crawler adoption, search placement or revenue.
Use existing Caddy request logs to compare aggregate requests for `/llms.txt`,
`/network.json`, `/docs.json` and linked public resources, including status codes
and known crawler user agents. A claimed crawler user agent is not verified
identity. Do not join this measurement to private accounts, device identities,
subscriber content, chats, notebooks or local MCP telemetry.

Convention: https://llmstxt.org/ (including HTTP `Link: rel="describedby"`).
