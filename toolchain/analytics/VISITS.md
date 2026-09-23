# Public website interaction model

The first-party collector is `POST https://aesthetic.computer/api/visit-track`.
The browser module and reviewed host catalog live in
`system/public/aesthetic.computer/lib/visit-{tracker,model}.mjs`.
This supplements Cloudflare request/byte totals, boots, piece logs and stored
replays. It does not export operational records to PostHog.

## What the next 72-hour report can say

| Measure | Definition |
| --- | --- |
| Visit | One visible top-level page load with a random in-memory ID; returning from a private route starts a fresh visit |
| Interacted visit | Visible-page trusted pointer/touch/keyboard/wheel input outside form fields, or focused gamepad input (button or stick beyond 0.5) |
| Engaged visit | An interacted visit with at least 10 seconds of accumulated visible time |
| Action visit | At least one occurrence of a reviewed action during that visit |
| Known automation | WebDriver, recognizable bot UA, explicit render query, or `window.acAutomation = true` |
| Unknown audience | A non-automated visit without interaction evidence |

“Likely human” means non-automated + interacted; it is not proof of personhood.
An automation framework can generate trusted events. Untagged automation may
remain. IPs are not counted as people. No cross-page retention, unique-user,
cross-domain journey or conversion-attribution claims can be made from these
ephemeral IDs. A returning person loading three pages produces three visits.
In-page SPA navigation and Oskiewar's automatic round-URL changes preserve the
visit ID; the surface dimension describes the broad landing category.

Visible-time lower-bound buckets are 0, 10, 30, 60, 180 and 600 seconds.
This is foreground display time, not proof of attention. Polling gaps are
capped at two seconds so sleep/suspension does not fabricate engagement.
All actions are boolean per visit, not totals of clicks, rounds or downloads.
Download clicks do not prove completed downloads. Canvas interaction does not
prove a saved painting. Media starts count only after interaction.

## Actions

Common pages record `link_followed`, `download_clicked`, `canvas_interacted`
and `media_started`, with no destination URL, canvas content or media name.
Oskiewar additionally records `round_started`, `round_completed` (successful
replay upload), and `match_completed` (uploaded final score reaches five).
These are interaction-qualified, per-visit milestones; the existing replay
collection remains the authority for round totals. A failed upload will not
produce a completion milestone even if the player finished the round.

Product code can call `window.acVisits?.action(name)` for a reviewed action.
New actions must be added to the shared allowlist with a documented success
condition and a test. Do not infer publish/purchase/account success from clicks.

## Storage and privacy

`network-visits` stores one document per property + random visit UUID. A
cumulative snapshot with Mongo `$max` and a unique `_id` preserves milestones
despite retries or out-of-order delivery. Automation can be upgraded to true,
never downgraded. A TTL on `expiresAt` expires rows after 35 days; there is no
permanent raw-event stream. Dates come from the server, not the client clock.
The property comes from the HTTPS Origin allowlist, never a submitted hostname.
An origin header and self-reported events are not cryptographic proof of human
activity. The collector rejects unreviewed values and oversized bodies and
uses a bounded, in-memory rate guard (240 requests/minute/source).

No stored IP, user agent, account/handle, referrer, URL, query string, page text,
form value, key or pointer coordinate. The transient rate-limit digest is
process-salted, expires after one minute and never leaves memory. Requests omit
credentials and referrers. No tracking cookies or browser storage are used.
DNT, GPC, `window.acVisitTrackingDisabled = true`, private routes and embedded
frames suppress collection. The disclosure is `/network-privacy.html`.

Render/test harnesses should set `window.acAutomation = true` before loading
the module, or append `?ac-automation=1`. Existing `social-preview`,
`offline-render` and `jev-vs-jev` parameters also mark automation. Headless
browser QA must remain automated in production verification.

## Readout

On Lith:

```sh
cd /opt/ac/system
node --env-file=.env ../toolchain/analytics/visits-report.mjs --hours 72
```

The default scope is **Studio**. Use `--scope clients` for client properties or
`--scope all` for both. Scope applies to every table, time period and earliest
retained event. Classification is derived from the reviewed canonical domain,
including older records; clients never enter the default studio totals.

Optional `--end 2026-09-26T20:00:00Z` makes a report reproducible. Rows group by
property, automation classification and broad surface. Report automation
separately; subtract interacted from visits for unknown audience. The sum of
visible-time buckets is a lower bound, not exact duration. Compare with edge
requests to describe crawler/polling volume, but never subtract these different
instruments to invent a bot count. A report is grouped by visit start: later
milestones can update an earlier cohort. `lastSeenAt` is available for live use.

The earliest retained event indicates available history, not deployment time.
Check the deployment/coverage record before interpreting a zero. Archived
aggregate reports may be kept without visit IDs. No retrospective backfill is
possible for the period before installation.

## Coverage

The shared AC shell covers AC, notepat.com, nopaint.art, laklok.com and mime.ac
when those domains serve it. Static entry pages cover Whistlegraph, Jas,
KidLisp, Prompt, Aesel, Just Another System, Quiltnet and the public AC paper,
giving, bills, pop, NFT and language front doors. Oskiewar uses its standalone
shell. Sotce uses its function-generated HTML. Client domains on the same DNS
account (false.work, danzballet.studio, regarde.io, drvkforlife.com) are
collected under **Clients**, following explicit authorization. Public landing
pages only; draft/labs/builds hosts are excluded. Shopify uses
`visit-shopify.mjs` and the existing
[Customer Privacy API](https://shopify.dev/docs/api/customer-privacy)
`analyticsProcessingAllowed()` decision. Denial or revocation stops collection;
regrant begins a fresh visit. Merchant settings and consent are never changed.
www aliases roll up to the same property. Reviewed public subdomains are
explicit; unlisted hosts are denied. Archived RDP painting pages and other
static documents without the script are not automatically covered.

Known deployment boundaries discovered September 23, 2026:

- menuband.app `/` redirects to the App Store: no browser visit can fire on
  the redirect. Its hosted support/advanced pages are instrumented. App usage
  and App Store downloads are separate instruments.
- Client regarde.io is on Cloudflare Pages, outside the Lith deploy.
- Client drvkforlife.com is on Shopify, outside the Lith deploy.
- wipppps.world currently serves an external site despite old Lith routing.
- aesthetic.direct and digitpain.com did not answer the initial HTTPS probe;
  local entry sources are prepared, but live coverage is not assumed.
- sotce.net failed this host's TLS probe, but the collector subsequently
  received a non-automated visit and interaction from Sotce. Do not interpret
  a failed local probe as proof that the property is globally unavailable.

The domain allowlist is not a deployment-completion list. Run the coverage
audit after shipping; external deployments require their own source/control
path. Cloudflare's account inventory and Porkbun's registrar inventory were
both consulted; neither alone is a complete list of public web properties.
`visits-deployment.json` preserves the initial front-door audit and the four
properties verified end-to-end through a browser and MongoDB. Measurement
began September 23 at 18:34 UTC; no pre-installation history is invented.

## Verification

```sh
node --test system/tests/visit-tracking.test.mjs
PLAYWRIGHT_CHANNEL=chrome node --test system/tests/visit-tracking-browser.test.mjs
```

The browser check exercises trusted versus synthetic input, form exclusion,
engagement, duplicate installation, private SPA navigation and GPC. Production
checks should use marked automation and verify database milestones as well as
HTTP responses. Do not label a 204 alone as verified measurement.
