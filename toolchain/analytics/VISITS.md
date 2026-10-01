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

AC pieces running in the worker can send
`{ type: "visit:action", content: { action: "reviewed_name" } }` through their
existing `api.send`. BIOS forwards only the action name to the same collector;
no piece source, notes, command text, media identifiers or account data are sent.
The collector still requires visible-page interaction and respects private
routes, opt-outs and known automation. These hooks do not use PostHog.

| Action | Success condition |
| --- | --- |
| `canvas_interacted` | Trusted pointer/touch contact with a canvas, or inside AC's marked pointer-transparent display; overlaid DOM controls and keyboard input alone do not qualify |
| `note_played` | Notepat accepts a manual pad, keyboard or MIDI note and triggers its voice path; wrong song notes and Autopat do not qualify; this is not proof of audible output |
| `painting_edited` | No Paint commits an accepted proposal to the artwork and undo history; generated previews do not qualify |
| `recording_started` | The runtime's MediaRecorder emits `start`; recorder requests or permission prompts alone do not qualify |
| `painting_saved` | PNG upload tracking returns a successful media record with a code |
| `tape_saved` | ZIP/MP4/WebM upload tracking or tape draft finalization returns a successful record with a code; downstream transcoding may still be pending |

MIDI-only use still needs an independently recorded interaction in the visit.
The new creation hooks cover Notepat, No Paint and the shared media upload
paths, not every piece or every recording implementation. These per-visit
flags describe occurrence, not note/stroke totals or unique people. Historical
zeros before these hooks ship mean unmeasured activity, not absence of use.

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

No stored IP, user agent, account/handle, full URL, query string, page text,
form value, key or pointer coordinate. The transient rate-limit digest is
process-salted, expires after one minute and never leaves memory. Requests omit
credentials and referrers. No tracking cookies or browser storage are used.
DNT, GPC, `window.acVisitTrackingDisabled = true`, private routes and embedded
frames suppress collection. The disclosure is `/network-privacy.html`.

New visit records include `referrerHost`: the browser-reported referring
hostname, stripped of credentials, path, query and fragment. Local hosts and
IP literals are excluded. Null means direct or unavailable, not necessarily
direct traffic. Older visit records without this field are unmeasured.

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

Reports include `actionVisits` (visits with any reviewed action) and cumulative
`interacted30`, `interacted60`, `interacted180`, `interacted600` visible-time
thresholds. The analytics MCP exposes thresholds under `depth`, keyed by seconds.
Action totals overlap; do not sum them to count visits. Lith's daily rollup
retains these counts and each action flag for newly folded days. Previously
written daily rows are unchanged and may lack these fields; missing means
unavailable, not zero. Raw visit reports can still aggregate retained records.

### AC Human Fishery

The analytics MCP's `human_fishery` tool reads Silo's existing `_firehose`
(`silo/server.mjs`, MongoDB change stream → history + WebSocket dashboard),
filters to `network-visits`, and resolves recent, non-automated visits with
interaction. It adds no separate stream or raw event storage. Firehose
throttling/deduplication means this is a sampled operational feed, not a complete
audit log. Call with `{ "minutes": 5, "scope": "studio" }`, then repeat
after at least 15 seconds. Use `startedAfter` (ISO UTC) to watch only new visits
after a deployment. The maximum lookback is 60 minutes and the maximum result
is 200 visits; `truncated` explicitly marks an incomplete snapshot.

Each fish has a temporary name derived from its random visit ID and the UTC
day. Raw visit IDs stay on Lith. The tool exposes only the public property,
broad landing category, arrival/last-report time, visible-time bucket and
reviewed action flags. A fish is one visit, not a person. Same-page actions can
accumulate, but separate page loads and domains cannot be connected. There is
no stored event sequence: compare successive snapshots to see newly observed
milestones, not the exact order or time in which actions occurred. The last
report is the last changed snapshot, not continuous presence or departure.
The tool reads existing data; it adds no browser identifiers or new retention.

## Coverage

### Account activity and referrers

`POST /api/account-activity` verifies a bearer token through the existing
authorization service. Identity is taken only from the verified account;
submitted user/handle fields are ignored. AC shell piece loads and reviewed
actions use a separate in-memory session and client sequence. Built-in public
piece names are retained; published/inline programs use `published-or-code`.
Sotce uses its own authentication tenant and the broad `sotce` category, without
diary page IDs or contents. Embedded shells and private routes are excluded.
Only signed-in activity after installation is available. Login does not replay
anonymous actions, and there is no join to anonymous visit IDs.

`account-activity` stores server receipt time, verified account subject, tenant,
property, session, sequence, piece, action and referral hostname. Actions dedupe
per piece load; receipt order can differ from client sequence. The endpoint has
no public read route, bounded requests and a per-account rate limit. Rows expire
after 35 days; each site's account deletion removes its tenant's rows. Separate
Sotce identities remain separate accounts. Existing Silo operational firehose
history has its own retention. These records are not sent to PostHog.

The private analytics MCP exposes:

- `account_activity({hours:24, handle:"@handle"})`: verified account events,
  public handles where available, otherwise an account alias, and temporary
  session aliases. Counts are accounts, not unique people. Omit `handle` for
  all recorded accounts. Results are bounded and report truncation.
- `network_referrers({hours:24})`: referral hosts grouped by property, with
  visits, interacted visits and engaged visits. Existing AC boot logs supply
  a separate historical referral table, stripped to hostnames. Do not add boot
  counts to visit counts; they measure different things. Neither table proves
  human identity or a complete marketing attribution chain.

Both default to studio scope and accept `limit` (up to 500). No campaign tags
are collected. Missing data before deployment cannot be reconstructed.
Use `account_activity({hours:24, property:"sotce.net"})` or
`network_referrers({hours:24, property:"sotce.net"})` to isolate Sotce; add
`handle` to follow a particular account with a resolvable public handle.

Sotce's authenticated feed additionally records `sotce_page_viewed` after two
foreground display seconds and `sotce_page_visible_30s` after thirty. Only the
displayed, loaded card qualifies: prefetched pages, flipped backs, transitions,
editors and hidden tabs do not. Time gaps are capped at one second. Returning
to a page after viewing another can produce another milestone; no page key,
number or content leaves the browser through this feed. These indicate display,
not verified reading or unique pages. Canvas and virtualized DOM views are covered.

`sotce_page_touched` requires a newly inserted touch (`touchCreated: true`),
excluding existing touches, the author's own page and failed writes.
`sotce_question_submitted` requires a successful saved question; it is the sole
allowed milestone within `/ask`, with no form content. `/comment`, `/chat`,
`/write` and `/respond` remain excluded. These four Sotce milestones can repeat
within a session and carry client sequence numbers. They remain best-effort
browser reports with server-verified identity; the existing `sotce-touches`
and `sotce-asks` collections are authoritative for saved operation totals.

### Laer Klokken feature use

`/laer-klokken` aliases to `laklok`. Both that canvas piece and the standalone
HTML sister (`laklok.com/html/`, recorded as `laklok-vector`) now send repeated
authenticated feature events. `lib/laklok-activity.mjs` is the reviewed catalog
and identifies which controls exist in each interface. It contains no message,
recipient, link, chosen theme or language values. Boot restores and repeated
clicks on the already-selected theme/filter do not count as changes.

The canvas worker uses `account:action`; BIOS passes only the action name to
the first-party account collector. The HTML client uses its existing Auth0
session, rechecks token expiry when sending, and posts to the same endpoint.
The server verifies identity and rejects Laklok events attributed to another
piece. These detailed counts require sign-in; anonymous visits keep their
existing broad measurements. Opt-outs, private routes and automation guards
still apply. Radio/send/edit/media/navigation events are named `*_requested`
and must not be reported as successful playback, delivery or completed loading.

Use `feature_usage({hours:168})`, optionally with `handle:"@someone"`, for ranked
features, counts per account and UTC daily opens/action counts. Maximum lookback
is 840 hours (35 days), with at most 50 account rows. Top-feature totals cover
all matching accounts even when account detail is truncated. `property` can
restrict to a reviewed host; omitting it includes Laklok served through AC too.

Reports require `featureVersion:1`, so earlier piece opens do not fabricate
unused-feature rows. `notRecorded` means zero recorded uses of a control
supported by that account's observed interface, not proof the control was
visible or unused. Days without events are unobserved. Counts are best effort:
offline use, opt-outs, unloads and rate limits can leave gaps. Repeated Laklok
events are not deduplicated while a prior request is in flight; the client caps
concurrent sends at 20 and the server's per-account rate limit still applies.

### Website installation

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
- Client regarde.io is on Cloudflare Pages, outside the Lith deploy; deployed
  and browser/database verified September 23.
- Client drvkforlife.com is on Shopify, outside the Lith deploy; live theme
  and consent-allowed browser/database collection verified September 23.
- Client false.work redirects to www.false.work on Squarespace. The Lith
  source is prepared, but live installation requires Squarespace access.
  Do not count it as covered. Danz is deployed and browser/database verified.
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

`visits-clients-deployment.json` records the separate client rollout.

MIME's explicit media controls add `mime_interact`, `mime_scroll_feed`, and
`mime_original_open`. These count **visits with the action**, not total clicks.
Automatic re-locking when a card leaves the viewport does not count as a click.
They use the existing anonymous visit collector, opt-outs, automation flag,
retention, and Studio scope; no post ID or destination URL is added.

For bounded media loading probes, run `node toolchain/analytics/media-speed.mjs
--all-types --out report.json`. Set `MIME_CDP_URL` to a dedicated Chrome's
loopback DevTools URL to include video first-frame timing. The public MIME
catalog supplies samples of shared AC media, not an exhaustive crawl of every
studio domain. The probe reads at most a 128 KiB prefix per asset request and
checks declared MIME against MP4/WebM signatures. Video playback stops at the
first presented frame or a 20-second timeout. Source fetch timings for programs
are not runtime/render-ready timings. Do not compare a prefix download to a
full archive download or interpret one run as a stable network percentile.

## Native app launches

`POST /api/app-open` (`system/netlify/functions/app-open.mjs`) takes
`{app, version, platform, install, fresh}` from our native apps on each launch.
`install` is a random UUID minted on first launch and kept in the app's own
defaults (not the Keychain), so deleting the app forgets it; `fresh` is true
on that first launch. A reinstall therefore looks like a new install; the App
Store report (`app_downloads`) is where redownloads and restores are told apart.
`app-opens` stores one row per app + install + UTC day with the open count,
platform, version and `cf-ipcountry`, and expires rows after 35 days.
Debug builds, dev Electron and `acLaunchPingDisabled` skip the ping.

Readout: `node --env-file=.env ../toolchain/analytics/opens-report.mjs --days 7`
on lith, or the `app_opens` tool in `toolchain/mcp/analytics-mcp.mjs`.
