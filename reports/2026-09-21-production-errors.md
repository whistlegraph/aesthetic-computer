# September 21 production errors

Investigation window: September 21, 2026, 00:00–12:30 PDT
(07:00–19:30 UTC). Endpoint comparison uses the same hours September 20.
Boot comparisons below use midnight–noon on each day. Findings precede any
deployment of the fixes described here.

| Finding | Evidence | Interpretation / action |
| --- | --- | --- |
| Shared Redis connection closes during other requests | Retained Caddy logs: 1,569 MacPal status 500s and 454 MacPal art 500s. Lith journal repeatedly reports `The client is closed`. | Reproduced locally. Keep the shared connection alive after request cleanup; share the initial connection promise. Patch in `system/backend/kv.mjs`. |
| Image generation fails under programmatic traffic | PostHog: 181/231 endpoint requests report 5xx (78.4%), versus 20/53 yesterday (37.7%). Caddy: 234 requests, all with Node user agents, no referrer; 183 return 503, 50 return 200, one returns 502. Journal: 115 NVIDIA timeouts and 183 fallback-budget denials. | Provider failures exhaust the existing ten-per-process-hour paid fallback. These are not evidence of 234 visitors. Small discrepancies between Caddy and PostHog reflect different recording/flush boundaries. Provider/budget behavior remains unresolved; budget unchanged. |
| Unknown Sotce routes produce 500s | Scanner-like paths such as `/.git/config`, `/.env`, and `/.well-known/...`; journal reports undefined `statusCode`. | Handler falls through without a response. Added a 404 fallback in `sotce-net.mjs`; known static routes retain their responses. |
| Visitor boot tail is slower | Excluding bot/crawler/spider/headless user agents: successful-boot p95 is 20.37s today versus 3.19s yesterday; medians 895ms versus 766ms. | A real change in recorded duration, with mixed causes still unresolved. Several samples spend most time before parsing or between display setup and piece boot. Extreme 10–80 minute samples may include suspended tabs; wall-clock duration is not necessarily active waiting. |
| Version requests look slow by design | PostHog: 2,603/2,844 version requests are in the 2s+ bucket. Source holds matching-version requests for up to 45s. | Exclude expected long polling from ordinary latency alarms. No version-endpoint fix needed for this observation. |

## Boots and piece errors

Midnight–noon, `meta.localDev != true`, non-bot user agents; this heuristic
does not prove a session is human. Durations include only records currently
marked successful.

| Date | Starts | Successful | Error recorded | No completion recorded | Successful p95 | Over 10s |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Sep 18 | 238 | 207 | 0 | 31 | 3.00s | 5 |
| Sep 19 | 652 | 599 | 2 | 51 | 2.44s | 7 |
| Sep 20 | 186 | 145 | 1 | 40 | 3.19s | 6 |
| Sep 21 | 181 | 144 | 2 | 35 | 20.37s | 11 |

Through 12:30, all user agents: 247 boot records, 190 successful, seven
marked error, 50 without completion. Five errors have Googlebot user agents;
the other two are a Chrome module-fetch failure and a Laklok iPhone Safari
module-import failure. Missing completion can mean abandonment, a suspended
tab, telemetry loss, or a stalled boot; it is not a confirmed crash count.

There are 414 piece runs today, no records with explicit crash errors, but
29 runs contain console errors. Those include module-load failures on Laklok,
prompt and notepat, session-connection failures, and invalid KidLisp input.
Yesterday had 22 explicit Oskiewar terrain-render errors
(`cannot read property 'x' of undefined`); none recur in today's records.
Neither a zero crash count nor a completed lifecycle proves an error-free run.

## PostHog coverage

[AC project 540837](https://us.posthog.com/project/540837) has endpoint
aggregates and browser events, but no ingested log services or error-tracking
issues for this window. This matches the deliberate policy in
`shared/posthog-policy.mjs` and `lib/product-analytics.mjs`: raw logs and errors
stay in Lith/Mongo; browser exception capture is disabled.

Endpoint totals above sum the event's `count` property, rather than counting
aggregate envelopes. Browser pageviews during midnight–noon were 98 today
versus 112 yesterday, so those events do not indicate an audience surge.
These browser figures include internal traffic and miss blocked telemetry;
Laklok is not in the browser analytics host allowlist.

Primary evidence: production Mongo `boots` and `piece-runs`; retained Caddy
access logs; `journalctl -u lith`; `/lith/stats` and `/lith/errors`; read-only
PostHog event aggregates. Lith's in-memory counters reset at 12:20 PDT during
the earlier mail deployment, so they were not used as all-day totals. No raw
logs, messages, mail, or host data were uploaded to PostHog.

## Fix validation and remaining work

The remediation includes:

- Redis request cleanup retains the shared connection; first connection and
  automatic reconnect wait for readiness. Explicit shutdown remains available
  to scripts, and three known one-shot consumers now close Redis explicitly.
- Unknown Sotce routes return 404.
- NVIDIA outage retries back off exponentially, with one recovery probe.
  Blueberry wallpaper logs corroborate 72 proxy attempts during 09:04–12:30
  PDT: 57 returned 503, eight aborted at 35 seconds, six succeeded, and one
  failed on the network. This identifies one contributor, not all callers.
  Wallpaper cooldowns persist across subprocesses and subject changes;
  background requests opt out of paid fallback. `see` enforces Retry-After.
- Boot completion is independent of a hanging log upload. Every phase records
  identity and creation time even when start arrives late; completed boots
  retain late error evidence without losing their successful status.
  Visibility, timer gaps, and bounded timings for fixed public core modules
  remain in AC operational telemetry. No raw URLs or new private content are
  exported to PostHog.

Production data also confirms five boot records updated during 00:00–12:00
PDT lacked `createdAt` (three successful, two without a status). The changes
address future telemetry; historical data is not rewritten. New records use
a deterministic Mongo ID so concurrent first phases produce one record.

Added the nonunique `bootId_1` index to production `boots`. An identical
missing-ID lookup examined 195,391 documents in 164ms before, versus zero
documents in 5ms using the index after. These are single-query observations,
not a claim about every boot. Existing duplicate records were preserved.

Focused checks cover Redis connection/reconnection/shutdown, route fallthrough,
provider outage and paid budgets, browser and wallpaper cooldowns, boot phase
ordering, visibility/resource privacy, and stalled upload handling. Redis was
also exercised against the installed Redis 5 client with a local TCP fixture;
Mongo update expressions were checked using a read-only `$documents` pipeline.
The peer-request closure, premature connection readiness, and unknown-route
regression tests fail against the original source. An isolated headless browser with the changed boot modules completed in
512ms with zero page errors and the expected diagnostics. No paid image
generations were used for validation. The combined focused checks passed 42 tests:

```sh
node --experimental-vm-modules --test \
  system/tests/kv.test.mjs system/tests/sotce-route-fallback.test.mjs \
  system/tests/boot-telemetry.test.mjs tests/update-recovery.test.mjs \
  system/tests/flux.test.mjs system/tests/see.test.mjs \
  slab/test/slab-wallpaper.test.mjs
```

Rollout checks: verify the pushed revision, repeat overlapping MacPal reads,
confirm unknown Sotce routes return 404, and inspect new boot diagnostics.
Before the fix, a 20-request overlapping MacPal probe returned five 500s.

Remaining limits: NVIDIA availability is external; wallpapers wait during its
outage. Browser cancellations are not yet propagated through Lith to provider
requests. Other fleet checkouts need the updated wallpaper script and helper.
Slow-boot causes need new visibility/resource evidence; the present logs do
not establish one common cause for every flare-up. Expected version long
polling remains excluded from ordinary latency diagnosis. Existing saved
endpoint-rate insights already weight their batched `count` property correctly;
no PostHog settings were changed.
