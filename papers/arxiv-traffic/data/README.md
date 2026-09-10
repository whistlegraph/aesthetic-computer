# Data provenance — Aesthetic Network Traffic Report

All files pulled 2026-09-09 (evening, UTC). Four instruments, four different
things they can see. Nothing here is estimated; where an instrument is blind,
the column is absent rather than filled.

## Files

| File | Instrument | Window | Notes |
|---|---|---|---|
| `cloudflare-zone-ytd.csv` | Cloudflare zone analytics (`httpRequests1dGroups`) | 2026-01-01 → 2026-09-09 | All 16 zones on the account. `daily_unique_sum` is the **sum of daily unique-IP counts**, not distinct visitors for the period — it double-counts anyone present on more than one day, and it counts crawlers. |
| `cloudflare-zone-monthly.csv` | same | same | Per zone per month. |
| `ac-24h-path-mix.csv` | Cloudflare `httpRequestsAdaptiveGroups` | 2026-09-08 23:00 → 2026-09-09 23:00 UTC | Free-plan adaptive queries are capped at a 1-day range, so this is a single representative day, not a year. Zone total for the window: 1,927,263. |
| `ac-24h-browser-mix.csv` | same | same | `userAgentBrowser` is Cloudflare's classification. |
| `lith-snapshot.txt` | Caddy JSON access log on `lith.aesthetic.computer` | 2026-09-09 19:41 → 23:16 UTC (3.75 h) | 103,451 requests. Caddy rotates on size and retains ~4 h at current volume, so this is the whole of what the origin can be asked about. |
| `boots-daily.csv`, `boots-monthly.csv` | `boots` collection (MongoDB) | 2026-01-24 → 2026-09-09 | One document per page load **that executed JavaScript**. Filtered to remove `localhost`/`localDev`, IP-literal hosts, and bot user-agents. |
| `retention.csv`, `geography.csv` | same | 2026-07-14 → 2026-09-09 | Restricted to the window where client IP and country are trustworthy (see *Known gaps*). |
| `entry-paths.csv`, `referrers.csv`, `signed-in.csv` | same | 2026-01-24 → 2026-09-09 | |
| `piece-hits-top.csv` | `piece-hits` collection | 2025-12-31 → 2026-09-09 | Server-side HTML route counter. Includes crawler and exploit-probe paths (see *Known gaps*). |

## What is solid

- Cloudflare edge request, page-view and daily-unique counts per zone for the
  full year. Cloudflare is in front of every zone; nothing bypasses it.
- Boot-log records from 2026-01-24 (the day `/api/boot-log` shipped) onward.
  Every field written by the browser — host, path, referrer, user-agent,
  language, timezone, screen, embedded flag — is reliable for the whole period.
- The lith request mix and 5xx tally for the sampled 3.75 hours.

## What is soft

- **Client IP and geography before 2026-07-14.** `boot-log.mjs` read
  Netlify `x-nf-*` headers until commit `9dbe9b83fa`; on lith those headers do
  not exist, so `server.country` was null on every boot before that date and
  `server.ip` recorded the Cloudflare edge address rather than the client.
  Any per-client count before 2026-07-14 is an undercount and is not used.
- **Bot classification** is user-agent-based. Well-behaved crawlers identify
  themselves; headless browsers driving our own fleet do not, and are
  separated by host and by request pattern instead.
- **Cloudflare Web Analytics (RUM)** is installed on 10 zones but its beacon
  fires from our own headless browsers as readily as from people, and the
  free tier both samples and rounds (aesthetic.computer reports exactly
  "50000" visits in two consecutive months). It is not used for any figure.

## Known gaps

- `piece-hits` accepts any slug the HTML router saw. The second most-hit
  "piece" of 2026 is `+CSCOE+/logon.html` (20,435), a Cisco VPN exploit probe.
- The `kidlisp` collection stopped recording reads on 2026-04-19 and stopped
  receiving new codes on 2026-06-17, while `/api/store-kidlisp` continues to
  serve ~290 requests every 3.75 h. Its `hits` and `lastAccessed` columns are
  frozen and are not used for anything in this report.
- `@handles` documents carry no creation timestamp, so handle registration
  cannot be plotted over time. Only the total (2,973) is available.
- Caddy log retention (~4 h) means the origin cannot answer any question
  about the past.
