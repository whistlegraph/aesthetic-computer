# data — iOS So Far

Evidence tables behind the paper. Every figure is derived from these files.

| File | Source | Pulled |
|---|---|---|
| `app-records.csv` | App Store Connect API: `/v1/apps`, `/v1/apps/{id}/appStoreVersions`, `/v1/apps/{id}/appInfos`, `/v1/apps/{id}/customerReviews`, `/v1/appPriceSchedules/{id}/manualPrices` | 2026-09-09 |
| `menuband-transactions.csv` | ASC Analytics API, ONGOING report `r12-33b8b206-…`, "App Store Purchases Standard", all granularities de-duplicated by latest `processingDate` | 2026-09-09 |
| `nopaint-reviews-by-month.csv` | ASC API `/v1/apps/1107427275/customerReviews` (131 reviews, complete) | 2026-09-09 |
| `boots-by-month.csv` | Aesthetic.Computer `boots` MongoDB collection, one document per page load | 2026-09-09 |

## Solid

App record metadata, version and build histories, review text and ratings,
the five Menu Band transactions with territory and discovery source.

## Soft

Boot counts are page loads, not people; the runtime has no per-user
analytics. February 2026 web traffic is an unexplained outlier and is
excluded from typical-month claims. The iOS app is identified by the
custom user-agent string `Aesthetic`, which its macOS sibling also sets;
handheld screen geometry separates the two.

## Missing

Unit sales before 2026-07-13 (the analytics backfill horizon), so the
Menu Band launch window 2026-05-07 to 2026-07-12 is invisible. Lifetime
download totals for every app, which require the Sales and Trends vendor
number. The date the Menu Band price moved from $4.99 to $9.99, which
the price-schedule endpoint does not retain.
