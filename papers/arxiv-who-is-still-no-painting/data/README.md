# Data: Who Is Still Painting

Read 16 November 2026. Every file is an aggregate count; no painting codes, images, addresses or note text.

- `classic-saves-per-day.csv`: 2021 No Paint saves (`nopaint-classic-paintings`, by `savedAt` UTC day), from the analytics `nopaint_report` tool over 840 hours. 16 November is partial (to 21:41 UTC).
- `save-windows.csv`: the same saves in UTC windows, from `nopaint_report` with `hours` and `end`. The 10 November 18:00–24:00 row is the day total less the other windows. The 16 November 18:00 save comes from the 840-hour read's `lastSavedAt`.
- `search-nopaint.csv`: Google Search Console for nopaint.art by date and by country (data ends 14 November).

Visits per day come from the analytics `daily_metrics` tool (non-automated visits, UTC days). App downloads from `app_downloads`.

Not available: lith's access log (it rolls within three days and the read was not run) and therefore any network or country data for 10, 11 and 16 November.
