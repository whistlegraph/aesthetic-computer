# Data: Who Is Painting on nopaint.art?

Pulled 4 November 2026. Every file here is aggregate or already public.

- `classic-saves-per-day.csv`: rows in Mongo `nopaint-classic-paintings` by `savedAt` UTC day (read-only query on lith).
- `classic-saves-since-spike.csv`: save timestamps (UTC and Lima, UTC-5) for the 331 saves from 2026-11-02T18:00Z on. Painting codes are omitted.
- `upload-networks-wed.csv`: network owner (ASN name via Team Cymru) for the `POST /upload-painting` lines still in lith's Caddy access log (oldest line 2026-11-04T13:36:57Z). No addresses were printed or kept.
- `nopaint-visits.csv`: first-party page visits to nopaint.art from lith's network-visits collector, in 24-hour windows.
- `notepaint-paintings-public.json`: notepaint.art's public sitemap and page titles for the 30 paintings saved over the same days.

Solid: save counts, timestamps, visit counts. Soft: network attribution (Tuesday's log lines had rolled away; carriers do not identify schools). Missing: who the teacher is.
