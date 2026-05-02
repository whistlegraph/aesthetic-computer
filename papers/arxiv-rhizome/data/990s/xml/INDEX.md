# Rhizome 990 XML files (raw, IRS-direct)

These XMLs were pulled from the IRS public e-file dataset
(`https://apps.irs.gov/pub/epostcard/990/xml/{year}/`) — the canonical, no-auth,
no-Cloudflare source. Path: `apps.irs.gov/pub/epostcard/990/xml/<filing-year>/<batch>.zip`,
each XML inside named `<OBJECT_ID>_public.xml`.

Discovery worked by reading the per-year `index_<YEAR>.csv` for the EIN
(`133995725`) → the row gave the OBJECT_ID, and either the `XML_BATCH_ID` (2024+)
or a search across the year's batches (pre-2024) located the zip.

Tooling lives in `/tmp/fetch_irs_xml.py` (uses `remotezip` for HTTP-range central
directory reads) and `/tmp/fetch_all_rhizome.sh` (batch driver).

## Files in this directory

| File | Tax period | Filed | OBJECT_ID | Notes |
|------|-----------|-------|-----------|-------|
| `rhizome-FY2021-202210259349301111.xml` | 2020-07 → 2021-06 | 2022 | `202210259349301111` | only filing |
| `rhizome-FY2022-202341359349313669.xml` | 2021-07 → 2022-06 | 2023 | `202341359349313669` | only filing — Schedule J included (Kaplan) |
| `rhizome-FY2023-original-202421359349313247.xml` | 2022-07 → 2023-06 | 2024 | `202421359349313247` | original; total_revenue $882,002 |
| `rhizome-FY2023-amended-202520359349301517.xml` | 2022-07 → 2023-06 | 2025 | `202520359349301517` | amended; total_revenue $756,974 (-$125K from original) |
| `rhizome-FY2024-original-202541359349310014.xml` | 2023-07 → 2024-06 | 2025-06 | `202541359349310014` | original; total_revenue $923,010; gala booked as $89,577 of pure contributions, no event |
| `rhizome-FY2024-990-amended-202521899349301402.xml` | 2023-07 → 2024-06 | 2025-07 | `202521899349301402` | amended; total_revenue $821,964; gala restated to $104,407 gross w/ -$67,115 net loss |
| `newmuseum-FY2023-202440509349300734.xml` | 2022-07 → 2023-06 | 2024 | `202440509349300734` | New Museum 990; Schedule R lists Rhizome as related-but-not-controlled |
| `newmuseum-FY2024-202540809349300124.xml` | 2023-07 → 2024-06 | 2025 | `202540809349300124` | New Museum 990; Schedule R lists Rhizome as related-but-not-controlled |

## Filings indexed but not yet pulled

The IRS publishes machine-readable e-file XML for tax years 2017+ via apps.irs.gov,
but pre-2022 batches are packaged in 7–8 chunks of ~390MB each that don't support
HTTP range requests, so each chunk has to be fully downloaded. These are deferred:

- FY2020 (`202003359349300915`, in `download990xml_2020_*.zip`)
- FY2019 (`202030309349300328`, in `download990xml_2020_*.zip`)
- FY2018 (`201900309349300620`, in `download990xml_2019_*.zip`)
- FY2017 (`201800299349301230`, in `download990xml_2018_*.zip`)
- FY2016 (`201700259349300905`, in `download990xml_2017_*.zip`)

Pre-2017 (FY2002–FY2015) is not on apps.irs.gov; only PDFs from ProPublica's viewer
are available for those years (see `../INDEX.md`).

The pre-FY2021 line items already exist in `../../financials.csv` from the ProPublica
JSON API. Pulling the XMLs would only add Schedule G / J / O / R detail, which is
nice-to-have, not load-bearing for the dossier.
