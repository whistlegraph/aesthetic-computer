# arxiv-mellon / data — first-pass record pull

First-pass dossier data assembled 2026-05-02. Mellon is a *funder*, not a recipient, so this dossier is structured around the giving record. Numbers are sourced; gaps and TBDs are flagged inline rather than guessed.

## Files

- `financials.csv` — Form 990-PF top-line per fiscal year, FY2014–FY2024, from ProPublica Nonprofit Explorer (EIN 13-1879954). Columns: `fy_end`, `total_assets`, `total_revenue`, `investment_income`, `grants_paid`, `total_expenses`. Note: a private foundation's total revenue line is dominated by realized gains on asset sales; it bears no resemblance to a public-charity 990 revenue line.
- `people.csv` — Presidents (with full tenure dates 1969→present), board chairs (partial lineage), current trustees, key senior staff.
- `programs.csv` — current 5 program areas with descriptions.
- `grantees.csv` — flagship Mellon commitments by year-and-recipient. Larger than `recipient-spotlight.csv` in scope (any recipient) but shallower per-row.
- `recipient-spotlight.csv` — per-grant rows for a fixed list of digital-arts-adjacent recipient orgs (Rhizome, Internet Archive, ITHAKA/JSTOR, HathiTrust, Eyebeam, Pioneer Works, Studio Museum in Harlem, Whitney Museum, National Gallery of Art, Smithsonian, MIT/MIT Press, Wikimedia, SFPC). 2010–2025 window.
- `timeline.csv` — 1940 (Avalon) → 2026 program / leadership / strategy events.
- `locations.csv` — NYC headquarters + historical premises.

## What's solid

- **Founding**: Avalon Foundation (1940, Ailsa Mellon Bruce) + Old Dominion Foundation (1941, Paul Mellon) merged on 30 June 1969 as Avalon, renamed Andrew W. Mellon Foundation. Combined assets at merger \$273M; combined giving in 1968 \$11M.
- **Headquarters**: 140 East 62nd Street, New York, NY 10065.
- **EIN**: 13-1879954. Tax-exempt status September 1956 (continuity of Avalon).
- **Presidents** with exact dates from `mellon.org/history`:
  - Charles S. Hamilton, Jr. (1969–1971)
  - Nathan M. Pusey (1971–1975)
  - John E. Sawyer (1975–1987)
  - William G. Bowen (1988–2006)
  - Don Michael Randel (2006–2013)
  - Earl Lewis (2013–2018)
  - Elizabeth Alexander (2018–present)
- **2020 rebrand**: announced June 2020 by Pentagram (with Alexander, CCO Vanessa Corrêa). Dropped "Andrew" — formal "The Andrew W. Mellon Foundation" → "Mellon Foundation." Coupled with strategic shift toward social-justice grant-making.
- **Monuments Project**: announced 5 October 2020 with $250M commitment. Doubled to $500M on 28 November 2023. As of Nov 2023, $170M+ across 80+ projects.
- **Creatives Rebuild New York**: 28 June 2021. $115M Mellon grant via Tides Center, plus $5M Ford + $5M SNF for $125M total. 2,700 NY State artists.
- **FY2024 grants paid**: $635.4M, ~650 grants. FY2024 total assets $7.82B.
- **Endowment trajectory** (year-end approx): 1969 $273M → 1980 $880M → 2014 $6.43B → 2021 peak $9.55B → 2024 $7.82B.
- **Cumulative grant-making 1969–2023**: foundation reports ~$9.1B.
- **Board chair** as of 2026: Kathryn A. Hall (since March 2019). Predecessor: Danielle S. Allen (chair 2015–2019, board member 2008–2019).

## Confirmed individual recipient grants (per per-grant URLs)

- Internet Archive, $200K, 5 Mar 2020, 24 mo., Public Knowledge — ML for online scholarly journal preservation.
- Internet Archive, $1.13M, 18 Sep 2020, 24 mo., Public Knowledge — Community Webs national network.
- Eyebeam, $600K, 11 Jun 2021, 36 mo., Presidential Initiatives — general operating.
- Eyebeam, ~$150K, 2020, Rapid Response Fund for a Better Digital Future.
- ITHAKA (JSTOR), $1.5M, Sep 2021 — JSTOR-in-Prison program.
- HathiTrust, $1M, Feb 2023, 5 yr — core operations strengthening.
- Smithsonian, $250K, 11 Mar 2022, 24 mo., Arts and Culture — "1898: The American Imperium."
- Whitney Museum, $500K, May 2022 — "no existe un mundo poshuracán" exhibition.
- Studio Museum in Harlem, $5M, Sep 2018 — capital campaign + capacity building. Multiple Curatorial Fund phases (I, II, III) over a decade.
- National Gallery of Art, $30M, March 2016 — 75th-anniversary endowment challenge; matched with $50M for $80M total endowment lift.
- Tides Center / Creatives Rebuild New York, $115M, 28 Jun 2021, 36 mo.
- ALAM (Latinx Art in Museums) — $5M shared with Ford, Getty, Terra Foundations, Feb 2023; ten $500K curatorial-position grants.

## Rhizome cross-reference

The `arxiv-rhizome` dossier maps **7 Mellon grants totalling $4.011M** to Rhizome between 2015 and 2025:

- 2015-12-10  $600K   Webrecorder development (24 mo)
- 2017-12-07  $1.0M   Webrecorder Phase Two (24 mo)
- 2019-12-09  $146K   Webrecorder sustainability planning (6 mo)
- 2021-09-13  $1.0M   Change Capital (60 mo)
- 2024-06-24  $750K   ArtBase overhaul (36 mo)
- 2024-12     $195K   Change-capital business model continuation
- 2025        $320K   project unspecified

Source: Mellon Foundation grants database + CauseIQ. See `papers/arxiv-rhizome/data/grants.csv` for per-row provenance.

## What's known but not fully quantified

- **Mellon Mays Undergraduate Fellowship**: long-running named program in higher education, 1988–present. Cumulative spend across the program is in the foundation's annual reports but not extracted to this dossier.
- **Per-program-area annual breakdown**: the FY2024 annual report references ~650 grants / ~$540M but does not publish a per-program-area dollar breakdown in the parts of the report scraped here.
- **Qualifying distributions vs. grants paid**: Part XII of 990-PF is the regulatory line; this dossier has Part I line 25 (charitable disbursements) for FY2014–FY2024 from ProPublica but not Part XII. The two figures differ by program-related investments + qualifying admin costs + amounts set aside.
- **Investment portfolio composition**: 990-PF Part II has the asset-class breakdown (corporate stocks, bonds, real estate, alternatives). Not extracted in this pass.
- **Board chair lineage** before Allen: Hanna Holborn Gray (chair through ~2003 after 23 years on board), Anne M. Tatlock (~2003–2012), W. Taylor Reveley III (chair from ~2012). Exact handover dates TBD.

## What's missing entirely

- **SFPC Mellon grants (if any)**: SFPC is an LLC; tax-deductible giving runs through an unnamed fiscal sponsor. No Mellon-to-SFPC grant has been located in the database. If any exists it is recorded under the fiscal sponsor's name with SFPC as project.
- **Pioneer Works**: not located in the Mellon database despite multiple targeted searches. Pioneer Works' main institutional funders appear elsewhere (Mellon does not appear to be a primary funder).
- **MIT Press / Direct to Open**: known scholarly-communications context, but no per-grant URL located in this pull.
- **Eyebeam pre-2020 grants**: Mellon's news on the Eyebeam 2021 grant references "additional grants documented in 2024, 2023, 2020, and 2013" but those exact rows aren't in this CSV yet.
- **Pre-2014 financial line items**: only summary endowment milestones (1969 $273M, 1970 $700M, 1980 $880M) for the pre-ProPublica window. ProPublica's data goes back further; deeper extraction is deferred.
- **Top-100 recipients by aggregate dollars**: would require a full crawl of `mellon.org/grants/grants-database/`. The site is queryable but does not expose a stable JSON-export endpoint.

## Data quality notes

- Mellon's 990-PF fiscal year ends **December 31** (calendar year). Unlike Rhizome's June 30 FY end, Mellon-FY = calendar year, so "FY2024" is calendar 2024.
- The foundation is named "The Andrew W. Mellon Foundation" on all federal filings; "Mellon Foundation" is the doing-business-as adopted in June 2020 for public-facing communications. Funder databases use both names.
- Press announcements often cite *new commitments* (e.g. "Monuments Project: $500M") that are paid out over multiple years. The grants-paid line on a given 990-PF reflects *cash outflows* in that fiscal year, not commitments. The two views diverge significantly during multi-year flagship cycles.
- Mellon issued $300M of *social bonds* in July 2020 to fund COVID-emergency grants. This is borrowed capital on the foundation's balance sheet and inflates the FY2020–FY2022 grants-paid lines relative to a "pure 5%-of-assets" baseline.

## Source URLs

- Mellon history: https://www.mellon.org/history
- Mellon financials: https://www.mellon.org/financials
- Mellon trustees: https://www.mellon.org/people/trustees
- Mellon grant programs: https://www.mellon.org/grant-programs
- Mellon grants database: https://www.mellon.org/grants/grants-database/
- ProPublica record: https://projects.propublica.org/nonprofits/organizations/131879954
- IRS direct e-file XML: https://apps.irs.gov/pub/epostcard/990/xml/
- Pentagram rebrand case study: https://www.pentagram.com/work/the-mellon-foundation/story
- Wikipedia: https://en.wikipedia.org/wiki/Andrew_W._Mellon_Foundation
- InfluenceWatch: https://www.influencewatch.org/non-profit/andrew-w-mellon-foundation/
- Rhizome cross-reference: papers/arxiv-rhizome/data/grants.csv
