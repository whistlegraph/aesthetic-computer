# arxiv-eyebeam / data — first-pass record pull

First-pass dossier data on Eyebeam (Eyebeam Atelier, Inc., EIN 13-3952075) assembled 2026-05-02. Eyebeam is the NYC art-and-technology nonprofit founded by John S. Johnson III in 1997–1998; the data path is the same as Rhizome's: IRS XML / ProPublica + funder grant databases + contemporaneous press.

## Files

- `financials.csv` — Form 990 line items per fiscal year, FY2011–FY2024. Sourced from the ProPublica Nonprofit Explorer record summary; raw IRS XML downloads not yet pulled (URLs in `990s/INDEX.md`).
- `people.csv` — founders, executive directors, current (2026) board, advisory board, key staff, notable alumni residents.
- `programs.csv` — every program identified, with launch year, status, principal funders.
- `timeline.csv` — 1996/1997 → 2026 program / leadership / funding events.
- `funders.csv` — institutional funders from Eyebeam's own `eyebeam.org/support/` page, plus government and confirmed funders. Year/amount entries filled where confirmed.
- `grants.csv` — named grants with confirmed dates and amounts (Mellon ×4 confirmed, Henry Luce, Ford, plus partial entries).
- `locations.csv` — 540 W 21st (Chelsea, founding) → Industry City Sunset Park (2014–2017) → 199 Cook Street, Bushwick (2017–).
- `990s/INDEX.md` — direct viewer/download URLs for Form 990 filings on ProPublica (TBD — first-pass placeholder).

## What's solid

- **Legal entity**: `Eyebeam Atelier, Inc.` — EIN 13-3952075, 501(c)(3) since October 1997, NTEE coded under "Employment, Job-Related / Vocational Training" (an idiosyncratic NTEE choice for an art-and-technology nonprofit, but consistent across ProPublica records). Founded by John S. Johnson III (filmmaker, J&J family heir), with co-founders David S. Johnson (digital artist) and Roderic R. Richardson.
- **Form 990 line items, FY2011–FY2024**: total revenue, contributions/grants, program-service revenue, total expenses, total assets, total liabilities, net assets — pulled from ProPublica's summary table. ED comp is surfaced for FY2024 only (Schrock $125,491 + $9,888 other) on the public summary; pre-2024 ED-comp by name requires the IRS XML pull.
- **The 2014 Chelsea-exit cash event is visible in the line items**: FY2013 revenue $2.41M, FY2014 revenue $2.30M against a decade-average of ~$1.0–1.5M. Press confirms "seed funding for the move from Chelsea was provided by the Atlantic Foundation with proceeds from the sale of Eyebeam's former building on West 21st Street" (Technically Brooklyn, 2014).
- **Mellon Foundation grants**: **4 grants totaling $992,400** confirmed via the Mellon Foundation grants database, plus a Democracy Machine entry that press reports at $1.1M total (multiple Mellon line items aggregated). Mellon is the largest documented institutional funder of the post-2020 era.
- **Henry Luce Foundation**: $150,000 confirmed (April 2020 COVID Emergency Grant, Public Policy program).
- **Ford Foundation**: $650,000 confirmed via press for the Democracy Machine; the direct grant-database entry at `fordfoundation.org/work/our-grants/awarded-grants/grants-database/eyebeam-atelier-inc-139325/` was 404'ing at fetch time (2026-05-02) — needs a re-pull or a different Ford record ID.
- **Leadership chain (six EDs)**: Johnson (1997–2004) → Tremble (2004–2005) → Crowley (2005–2011) → Jones (2011–2015) → Schrock (2015–2025) → Prajapati (interim, July 2025–April 2026) → Kaganskiy (April 2026–).
- **Locations (three eras)**: 540 W 21st St, Chelsea (Manhattan, 1998–2014); Industry City, Sunset Park (Brooklyn, 2014–2017); 199 Cook Street, Suite 104, Bushwick (Brooklyn, 11222, 2017–). The unbuilt Diller Scofidio museum (designed 2000–2001 for the W 21st site) is in MoMA's collection as a scale model.
- **2025/2026 leadership transition is fully sourced**: Schrock departure announcement March 2025, Prajapati announcement June 30 2025, ED search opened November 2025, Kaganskiy announced April 6 2026.

## What's known but not fully quantified

- **NEA grants**: Eyebeam appears as a recurring NEA grantee on the org's own funder list, but specific year/amount entries are not yet pulled. Same JS-grid problem as the Rhizome dossier hit at `grantsearch.nea.gov`.
- **NYSCA / NYC DCLA**: confirmed recurring funders (`eyebeam.org/support/` lists NYSCA explicitly); year-by-year amounts TBD via state public records.
- **Atlantic Foundation $500K+ entry**: confirmed by press ("seed funding for the move from Chelsea") and visible on the funder page at the $500K+ tier; specific year and grant amount have not been pinned down separately from the W 21st building proceeds.
- **Pre-2020 Mellon grants**: there is no Mellon record before April 2020 in the database for Eyebeam Atelier; this is consistent with Mellon's relationship to Eyebeam being a 2020-onwards relationship rather than a Webrecorder-style 2015-onwards relationship (the difference between the Eyebeam and Rhizome dossiers).
- **Andy Warhol Foundation, MacArthur Foundation, Surdna, Teiger, Helen Frankenthaler, Stavros Niarchos, Cy Twombly, Willem de Kooning Foundation, Capital One, Craig Newmark Philanthropies**: all confirmed on the support page; year-by-year entries TBD.
- **Pre-FY2011 financials**: ProPublica likely has 990 PDFs back to 2002 or earlier; not yet indexed into `990s/INDEX.md`. The Diller Scofidio architectural-fundraising era of the early 2000s is not visible in this first pass.
- **Government-grants line**: Eyebeam's filings appear to lump government grants into "All Other Contributions" rather than breaking them out as a separate line, contra Rhizome's FY2021+ practice. This means CSV column `government_grants` is empty even where NYSCA / DCLA / NEA funding exists.
- **Officer compensation by year**: only FY2024 ED comp is on the ProPublica summary card. Pre-2024 ED comp by year requires the IRS XML pull (deferred for first pass).
- **2018 reorganization**: the prompt mentioned a "2018 Brendan Fernandes era" but I could not corroborate a Brendan Fernandes connection to Eyebeam in the public record — Brendan Fernandes is a Toronto-Chicago artist with no Eyebeam tenure. The 2018 events were the 20th-anniversary programming and the launch of the Center for the Future of Journalism under Marisa Mazria Katz; flagged here so it doesn't propagate.

## What's missing entirely (not yet attempted)

- **IRS XML downloads for FY2017+** — referenced in the dossier but not yet pulled to `990s/xml/`. The XML would surface board roster, Schedule J officer-compensation detail, Schedule G fundraising-event accounting, and Schedule R related-org disclosures.
- **Pre-2020 Mellon search** — Mellon's database UI does not always surface older grants; CauseIQ / 990 Schedule B detail would catch any pre-2020 institutional grants that aren't in the Mellon DB.
- **Residency cohort lists by year** — `eyebeam.org/artists/` is the public archive; not yet scraped for this first pass. The 550+ artists figure on the About page is an aggregate.
- **Center for the Future of Journalism funded-project list** — would let us tie specific Pulitzer/Emmy citations to Eyebeam financial support.
- **Diller Scofidio building fundraising record** — the early-2000s capital campaign for the unbuilt W 21st building should appear on early 990s as a building-fund line; not yet pulled.
- **2020 Rapid Response cohort full list** — 30 artists Phase I, 8 Phase II; partial names recoverable from press (Valencia James, Harris Kornstein) but no consolidated roster pulled.

## Data quality notes

- The 990 fiscal year ends June 30. So "FY2022" means July 2021–June 2022. Many press / grant announcements use calendar year; cross-walks may need adjustment by ±1 year.
- The legal name on every IRS filing is "Eyebeam Atelier, Inc." but every funder database uses some variant: "Eyebeam Atelier", "The Eyebeam Atelier, Inc.", "Eyebeam", "Eyebeam Art and Technology Center". Searches need all variants.
- Wikipedia gives the founding year as 1998, citing the Digital Day Camp launch and the Interaction online forum. The IRS 501(c)(3) determination is dated October 1997 (per ProPublica); the prompt's "1996" figure does not appear in any source I could verify and is likely a conflation with Rhizome (which is 1996 in Berlin, 1997 in NYC, with the IRS determination August 1999). The dossier uses 1997–1998 throughout.
- The Wikipedia article header was updated to reflect the leadership transition (Schrock 2015–2025, Prajapati 2025–) but had not yet caught up to the April 2026 Kaganskiy announcement at the time of fetch.
- Roderic R. Richardson's role is described as "mutual friend" / co-founder in Wikipedia. He is not listed as an executive director and his post-founding involvement is not documented.

## Source URLs

- ProPublica record: https://projects.propublica.org/nonprofits/organizations/133952075
- GuideStar profile: https://www.guidestar.org/profile/13-3952075
- CauseIQ: https://www.causeiq.com/organizations/the-eyebeam-atelier,133952075/
- Wikipedia: https://en.wikipedia.org/wiki/Eyebeam_(organization)
- Eyebeam financials page: https://eyebeam.org/financials/
- Eyebeam support / funders page: https://eyebeam.org/support/
- Eyebeam people page: https://eyebeam.org/about-us/people/
- Eyebeam changelog: https://eyebeam.org/changelog/
- Mellon Foundation grants database: https://www.mellon.org/grant-database/The%20Eyebeam%20Atelier,%20Inc. (404 at fetch; use individual grant-detail URLs in `grants.csv`)
- Henry Luce Foundation grant: https://www.hluce.org/grants/grants/covid-emergency-grant-eyebeam/
- Ford Foundation grant DB: https://www.fordfoundation.org/work/our-grants/awarded-grants/grants-database/ (404'd on direct Eyebeam record; needs re-pull)
- Hyperallergic 2017 Bushwick coverage: https://hyperallergic.com/eyebeam-new-bushwick-home-at-20/
- Technically Brooklyn 2014 Sunset Park coverage: https://technical.ly/brooklyn/2014/06/30/eyebeam-moved-sunset-park/
- ARTnews 2021 restructure coverage: https://www.artnews.com/art-news/news/eyebeam-residency-program-change-ford-foundation-1234584327/
- Artnet 2021 Democracy Machine coverage: https://news.artnet.com/art-world/eyebeam-new-fellowship-program-2026181
- IRS direct e-file XML (canonical): https://apps.irs.gov/pub/epostcard/990/xml/

## Relationship to the author

The author has no commission or formal relationship with Eyebeam recorded as of 2026-05-02. The org is a sibling of Rhizome (where the author had a 2022 First Look commission), and shares two known board / advisory members with the AC orbit (Salome Asega — also Rhizome board; Ramsey Nasser — also recurring SFPC teacher). These ties are noted in the dossier as facts.
