# arxiv-rhizome / data — first-pass record pull

First-pass dossier data assembled 2026-05-02. All numbers are sourced; gaps and TBDs are flagged inline rather than guessed.

## Files

- `financials.csv` — Form 990 line items per fiscal year, FY2011–FY2024. FY2011–FY2020 from ProPublica's JSON API v2; FY2021–FY2024 from IRS-direct XML (with both *original* and *amended* rows for FY2023 and FY2024). New columns vs the first pass: `filing_status`, `government_grants`, `all_other_contributions`, `fundraising_event_net`.
- `compensation.csv` — derived: ED of record by year, Part VII officer comp, Schedule J detail when filed, personnel as % of expenses. The FY2022 ED line is now broken out three ways (Connor + Kaplan + Bailey) instead of being attributed to Kaplan severance.
- `fundraising_events.csv` — Schedule G Part II per year. The auction has lost money in every year for which Schedule G detail is available (FY2022, FY2023-amended, FY2024).
- `grants.csv` — named grants from Mellon (7 confirmed, totaling **$4.011M** between 2015–2025), plus DAF pass-throughs (Fidelity Charitable, Daffy), Tribe family foundation, Teiger, plus partial entries for Knight, NEH, Warhol, Jerome, Greenwall, American Chai Trust, NEA, NYSCA, NYC DCLA, IMLS, Deutsche Bank Americas Foundation.
- `timeline.csv` — 1996 → 2026 program / leadership / funding events.
- `people.csv` — founders, executive directors, recent board (2021 cohort), key staff.
- `programs.csv` — every program identified, with launch year, status, principal funders.
- `990s/INDEX.md` — direct viewer/download URLs for every Form 990 filing back to FY2002 on ProPublica.
- `990s/xml/` — six raw XML filings (FY2021–FY2024 inclusive of amendments) pulled from apps.irs.gov; `990s/xml/INDEX.md` documents the pull.

## What's solid

- **Form 990 line items**: total revenue, contributions/grants, government grants vs other contributions, program-service revenue, investment income, fundraising-event gross + net, total expenses, officer compensation, other salaries, payroll tax, total assets, total liabilities, net assets, 5-year public-support test — FY2011 → FY2024. FY2011–FY2020 from ProPublica API v2; FY2021–FY2024 from IRS-direct XML.
- **The FY2023 and FY2024 amendments** are now visible. Each year was filed twice; the amendments materially restate revenue and event accounting (FY2023 amended -$125K vs original; FY2024 amended -$101K vs original; FY2024 amendment also restated the GALA from "$89K of pure contributions, no event" to "$104K gross with $84K of expenses → -$67K net").
- **Mellon Foundation grants**: **7 grants totaling $4.011M** between 2015 and 2025 (per Mellon's own database + CauseIQ). Mellon is by far the dominant institutional funder of Rhizome's last decade.
- **Auction (Schedule G fundraising-event) revenue per year** is now extracted: never above ~$36K gross in any year through FY2020; jumped to $103K–$123K gross FY2022–FY2024 (rebranded "GALA" by FY2024) but **net loss in every year for which Schedule G detail is available** (-$42,947 FY2022, -$68,967 FY2023-amended, -$67,115 FY2024-amended). The benefit gala is symbolically important and increasingly expensive but consistently fundraises *negative*.
- **Government grants** as a separately broken-out line: $296,794 (FY2021 — COVID peak) → $163,122 (FY2022) → $107,000 (FY2023) → $99,500 (FY2024). Steady decline post-COVID.
- **ED compensation per year**, FY2011→FY2024, with names attached. Importantly, the **FY2022 $241,690 line that earlier looked like a Kaplan severance bump is actually three EDs in transition**: Connor (Co-ED) $96,855 + Kaplan (ED through April 2022) $86,802 + Bailey (Co-ED) $58,032 ≈ $241,689. Schedule J shows Kaplan's total comp was $97,909 (base $86,802 + other $11,107), no bonus, no deferred — i.e. it was *not* an inflated severance. The number was a transition-period sum.
- **Personnel as % of total expenses** is consistently 47–77% across the decade (revised slightly upward for FY2021–FY2022 because IRS XML reports total Part I line 15 incl. benefits, where ProPublica only had wages).
- **Program timeline** (mostly): launch dates for ArtBase (1999), Commissions (2001), New Museum affiliation (2003), preservation program (2010), Seven on Seven (2010), Webrecorder (2016 public release), Net Art Anthology (2016 launch / 2019 conclusion), Conifer (2020 rename / 2026 sunset).
- **Leadership chain**: Tribe (1996–2006) → Cornell (2006–2012) → Corcoran (2012–2016) → Kaplan (2016–April 2022) → Bailey + Connor co-EDs (April 2022 onwards) → Connor (post-Bailey).
- **FY2024 board roster (per IRS XML)**: 18 voting members. Notable seats: Mark Tribe (founder, returned to board), Lisa Phillips (director of the New Museum — direct parent-org tie), Skawennati (artist + Initiative for Indigenous Futures, tied to NEH grant), Greg Pass (ex-CTO of Twitter, board chair through Nov 2024), Trevor McFedries (Brud / Lil Miquela founder, departed Sept 2024), Keith Obadike + Mendi Obadike (married artistic collaborators — Schedule O discloses this), Lindsay Howard, JK Brown (board president), Fred Benenson (vice chair), Lisa Roumell (treasurer), Karen Wong, John Wotowicz, Tracy Chou, Hillary Neve, Renny Gleeson, William Palmer, Ron Rosenzweig.
- **Pre-2011 filings indexed**: ProPublica has PDFs back to FY2002 (the org's first 990). FY2009 and FY2010 are 990-EZ (revenue under $200K threshold) — the org downshifted to the simplified form during the financial crisis. URLs preserved in `990s/INDEX.md`.

## What's known but not fully quantified

- **NEA grants**: Rhizome receives recurring NEA support for the Commissions Program. Full year-by-year list is in NEA's grant database (`grantsearch.nea.gov`) but the public search interface is a JS-rendered Kendo grid that doesn't return data via simple HTTP. Need a manual browser-based pull, or to identify the AJAX endpoint, or to download the NEA's annual discipline-grants PDFs.
- **Warhol Foundation**, **Jerome Foundation**, **Greenwall Foundation**, **Teiger Foundation**, **American Chai Trust**: confirmed as recurring funders of the Commissions Program; specific year/amount rows still TBD.
- **Knight Foundation**: $200K confirmed for Webrecorder public engagement; year and full grant history TBD.
- **NEH**: $45K confirmed for *Early Online Communities in Context* (The Thing BBS + CyberPowWow with Initiative for Indigenous Futures); year TBD. Skawennati on the Rhizome board is co-director of Initiative for Indigenous Futures — direct organizational tie.
- **IMLS**: confirmed funder of digital social memory research; specific grants TBD.
- **NYSCA** and **NYC DCLA**: confirmed recurring funders; year-by-year amounts TBD via state public records.
- **Pre-FY2021 Schedule G/J/R detail**: ProPublica's API gave us line items but not Schedule details for FY2011–FY2020. The IRS publishes XML for tax years 2017+ but pre-2022 batches require multi-GB downloads (no range support). Deferred unless we need the prose or the related-party detail.
- **Pre-2017 filings**: ProPublica viewer pages (HTML) and PDFs only; no IRS-direct XML for FY2002–FY2015 / FY2016.

## What's missing entirely (not yet attempted)

- **Schedule R on the New Museum's 990** — to map Rhizome / New Museum financial entanglement. We now know Lisa Phillips (New Museum director) sits on Rhizome's board, and Rhizome's address is `235 Bowery c/o New Museum`, strongly suggesting in-kind support that would appear as a related-org disclosure on the New Museum's filing. Rhizome's own FY2024 990 doesn't include a Schedule R section we've extracted yet.
- **Pre-2017 990 XMLs** — only PDFs available; line items already in `financials.csv` from ProPublica.
- Annual report PDFs from rhizome.org (where published) — would supply named-donor lists by giving level. rhizome.org is also Cloudflare-protected against direct fetch.
- **Rhizome Commissions list** by year (artist + project + funder credit) — would let us tie funders to specific commissioned works.
- **ArtBase accessions list** — public on rhizome.org but Cloudflare-blocked.

## Data quality notes

- The 990 fiscal year ends June 30. So "FY2022" means July 2021–June 2022. Many press / grant announcements use calendar year; cross-walks may need adjustment by ±1 year.
- The org is sometimes listed as "Rhizome Communications, Inc.", sometimes "Rhizome Communications", sometimes "Rhizome", and sometimes "Rhizome.org" in funder databases. Searches need all variants.
- Both FY2023 and FY2024 were filed twice: an original return then an amendment ~12 months later. The amendments restate revenue downward and surface fundraising-event detail that the originals omitted. Use the amended figures unless explicitly comparing original-vs-amended.
- The FY2024 amended 990 discloses Connor + Bailey both at $109K–$118K reportable comp; **neither crossed the $150K threshold that triggers a full Schedule J Part II compensation breakdown** for FY2024. The org last filed a Schedule J Part II for FY2022 (covering Kaplan).
- "Senior management reviews and approves and submits to board of trustees" appears in Schedule O Part VI line 11b for both FY2022 and FY2024 — i.e. management originates compensation policy and the board ratifies. Comp itself is set via "competitive search."
- "AVAILABLE UPON REQUEST" appears in Schedule O Part VI line 19 — i.e. governing documents (bylaws, conflict-of-interest policy) are not posted publicly on the org's website.

## Source URLs

- ProPublica record: https://projects.propublica.org/nonprofits/organizations/133995725
- GuideStar profile: https://www.guidestar.org/profile/13-3995725
- CauseIQ: https://www.causeiq.com/organizations/rhizome-communications-inc-co-new-museum-of-contem,133995725/
- Wikipedia: https://en.wikipedia.org/wiki/Rhizome_(organization)
- IRS direct e-file XML (the Cloudflare-bypass canonical source): https://apps.irs.gov/pub/epostcard/990/xml/
- Mellon Foundation grants database: https://www.mellon.org/grant-details/ (search "Rhizome")
- NEA grant search (JS-blocked): https://grantsearch.nea.gov/
- Rhizome 25 announcement (governance + Mellon Change Capital): https://rhizome.org/editorial/2021/oct/06/strengthening-rhizome-for-its-next-25/
