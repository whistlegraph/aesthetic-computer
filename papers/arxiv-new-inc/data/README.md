# arxiv-new-inc / data — first-pass record pull

First-pass dossier data assembled 2026-05-02. NEW INC is a program of the New Museum of Contemporary Art (EIN 13-2986881); it does **not** have a separate IRS exemption. All financial recovery has to flow through the parent-org 990 — a tractability problem flagged throughout.

## Files

- `financials.csv` — Form 990 line items for the **New Museum** (parent org), fiscal years FY2016 onward. NEW INC line items are **not** separately broken out in any year. Sponsor revenue and member fees plausibly route through the parent-org contributions and program-service lines but are not isolable from the public 990s.
- `people.csv` — co-founders Lisa Phillips + Karen Wong, founding director Julia Kaganskiy (2014–2018), Stephanie Pereira (2018–2021), Salome Asega (2021–), parent-org leadership.
- `funders.csv` — confirmed institutional supporters: Mellon, Knight, Patrick J. McGovern Foundation, Simons Foundation, EY (Metaverse Lab), Onassis ONX, Jonathan D. & Mark C. Lewis Foundation, plus prior partners Meta Open Arts, Nokia Bell Labs.
- `grants.csv` — named grants where amount and date are confirmed: Mellon $1.5M / 36mo (Dec 2022) for "New Director Vision" supporting Asega; Knight $250K (April 2017) for Museum Tech Track; Knight $660K follow-on; Mellon $486K (March 2011, parent-org pre-NEW INC).
- `timeline.csv` — 2013 conception, 2014 launch, 2017 Knight, 2018 Pereira, 2021 Asega, 2022 Mellon $1.5M, 2024–2025 Year 11 / Year 12 cohorts, March 2026 OMA expansion + Phillips retirement, April 2026 Kaganskiy named Eyebeam ED.
- `locations.csv` — 231 / 235 Bowery (NEW INC has been embedded in the New Museum's adjacent building since launch); 60,000 sq ft OMA-designed expansion opens March 21 2026 with NEW INC's first "fully dedicated" permanent space.
- `programs.csv` — current 5-track structure (Art & Code / Creative Science / Extended Realities / Social Architecture / Cooperative Studies) and notes on the user-prompt-supplied earlier 3-track structure (Art-Design-Technology / Strategic Program for Open Knowledge / Creative Science) that appears to have restructured under Asega.
- `cohort-members.csv` — partial Year 11 Art & Code cohort (5 members extracted from Rhizome's announcement post); Year 12 total of 74 across 5 tracks; Asega's own pre-directorship membership (2016–17) noted as institutional continuity.
- `990s/` — empty placeholder. The parent-org 990 PDFs are downloadable from ProPublica's profile (https://projects.propublica.org/nonprofits/organizations/132986881) but were not pulled for this first pass; the financials are summary numbers from the ProPublica list view.

## What's solid

- **The embedding fact.** NEW INC has no IRS EIN of its own. It is a program embedded in the New Museum (EIN 13-2986881). All federal-tax-record financial recovery flows through the parent-org Form 990, which does not break NEW INC out as a line item. This is the central data-tractability constraint of any NEW INC dossier and is foregrounded in the dossier text.
- **Co-founders.** Lisa Phillips (Toby Devan Lewis Director, parent org) and Karen Wong (Deputy Director). Both are named on press materials as co-founders. Conception date 2013, public launch September 2014.
- **Founding director Julia Kaganskiy (2014–April 2018).** Came from VICE Media's *The Creators Project* (Global Editor 2010–2014); had founded ArtsTech meetup 2008. Built NEW INC across roughly 4 years.
- **Pereira interregnum.** Stephanie Pereira (ex-Kickstarter, first director of arts program at Kickstarter) appointed May 2018, started July 2018, departed May 2021.
- **Asega era (July 2021–).** Salome Asega arrived from Ford Foundation (4 years as inaugural New Media Art Research Fellow) + Parsons. Was herself a NEW INC member 2016–17, giving the appointment an institutional-continuity reading. Has reorganized DEMO Day into the multi-day DEMO festival, restructured the track system, raised Mellon's largest single NEW-INC-tagged grant.
- **Mellon $1.5M / 36mo "New Director Vision" grant (December 8, 2022).** Confirmed via Mellon's grant database. Largest single confirmed institutional grant tagged to NEW INC.
- **Knight $250K (April 13, 2017) Museum Tech Track.** Confirmed via Knight Foundation press release. Year 4 expansion subsidizing 5 teams plus Museum Tech Bootcamp.
- **Five current tracks (Year 12, 2025-26):** Art & Code (in partnership with Rhizome, supported by Patrick J. McGovern Foundation); Creative Science (Simons Foundation); Extended Realities (EY Metaverse Lab + Onassis ONX); Social Architecture (Jonathan D. & Mark C. Lewis Foundation); Cooperative Studies. 74 new members for 2025–26.
- **2026 expansion + leadership transitions.** OMA-designed 60,000 sq ft expansion opens March 21 2026; doubles the museum to 120,000 sq ft; gives NEW INC its first dedicated permanent space. Phillips retires April 2026 after 27-year directorship. **April 6 2026: Julia Kaganskiy named Eyebeam ED** — connecting NEW INC to two other AC dossiers (Eyebeam, Rhizome).

## What's known but not fully quantified

- **NEW INC member fees.** NEW INC charges a membership fee and operates a tiered model; specific dollar amounts per cohort year are not pulled here. Likely on the public application page (`newinc.org/apply`) but the page redirects through `newmuseum.org`.
- **Track-level sponsorship dollar amounts.** Patrick J. McGovern Foundation, Simons Foundation, EY, Onassis ONX, Jonathan D. & Mark C. Lewis Foundation — confirmed as track sponsors but specific grant amounts not pulled.
- **Knight Foundation $660K follow-on.** Year and term TBD; cited in a subsequent Knight press release.
- **Years 1–4 cohort lists.** Press coverage (Fast Company on the founding cohort) exists but full member rosters were not extracted in this first pass.
- **Year 12 per-track member counts.** 74 total across 5 tracks confirmed; per-track splits not extracted.
- **DEMO festival sponsor history year-by-year.** Meta Open Arts cited as a prior sponsor; current sponsor stack TBD.

## What's missing entirely (not yet attempted)

- **Direct extraction from the New Museum's Form 990 of any NEW-INC-flagged narrative.** Schedule O sometimes includes program descriptions that mention NEW INC by name; the parent-org 990s on ProPublica back to FY2014 should be pulled and grep'd. Not done in this first pass.
- **NEW INC's own annual report / impact PDFs.** If published, would carry alumni-funding totals, track-by-track sponsor budgets, member fees. The most reliable cited figure — "NEW INC alumni have raised over \$28.9 million across their ventures over the past decade" — is from Phillips's March 2026 *Art Newspaper* expansion announcement. Earlier, smaller figures appear in older press (e.g. "more than \$12 million" cited in Year-1 era coverage).
- **Advisory Council 2024 + 2025 membership lists.** A New Museum press release headlined "NEW INC … Announces 2024 Advisory Council Members" exists but the page returned empty content on fetch. Direct manual pull TBD.
- **Year 11 (2024–25) full cohort list across all five tracks.** Only the Art & Code subset (5 members) was extracted from Rhizome's partner announcement.

## Data quality notes

- **NEW INC vs The New Museum.** In funder databases (Mellon especially), NEW-INC-related grants are *recorded under* "New Museum of Contemporary Art" — not under "NEW INC." A grant database search for "NEW INC" alone misses everything.
- **231 vs 235 Bowery.** NEW INC press materials use both addresses; the New Museum's main building is 235 Bowery (SANAA, 2007), and NEW INC has historically used the adjacent 231 Bowery space within the parent-org footprint.
- **Track-name continuity.** The user-supplied earlier track names — Art-Design-Technology (ADT), Strategic Program for Open Knowledge (SPOK), Creative Science — are not surfaced on the current NEW INC site. The Asega-era restructure (post-2021) appears to have consolidated and renamed; primary-source confirmation of the exact transition year TBD.

## Source URLs

- ProPublica record: https://projects.propublica.org/nonprofits/organizations/132986881
- GuideStar profile: https://www.guidestar.org/profile/13-2986881
- Charity Navigator: https://www.charitynavigator.org/ein/132986881
- New Museum: https://www.newmuseum.org/new-inc/
- NEW INC current site: https://www.newinc.org (redirects to newmuseum.org/new-inc/)
- Mellon Foundation grant database: https://www.mellon.org/grants/grants-database/grants
- Knight Foundation grant pages: https://knightfoundation.org/press/releases/
- Wikipedia: https://en.wikipedia.org/wiki/Julia_Kaganskiy ; https://en.wikipedia.org/wiki/Salome_Asega ; https://en.wikipedia.org/wiki/New_Museum
