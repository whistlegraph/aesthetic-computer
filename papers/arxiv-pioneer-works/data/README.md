# arxiv-pioneer-works / data — first-pass record pull

First-pass dossier data assembled 2026-05-02. All numbers are sourced; gaps and TBDs are flagged inline rather than guessed.

EIN: **46-1097738** (Pioneer Works Art Foundation, Inc.)

## Files

- `financials.csv` — Form 990 line items per fiscal year, FY2012–FY2024. Pulled from ProPublica's Nonprofit Explorer summary table for EIN 46-1097738. Schedule G (auction/gala detail) and Schedule J (compensation breakdown) are deferred to a later IRS XML pull.
- `people.csv` — founders, executive directors / CEO, Director of Sciences, key staff, board members.
- `programs.csv` — every program identified, with launch year, status, principal funders.
- `funders.csv` — institutional and named individual funders (first pass).
- `grants.csv` — named grants TO Pioneer Works (sparse; the federal record at the FY2012–FY2024 ProPublica level does not break out government-grants line, and Mellon's database did not return Pioneer Works hits via the search interfaces tested).
- `timeline.csv` — 2011 (building purchase) → 2026 program / leadership / funding events.
- `locations.csv` — 159 Pioneer Street, Red Hook, Brooklyn (single primary site, with the 2024 ADA renovation as a major sub-event).
- `residents.csv` — historical resident lists by program area + year, where recoverable.

## What's solid

- **EIN and federal filing status**: 46-1097738, Pioneer Works Art Foundation, Inc., 501(c)(3) public charity, first 990 filed FY2012 (ProPublica).
- **Form 990 top-line, FY2012–FY2024**: revenue, contributions/grants, program-service revenue, total expenses, top-officer compensation, net assets — all 13 years. Source: ProPublica Nonprofit Explorer summary view.
- **Founding triad and timeline**: Yellin (founder/president, since 2012), Florenz (founding artistic director, since 2012; Yellin's second cousin), Levin (founding director of sciences, since 2014). Documented in multiple press accounts (BkMag 2022; Wikipedia; FvF 2017).
- **Building purchase**: 2011, $3.7M, by Yellin personally. Initial renovation budget ~$200K, "largely funded by his own art" (FvF 2017; BkMag 2022).
- **The Hurricane Sandy flood (Oct 2012)** and the 2013 reopening under the current name are confirmed across multiple sources.
- **Capital campaign**: $30M campaign launched 2019; $28.3M raised by Aug 2024; closed FY2025 under Manus's tenure, ahead of the original two-year goal.
- **2024 reopening**: 6 September 2024 after eight-month renovation; $12.7M total renovation cost ($2.3M from NYS / Empire State Development; $10.4M private). Renovation completion announced 13 February 2025.
- **Executive leadership chain**:
  - Yellin (President + Founder, 2012–)
  - Florenz (Founding Artistic Director, 2012–; expanded to Executive Director March 2025)
  - Levin (Director of Sciences, 2014–)
  - Shiner (first-ever Executive Director, appointed 19 Dec 2019, started 6 Jan 2020; departed 2021 for Powerhouse Arts)
  - Manus (inaugural CEO, September 2023–end of March 2025)
  - Hemshrot (COO, April 2025–)
- **Residency structure**: Visual Arts (5-month, 12 residents/year, $2,500 honorarium); Music (1-month, 12 residents/year, $1,000 honorarium); Technology (~3-month, formerly open-call, now invite-only). Narrative Arts piloted 2020.
- **Programs of record**: Second Sundays (2013–, free monthly open house); Village Fête (May 2014–, annual spring benefit); Broadcast (2020–, magazine; print Issue 01 Nov 2023); Pioneer Works Press; Scientific Controversies discussion series.
- **Board chair** (post-2023): Austin Hearst (Hearst family). Co-vice-chair (per ESD 2025 release): David Belt.

## What's known but not fully quantified

- **The Yellin-as-funder mechanism is well-attested in press but not quantifiable from public 990s.** Whether the building (159 Pioneer St, purchased 2011 by Yellin for $3.7M) is now owned by the foundation, by Yellin personally, or jointly is not surfaced in the ProPublica summary lines. The IRS Schedule L (transactions with interested persons) and Schedule R (related entities) for the post-2014 filings would resolve this; not pulled in this first pass.
- **Government-grants line is not broken out separately in the ProPublica summary**. Total contributions are listed but the contributions-vs-grants-vs-government-grants split requires the IRS XML or PDF detail. NYSCA and NEA are very likely recurring funders (the institution is large enough and Manus came from NYSCA), and ESD provided $2.3M for the renovation, but year-by-year amounts TBD.
- **Mellon Foundation grants** to Pioneer Works: we did not surface confirmed Mellon grants via the search interfaces tested. Pioneer Works is sciences-and-arts-multi-disciplinary, which is not Mellon's primary funding lane (Mellon is humanities-leaning), so the absence is plausible but should be confirmed directly via mellon.org's grants database.
- **Schedule G** (Form 990, Part II, fundraising-event detail) for Village Fête and Fall Benefit: gross / contributions / direct expenses / net per year. Deferred to a later IRS XML pull.
- **Schedule J** (Form 990, officer-compensation detail) for Shiner FY2020/2021 and Manus FY2023/2024: deferred. The top-officer-comp line in `financials.csv` is the highest single Part VII row by year and is not always the executive director (in transition years it can be a board officer or other paid staff).
- **Pre-2017 top-exec-comp lines** are blank in the ProPublica summary (the org was small enough that no single Part VII row may have crossed the disclosure threshold; or the data simply isn't surfaced). For FY2012–FY2016 the top-exec-comp column in `financials.csv` is left empty.
- **Comprehensive board roster** across years: we have a 2019 snapshot (the Shiner appointment release named Yellin, Florenz, Levin, Hearst, Zohrenejad, Belt, Bator, Tisch, Santo Domingo) and a 2023 snapshot (the Manus appointment cited Hearst as chair plus Bator, Belt, Black, Cunney, Dalio, Davis, Elum, Harris, Ingrassia, Katyal, Lee, Meyer, Pucker, Sarnoff, Tirana Barry). Year-by-year roster delta needs the 990 Part VII pull.
- **Annual Spring Benefit ↔ Christie's connection**: the prompt mentioned a "Christie's auction" structure; my searches surfaced an in-house benefit auction at `auction.pioneerworks.org` and the Village Fête as the principal annual gala, but did **not** surface a confirmed Christie's partnership. This is flagged as TBD; either the connection is more occasional than annual or the Village Fête / online auction has supplanted any earlier Christie's relationship.

## What's missing entirely (not yet attempted)

- **Year-by-year resident lists** beyond press-released cohorts (2018, 2019, 2025, 2026). The full archive at `pioneerworks.org/archive/residents` was not fetched in this pass (Cloudflare 403s on `pioneerworks.org/about` and `pioneerworks.org/news/...` in this session).
- **Pioneer Works Press catalog** of book releases.
- **Broadcast contributor list** beyond Issue 01.
- **Property-ownership document trail** for 159 Pioneer St (would require NYC ACRIS / property-deed search).
- **NEA / NYSCA / NYC DCLA grant rows** for Pioneer Works specifically.
- **Christie's auction record** for any Pioneer Works benefit or related auction.
- **Comparable-entities list**: NEW INC, Eyebeam, Knockdown Center, MASS MoCA, Watermill Center — for scale-comparison context.

## Data quality notes

- The Pioneer Works 990 is filed under fiscal year. Based on the ProPublica fiscal-year-end column the FY appears to align with calendar year, but this should be confirmed against the IRS XML.
- Pioneer Works is sometimes listed as "Pioneer Works Art Foundation Inc." and sometimes as "Pioneer Works"; funder-database searches need both variants.
- The institution uses two distinct top-exec titles in different periods: "Executive Director" (Shiner, then Florenz March 2025–) and "CEO" (Manus, 2023–2025). The titles are not interchangeable; "CEO" was a new title invented for Manus and apparently retired with her departure.
- Yellin holds the title "President" continuously from 2012 onward; this is a board-officer / founder role distinct from the executive directorship.
- Florenz is described in some press as Yellin's "second cousin" and elsewhere as his "cousin"; the "second cousin" framing comes from the May 2025 ArtForum departure piece.
- The square-footage figure varies across sources (24,000 / 25,000 / 27,000 sq ft for the building; 20,000 sq ft adjoining garden in the post-2025 ESD release). Most consistent number is 25,000 sq ft.

## Source URLs

- ProPublica record: https://projects.propublica.org/nonprofits/organizations/461097738
- GuideStar profile: https://www.guidestar.org/profile/46-1097738
- Wikipedia: https://en.wikipedia.org/wiki/Pioneer_Works
- IRS direct e-file XML (canonical source for FY2017+): https://apps.irs.gov/pub/epostcard/990/xml/
- Pioneer Works (Cloudflare-protected at the asset level for some endpoints): https://pioneerworks.org/
- Empire State Development renovation press: https://esd.ny.gov/esd-media-center/press-releases/esd-announces-completion-12-7-million-renovation-enhancing-accessibility-pioneer-works-brooklyn
- ArtForum departure piece: https://www.artforum.com/news/mara-manus-steps-down-as-ceo-pioneer-works-1234730585/
- ArtForum appointment piece: https://www.artforum.com/news/mara-manus-to-lead-pioneer-works-252978/
- BkMag 10-year recap: https://www.bkmag.com/2022/09/22/pioneer-works-at-10/
- Surface gala recap: https://www.surfacemag.com/articles/pioneer-works-open-gala-recap/
- FvF founding profile: https://www.freundevonfreunden.com/art/inside-pioneer-works-new-yorks-collaborative-wonderland-for-the-arts-and-sciences/
