# arxiv-the-kitchen / data — first-pass record pull

First-pass dossier data assembled 2026-05-02. The Kitchen NYC is one of the oldest organizations in this dossier series — formally incorporated as **Haleakala, Inc.** (DBA *The Kitchen*) in 1973, two years after the artist-collective phase began in 1971 in the disused kitchen of the Mercer Arts Center at 240 Mercer Street.

EIN: **13-2829756**. IRS tax-exempt status effective May 1975.

All numbers are sourced; gaps and TBDs are flagged inline rather than guessed.

## Files

- `financials.csv` — Form 990 line items per fiscal year, FY2011–FY2024 from ProPublica's summary view; pre-FY2011 filings are PDF-only on ProPublica back to FY2002 (URLs preserved). FY ends June 30.
- `people.csv` — founders (Vasulkas, Mannik), the original collective period co-directors (Chatham, Bapat, Devyatkin, Bowes), the long sequence of executive directors (Stearns 1973+ → MacArthur 1978-84 → Amazeen 1991-97 → Bernhardt 1998-2004 → Singer 2004-2011 → Griffin 2011-2021 → Russell 2021-), the music-director chain (Chatham, Russell, List, Lewis, DeMarinis, Lindsay), key curators (Lumi Tan, Matthew Lyons, Robyn Farrell, Alison Burstein), and named board members (Paula Cooper, Philip Glass, Caroline Stone as founding board; Robert Soros as current chair).
- `funders.csv` — Mellon, Andy Warhol Foundation, NEA, NYSCA, NYC DCLA, plus Hurricane Sandy recovery funders (Time Warner, ADAA), capital-campaign donors (collective).
- `grants.csv` — named grants TO The Kitchen with date / amount / project / term where confirmable. Mellon $2M (June 2023, 36 mo) is fully verified. Warhol $80K (fall 2023) is on the Warhol Foundation grants archive page.
- `timeline.csv` — 1971 → 2026: founding through current Without Walls programming.
- `locations.csv` — Mercer Arts Center kitchen (1971-73) → 484 Broome Street SoHo (1973-85) → 512 W 19th Street Chelsea (1985-2022, owned since 1987) → Westbeth Bank Street (2022-) → reopening TBD.
- `programs.csv` — long-running named series with launch dates and current status.

## What's solid

- **EIN and legal identity**: 13-2829756, Haleakala Inc DBA The Kitchen (Wikipedia, GuideStar, ProPublica all align).
- **Founding date**: June 15, 1971, in the disused kitchen of the Mercer Arts Center at 240 Mercer Street (Wikipedia + Field Notes; both sources align).
- **Incorporation**: 1973 as Haleakala Inc, the same year the Mercer Arts Center building collapsed (August 1973). Founding board: Paula Cooper, Caroline Stone, Philip Glass.
- **Building purchase**: The Kitchen purchased its 512 W 19th Street building from Dia Art Foundation in 1987.
- **Form 990 line items, FY2011–FY2024** from ProPublica's summary table: total revenue, contributions/grants, program-service revenue, total expenses, top exec compensation, total assets/liabilities, net assets. The dataset shows three revenue spikes — FY2014 ($2.6M; likely Getty archive sale), FY2016 ($3.5M; one-time gift?), FY2021 ($5.1M; capital campaign launch), FY2023 ($6.7M; Mellon $2M + capital campaign continuation).
- **Net asset trajectory**: $3.5M (FY2011) → $5.4M (FY2016) → $6.5M (FY2019) → $11.7M (FY2022) → $16.0M (FY2023) → $16.6M (FY2024). The capital campaign is highly visible on the balance sheet — net assets nearly tripled between FY2019 and FY2024.
- **Mellon Foundation**: a single grant on the public Mellon database — **$2,000,000 / 36 months / Arts and Culture** awarded June 8, 2023, to support "infrastructure and staffing under Legacy Russell's leadership." The Kitchen's own press release describes it as the largest single gift in The Kitchen's 50-year history.
- **Andy Warhol Foundation**: at least one grant of **$80,000 (fall 2023, Multi-year Program Support)**; Warhol also supported Hurricane Sandy recovery in 2012-2013.
- **Renovation architect**: Rice+Lipka Architects (Lyn Rice, principal) per Architects Newspaper 2021.
- **Capital campaign**: The Next 50 — public goal $40M; ArchPaper reported $19M pledged at the September 2021 launch with a $28M / 5-year initial framing.
- **Leadership chain (verified)**: Stearns (1973+) → MacArthur (1978-84) → Amazeen (1991-97) → Bernhardt (1998-2004) → Singer (2004-2011) → Griffin (2011-2021) → Russell (2021-).
- **Music-director chain (verified)**: Chatham (1971-73, 1977-80) → Russell (1974-75) → List (1975-77) → Lewis (1980-82) → DeMarinis (1982-85) → Lindsay (1986-87).
- **Getty acquisition**: 5,410 videotapes + 600+ audiotapes + 246 artist-designed posters (1971-1999) acquired by Getty Research Institute in 2014. Acquisition price not public.
- **Hurricane Sandy damage**: ~$450K (Oct 2012); Time Warner, Art Dealers Association, and Andy Warhol Foundation among recovery funders.

## What's known but not fully quantified

- **Gap years 1985-1991 ED**: Wikipedia / Kitchen sources give Stearns starting 1973 and Mary MacArthur Griffin 1978-84, then Amazeen starting 1991. The 1985-1991 ED is not yet identified in the public record consulted.
- **Pre-2011 financials**: ProPublica has Form 990 PDFs back to FY2002 but only summary-line data for FY2011+. Pre-FY2011 line items not yet extracted from the PDFs.
- **NEA grant history**: recurring funder; year-by-year amounts blocked behind JS-rendered Kendo grid at grantsearch.nea.gov; not extracted in this pass.
- **NYSCA / NYC DCLA**: recurring funders; year-by-year amounts not extracted.
- **Capital campaign donor names**: $19M pledged at launch (per ArchPaper) but the breakdown is not in any public document I located.
- **Schedule J / Schedule G detail**: not yet extracted from any year's IRS XML (we only have the ProPublica summary lines). Auction / gala net not separately tracked.
- **Renovation budget**: stated as $28M-five-year at the 2021 announcement; campaign goal is $40M. Actual spend-to-date and confirmed reopening date not in public sources as of May 2026.
- **Board roster**: Robert Soros confirmed as current chair; Paula Cooper and Philip Glass as Chairperson Emeritus. Full FY2024 board roster (per IRS XML) not yet pulled.

## What's missing entirely (not yet attempted)

- **IRS direct e-file XML pull** (FY2017+ should be available on apps.irs.gov) — not retrieved in this pass; would surface Schedule J/G/R detail.
- **Pre-FY2011 line items** from the PDF-only filings — would extend the 14-year financial trajectory back to 2002.
- **Vasulkas archive** at vasulka.org — has primary documents from the 1970s including Kitchen-Letter-Mailout (KLM) issues; not consulted in this pass beyond confirming legal name.
- **NEH / IMLS grant history** — not searched in this pass.
- **Press coverage of the Tim Griffin → Legacy Russell transition (2020-2021)** beyond the announcement. Some coverage of the transition was framed as part of a broader 2020 reckoning at downtown NYC institutions; not consolidated here.

## Data quality notes

- The legal name is **Haleakala, Inc.** Funder databases sometimes list the org as "The Kitchen", "The Kitchen Center for Video Music Dance Performance Film and Literature", "Haleakala Inc DBA The Kitchen", or just "Kitchen". Searches need all variants.
- The 990 fiscal year ends June 30. So "FY2024" means July 2023–June 2024. Many press / grant announcements use calendar year; cross-walks may need adjustment by ±1 year.
- Several program launch dates in this dossier are best-effort from secondary sources (Wikipedia and The Kitchen's own About page); primary archival corroboration would tighten them.

## Source URLs

- ProPublica record: https://projects.propublica.org/nonprofits/organizations/132829756
- GuideStar profile: https://www.guidestar.org/profile/13-2829756
- The Kitchen About: https://thekitchen.org/about/
- The Kitchen OnScreen archive: https://onscreen.thekitchen.org
- Field Notes: The Kitchen, 2021–2022: https://thekitchen.org/on-mind/field-notes-the-kitchen-2021-2022/
- Wikipedia: https://en.wikipedia.org/wiki/The_Kitchen_(art_institution)
- Mellon Foundation grant detail: https://www.mellon.org/grant-details/the-kitchen-20453597
- Andy Warhol Foundation archive page: https://warholfoundation.org/grants/archive/the-kitchen/
- Architects Newspaper renovation announcement: https://www.archpaper.com/2021/09/legendary-new-york-arts-center-the-kitchen-announces-an-expansive-renovation/
- The Next 50 capital campaign: https://www.thenext50.thekitchen.org
- The Kitchen Mellon press release: https://thekitchen.org/press/the-kitchen-receives-@m-from-the-mellon-foundation/
- Vasulkas archive (Kitchen Letter-Mailouts): https://www.vasulka.org/archive/Kitchen/
- Getty Research Institute archive collection: https://www.getty.edu/research/collections/group/10RB8S
- IRS direct e-file XML: https://apps.irs.gov/pub/epostcard/990/xml/
