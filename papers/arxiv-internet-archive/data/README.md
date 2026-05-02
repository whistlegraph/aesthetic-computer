# arxiv-internet-archive / data --- first-pass record pull

First-pass dossier data assembled 2026-05-02. All numbers are sourced; gaps and TBDs are flagged inline rather than guessed.

## Files

- `financials.csv` --- Form 990 line items per fiscal year. The Internet Archive filed Form 990-PF (private foundation) through FY2015 and switched to Form 990 (public charity) starting FY2016, when its revenue mix had shifted enough to warrant public-charity status. Line items extracted FY2016-FY2024 from ProPublica's JSON API v2. FY2002-FY2015 PDFs exist on ProPublica but the structured line items were not extracted in this pass.
- `people.csv` --- Brewster Kahle (founder / digital librarian / board chair); board members (Rick Prelinger, David Rumsey, Kathleen Burch, Scott Fong); senior staff (Mark Graham, Jefferson Bailey, Joy Chesbrough, Chris Freeland); senior engineers; relevant historical figures (Aaron Swartz, George Blood).
- `funders.csv` --- top institutional funders. Mellon, Knight, Sloan, NEH, IMLS plus the Kahle/Austin Foundation (founder family foundation) and major DAFs (Permanent Archive Fund, Fidelity Charitable, American Online Giving Foundation).
- `grants.csv` --- named grants TO Internet Archive with year + amount + project. 8 Mellon grants documented (2006-2023, with several in the Long Tail journals / Community Webs lines), Knight $1M for TV News, Sloan $1M for library digitization, plus the headline 2024 grants.
- `timeline.csv` --- 1996 founding through 2026; emphasizes the litigation arc (Hachette filed 2020 / district 2023 / 2nd Circuit 2024 / IA declines SCOTUS) and the UMG arc (2023 filed / 2025 settled).
- `locations.csv` --- 300 Funston Avenue (the iconic Greek Revival former Christian Science church), pre-2009 Presidio office, ~30 historical scanning centers worldwide (Cebu, Toronto, BPL, Andover, Latrobe, etc.), and the Bibliotheca Alexandrina mirror.
- `programs.csv` --- Wayback Machine, Open Library, Archive-It, TV News Archive, Software Library, Great 78, IA Scholar, Community Webs, Long Tail Journals, CDL/Open Libraries, National Emergency Library (discontinued), DWeb, Political TV Ad Archive.
- `litigation.csv` --- Hachette v. Internet Archive (filed June 2020, district court ruling March 2023, 2nd Circuit ruling Sept 2024, SCOTUS review declined Dec 2024); UMG v. Internet Archive (filed Aug 2023, settled Sept 2025).

## What's solid

- **EIN**: 94-3242767. **IRS ruling date** 2017-04-01 reflects the reclassification from Form 990-PF to public-charity Form 990; the org's actual founding is May 1996.
- **Address**: 300 Funston Avenue, San Francisco, CA 94118-2116. Purchased September 2009 for $4.5M from the Fourth Church of Christ, Scientist. 1923 Greek Revival design by Carl Werner; the architecture inspired the IA logo (or vice versa, depending on whose telling).
- **Form 990 line items, FY2016-FY2024**: total revenue, contributions/grants, program service revenue, total expenses, total assets, total liabilities, net assets --- all from ProPublica's JSON API v2.
- **Revenue mix is split unusually for a 501(c)(3) preservation org**: roughly two-thirds contributions, one-third earned program service revenue (scanning services for partner libraries; Archive-It subscriptions). Most Rhizome-scale digital preservation orgs are >90% contributions.
- **Revenue trajectory**: $17.5M (FY2016) -> $20.3M (FY2018) -> **$36.7M peak (FY2019)** -> $21.8M (FY2020) -> $29.4M (FY2021) -> $30.5M (FY2022) -> $23.7M (FY2023) -> $26.8M (FY2024).
- **The FY2023 net-asset cliff**: net assets fall from +$4.21M (FY2022) to **-$3.53M (FY2023)**, a swing of -$7.7M in one year. FY2023 expenses ($32.7M) outran revenue ($23.7M) by $9M. This corresponds to the Hachette district-court ruling (March 2023) and is consistent with material legal/judgment costs landing in that fiscal year, though specific settlement terms with the publishers are confidential.
- **Mellon Foundation grants**: 8 confirmed grants between 2006 and 2023, dollar amounts confirmed for $50K (2012), $100K (2006), $200K (2020-03), $1.13M (2020-09), $750K (2023-11). 2007 / 2017-10 / 2018-03 grants are confirmed but amounts are not in the public Mellon database UI for this pass.
- **Knight Foundation**: $1M confirmed for TV News Search & Borrow expansion (year of award 2013, but service launched September 2012 with prior support).
- **Sloan Foundation**: $1M confirmed for digitizing five major library historical collections (year TBD).
- **Hachette litigation**: filing date June 1 2020; district court ruling March 24-25 2023; 2nd Circuit ruling September 4 2024; IA's December 4 2024 announcement of no SCOTUS petition. Permanent injunction limits IA's CDL to books without current commercial ebook editions.
- **UMG Great 78 litigation**: filing date August 11 2023; motion to dismiss denied May 16 2024; settled September 2025 (confidential resolution).
- **2024 cyberattacks**: DDoS by SN_BLACKMETA in May; data breach exposing ~31M user accounts in October 9 2024 (root cause: GitLab auth tokens left exposed for ~2 years); Zendesk breach October 20 2024.
- **Storage scale**: ~99 PB as of October 2025; Wayback Machine reached 1 trillion archived web pages in 2025.

## What's known but not fully quantified

- **Pre-FY2016 financials**: FY2002-FY2015 are 990-PF filings (private foundation accounting). PDFs are public on ProPublica but structured line items were not pulled in this pass. PFs report a different schema (assets/liabilities lines + grants paid out, not contributions in/program service revenue split). Worth a separate pull when needed.
- **Brewster Kahle compensation history**: FY2024 990 reports $0 reportable comp for Kahle and $0 for the named board members; the highest reported compensation in FY2024 is Joy Chesbrough at $246K. The $0 line for Kahle does not mean he is unpaid in fact -- it means he is not paid through Internet Archive's payroll. He is independently wealthy (Alexa Internet sold to Amazon for $250M stock in 1999) and runs the Kahle/Austin Foundation, which has funded IA in an unknown ratio over the years.
- **Government grants line**: not separately broken out in the ProPublica API v2 dump for this pass. The 990s likely tabulate this under contributions; pulling the full Schedule A or Schedule B would split out federal vs private grants. Federal funders (NEH, IMLS, FedScan / Library of Congress) are confirmed but year-by-year amounts are TBD.
- **Mellon grants pre-2017 amounts**: 2006 ($100K confirmed), 2007 (TBD), 2012 ($50K confirmed). Earlier Mellon support for the Open Content Alliance (2006-) is likely flowing through general operating support but specific line items TBD.
- **Open Content Alliance member contributions**: a network effect -- Microsoft was a major early funder ($5M+ committed) before withdrawing in 2008. Specifics TBD.
- **Microsoft Live Search Books contributions**: pre-withdrawal contributions to OCA; TBD.
- **Kahle/Austin Foundation transfer to IA**: the family foundation's 990-PF lists IA as a recurring grantee. Specific year-by-year amounts not pulled in this pass.

## What's missing entirely (not yet attempted)

- **Hachette settlement terms**: the public record shows district court ruling and 2nd Circuit affirmation, plus an undisclosed monetary settlement to publishers. The exact dollar amount is not in the public record but is likely material to the FY2023 net-asset cliff.
- **UMG settlement terms**: confidential.
- **IRS XML for Internet Archive**: the IRS publishes structured XML for FY2017+ at apps.irs.gov/pub/epostcard/990/xml/. Pre-2022 batches require multi-GB downloads; the pull was deferred for this first pass and would surface Schedule G/J/R detail (board roster with comp, related-org disclosures, fundraising-event detail).
- **Annual Report PDFs from archive.org**: IA publishes annual operating reports in some years; not pulled in this pass.
- **Board of directors full roster across years**: this pass identifies the headline board members (Kahle, Prelinger, Rumsey, Burch, Fong) but a Schedule J / Part VII full roster across 5+ years would build a proper governance timeline.
- **In-kind contributions from partner libraries**: scanning operations are partner-funded but the financial flows are mixed (subscription vs in-kind staffing vs facility donations). Not separated in this pass.

## Data quality notes

- The 990 fiscal year ends **December 31** (calendar year), unlike Rhizome's June 30 fiscal year. So "FY2023" means January-December 2023 and the Hachette district court ruling (March 24 2023) and any associated legal costs would land squarely in the FY2023 990.
- The org is listed on filings as "INTERNET ARCHIVE" with no parent or sub-name; very clean.
- Brewster Kahle's name appears in three roles on the 990: Founder, Digital Librarian, and Board Chair. The titling has been stable since 1996.
- Internet Archive's Form 990-PF history (FY2002-FY2015) reflects its initial classification as a private operating foundation. The 2017 transition to 990 / public-charity status reflects revenue diversification (Archive-It subscriptions, public donations) crossing the public-support test threshold.
- Several DAFs (Fidelity Charitable, American Online Giving Foundation, Permanent Archive Fund) flow through individual donor money. The 2024 Permanent Archive Fund grant of $6.6M for Open Library is exceptional and worth marking.
- "Internet Archive" sometimes appears in funder databases as "The Internet Archive" or "Internet Archive Inc." -- searches need both variants.

## Source URLs

- ProPublica record: https://projects.propublica.org/nonprofits/organizations/943242767
- ProPublica API v2: https://projects.propublica.org/nonprofits/api/v2/organizations/943242767.json
- CauseIQ: https://www.causeiq.com/organizations/internet-archive,943242767/
- GuideStar profile: https://www.guidestar.org/profile/94-3242767
- Wikipedia (org): https://en.wikipedia.org/wiki/Internet_Archive
- Wikipedia (building): https://en.wikipedia.org/wiki/Internet_Archive_building
- Wikipedia (Brewster Kahle): https://en.wikipedia.org/wiki/Brewster_Kahle
- Wikipedia (Hachette case): https://en.wikipedia.org/wiki/Hachette_v._Internet_Archive
- Mellon Foundation grants: https://www.mellon.org/grant-database/Internet%20Archive
- IRS direct XML: https://apps.irs.gov/pub/epostcard/990/xml/
- IA blog: https://blog.archive.org/
- IA scanning labor history: https://scanninglabor.github.io/IAScanningLabor/scanningcenters.html
- Internet Archive unofficial wiki: https://internetarchive.archiveteam.org/
