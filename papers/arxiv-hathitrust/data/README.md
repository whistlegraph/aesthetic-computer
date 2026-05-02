# arxiv-hathitrust / data --- first-pass record pull

First-pass dossier data assembled 2026-05-02. All numbers are sourced; gaps and TBDs are flagged inline rather than guessed.

## Files

- `financials.csv` --- HathiTrust does not file a Form 990 in its own name; it is a service of the University of Michigan, not a standalone 501(c)(3). The financial record therefore reads through cost-per-volume figures, the 2019 projected budget, and the year-by-year tier fees rather than ProPublica/IRS structured data.
- `members.csv` --- partial member list. 14 founding institutions (CIC 12 + UC system + UVa) with subsequent additions. As of 2024, 219+ research libraries; as of 2025, 290+. Full alphabetical list at hathitrust.org/member-libraries/member-list/.
- `people.csv` --- John Wilkin (founding Executive Director 2008-2013), Mike Furlough (2014-), Paul Courant (UMich co-creator), HTRC co-PIs Beth Plale (Indiana) and J. Stephen Downie (Illinois), recent Board electees.
- `funders.csv` --- Mellon Foundation, Sloan Foundation, IMLS, member library dues, in-kind contribution from Google (digitization).
- `grants.csv` --- Mellon $1M (2023-02) is the headline named grant; Sloan funding for HTRC Data Capsule + IU's $600K non-consumptive research investigation (2011); IMLS National Leadership Grants underwriting CRMS (2008+) and CRMS-World (2011-2014).
- `timeline.csv` --- 2008 founding through 2026; emphasizes the litigation arc (Authors Guild filed 2011 / district 2012 / 2nd Circuit 2014 / settlement 2015), the COVID-era ETAS launch (March 2020), and the HTRC sunset (end of 2026).
- `locations.csv` --- University of Michigan Library (host); Indiana / Illinois (HTRC co-hosts, sunsetting); BTAA + CDL coordinating roles.
- `programs.csv` --- Digital Library, PageTurner, Catalog/API, HTRC, CRMS, ETAS (discontinued), Shared Print, US Federal Documents, Disability Access, Orphan Works (discontinued).
- `litigation.csv` --- Authors Guild Inc. v. HathiTrust (filed Sept 2011, district court Oct 2012, 2nd Circuit June 2014, settlement Jan 2015).

## What's solid

- **Founding date**: October 13, 2008 (launch announcement). Founding members: 12 universities of the CIC (now Big Ten Academic Alliance) + 11 University of California libraries + University of Virginia.
- **Headquarters**: 1001 N. Buhr Building, 200 Hill Street, Ann Arbor MI. Embedded inside the University of Michigan Library.
- **Legal structure**: NOT a standalone 501(c)(3). HathiTrust is a service of the University of Michigan, a public-university body. Funded by member dues + external grants. Member-elected Board of Governors (12 voting members + 2 ex officio non-voting).
- **Authors Guild v. HathiTrust** ruling: filing September 2011; District Court (Judge Harold Baer Jr.) ruling October 10, 2012; Second Circuit (Judges Parker / Walker / Cabranes) affirmation June 10, 2014; settlement January 6, 2015. Two of the three uses (full-text search + accessibility) found to be fair use; preservation-copy claim remanded on standing and ultimately settled.
- **Corpus scale**: 18M+ volumes (Sept 2024); 19M+ (2025); 6.7M in US public domain.
- **Members**: 219+ (2024); 290+ (2025).
- **Cost-per-volume**: $0.2294 (2023); $0.2403 (2026).
- **Tier 2 baseline public-domain fee** (majority of members): $7,414 (2023).
- **2019 projected budget**: $3,777,445 (Wikipedia citation).
- **Mellon $1M grant (Feb 2023)**: cross-referenced with arxiv-mellon dossier.
- **HTRC sunset**: HathiTrust voted in 2024 to end HTRC funding at end of 2026.
- **ETAS launch date**: March 2020; in use at 171 campuses within 2 months; ~300K accesses by mid-2020.

## What's known but not fully quantified

- **Year-by-year HathiTrust budget**: only the 2019 projected number ($3.78M) and 2023 / 2026 cost-per-volume rates are publicly cited. Total annual budget by year is not pulled in this pass; would require the Members area newsletters or annual reports from hathitrust.org/newsletters.
- **Member dues income vs grants income split**: structurally member dues dominate, but exact ratio not surfaced in this pass.
- **Sloan grant amounts for HTRC Data Capsule**: confirmed Sloan funded the Data Capsule, but specific year and amount TBD beyond IU's $600K investigation grant (2011).
- **IMLS grant amounts for CRMS**: National Leadership Grants confirmed for 2008 (US) and 2011-2014 (CRMS-World), but dollar figures not retrieved in this pass.
- **Full board roster across years**: only most recent elections (Romaniuk, McAllister) and founding-period leadership (Wilkin, Courant, Furlough) captured. Full Board / PSC roster across 2012-2026 is partial.
- **Full member roster**: 290+ as of 2025; ~30 captured in members.csv. Full alphabetical list lives at hathitrust.org/member-libraries/member-list/.

## What's missing entirely (not yet attempted)

- **The HathiTrust Bylaws (2012, with amendments)**: the governing document was not pulled in this first pass.
- **Year-by-year annual reports**: the HathiTrust Newsletters page (hathitrust.org/newsletters) likely contains member-meeting minutes and budget-vote outcomes.
- **University of Michigan Library annual reports**: would surface HathiTrust expenditures and revenue inside the host budget; not pulled.
- **Full Mellon grant history to HathiTrust**: 2023 $1M is the headline; possible earlier or later grants to HathiTrust specifically (vs to UMich for HathiTrust-related work) not exhaustively pulled.
- **Big Ten Academic Alliance financial alignment**: BTAA is co-located and programmatically aligned with HathiTrust; the funding flows between BTAA and HathiTrust are not pulled in this pass.
- **Settlement amounts in Authors Guild v. HathiTrust**: the 2015 settlement was on the preservation-copy and orphan-works claims. Monetary terms (if any) of the settlement are not in the public summaries.
- **HTRC final-year transition costs**: the 2024 Board decision to end HTRC funding at end of 2026 implies a transition budget; specifics not surfaced.

## Data quality notes

- HathiTrust does NOT have an EIN of its own. The legal employer of HathiTrust staff is the University of Michigan (EIN 38-6006309). For tax/financial-disclosure purposes, HathiTrust's footprint lives inside UMich filings rather than as a separate Form 990.
- The "Committee on Institutional Cooperation" was renamed "Big Ten Academic Alliance" in 2016. Pre-2016 sources will say CIC; post-2016 sources will say BTAA. Both refer to the same 12-university consortium.
- Several member institutions appear under multiple names in source documents (e.g., "University of California" can mean the UC system, a specific UC campus, or the California Digital Library).
- The Authors Guild ruling and the Internet Archive's Hachette ruling sit on the same Second Circuit fair-use corridor and are decided by the same court a decade apart. The cases are doctrinally tightly linked: HathiTrust won on transformative fair-use (search + accessibility); the Internet Archive lost on whole-book lending.

## Source URLs

- HathiTrust home: https://www.hathitrust.org/
- HathiTrust About: https://www.hathitrust.org/about/
- HathiTrust Governance: https://www.hathitrust.org/about/governance/
- HathiTrust Cost Model: https://www.hathitrust.org/Cost
- HathiTrust Member List: https://www.hathitrust.org/member-libraries/member-list/
- HathiTrust Newsletters: https://www.hathitrust.org/newsletters/
- HathiTrust 2026 Budget Press: https://www.hathitrust.org/press-post/2026-budget-new-board-members/
- HTRC: https://www.hathitrust.org/about/research-center/
- HTRC Transition FAQ: https://www.hathitrust.org/about/research-center/htrc-transition-faq/
- ETAS: https://www.hathitrust.org/member-libraries/services-programs/etas/
- 2023 Mellon Grant: https://www.hathitrust.org/hathitrust-receives-1-million-mellon-grant-to-enhance-core-operations
- Wikipedia: https://en.wikipedia.org/wiki/HathiTrust
- Wikipedia (Authors Guild case): https://en.wikipedia.org/wiki/Authors_Guild,_Inc._v._HathiTrust
- 2nd Circuit ruling: https://law.justia.com/cases/federal/appellate-courts/ca2/12-4547/12-4547-2014-06-10.html
- Big Ten Academic Alliance: https://btaa.org/library/programs-and-services/hathitrust-digital-library/governance
- 2008 Launch (CDL): https://cdlib.org/cdlinfo/2008/10/15/major-library-partners-launch-hathitrust-shared-digital-repository/
- 2008 Launch (UMich): https://news.umich.edu/major-library-partners-launch-hathitrust-shared-digital-repository/
