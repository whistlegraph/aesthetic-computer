# arxiv-sfpc / data — first-pass record pull

First-pass dossier data on the School for Poetic Computation (SFPC) assembled 2026-05-02. SFPC is a fundamentally different organizational shape from Rhizome: an LLC (not a 501(c)(3)), funded primarily by tuition (not foundations), and committed enough to "radical openness and generosity" that they publish their own books to GitHub. The data path is therefore inverted from the Rhizome dossier — instead of pulling IRS XML and reverse-engineering grant rows, we mirror SFPC's own published finance-and-administration reports and add what has been disclosed publicly since the repo went quiet in 2019.

## Files

- `financials.csv` — high-level P&L per session/year as published. Columns: total income, sales (tuition), sponsorship, event income, COGS, operating expenses, wages, rent, net result. 2013--2019 from the GitHub repo; 2020--2026 are public disclosures only (the repo has not been updated past 2019).
- `people.csv` — co-founders, LLC partners, administrators, steering committee members, current Co-Directors, current Board of Directors, "stewards" identified in Wikipedia's 2020 governance restructure description, and recurring teachers.
- `programs.csv` — every session identified with location, student count where disclosed, tuition, sponsor.
- `timeline.csv` — 2013 → 2026 events: session-by-session, governance shifts, COVID rupture, residency at National Academy of Design.
- `funders.csv` — institutional partners and disclosed individual supporters.
- `locations.csv` — physical address history (Hudson → Orbital/Rivington → Babycastles → Westbeth → National Academy of Design 2026).
- `finance/` — verbatim mirror of `github.com/SFPC/finance-and-administration` as of clone date: per-year P&L statements (CSV/PDF/XLSX), the `01/` first-semester report, and 2015/2018/2019 financial-narrative readmes.

## What's solid

- **Legal structure**: SFPC is **"SFPC, LLC."** — every P&L statement in the repo prints that legal name. It is not a 501(c)(3). Tax-deductible donations route through an unnamed fiscal sponsor (per `sfpc.io/faq`). ProPublica's nonprofit search returns no entity. The 2019 narrative confirms passthrough-LLC mechanics including K-1 distributions covering "phantom income" tax burden for partners.
- **Governance arc**: founders + admin (2013--2014) → founders + Steering Committee (Fall 2014 onwards) → 2020 collective re-org with five "SFPC Stewards" (Wikipedia: Aliyu, Anderson, American Artist, Bomani, Hoff, Macdonald) after Lieberman publicly stepped down → current (2026) public-facing language: Co-Directors (Todd Anderson, Neta Bomani) + Board of Directors (American Artist, Salome Asega, Tega Brain, Taylor Levy, Che-Wei Wang, Galen Macdonald, Celine Wong Katzman). Whether the LLC was ever converted to a 501(c)(3) is not publicly disclosed; the public-facing language adopts nonprofit phrasing without the EIN to back it.
- **Financial size**: 2013 partial $67K total income → 2014 $108K → 2018 Spring $96K → 2019 $344K (annual). 2017 surplus $48K, 2018 surplus $44K, 2019 deficit −$84K. The 2019 deficit drained reserves and combined with COVID-19 cancelling Spring 2020 forced the GoFundMe.
- **Founding self-description**: from the very first finance report ("01"), SFPC's commitment to "radical openness and generosity" was load-bearing. The "$700 tuition refund" letter to all 15 first-semester students — "we always intended to give back any money that we didn't spend to you" — established the school's self-conception as a non-extractive operation. This is documentary, not aspirational.
- **Scholarship practice**: from Spring 2015 forward, half of every cohort had partial or full scholarship (e.g. 9 full-tuition + 9 scholarship students of varying levels). Funding came partly from teacher pay-cuts.
- **Locations**: precisely traced. 2014--2017 at Orbital (155 Rivington), 2018+ at Westbeth (155 Bank St). Babycastles hosted final shows and Summer 2015. National Academy of Design hosts the 2026 Future Schools residency.

## What's known but not fully quantified

- **2016--2017 finances**: the GitHub repo skips these years — no P&L published. We have aggregate retrospective figures from the 2019 narrative ("2017 surplus $47,959; 2018 surplus $43,946") but no per-program breakdown.
- **2020--2026 finances**: the repo has not been updated since 2019. Online classes ramped, Ford Foundation and Art for Justice support is disclosed as a fact on `sfpc.study/about` without amounts or year ranges, GoFundMe receipts are visible on the GoFundMe page only.
- **Knight Foundation / YCAM grant amounts**: 2019 narrative says YCAM contracted $12,500. Knight Foundation amount not disclosed in the narrative.
- **Fiscal sponsor identity**: SFPC's FAQ says "We are also able to receive tax deductible donation through our fiscal sponsor" without naming the sponsor. NYFA is the most common NYC arts fiscal sponsor and is the strong default guess but is not confirmed.
- **Student counts post-2018**: Spring 2019 enrollment, Fall 2019 enrollment, online-class enrollment 2020+ — none disclosed in published material.
- **2020 GoFundMe total raised**: GoFundMe page should disclose; not yet pulled.

## What's missing entirely (not yet attempted)

- **Alumni list**: SFPC's alumni network is large and includes many notable practitioners (Mimi Onuoha, Allison Parrish recurring teacher, Zainab Aliyu, Neta Bomani, Sebastian Morales Prado, Ramsey Nasser, Lauren Gardner, etc.) but a structured year-by-year roster is not centralized.
- **Wikipedia is sparse**: no in-line citations in the entry's governance / leadership sections. Sourcing the 2020 stewards list to a primary document (an SFPC blog post or social-media announcement) is the next archival step.
- **Per-session narratives 2016--2017, 2020+**: would require interview-based reconstruction or scraping `sfpc.io/<sessiontag>/` archives via Wayback.

## Data quality notes

- The repo's P&L statements list "SFPC, LLC." as the entity — that is the *legal* name on the books. Public-facing language is "School for Poetic Computation" or "SFPC".
- Financial reporting is partly cash-basis, partly accrual (per the 2019 narrative). Year-over-year comparisons should be made cautiously; the narratives flag specific reconciliations (e.g. YCAM $12,500 contracted but $11,913.48 received in Jan 2020, included in 2019 to credit the program).
- The 2013 "01" report is unique: written by Casey Gollan as a monetary-explanatory letter to the *students*, not as an accounting record. Tone, granularity, and detail differ from later reports authored by Choi (2015, 2018) and Gardner (2019).
- The 2020 governance pivot is documented in Wikipedia but the primary-source SFPC announcement has not yet been located. Treat the "Stewards" list as Wikipedia-current rather than verified.

## Source URLs

- Active site: https://sfpc.study/ (current programming)
- Archive site: https://sfpc.io/ (2013--2022 sessions, FAQ, mission)
- Finance repo: https://github.com/SFPC/finance-and-administration
- Wikipedia: https://en.wikipedia.org/wiki/School_for_Poetic_Computation
- 2020 GoFundMe: https://www.gofundme.com/f/help-the-school-for-poetic-comptuation
- 2026 residency host: https://nationalacademy.org/

## Relationship to the author

Jeffrey Alan Scudder is named on `sfpc.study/about` as one of three explicitly-named individual supporters ("Galen MacDonald, Jeffrey Alan Scudder, Maya Man"). This is recorded as a fact in the dossier's footprint, not as positionality.
