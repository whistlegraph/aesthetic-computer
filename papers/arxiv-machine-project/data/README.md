# arxiv-machine-project / data — first-pass record pull

First-pass dossier data on Machine Project, the Echo Park, Los Angeles artist-run nonprofit founded in 2003 by Mark Allen and concluded as a public-facing operation in January 2018. Assembled 2026-05-02.

Machine Project sits between the SFPC and Rhizome dossier shapes: like SFPC it is small, artist-run, and minimally bureaucratic; unlike SFPC, it *was* a 501(c)(3) (EIN 75-3193159, IRS exemption May 2006), so eight years of public Form 990 data is recoverable from ProPublica (FY2011–FY2018). It does not, however, publish its books to GitHub; its self-documentation lives in two physical books — the 2010 *Field Guide to LACMA* (Machine Project Press, ISBN 978-0-9753140-4-3) and the 2016 Tang/Prestel *Machine Project: The Platinum Collection* (ISBN 978-3-7913-5619-8) — and in the still-live press / events / news archive at machineproject.com.

## Files

- `financials.csv` — Form 990 top-line per fiscal year, FY2011–FY2018, sourced from ProPublica's Nonprofit Explorer (EIN 75-3193159). Columns: revenue, expenses, net, net assets, ED compensation. FY2019+ not on file (the org wound down public programming after the Jan 13 2018 farewell event).
- `people.csv` — founder/director, ops manager, FY2017 board of directors as listed on Form 990, key co-editors of published books, the Hammer A.I.R. counterpart. Tenure dates are best-effort; the ProPublica board snapshot is FY2017.
- `programs.csv` — the storefront workshop categories, the Field Guide series, the Hammer residency, and major institutional commissions. Where exact dates and counts are disclosed they are recorded; storefront cadence is recorded as ``~2 events/week, free.''
- `funders.csv` — institutional funders disclosed across LACMA, Hammer, PAA, Tang, and LA County Arts. Mike Kelley Foundation does NOT appear on the public grantee lists — removed from this record. Andy Warhol Foundation is recorded only as an org Mark Allen sits on the board of, not a confirmed Machine grantor.
- `timeline.csv` — 2003 founding → 2005 incorporation → 2006 IRS exemption → 2008 LACMA Field Guide → 2009–10 Hammer A.I.R. → 2014 Gamble House → 2015–16 Tang Platinum Collection → Jan 13 2018 closure → 2021 Heavy Manners Library opens at the same address.
- `locations.csv` — primary storefront (1200 D N Alvarado, 2003–2018) and the cross-reference to Heavy Manners Library which took over the unit in 2021.
- `notable-events.csv` — the one-off events that defined the org's voice: LACMA Field Guide 2008, Hammer residency 2009–11, Gamble House 2014, Tang Platinum Collection 2015–16, the closure event itself.

## What's solid

- **Legal entity**: Machine Project, EIN 75-3193159, 501(c)(3) public charity, NTEE "Arts Education." Tax-exempt determination May 2006. Address on file: 530 Berkeley Ave, Claremont CA 91711-4226 (Charity Navigator). Storefront mailing/contact address: 1200 D North Alvarado, Los Angeles CA 90026 — phone 213-483-8761, email machine@machineproject.com.
- **Founding date**: December 6, 2003 (first show: Tom Jennings — Story Teller, runs through Jan 24 2004).
- **Founder**: Mark Allen (artist, educator, Pomona College Associate Professor; previously ran informal artist-run spaces in Houston).
- **Closure**: physical storefront closed with a single farewell event on Saturday, January 13, 2018, 2–9pm. Mark Allen's framing is generative ("dissolves so that the next thing can emerge"), not financial.
- **Financial size**: FY2011–FY2017 ran in the $290K–$400K revenue range, near break-even year over year. Peak revenue FY2014 at $397,667 (Gamble House intervention year). FY2018 was a half-year run with -$96,419 deficit; net assets drew down to $34,666.
- **Key institutional partnerships, dated**:
  - **LACMA Field Guide**: November 15, 2008 (one day, 10 hours, 60+ projects, 35 artists, co-funded by LACMA Public Programs + the Ralph M. Parsons endowment + Machine's own funders).
  - **Hammer residency**: research phase June–October 2009; official residency November 2009–December 2010; ~75–80 onsite programs; underwritten by a $1M James Irvine Foundation Arts Innovation Fund grant *to the Hammer* (not directly to Machine).
  - **Tang Platinum Collection**: exhibition Sept 19 2015–Jan 3 2016; book published 2016 (ISBN 978-3-7913-5619-8); book launch Extravaganza at Echo Park 2017.
  - **Pasadena Art Alliance**: 2015 grant (amount undisclosed) for Patrick Ballard's *Return to Foreverhouse*.
- **Building lineage**: 1200 D N Alvarado, built 1932 / re-outfitted 1972, was Machine Project 2003–2018. Heavy Manners Library, founded by Matthew James-Wilson in 2021, occupies the same unit and explicitly names Machine Project as its predecessor on its about page. Heavy Manners is its own dossier in this batch.

## What's known but not fully quantified

- **Pre-FY2011 finances**: not in ProPublica (FY2006–FY2010 990s would exist on file with the IRS but are not in the standardized public extract). Eight years of clean Form 990 data is what's pulled here.
- **Post-FY2018 status of the legal entity**: the EIN persists in the IRS database; no 990 has been filed for FY2019 or later that surfaces in ProPublica. Charity Navigator records Machine Project on California's delinquent-registrations list per AB 488 — donations through CN are disabled. The org has not been formally dissolved on the public record but has not programmed publicly since the closure.
- **Specific grant ledger**: the org's financial statements break revenue into total contributions and program service revenue but do not publish a grants-by-funder ledger. Named funders here are the ones that disclose their grants on their own public pages (PAA, Hammer/Irvine via the Hammer's own credit lines, LACMA via the Field Guide's funding statement, LA County Arts via its grantee page).
- **Total artist count across 15 years**: not centrally tabulated. The two books — *Field Guide to LACMA* and the Tang *Platinum Collection* — together name 100+ collaborators but neither is a comprehensive roster.
- **Workshop attendance / class enrolment**: not disclosed.

## What's missing entirely (not yet attempted)

- **The 2018 silkscreen poster archive** referenced in the closure announcement as ``acquired by an institutional archive later this year'' — no public confirmation of which institution.
- **A full FY2006–FY2010 financial picture** — would require pulling old paper-filed 990s directly from the IRS or the California Attorney General Registry of Charitable Trusts.
- **Mark Allen's post-2018 curatorial / itinerant Machine activity** — none disclosed publicly; the website has not posted news since 2018.

## Data quality notes

- Form 990 financials are the cleanest source; eight years of consistent line items.
- Board snapshot in `people.csv` is FY2017 (the last clean board listing on a 990 before the closure year). Earlier boards are not in the search index; later boards (FY2018) are on the closure-year filing but the 8-year dataset stabilizes the FY2017 set as the working roster.
- Two foundation funders that were *speculatively* searched but did NOT surface as confirmed Machine grantors: **Mike Kelley Foundation for the Arts** (no Machine entry in their published awardee list) and **Andy Warhol Foundation for the Visual Arts** (Mark Allen sits on its board, but Machine itself is not in the public grantee list pulled here). Both omitted from `funders.csv` to keep the file factual.

## Source URLs

- Active site (archival): https://machineproject.com/
- Wikipedia: https://en.wikipedia.org/wiki/Machine_Project
- ProPublica 990s: https://projects.propublica.org/nonprofits/organizations/753193159
- Charity Navigator: https://www.charitynavigator.org/ein/753193159
- Hammer residency page: https://hammer.ucla.edu/artist-residencies/2009/machine-project
- Hammer A.I.R. report (PDF): https://machineproject.com/build/engine/wp-content/uploads/2015/09/Machine_Project_Public_Engagement_Artist_in_Residence_Report_compressed.pdf
- LACMA Field Guide event page: https://machineproject.com/2008/events/a-machine-project-field-guide-to-lacma-2/
- LACMA Field Guide book listing: https://www.amazon.com/dp/0975314041 (ISBN 978-0-9753140-4-3)
- Tang exhibition page: https://tang.skidmore.edu/exhibitions/109-machine-project-the-platinum-collection-live-by-special-request
- Tang book listing: https://tang.skidmore.edu/publications/64-machine-project
- Hyperallergic closure: https://hyperallergic.com/420736/after-15-years-las-machine-project-closes-with-a-farewell-event/
- Artforum closure: https://www.artforum.com/news/after-fifteen-years-los-angeles-s-machine-project-shutters-73376
- LA Times closure (mirror): https://eastofborneo.org/archives/after-15-years-a-forest-a-pig-and-a-giant-tongue-echo-park-alternative-arts-space-machine-project-is-closing-its-doors-los-angeles-times/
- Heavy Manners Library: https://heavymannerslibrary.com/about

## Relationship to the author

The author has no disclosed prior intersection with Machine Project. Recorded as a fact, not as positionality.
