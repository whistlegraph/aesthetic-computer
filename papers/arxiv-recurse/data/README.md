# arxiv-recurse / data — first-pass record pull

First-pass dossier data on the Recurse Center (formerly Hacker School) assembled 2026-05-02. RC is the **for-profit counterfactual** to SFPC: same year (2011), same NYC, same small-cohort no-grades posture, but a privately-held company funded by recruiting fees rather than tuition, foundations, or fiscal sponsorship. There is no IRS Form 990 because RC is not a 501(c)(3); the data path is the founders' own essays, the recurring *User's Manual*, the public hiring page, the Recurse blog, the Wikipedia entry, and a small set of journalist interviews.

## Files

- `financials.csv` — best-effort financial record. Columns mix disclosed model parameters (per-head cost, fee rate, year of self-sufficiency) with one third-party revenue estimate (Latka, unverified). Almost all entries are **disclosed model facts** rather than annual P&L.
- `people.csv` — the eight people on `recurse.com/team` plus notable alumni who have taken on visible RC-adjacent roles (returning facilitators, residents).
- `funders.csv` — investors, not funders, because RC is for-profit. Y Combinator (Hackruiter seed, Summer 2010) is the only confirmed external capital event in the public record. Etsy and Dropbox/Tumblr/Jane Street/Tapad funded the early Hacker Grants program, which is recorded here as program-sponsorship rather than equity.
- `timeline.csv` — 2010 (Hackruiter) → 2011 (Hacker School launch) → 2015 (rename) → 2020 (COVID closure + RC Together) → 2023 (reopening + hybrid) → 2025 (AI position post).
- `locations.csv` — Manhattan early years → Brooklyn 397 Bridge Street → fully online (2020--2023) → hybrid (2023--).
- `programs.csv` — every batch type with format and duration.
- `hiring-partners.csv` — the named hiring partners across the public record (Stripe, Etsy, Dropbox, Jane Street, OpenAI, etc.). The full roster is over 100 companies and is not surfaced as a single public list.

## What's solid

- **Founding lineage**: Hackruiter (Summer 2010, YC-seeded) → Hacker School (July 2011) → Recurse Center (March 25, 2015). Founders Bergson-Shilcock, Sridhar, Albert. Triangulated from the rename announcement, Wikipedia, and the YC company profile.
- **Recruiting model parameters**: 25% of first-year salary, 20% for 501(c)(3)s, 90-day refund guarantee, internships free with conversion-fee on full-time hire. All disclosed verbatim on `recurse.com/hire`.
- **Self-sufficiency from recruiting**: 2014, per Wikipedia. The 2015 transition from corporate-funded to self-funded grants (with leadership taking 60% pay cuts for four months) is disclosed in the seven-years diversity post.
- **Total grants disbursed**: $1.5M+ by 2019, per the seven-years post. Unbroken-down by year.
- **Per-participant cost**: ~$12,000, per Wikipedia citing the manual.
- **Social rules + self-directives**: the four social rules and three self-directives are textual/canonical and well-attributed.
- **2020 COVID transition**: posted 2020-03-12, physical space closed 2020-03-14 at 5pm.
- **2023 reopening**: posted 2023-05-08 by Rachel Petacat; David Albert transitioned to Co-founder Emeritus in late March 2023.
- **Locations**: Brooklyn 397 Bridge Street (current physical hub).

## What's known but not fully quantified

- **Hiring-partner full roster**: 100+ companies per the manual, but the page rotates testimonials and never shows the full list. We have ~15 named partners across the public corpus.
- **Annual revenue**: Latka.com lists $11.1M and 72 employees; this is third-party, the methodology is undisclosed, and the date of measurement is unclear. Treat as a single anchor data point, not a series.
- **Y Combinator class/year**: Summer 2010 confirmed via the YC company page; the original investment terms (check size, equity stake) are not public.
- **Subsequent funding rounds**: Crunchbase returned 403 on this attempt; no subsequent rounds are surfaced in Wikipedia or YC profile, suggesting Hackruiter's seed is the only equity event but this is not certain.
- **Batch enrollment counts**: not disclosed per-batch.

## What's missing entirely

- **Legal entity form**: whether the operating company is an Inc. (C-corp), LLC, or something else is not surfaced in current public material. The 2015 rename was branding, not a re-incorporation per the announcement; the underlying entity has presumably been the same since 2011 Hacker School.
- **Board of directors composition**: only Albert is named publicly as a board member. Whether the board is a fiduciary corporate board or advisory body is not stated.
- **Profitability**: never disclosed in public.
- **Any controversy record**: Wikipedia surfaces no notable alumnus or community controversies in its current revision; this is mildly surprising for a 15-year-old programming community and may indicate either editorial conservatism or a clean record.
- **Crunchbase data**: blocked by 403 on the attempt of 2026-05-02.

## Data quality notes

- The recruiting-fee figures are *current rate-card* (2026), not historical. Whether the 25% rate has held since 2011 or has changed over time is not documented.
- Etsy's Hacker Grants commitments ($50K initial / $200K by 2014) are mixed cash + commitment figures; reading the 2014 post carefully suggests the $200K is a *committed* number including future grants, not a recognized-revenue figure.
- The CB Insights page lists the entity under the legacy "hacker-school" slug; that is consistent with Hacker School being the original incorporated name.
- The Latka revenue figure ($11.1M) should be treated as a single, low-confidence anchor.

## Source URLs

- Active site: https://www.recurse.com/
- User's Manual: https://www.recurse.com/manual
- Hire page: https://www.recurse.com/hire
- Team page: https://www.recurse.com/team
- 2015 rename announcement: https://www.recurse.com/blog/77-hacker-school-is-now-the-recurse-center
- 2020 COVID post: https://www.recurse.com/blog/152-RC-is-online-only-until-at-least-May
- 2023 reopening post: https://www.recurse.com/blog/187-a-new-kind-of-retreat
- Wikipedia: https://en.wikipedia.org/wiki/Recurse_Center
- YC profile: https://www.ycombinator.com/companies/recurse-center
- Crunchbase (403 on 2026-05-02): https://www.crunchbase.com/organization/hacker-school

## Relationship to the author

Jeffrey Alan Scudder has no documented relationship to the Recurse Center. The dossier is recorded as outside-observer.
