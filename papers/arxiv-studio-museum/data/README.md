# arxiv-studio-museum / data — first-pass record pull

First-pass dossier data assembled 2026-05-02. The Studio Museum in Harlem (EIN 13-2590805) is a Manhattan-based 501(c)(3) public charity, founded 1968, with FY ending June 30. All numbers are sourced; gaps and TBDs are flagged inline rather than guessed.

## Files

- `financials.csv` — Form 990 line items per fiscal year, FY2011–FY2024. Source: ProPublica Nonprofit Explorer for EIN 13-2590805.
- `people.csv` — Founders (1968), full director lineage from Inniss to Golden, current 2026 board of trustees.
- `funders.csv` — Institutional and named individual funders.
- `grants.csv` — Named grants TO the Studio Museum, with dates and amounts where confirmed (Mellon $10M 2022, Ford $10M 2024, NYC $62M, MacKenzie Scott 2021 undisclosed).
- `programs.csv` — All programs identified, with launch year, status, principal funders.
- `residents.csv` — Artist-in-Residence alumni from the Wikipedia AIR table (well-documented from 1979 onward) plus the 2026 cohort and notable named alumni from museum press.
- `timeline.csv` — 1968 → 2026 chronology of leadership, programs, exhibitions, building, funding.
- `locations.csv` — Original 2033 Fifth Ave loft (1968–1981) → 144 W 125th (1982–2018) → diaspora period (2018–2025) → new Adjaye-designed building (2025–).

## What's solid

- **Form 990 line items** FY2011–FY2024 (14 fiscal years) from ProPublica. Net assets grew from $9.6M (FY2011) to $244.8M (FY2024). Three revenue peaks correspond to capital-campaign milestones: FY2018 ($61.7M), FY2021 ($40.5M), FY2022 ($59.9M).
- **Director lineage**: Inniss (1968–69) → Spriggs (1969–75) → Callender (1975–77) → Campbell (1977–2000, museum-recorded) → Sims (2000–05) → Golden (2005–present). Conwill served as deputy / acting director through the late 1980s and 1990s.
- **Founders** confirmed by museum's own history page: Eleanor Holmes Norton, Carter Burden, Charles E. Inniss, Campbell Wylly, Betty Blayton-Taylor, Frank Donnelly. Mahler B. Ryder served as founding secretary.
- **Mellon Foundation grants**: $10M in December 2022 for Capital Campaign (Presidential Initiatives line) per Mellon's grants database, plus a multi-decade pattern of Curatorial Fund grants (Phases I, II, III), Critical Collections, Collections Management, VanDerZee Collection, Sustainability Plan, and Art Museum Futures Fund grants. The Met received a parallel $2M Mellon grant in 2022 for the joint Met-SMH James Van Der Zee Archive initiative.
- **Ford Foundation $10M**: October 2024 announcement, permanently endowing the position of Director and Chief Curator (now styled "Ford Foundation Director and Chief Curator"). Announced at Studio Museum Gala on Thelma Golden's 20th anniversary.
- **MacKenzie Scott 2021**: Confirmed as a recipient of Scott's June 2021 $2.74B disbursement; specific amount not disclosed by the museum.
- **Capital campaign aggregate $300M+** raised for the new building project (per museum's 2025 reopening release). City of New York contributed $62M.
- **Adjaye separation**: July 2023, the Studio Museum severed ties with David Adjaye personally following the Financial Times report; Adjaye Associates' NYC office continued under Pascale Sablan.
- **New building**: 82,000 sq ft, 7 floors, ~17,000 sq ft exhibition space, opened 15 November 2025. Inaugural exhibition: Tom Lloyd retrospective. The new AIR studios are named the J. Bruce Llewellyn Artist in Residence Center.
- **F-shows**: All five exhibitions in the F-show series confirmed: Freestyle (2001), Frequency (2005), Flow (2008), Fore (2012), Fictions (2017).
- **Joyce Alexander Wein Artist Prize** (2006–) recipients confirmed for most years 2006–2025.
- **2026 AIR cohort**: Derriann Pharr, Simonette Quamina, Taylor Simmons — first to occupy the J. Bruce Llewellyn AIR Center.
- **Studio magazine**: Founded 2005; biannual; AAM 2015 first prize.

## What's known but not fully quantified

- **Mellon grant amounts beyond the 2022 $10M**: Multiple Curatorial Fund, Critical Collections, Sustainability, VDZ, and Collections Management grants exist in the Mellon database. Per-grant amounts and exact dates need direct examination of each Mellon grant detail page; the Curatorial Fund Phase II $1M (December 2017) and Phase I (~2011) are confirmed at $1M each.
- **AIR alumni 1969–1978**: The first decade of residents is poorly indexed in public sources (Wikipedia's AIR table starts at Louis Delsarte 1979–80). Tom Lloyd is confirmed as the inaugural 1969 resident. The 1970s decade requires direct examination of museum archives. The dossier brief at intake referenced AIR cohort naming conventions (F, X, H, Inheritance) — these terms map to F-shows (which are *not* AIR culminating exhibitions but separate thematic surveys); X-show, H-show, and "Inheritance" exhibition terminology was *not* confirmed in this draft pass.
- **Government grants** (NEA, NYSCA, IMLS, NYC DCLA): confirmed as recurring; year-by-year amounts TBD.
- **Pre-FY2011 financials**: ProPublica has earlier filings (back to FY2002 in principle); not yet pulled.
- **Schedule J officer compensation breakdown** for Thelma Golden: visible in 990s but not yet extracted.
- **2018 Peggy Cooper Cafritz bequest**: 400+ contemporary works added to permanent collection; complete accession list not in this pass.

## What's missing entirely (not yet attempted)

- **Capital campaign full donor list with amounts**: The "Creating Space" campaign closed at $300M+, but only a handful of donors are publicly named (NYC $62M, Mellon $10M, Ford $10M, plus various corporate sponsors). The donor wall and the museum's annual reports would surface the full breakdown — including the **claimed Fred Eychaner $25M lead gift** referenced in the dossier brief, which could not be confirmed in any public source on this draft pass.
- **Renaming to "Studio Museum 125"**: Referenced in the dossier brief at intake; not present in any 2025–2026 press materials reviewed. The current name remains "The Studio Museum in Harlem."
- **Pre-2017 990 XMLs** (Schedule G, Schedule J, Schedule R details).
- **Full Studio magazine archive** (issue-by-issue table of contents).
- **Year-by-year exhibition list** beyond the F-shows and AIR culminating exhibitions.

## Data quality notes

- Fiscal year ends June 30 — "FY2018" means July 2017–June 2018.
- The museum's own historical timeline lists Mary Schmidt Campbell's directorship as 1977–2000. Wikipedia and other secondary sources frequently date her departure to 1987 (the year she became NYC Cultural Affairs Commissioner under Mayor Koch). The discrepancy is likely a difference between her full title-of-record (chief executive officer) and her active operational tenure. Kinshasha Holman Conwill served as deputy director / acting director from 1988 onwards. This dossier follows the museum's own dating but flags the variance.
- The dossier brief at intake referenced founder "Edward Spriggs" as the first director. Spriggs was the *second* director (1969–1975), not the first. Charles E. Inniss was the founding director.
- The dossier brief at intake stated the Mellon gift as "$5M (2018)". Mellon's own grants database records the Capital Campaign grant as $10M dated 8 December 2022. This dossier follows Mellon's database.
- The dossier brief at intake referenced a $25M lead gift from Fred Eychaner. This could not be confirmed in any publicly available source reviewed — Eychaner does not appear in the 2025 reopening press materials, the 2021 Art Newspaper capital-campaign coverage, or the Wikipedia article on Eychaner himself. The figure is recorded in `grants.csv` and `funders.csv` as TBD / unverified.

## Source URLs

- ProPublica record: https://projects.propublica.org/nonprofits/organizations/132590805
- Wikipedia: https://en.wikipedia.org/wiki/Studio_Museum_in_Harlem
- Studio Museum history: https://www.studiomuseum.org/history
- Studio Museum board: https://www.studiomuseum.org/board-of-trustees
- Studio Museum AIR program: https://www.studiomuseum.org/residency
- Wein Prize: https://www.studiomuseum.org/wein-prize
- The Building: https://www.studiomuseum.org/the-building
- 2025 opening press release: https://www.studiomuseum.org/press/studio-museum-in-harlem-opens-to-the-public-on-saturday-november-15
- 2024 Ford endowment press release: https://www.studiomuseum.org/press/ford-foundation-endows-position-of-director-and-chief-curator
- 2021 MacKenzie Scott press release: https://www.studiomuseum.org/press/the-studio-museum-in-harlem-awarded-endowment-gift-from-mackenzie-scott
- Mellon 2022 capital grant: https://www.mellon.org/grant-details/capital-campaign-20449604
- The Art Newspaper 2021 coverage: https://www.theartnewspaper.com/2021/10/28/studio-museum-in-harlem-marks-milestone-in-construction-of-new-home
- ARTnews on Adjaye separation: https://www.artnews.com/art-news/news/studio-museum-in-harlem-cuts-ties-david-adjaye-shelburne-museum-1234673549/
