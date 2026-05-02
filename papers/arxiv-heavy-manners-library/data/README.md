# arxiv-heavy-manners-library / data --- first-pass record pull

First-pass dossier data on Heavy Manners Library (HML) assembled 2026-05-02. HML is a small artist-run library / gallery / bookstore at 1200 N Alvarado St Unit D in Echo Park, Los Angeles --- the same building that previously housed Machine Project (2003--2018) and Echo Park Film Center (2002--Jan 2022). HML is the smallest of the three orgs the AC papers project has dossiered so far (Rhizome >> SFPC > HML in publicly-disclosed institutional footprint).

## What "small" means here

Public footprint is genuinely thin. HML does not publish any financial document. Legal status is not stated anywhere on its website, in interviews, or in third-party reporting. ProPublica's Nonprofit Explorer returns no entity for "Heavy Manners". The bulk of the public record is:

- the org's own about page (heavymannerslibrary.com/about) listing seven staff names and a short prose mission;
- the events listings on heavymannerslibrary.com/events and Eventbrite;
- a 2023 profile in The Eastsider LA;
- a 2024 inclusion in Hyperallergic's "Seven Indie Art Spaces to Check Out in Los Angeles";
- a 2024 podcast with Flipboard's Mia Quagliarello;
- a 2025 dublab "Channels" episode recorded at HML;
- the Instagram feed @heavymannerslibrary.

This dossier surfaces what those sources disclose and stops there. Where the record is silent (legal entity, fiscal sponsor, revenue, named foundation grants), the dossier says so.

## Files

- `financials.csv` --- there are no published financials. The file documents what is *not* disclosed and the few public revenue signals (membership tiers, Spring Fundraiser exhibition, shop sales, workshop fees).
- `people.csv` --- founders, staff (per about page), recurring collaborators (EPFC Collective screening hosts, dublab "Channels" hosts).
- `programs.csv` --- recurring program series (Risograph Printing 101 workshop, Heavy Manners Zine Fair, Heavy Manners Comics Fair, dublab Channels live broadcasts, EPFC Collective screenings, gallery exhibits).
- `timeline.csv` --- 2003 (Machine Project opens at 1200 N Alvarado) through 2026, surfacing the building's tenancy chain and HML's launch and program rollout.
- `funders.csv` --- nothing institutional has been disclosed publicly. The file documents the income channels visible on the website (memberships, donations, shop, workshop tuition, Spring Fundraiser).
- `locations.csv` --- 1200 N Alvarado St Unit D, with explicit cross-reference to Machine Project (2003--2018) and Echo Park Film Center (2002--Jan 2022) prior occupancies.
- `collection-notes.csv` --- what is documented about the holdings (artist books, zines, magazines, comics, audiobooks, essays per the catalog filters; Molly Soda's *Chick Magnet* publication; Tiny Splendor 10th anniversary acquisition window).

## What's solid

- **Founding**: HML opened in 2021. Founder is Matthew James-Wilson; co-founder / co-curator is Molly Soda. James-Wilson previously self-published the 300-page art quarterly *FORGE* (2012--2018). The library's seed materials came from unsolicited book donations to a never-completed James-Wilson + Soda book project on "the impact of the internet on contemporary art for the last 30 years".
- **Building lineage**: 1200 N Alvarado St housed Machine Project (2003--Jan 2018, founded by Mark Allen) and Echo Park Film Center (2002--Jan 2022, lease expired). The building is multi-tenant; HML is in Unit D. The Hyperallergic profile (2024) confirms that the Echo Park Film Center Collective (EPFC's successor org after the storefront closed) now hosts screenings in HML's basement. So the *programmatic* lineage between the building's two prior arts tenants and HML is documented, not inferred.
- **Name**: "Heavy Manners" is from Prince Far I's 1976 album *Under Heavy Manners* (produced by Joe Gibbs and Errol Thompson at Joe Gibbs Recording Studio in Kingston). The album title refers to the Jamaican State of Emergency declared by PM Michael Manley in 1976. James-Wilson has said in podcast interviews that the name derives from "reggae and punk culture traditions". (Note: dossier original brief listed the album as 1977; correct year per Wikipedia is 1976.)
- **Current staff (seven people, per the about page)**:
  - Matthew James-Wilson --- Founder
  - Molly Soda --- Co-Curator and Co-Founder
  - Yulia Cymbura --- Head Librarian
  - Jane Shin --- Programming Director
  - Saria Dang --- Librarian
  - Brock Stuessi --- Archiving Consultant
  - Maya Man --- Web Designer
  - Lucas Vocos --- Web Developer
- **Membership model**: two tiers (Cyan, Magenta); 6-week checkout, 6-book limit; damage fee = 50% of replacement cost; loss/destruction = 100% of replacement cost. Borrowing geographically restricted to the LA area, with mail-order under consideration. Specific Cyan vs. Magenta pricing not exposed without entering the join flow.
- **Programming pattern**: monthly-to-quarterly gallery exhibits (Kendra Yee, Jesse Moynihan, Tiny Splendor 10th Anniversary, etc.), recurring Risograph Printing 101 workshop, recurring Heavy Manners Zine Fair (annually in spring; 2026 edition is Apr 11--12), recurring Heavy Manners Comics Fair, dublab "Channels" live broadcasts, EPFC Collective screenings (incl. a Craig Baldwin benefit), Hollywood Entertainment "Overnight" series, comedy nights via The Comedy Bureau.
- **First Spring Fundraiser**: held with opening Jun 5 (year not specified in the event title, but likely 2024 or 2025 based on event-page metadata) --- "two-week-long fundraiser for the space that featured art, prints, classes, and gatherings".

## What's known but not quantified

- **Legal entity**: not disclosed. No 501(c)(3) language on the donate or join pages. No EIN visible. ProPublica returns no entity. The org may be a sole proprietorship, an LLC, a fiscally sponsored project, or an unincorporated association --- none of those is publicly confirmed. Membership fees and the shop suggest revenue generation; book donations are not advertised as tax-deductible.
- **Revenue size**: no data. The Spring Fundraiser is the only acknowledged community-fundraising event.
- **Collection size**: not disclosed. The catalog (heavymannerslibrary.com/catalog) supports filters --- visible filters include Magazine and what appears to be a multi-format taxonomy (audiobooks, magazines, essays) per third-party descriptions --- but no published item count.
- **Foundation funding**: nothing named publicly. No NEA, NEH, Mellon, Warhol, or Knight grants have been disclosed in any source surfaced.
- **Hours**: third-party sources list both (a) Sun--Sat 11am--7pm with Tue/Wed closed, Mon/Thu 1pm--7pm (per official about-page extraction); and (b) Fri--Sat 7--11pm, Sun 1--11pm (per Hyperallergic, Sept 2024). The Hyperallergic figures may reflect event-night hours. The official about-page hours are taken as authoritative for general operations.

## What's missing entirely

- **Annual program count**: would require scraping the full Eventbrite history.
- **Fiscal-sponsor or LLC paperwork**: not surfaced.
- **Named donor / acquisitions list**: not published. The Molly Soda *Chick Magnet* book and Tiny Splendor's 10-year anthology are visible *as items in the shop / on view*, not as accessioned acquisitions in a catalog with provenance.
- **Foot traffic / membership counts**: not published.
- **Relationship to Maya Man** (web designer at HML, also named on SFPC's about page as one of three explicitly-named individual SFPC supporters): noted as a fact across the AC papers project; not inflated into a thesis.

## Source URLs

- Site: https://heavymannerslibrary.com/ ; About: https://heavymannerslibrary.com/about ; Events: https://heavymannerslibrary.com/events ; Shop: https://shop.heavymannerslibrary.com/ ; Catalog: https://heavymannerslibrary.com/catalog ; Journal: https://heavymannerslibrary.com/journal ; Donate: https://heavymannerslibrary.com/donate ; Join: https://heavymannerslibrary.com/join
- Linktree: https://linktr.ee/heavymannerslibrary
- Instagram: https://www.instagram.com/heavymannerslibrary/
- Eventbrite organizer page: https://www.eventbrite.com/o/heavy-manners-library-35158850143
- Hyperallergic, "Seven Indie Art Spaces to Check Out in Los Angeles": https://hyperallergic.com/772908/seven-indie-art-spaces-to-check-out-in-los-angeles/
- The Eastsider LA, "Heavy Manners Library amplifies Echo Park's creative community": https://www.theeastsiderla.com/neighborhoods/echo_park/heavy-manners-library-amplifies-echo-parks-creative-community/article_1a5e869e-0563-11ee-91ef-bf99ed99af46.html
- SJSU iSchool SLA Student Chapter profile: https://ischoolgroups.sjsu.edu/slasc/heavy-manners-library/
- LA Municipal Art Gallery program partner page: https://lamag.org/heavy-manners-library/
- Flipboard / Storyboard podcast with founders: https://about.flipboard.com/inside-flipboard/curating-a-library-from-scratch-meet-the-founders-of-heavy-manners-library-podcast/
- dublab Channels archive (Heavy Manners episode 2025-05-10): https://www.dublab.com/archive/nia-michelle-jordan-collins-lewis-thompson-channels-heavy-manners-library-05-10-25
- Building lineage: Hyperallergic on Machine Project closure (Jan 2018): https://hyperallergic.com/420736/after-15-years-las-machine-project-closes-with-a-farewell-event/ ; The Eastsider on EPFC closure (Jan 2022): https://www.theeastsiderla.com/eastsider_on_the_go/arts_and_culture/echo-park-film-center-closes-micro-cinema-and-starts-new-era/article_ae2e980e-69b1-11ec-b462-c769fa40b294.html ; Machine Project Wikipedia entry: https://en.wikipedia.org/wiki/Machine_Project
- Name origin: Wikipedia, *Under Heavy Manners* (Prince Far I, 1976): https://en.wikipedia.org/wiki/Under_Heavy_Manners

## Relationship to the author

Maya Man is listed on heavymannerslibrary.com/about as Web Designer for HML, and is named on sfpc.study/about as one of three explicitly-named SFPC individual supporters (alongside Galen MacDonald and Jeffrey Alan Scudder). The dossier records this as a fact across the AC papers project, not as positionality.
