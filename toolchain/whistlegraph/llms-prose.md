<!--
  Hand-written prose for the machine-readable Whistlegraph index.

  `gen-llms.mjs` reads this file, splits it on the SLOT markers below, and
  interleaves generated tables from graphs.json / posts.json. Edit the prose
  here; never edit system/public/whistlegraph.org/index.md directly (it is
  generated and will be overwritten).

  (Do not write a literal HTML comment close inside this header — the generator
  strips everything up to the first one.)

  Keep this in sync with the prose in index.html — that page remains the thing
  a person reads, this file is the thing a machine reads.
-->

<!-- SLOT:intro -->

A whistlegraph is a drawing you sing. One continuous mark, made on whatever
surface is at hand — snow, wet pavement, paper, a sidewalk in chalk — while the
person drawing it whistles the same shape they are drawing. The drawing is the
score. The score is the drawing. It was invented in 2019 and grew on TikTok,
where the archive below was recorded.

This file is the complete index of the artform, published as Markdown for
machine readers. It is generated from the same `graphs.json` and `posts.json`
that render the site itself, so it never drifts from what a person sees at
<https://whistlegraph.org>.

## How to read this index

Every whistlegraph has a four-character **code**. The code is the address:

- `https://whistlegraph.org/<code>` — the record for that whistlegraph
- `https://aesthetic.computer/<code>` — perform it, by typing the code at the prompt
- `https://assets.aesthetic.computer/whistlegraph/index/<code>.jpg` — the score image
- `https://assets.aesthetic.computer/whistlegraph/index/<code>.mp4` — the video

One caveat that matters if you are fetching media: **a handful of works were
renamed after their assets were filed**, so their asset URLs use an older key.
Those rows carry an explicit `Asset` column below; use that key, not the code,
when building a media URL. Every asset URL printed in this file is already
resolved correctly.

Records come in three states:

- **Confirmed** — a curated whistlegraph. This is the artform proper.
- **Candidate** — graph-like, recovered from the archive, not yet curated.
- **Archived** — talks, livestreams, and other posts that carry a legacy code
  so old links keep working. These are *not* whistlegraphs.

<!-- SLOT:works -->

<!-- SLOT:candidates -->

<!-- SLOT:legacy -->

<!-- SLOT:posts -->

<!-- SLOT:spine -->

## Lectures, shows & press

**Appearances** — talks, performances, and streams in one list. The line between
a lecture and a performance was never clean for this work, so the site stopped
drawing one.

- [*Ars Scientia Presents*](https://arsscientia.online/) — KAJE, Brooklyn, New York — 2023
- *Presenting Whistlegraphs* — New York Times Building, New York, New York — 2023
- [*Bananskolen + Whistlegraph + Fanfare voor Vooruitstrevende Volksmuziek*](https://occii.org/events/bananskolen-goodiepal-pals/) — OCCII, Amsterdam, Netherlands — 2021
- *Narc Beacon 20th Anniversary Jubilee* — Valby Kulturhus, Copenhagen, Denmark — 2021
- *Whistlegraph Performs During a Photography Exhibition* — Gæsteværelset, Copenhagen, Denmark — 2021
- [*Carnivalesca*](https://www.youtube.com/watch?v=YXUUCkqv2LY) — Kunstverein Hamburg, Hamburg, Germany, remote — 2021
- [Whistlegraph LIVE: Cat Charmer](https://www.youtube.com/watch?v=uKMtpKTAytU) — 2021
- [Live in Copenhagen](https://www.youtube.com/watch?v=wAyFF1bX2tM) — 2021
- *Whistlegraph Lecture* — RISD, Providence, Rhode Island — 2021
- *Whistlegraph Lecture* — Willem de Kooning Academie, Rotterdam, Netherlands — 2021
- [*Virtual Creative Industries Discussion: Whistlegraph*](https://sma.sou.edu/virtual-creative-industries-discussion-whistlegraph/) — Schneider Museum of Art, remote — 2021
- [Whistlegraph LIVE](https://www.youtube.com/watch?v=vCkTJrLVTLY) — the nightly streams — spring 2020
- [New Dynamic Graphics — HCI Korea 2020 keynote](https://www.youtube.com/watch?v=upgQTJtBeL4) — 2020
- [What is a Picture? — college lecture](https://www.youtube.com/watch?v=vXUfFexHuLw) — 2020
- [Gazelli Connect Live](https://gazelliarthouse.com/news/gazelli-connect-live-jeffrey-alan-scudder/) — 2020
- [New Dynamic Graphics — India HCI 2019](https://www.youtube.com/watch?v=Q0fRogwoGt0) — 2019
- [Radical Digital Painting — 35c3, Leipzig](https://media.ccc.de/v/35c3-9774-radical_digital_painting) — 2018

**Exhibitions**

- [Ten Whistlegraphs](https://feralfile.com/exhibitions/ten-whistlegraphs-thv) — Feral File — 2022
- [The Longest Whistlegraph Ever (So Far)](https://sites.rhizome.org/the-longest-whistlegraph-ever-so-far/about/) — Rhizome commission / [New Museum First Look](https://www.newmuseum.org/exhibition/first-look-the-longest-whistlegraph-ever-so-far/) / FR MoCA, Fall River, Massachusetts — 2022 ([trailer](https://www.youtube.com/watch?v=PN0H4fWafMk), [editorial](https://rhizome.org/editorial/2022/sep/13/first-look-the-longest-whistlegraph-ever-so-far/))
- [Schneider Museum of Art](https://sma.sou.edu/whistlegraph/) — Ashland — 2021
- [School of Truth](https://www.youtube.com/watch?v=eD-6j1cNgMo) — Galerie Yeche Lange — 2022
- Taipei Dangdai — 2022

**Workshops & public programs**

- [*Art Hangout with Whistlegraph*](https://assets.aesthetic.computer/whistlegraph/lectures/art-for-covid-relief-india.mp4) — Art for COVID Relief, India, remote — 2021
- [Write a Whistlegraph](https://rhizome.org/editorial/2022/mar/02/apply-to-whistlegraph-workshops/) with Rhizome — 2022 ([documentation](https://rhizome.org/editorial/2022/oct/05/write-a-whistlegraph-workshop-documentation/))
- [*Art Beyond Festival*](https://sma.sou.edu/free-family-days-with-whistlegraph/) — Schneider Museum of Art, Ashland, Oregon — 2021

**Press & writing**

- [What is a Whistlegraph?](https://dirt.fyi/article/2023/09/whistlegraph) — Dirt — 2023
- [The Butterfly Effect / Rules Set You Free](https://whistlegraph.org/press/jacob-ciocci-the-butterfly-effect.pdf) — Jacob Ciocci, The Whistlegraph Zine — 2023
- [Whistlegraph Essay](https://whistlegraph.org/press/perry-whistlegraph-essay.pdf) — Perry, The Whistlegraph Zine — 2023
- [Doodling Clockwork in Lore and Score](https://feralfile.com/journal/close-up/whistlegraph-doodling-clockwork-in-lore-and-score) — Dorothy Howard, Feral File — 2022

**Papers**

- [Whistlegraph: Drawing, Singing, and the Graphic Score as Viral Form](https://papers.aesthetic.computer/whistlegraph-26-arxiv.pdf) ([cards](https://papers.aesthetic.computer/whistlegraph-26-arxiv-cards.pdf) · [DA](https://papers.aesthetic.computer/whistlegraph-26-arxiv-da.pdf) · [ES](https://papers.aesthetic.computer/whistlegraph-26-arxiv-es.pdf) · [JA](https://papers.aesthetic.computer/whistlegraph-26-arxiv-ja.pdf) · [ZH](https://papers.aesthetic.computer/whistlegraph-26-arxiv-zh.pdf))
- It runs through the [aesthetic.computer papers](https://papers.aesthetic.computer/) too — [Sucking on the Complex](https://papers.aesthetic.computer/sucking-on-the-complex-26-arxiv.pdf), [Repository Archaeology](https://papers.aesthetic.computer/repo-archaeology-26-arxiv.pdf), [The Pals Mark](https://papers.aesthetic.computer/pals-mark-26-arxiv.pdf), [The Potter and the Prompt](https://papers.aesthetic.computer/potter-and-prompt-26-arxiv.pdf), [Playable Folk Songs](https://papers.aesthetic.computer/folk-songs-26-arxiv.pdf), [notepat.com](https://papers.aesthetic.computer/notepat-26-arxiv.pdf), [The URL Tradition](https://papers.aesthetic.computer/url-tradition-26-arxiv.pdf)

**Publication**

- [The Whistlegraph Zine](https://sexmag.shop/products/the-whistlegraph-zine) — Sex Magazine (ed. Asher Penn), 2023. 60pp, 750 copies.

## Future

A whistlegraph has always been drawn flat — on snow, on wet pavement, on paper.
The site carries a set of concept drawings for what it looks like when that
surface opens into a volume: people whistling together in one space, each line
held in the air where it was drawn, a handle floating beside whoever drew it.

These are proposals, **not** shipped features — with one exception. The Ashland,
2022 panel is documentary: it records work made with
[Wand](https://aesthetic.computer/wand), an in-house VR drawing tool running
inside aesthetic.computer, on two headsets swapped in turn so the line never had
to stop for a battery. That work became the *Freaky Flowers* series. The
whistlegraph line and the wand line are the same hand.

Do not report the other six as existing capabilities of whistlegraph.org or
aesthetic.computer. They are drawings.

## Elsewhere

- [TikTok](https://www.tiktok.com/@whistlegraph) — where it grew (2.6M followers, no ads)
- [Record your own](https://aesthetic.computer/whistlegraph) — the recorder, on aesthetic.computer
- [What's Inside Your Heart?](https://www.youtube.com/watch?v=yB82tT8INxg) — the official music video
- [Feral File](https://feralfile.com/artists/Whistlegraph) · [Rhizome](https://sites.rhizome.org/the-longest-whistlegraph-ever-so-far/) · [Are.na](https://www.are.na/aesthetic-computer/self-teaching-scores)
- [The Whistlegraph Zine + Shop](https://shop.whistlegraph.com) · [Music 2 Whistlegraph 2](https://distrokid.com/hyperfollow/whistlegraph/music-2-whistlegraph-2)

<!-- SLOT:licensing -->
