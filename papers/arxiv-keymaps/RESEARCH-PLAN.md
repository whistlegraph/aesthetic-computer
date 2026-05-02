# Research Plan — Keymaps as Social Software

**Status:** planning, not yet drafted. Promote to `papers/SCORE.md` once an outline-to-prose pass produces a working `.tex`.
**Initiated:** 2026-04-30
**Driver question:** Why do *keymaps* — the table that says "QWERTY-`A` plays MIDI 60" — persist, propagate, and demand cross-tool compatibility, despite having no canonical home repo, no maintainer org, no funding mechanism, and often no named "owner"? They are software in every functional sense, but they are not a binary. What kind of object are they?

---

## 1. Thesis

**Keymaps are *social software*** — versioned virtual objects that persist via convention rather than via binaries. Users demand cross-tool compatibility with them. They have inventors (sometimes named, sometimes diffuse), revision histories (sometimes documented, sometimes oral), and forks (Colemak → Workman → Colemak-DH). But they do not live in any one project, are not "owned" by any one company, and rarely have a financial mechanism behind them. They are an entire category of computing artifact that the standard "open-source vs proprietary" frame cannot see, because the unit of distribution is *the table*, not *the program that consumes the table*.

This paper:

1. Names the category (**social software**, deliberately reclaiming Shirky's 2003 term as a pun: software *of* social agreement, not software *for* social interaction).
2. Establishes the lineage from physical typewriter layouts → digital keyboard layouts → musical instrument mappings → DAW QWERTY-piano overlays → notepat.com.
3. Catalogs corollaries — chess notation, Singmaster cube notation, fighting-game numpad notation, Nashville Number System, IPA, Camelot Wheel, Vim/Emacs keys, controller buttons — to show the category is not narrow.
4. Frames the political stake through Lialina's **Turing Complete User**: keymaps are precisely the "gaps that can be filled by users" — and the disappearance of that programmable substrate is the disappearance of the user as author.
5. Positions notepat.com as a **deliberate intervention** in this tradition. Notepat does *not* inherit the de-facto DAW chromatic-staircase mapping (Ableton/Logic/GarageBand/FL — verified 2026-04-30 — all converge on `Z X C V B N M = C D E F G A B` chromatic, with sharps on the row above). Notepat invents a new keymap whose design idea is **"notes that name themselves"**: the QWERTY letter `C` plays C, `D` plays D, ... `B` plays B; the upper octave continues alphabetically `H I J K L M N` for C5–B5; sharps sit above their natural by piano-black-key analogy where possible (`V S W R Q` = C♯ D♯ F♯ G♯ A♯). The DAW convention is the inferior thing the paper argues against — it ignores letter-name affordance, splits no octaves between hands, and is illegible without a tutorial. Notepat is empirical evidence that the social-software gap is **open for re-authorship**.

---

## 2. Why Lialina

Lialina's **"Turing Complete User"** (Contemporary Home Computing, October 2012; reissued 2021 as the lead essay in the open-access *Interface Critique* book *Turing Complete User: Resisting Alienation in HCI*, Heidelberg UP) is the load-bearing citation. The four passages we will lean on:

> "General Purpose Users can write an article in their e-mail client, layout their business card in Excel and shave in front of a web cam."

> "There is nothing one user can do, that another can't given enough time and respect. Computer Users are Turing Complete."

> "We need to take care of this word [user] because addressing people and not users hides the existence of two classes of people — developers and users."

> "I mean a situation when the work flow of an application has gaps that can be filled by users, where smoothness and seamlessness are broken."

The argument we add: a *keymap* is exactly such a gap. The hardware is fixed; the binary is closed; but the table mapping `QWERTY-A → MIDI-60` is **user-authorable, user-versionable, and user-demanded across applications**. When that gap closes — when an application bakes in one mapping and forbids overrides — the user-as-author dies in that surface. The Lialina argument predicts the keymap; the keymap is the empirical evidence that the Lialina argument is materially true.

Pair with: Lialina, *"Rich User Experience, UX and Desktopization of War"* (*Interface Critique Journal* 1, 2018) and Lialina/Espenschied, *Digital Folklore* (2009) — the "vernacular" framework — already cited in `arxiv-url-tradition/references.bib` as `lialina2005vernacular`.

---

## 3. The corollaries (the catalog)

This is the heart of the paper's contribution. The keymap is one instance of a much larger genus: **versioned virtual objects that propagate via social contract, not via binaries**. We will catalog these and argue they form a coherent category. Initial inventory (from research dossier 2026-04-30; see §7 for verification status):

### 3.1 Strong analogues (input-mapping cousins)

| Object | Originator | Propagation mechanism | Cross-binary demand? |
|---|---|---|---|
| **QWERTY** | Sholes, 1873 | Manufacturer adoption → ISO/AFNOR → OS keyboard tables | Total |
| **Dvorak** | Dvorak & Dealey, 1936 (US Pat. 2,040,248) | Patent → US Navy adoption → user-installed OS layouts | Total |
| **Colemak** | Shai Coleman, 2006 | colemak.com + GitHub forks (Colemak-DH, Workman, Halmak, Norman) | Total |
| **AZERTY / QWERTZ / BÉPO** | National conventions; BÉPO 2005 → AFNOR 2019 | Community → state standard | Total within region |
| **Wicki–Hayden** | Wicki 1896 / Hayden 1986 (independent re-invention) | Concertina + bandoneon community → modern MPE controllers | Within isomorphic-keyboard subculture |
| **Janko** | Paul von Jankó, 1882 | Decker / Blüthner / Bösendorfer pianos; conservatory; Janko Society — *failed to propagate* | Negative case study |
| **Bosanquet** | R. H. M. Bosanquet, 1875 | South Kensington Museum demos → microtonal community → Lumatone | Within xenharmonic scene |
| **DAW chromatic-staircase** (Ableton/Logic/GarageBand/FL Studio) | *Anonymous*; converged from tracker software by mid-2000s | Tutorial documentation; user expectation; cross-DAW pedagogy | **Total — the dominant unowned convention; load-bearing case** |
| **Notepat "notes that name themselves"** | @jeffrey, 2024 | notepat.com + AC Native + MenuBand + Max for Live + AC-Notepat | The *paper's intervention* — a competing, better-designed alternative |
| **Plover steno theory** | Mirabai Knight / Open Steno Project, 2010s | GitHub + community-forked dictionaries | Within steno community |
| **Vim keybindings** | Bill Joy 1976 (`vi`) → Bram Moolenaar 1991 (Vim) | Cross-binary "everywhere" lists; macOS Cocoa text fields ship Emacs keys natively | Total |

### 3.2 Notational corollaries (output-encoding cousins)

| Object | Originator | Notes |
|---|---|---|
| **Singmaster cube notation** | David Singmaster, 1981 | "Alternative notations failed to catch on, and today the Singmaster scheme is used universally" |
| **Algebraic chess notation** | Philipp Stamma 1737 → FIDE 1981 (federation crystallizing one of several conventions) | |
| **Nashville Number System** | Neal Matthews Jr., ~1957; extended by Charlie McCoy | Nashville session-musician oral tradition |
| **Roman numeral analysis / figured bass** | 18th-c. pedagogy, no single inventor | |
| **ABC notation** | Chris Walshaw, late 1980s; abc2mtex 1993 | Hundreds of thousands of folk tunes shared as plain text |
| **Helmholtz pitch / scientific pitch** | Helmholtz 1863 / Sauveur 1713 | C4=middle-C convention propagated via acoustics + MIDI documentation |
| **MIDI note numbers** | MIDI 1.0, MMA 1983 | Note 60 = middle C as community convention atop the spec |
| **Guitar / lute tablature** | 16th c. (lute) → 1990s Usenet (`rec.music.makers.guitar.tablature`, OLGA) for ASCII guitar tab | |
| **Harmonica blow/draw numbering** | Hohner pedagogy, late 19th–mid 20th c. | Needs primary citation |
| **IPA** | International Phonetic Association, 1888; revised Kiel 1989, 2005, 2015 | Federation-standardized; closer to ISO than vernacular |
| **NATO spelling alphabet** | ICAO, 1956 | Federation-standardized |
| **Camelot Wheel** | Mark Davis, 1991 ("Easymix System") → Mixed In Key | Now the de-facto DJ key-detection notation |
| **Fighting-game numpad notation** (236P, 623K) | Japanese FGC, no named originator | Pure vernacular; every wiki and matchmaking forum speaks it |

### 3.3 Tactile / categorical corollaries

| Object | Originator | Notes |
|---|---|---|
| **Game-controller button labels** (A/B/X/Y vs △○✕□) | Nintendo SNES 1990 vs Teiyu Goto / Sony 1994 | Cross-platform games ship both — perfect demonstration of "label as social object surviving across hardware regimes" |
| **CSS named colors** | X11 `rgb.txt` (~1987) → SVG 1.0 (W3C, 2001) | Inherited unchanged across browser implementations |
| **Pantone / NCS / RAL** | Pantone 1963 / NCS 1979 / RAL 1927 | Mixed ownership models — Pantone proprietary, RAL federation |
| **Knitting pattern abbreviations** (k2, p1, yo, k2tog) | Craft Yarn Council standards (US); UK parallel | Propagates by being printed in every pattern |
| **Sign-language fingerspelling** | ASL/BSL pedagogy, 19th c. | |
| **Phone-keypad letter mapping** (3x4 with letters A–Z) | AT&T 1960s → ISO 9995-8 | Predates iPhone; demanded compatibility |
| **Q-codes / 10-codes / phonetic alphabets** | Telegraphy, CB radio | |

### 3.4 Negative space — what *isn't* this category

Worth being explicit so the category has crisp edges:
- **Open formats** like PNG/MP3 are governed by specs and reference implementations — closer to "format" (Sterne) than "social software."
- **Open protocols** like HTTP/IRC have RFCs and steward bodies — Galloway's *Protocol* covers this terrain.
- **Folksonomies** (tags, hashtags) are content-classification, not interface-as-object.
- **Memes** propagate but are content, not interface.

The keymap is distinct: **a small declarative table that mediates input/output, with no governing body, that nonetheless behaves as if standardized.**

---

## 4. Paper outline (target: 8 pages, arXiv two-column)

1. **Opening: notepat.com as artifact** (½ pp) — show the keymap diagram (we already have `slides/notepat-keymap/notepat-keymap.png`); state that this overlay isn't ours, it's inherited.
2. **The category problem** (1 pp) — keymaps are software-shaped but not software. Open-source/proprietary is the wrong axis. Introduce the term "social software" with a deliberate nod to and reframing of Shirky 2003.
3. **Lineage I: keyboard layouts** (1½ pp) — Sholes → Dvorak → Colemak / fork ecosystem → AZERTY/BÉPO → ISO 9995. Anchor with David 1985 vs Liebowitz & Margolis 1990 (path-dependence as the canonical scholarly treatment).
4. **Lineage II: instrument-layout inventors** (1½ pp) — Janko (negative case), Bosanquet, Wicki-Hayden, Lumatone, LinnStrument, ROLI Seaboard. Convergent re-invention of Wicki-Hayden as evidence of *discoverability* of these objects.
5. **The DAW chromatic-staircase case + notepat as intervention** (1½ pp) — Ableton, Logic, GarageBand, and FL Studio all converge on the same chromatic-staircase overlay (`Z X C V B N M` = white keys C–B; sharps on the row above); nobody owns it, no spec defines it, but users expect it across DAWs. The convention is **inherited from 1990s tracker software** (Renoise, FastTracker), where it made sense as a chromatic interface; it persists in piano-roll DAWs where it doesn't. Notepat's "notes that name themselves" mapping is presented as a deliberate, better-designed alternative — not a fork, but a competing proposal. The fact that notepat had to *invent* this rather than *adopt* it is itself the argument: the social-software gap was open and nobody had filled it well.
6. **The corollary catalog** (1 pp) — compressed table of §3 above, arguing the genus is real and large.
7. **Lialina: keymaps as the empirical body of the Turing Complete User** (1 pp) — keymaps *are* the gaps; their disappearance *is* the desktopization Lialina warns about.
8. **What this asks of designers** (½ pp) — short, polemical: ship the keymap as a first-class object. Make it readable, exportable, forkable. Notepat does. So should others.
9. **Related work + acknowledgments** (½ pp).

---

## 5. What the platter already gives us

Sister AC papers we should cross-cite (citations they already host, so we reuse instead of duplicating):

- **`arxiv-url-tradition/references.bib`** — already cites Lialina (`lialina1996boyfriend`, `lialina2005vernacular`), Galloway *Protocol* (`galloway2004protocol`), Manovich *Software Takes Command* (`manovich2013software`), Kittler "There is No Software" (`kittler1995nosoftware`), Bush *As We May Think* (`bush1945memex`), Nelson "A File Structure" (`nelson1965literary`), Brock *Distributed Blackness* (`brock2020distributedblackness`). This is roughly half our theoretical apparatus.
- **`arxiv-notepat/references.bib`** — already cites Magnusson 2010 *Designing Constraints*, McPherson & Tahıroğlu 2020, the Tahıroğlu/Magnusson/Parkinson/Garrelfs/Tanaka *DMI as Probes* paper, Marquez-Borbon *DMI Adoption and Longevity*. The DMI literature is already in the platter.
- **`arxiv-pieces/references.bib`** — Shklovsky, Adorno, Langer, Ingold (per `RESEARCH-DIRECTION.md`). Touch only if the "piece-as-aesthetic-unit" thread maps onto "keymap-as-aesthetic-unit" — probably worth a footnote, not central.
- **Recap of `RESEARCH-DIRECTION.md` line on notepat** — already plans McLuhan *Medium is the Message* citation for notepat; we inherit that frame.

The paper that overlaps the most is **`arxiv-notepat/notepat.tex`** ("notepat.com: From Keyboard Toy to System Front Door"). Decision needed (see §8): is this a *separate* paper, or a *companion* paper that the notepat draft should ship next to? Current bias: separate, so the keymap argument can stand on its own and address audiences who don't care about AC.

---

## 6. Bibliography to acquire

Organized into "high confidence — can cite as-is from dossier" and "needs verification before citing." Stub `references.bib` accompanies this plan and is pre-seeded with the high-confidence set.

### 6.1 High confidence (seed in `references.bib`)

- **Lialina 2012** — "Turing Complete User," *Contemporary Home Computing*, October 2012. <https://contemporary-home-computing.org/turing-complete-user/>
- **Lialina 2021** — *Turing Complete User: Resisting Alienation in HCI*, Interface Critique book, Heidelberg UP, open access.
- **Lialina 2018** — "Rich User Experience, UX and Desktopization of War," *Interface Critique Journal* 1.
- **Lialina & Espenschied 2009** — *Digital Folklore: To Computer Users, with Love and Respect*.
- **David 1985** — "Clio and the Economics of QWERTY," *American Economic Review* 75(2):332–337.
- **Liebowitz & Margolis 1990** — "The Fable of the Keys," *Journal of Law and Economics* 33(1):1–25.
- **Arthur 1989** — "Competing Technologies, Increasing Returns, and Lock-In By Historical Events," *Economic Journal* 99(394):116–131.
- **Sterne 2012** — *MP3: The Meaning of a Format*, Duke UP. **Most important precedent for the paper's method.**
- **Bowker & Star 1999** — *Sorting Things Out: Classification and Its Consequences*, MIT.
- **Star & Griesemer 1989** — "Institutional Ecology, 'Translations' and Boundary Objects," *Social Studies of Science* 19(3).
- **Shirky 2003** — "A Group Is Its Own Worst Enemy" (ETech keynote, April 24 2003) — for the original "social software" coinage.
- **Allen 2004** — "Tracing the Evolution of Social Software," *Life With Alacrity*, Oct 13 2004.
- **boyd 2005** — "The Significance of Social Software," BlogTalk Reloaded.
- **Singmaster 1981** — *Notes on Rubik's "Magic Cube"*, Penguin.
- **Walshaw n.d.** — ABC notation standard, <https://abcnotation.com/wiki/abc:standard:v2.1>.
- **NIME-isomorphic cluster** — Milne, Sethares & Plamondon 2007 *Tuning Continua and Keyboard Layouts* (Computer Music Journal); Milne 2011 *Hex Player* (NIME); Park & Gerhard 2013 *Rainboard and Musix* (NIME); Schiettecatte 2014 *Striso* (NIME).
- **Hookway 2014** — *Interface*, MIT.
- **Galloway 2004** — *Protocol*. *(Already in `arxiv-url-tradition`; reuse.)*
- **Emerson 2014** — *Reading Writing Interfaces*, Minnesota.

### 6.2 Needs verification before citing

(Verify before drafting; flagged in the dossier as such.)

- Sholes 1873 patent number for the QWERTY arrangement specifically.
- Maltron — Lillian Malt PIRA Symposium 1977 paper as primary source.
- Lippens layout dating.
- AXiS-49 / C-Thru / Thummer dates.
- Continuum Fingerboard (Lippold Haken) primary date.
- Hohner harmonica blow/draw notation first canonical citation.
- Whether FL Studio's QWERTY-piano default *actually* differs from Ableton/Logic/GarageBand (the paper's strongest counter-example, so this matters).
- Bratton "stack of conventions" — the closer attested phrase is "accidental megastructure"; either reword our usage or find primary text.
- Star "Ethnography of Infrastructure" (1999) full citation.
- Cohost authorship on the AI-era extension of "Turing Complete User."

---

## 7. Open questions

1. **Is the keymap *really* a discrete category, or just an instance of "format" (Sterne) / "protocol" (Galloway) / "boundary object" (Star)?** The paper's defensible position: it shares features with all three but is none — *format* implies a parser; *protocol* implies a wire spec; *boundary object* implies multiple communities of practice. A keymap is a one-page table that is none of those things and yet behaves like all of them.
2. **Is "social software" the right name?** It's a deliberate pun on Shirky and risks reader confusion. Alternatives: "vernacular interface," "social format," "tabular convention." Vote here is for *social software* because the pun is the point — the term in 2003 designated software *for* sociality; in 2026 it should designate software *of* social agreement. Both are real.
3. **Is notepat the right anchor, or should we lead with a more familiar example?** Argument for notepat-first: it is what the paper grew out of, and the platter has the artifact (`slides/notepat-keymap/notepat-keymap.png`) ready. **Resolved (2026-04-30)**: open with notepat as the *intervention*, then in §5 reveal the dominant DAW chromatic-staircase as the inherited-but-bad incumbent. The reader's "wait, isn't there already a standard for this?" is the rhetorical pivot — the answer is "yes, and it's worse than this one, and *that's the point*: the standard is unowned, unaccountable, and re-authorable."
4. **What does the paper *do* politically?** Lialina's stake is preserving the user-as-author. The paper's stake is asking software designers to **ship the keymap as a first-class object — readable, exportable, forkable.** This is concrete. It is also actionable. Probably worth a half-page polemical close.

---

## 8. Coordination with sibling papers

- **`arxiv-notepat/notepat.tex`** — overlaps but does *not* duplicate. The notepat paper is "what is notepat and where does it sit in AC's surface area." This paper is "what kind of object is the *keymap notepat ships*." Cross-cite.
- **`arxiv-url-tradition/url-tradition.tex`** — same Lialina/Kittler/Galloway theoretical apparatus. Cite explicitly: *both* papers argue AC participates in unowned conventions; URLs and keymaps are two species of the same genus.
- **`arxiv-folk-songs/folk-songs.tex`** — folk-songs literature already cites tunes-as-shared-object material. Likely a footnote in §3.2 (ABC notation, harmonica blow/draw).

---

## 9. Artifacts that should ship with the paper

- Reuse `slides/notepat-keymap/notepat-keymap.png` as Figure 1.
- New figure: **the corollary catalog** rendered as a wall-poster grid — one cell per object in §3, with originator, year, propagation icon. Reuses the design language of the notepat-keymap slide. Build script can be a sibling under `slides/`.
- New figure: **lineage tree** — Sholes → ISO/AFNOR → Janko/Wicki/Bosanquet → DAW typing → notepat. Drawn with TikZ or Penrose (`arxiv-penrose/` infrastructure).
- One-page card version via `cards-convert.mjs`, per the platter's standing convention.

---

## 10. Next steps (when this becomes work, not just a plan)

1. Verify the §6.2 "needs verification" list (one focused web pass).
2. Acquire and read the §6.1 high-confidence sources (Sterne *MP3* is the priority).
3. Draft `keymaps.tex` from the §4 outline, reusing layout packages from `arxiv-notepat/notepat.tex` (acpink/acpurple palette, draftwatermark, `\acbold` font).
4. Build the corollary-catalog figure (sibling to `slides/notepat-keymap/`).
5. Add row to `papers/SCORE.md` once a build produces `keymaps.pdf`.
6. Consider venue: SIGCHI alt.chi or Interface Critique journal would fit the Lialina-flavored argument; NIME if we lean on the instrument-layout half.

---

## Appendix A — Research dossier (raw)

The full literature dossier compiled 2026-04-30 is preserved verbatim in this repo's commit history of `RESEARCH-PLAN.md` (initial revision) and was the basis for §3, §6, and §7. Source quality flagged inline as "needs verification" where the search did not surface a primary-source URL or a confirmed date.
