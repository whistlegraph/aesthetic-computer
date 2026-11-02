# Laptop — platter

A reading platter for **the laptop and its QWERTY keyboard as a performed
instrument**: melodic key-mapping, rhythmic tapping and key-mashing,
typing-as-music, laptop ensembles, the liveness critique, and computers booted
into instruments. A sub-platter within the [papers platter](../SCORE.md),
parallel to [rhythm-platter](../rhythm-platter/) and
[lines-platter](../lines-platter/).

> The platter exists so that a claim like "nobody else maps the letter C to the
> note C" or "AC Native OS is the first laptop that boots into an instrument"
> resolves to **a checked source or a recorded negative search** — not to a
> feeling about the field.

Its first consumer is
[`arxiv-laptop-performance`](../arxiv-laptop-performance/), which locates
`notepat` and AC Native OS inside the laptop-performance genre. It sits beside
three earlier papers that already carry part of this ground:
[`arxiv-keymaps`](../arxiv-keymaps/) (the keymap lineage, the AWSED name,
"notes that name themselves"), [`arxiv-plork`](../arxiv-plork/) (laptop
orchestras), and [`arxiv-latency`](../arxiv-latency/) (the key-to-DAC path).
Sources those papers already cite (aQWERTYon, Wicki–Hayden, Jankó, Musical
Typing, Bela, Elk, Wessel & Wright) are listed here only where the new paper
leans on them again.

## Posture

**Index and citation only.** Nothing is mirrored. Proceedings PDFs are linked
at their NIME/ICMC/CMJ URLs; products, web toys and community sites are cited
as **evidence of practice**, never as authority. `checked: true` means the
primary page was retrieved in a platter pass and the citation was read off it;
`checked: false` entries carry an author or date from working knowledge and want
a pass before a paper leans on them.

## Shelves

| Shelf | What it settles | Count |
|---|---|---|
| **typewriter** | Typing was percussion before it was input: Anderson 1950, the Boston Typewriter Orchestra | 3 |
| **note-entry** | Where QWERTY-as-piano came from: Soundmonitor 1986 → Ultimate Soundtracker 1987 → every tracker and DAW | 7 |
| **vernacular** | A large, unstudied community already writes music as typed letters (Virtual Piano / Roblox sheets) | 3 |
| **ensembles** | PLOrk, CliX, the ABC Song, SMELT, SLOrk, Taptop, Laptap, MoPhO | 10 |
| **mappings** | The named mapping logics: harmonic (aQWERTYon), isomorphic, ASCII, letter-frequency, use-frequency | 6 |
| **typing-music** | Text as the gesture: Waite 2015, Vyping (NIME 2026), Typedrummer, Orca, the Stenophone | 8 |
| **liveness** | "They could be checking email": the 2003 CMR laptop issue, Cascone, Stuart, Collins, Bown et al., Rouages, projected and live code | 9 |
| **rhythm-latency** | Tapped-rhythm entry, latency and jitter budgets, rollover, velocity | 10 |
| **instrument-os** | Boxes (Satellite CCRMA, Bela, norns, Organelle) and live distros (dyne:bolic, Puredyne, MOD live-USB) | 8 |
| **theory** | Magnusson's epistemic tools, Gurevich on constraint, Jordà on learning curves | 4 |

Machine-readable index: [`sources.json`](sources.json) (68 entries, 66 checked).

## Negative searches (recorded, not proven)

A claim of absence is only as good as the search behind it. These are the
kill-checks run on 2026-11-02 and what they returned.

| Claim under test | Queries | Result |
|---|---|---|
| A prior QWERTY instrument maps letter keys to **their own note names** (`c`→C) | 2 targeted Exa queries ("letter keys play their note names", "pressing C plays note C") plus every mapping found incidentally | **None found.** Every web piano, tracker, DAW and toy found is positional (AWSED or Z-row), harmonic, isomorphic, ASCII, or frequency-based. Virtual Piano sheets use letters as *key labels*, not note names. |
| A prior system **boots an ordinary laptop into a single instrument** | 3 queries (live distros for artists; bootable USB synth; dyne:bolic) | **Partial priors found.** dyne:bolic (2001) and Puredyne boot old PCs into *media studios*; MOD live-USB boots a PC into a *pedalboard host* that needs an audio interface; Indamixx boots into a DAW desktop. None found boots into one instrument played on the laptop's own keys. |
| Typing-to-LLM as music | 1 query | Found and current: Encinas & Jensenius, NIME 2026 ("vyping"). |

## Query log (Exa, 2026-11-02)

QWERTY melodic input research · tapping rhythms on computer keyboard ·
laptop orchestra QWERTY (PLOrk/SLOrk) · keyboard mashing music generation ·
isomorphic layout on QWERTY · rollover/ghosting/latency for performance ·
SMELT / Don't Forget the Laptop · laptop liveness critique (Cascone, Collins) ·
dedicated music OS (norns, Organelle, Bela) · NIME laptop keyboard/trackpad
instruments · Satellite CCRMA · tracker two-row piano origin · Vyping NIME 2026 ·
note-name letter mapping · finger-tapping timing studies · note-name mapping
(second pass) · Virtual Piano / Roblox letter sheets · typewriter as instrument ·
Magnusson / Gurevich / Tanaka theory · Orca · MoPhO · Gurevich style and
constraint · Bown et al. authors · Jordà low entry fee · Wang *Artful Design* ·
aQWERTYon write-ups · pure:dyne · bootable instrument USB · dyne:bolic ·
Contemporary Music Review 22(4) contents. Plus one direct fetch of five
proceedings PDFs to verify author lists.

## Digest

- [`digest/01-positioning.md`](digest/01-positioning.md) — the four claims the
  paper makes about notepat and AC Native OS, each with its nearest prior and
  the study that would settle it.
