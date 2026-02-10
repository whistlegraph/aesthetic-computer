# Flusser's Phenomenology of Underdevelopment and Aesthetic Computer

**Source:** https://www.e-flux.com/journal/160/6776837/on-vil-m-flusser-s-phenomenology-of-underdevelopment

**Author:** Rômulo Moraes, e-flux journal #160, February 2026

---

## Summary of Article

Rômulo Moraes recovers Vilém Flusser's early Brazilian writings — work that has been largely ignored by Euro-American scholars who focus narrowly on Flusser as a media theorist (*Towards a Philosophy of Photography*). The central text is *Brasilien oder die Suche nach dem neuen Menschen: Für eine Phänomenologie der Unterentwicklung* ("Brazil or the search for a new man: For a phenomenology of underdevelopment"), written in German and Portuguese in the 1960s but published posthumously in 1994 (German) and 1998 (Portuguese). Moraes translates the chapter on "delays" for this issue.

### Key Concepts

**Underdevelopment as different structure, not deficit.** Flusser reframes "underdevelopment" not as being behind on a developmental ladder but as a fundamentally different framework of interspersion between historical elements. Brazil doesn't belong to the same structure that produces the social division of time and space responsible for the uniformization of historical development in Europe.

**Coexisting temporalities ("delays").** In Brazil, the medieval coexists with the industrial, the neolithic with the romantic — not as stages in a sequence but as overlapping lived realities. A Brazilian engineer develops complex technological systems at work and goes home to a favela following feudal patterns of vassalage. The sexual revolution functions alongside premodern religious moralities. These are not "delays" relative to Europe but a whole other structure.

**The migrant as post-historical consciousness.** Flusser (a Prague-born Jewish exile who settled in São Paulo in 1940 after escaping the Nazis) argues the migrant breaks national identity and re-curates cultural elements with conscious freedom. The "ugly stranger" works freely from within an established culture, makes its mystery explicit by selecting what's most interesting, and blends it "like a divine curator." The migrant intelligence is a microcosm of future global culture.

**Brazil as "country of the future" (ontologically).** Borrowing Stefan Zweig's phrase, Flusser reinterprets it: Brazil is futural not in terms of occupying a position on the ladder of development, but in terms of its mindset — "a country of endless expectation, openness, projection, and hope." Creolized cultures anticipate future social and cultural practices because their very foundation lies in those practices.

**Post-historicity rooted in colonial experience.** Flusser's later media theory (post-history, imagetic consciousness, the end of textual meaning) is inseparable from his Brazilian decades. The *barroco mineiro* is not a lesser European baroque — it's "beyond baroque, beyond Prague, beyond history, beyond any European labelling of art phases." Brazil borrows history only to free itself from it and create something else entirely.

**Cultural cannibalism across movements.** The same process Flusser identified in the *barroco mineiro* repeats across Brazilian art: the "cultural cannibalism" of early modernism, the pop potpourri of 1960s Tropicália, and late-twentieth century visual art anticipating postmodern/post-historical trends.

### Biographical Context

- Born Prague, 1920. Family killed by Nazis; he and his brother were the only survivors.
- Settled São Paulo 1940. Worked at a radio transistor factory while writing *Language and Reality* (1963) and *History of the Devil* (1965).
- Deep engagement with Brazilian intellectual life: the Campos brothers (concrete poetry), João Guimarães Rosa, Mira Schendel, and the Instituto Brasileiro de Filosofia circle.
- Fascinated by Brasília (architecture as writing), Brazilian football (published "Peleology"), bossa nova ("a bomb effect to the whole of Western culture").
- Politically ambivalent: classical liberal with conservative hints, friends across the political spectrum including some who became part of the post-1964 military regime and others who were imprisoned and tortured by it.
- Left Brazil for Europe in 1972, partly due to disillusionment.

---

## Resonances with Aesthetic Computer

### 1. Coexisting Temporalities in One Runtime

Flusser's key claim: in Brazil, feudal and industrial, neolithic and romantic exist simultaneously — not as stages but as overlapping lived realities.

AC's piece system does this literally. A `.lisp` file using `(wipe blue)` and `(box 10 10 50 50)` runs in the same Web Worker sandbox as a `.mjs` piece initializing WebGPU shaders and UDP multiplayer via geckos.io. KidLisp and JavaScript aren't successive levels of sophistication — they're concurrent layers with the same API surface, same `$` object, same publish path. The runtime doesn't enforce a uniform level of technological complexity.

### 2. The Migrant as Piece Author

Flusser: the migrant breaks national identity and re-curates cultural elements with conscious freedom.

AC's `source` → edit → `publish` loop is structurally identical. `source notepat` forks an existing piece; the author recombines its patterns under their own `@handle`; `publish` puts the remix at a new URL. The handle system (Auth0 → MongoDB → AT Protocol) means identity travels across aesthetic.computer, kidlisp.com, and any subdomain piece. You're always arriving somewhere already configured, selecting what to embody from it.

### 3. Single-File Pieces as "Underdeveloped" Architecture

Flusser: the *barroco mineiro* is not a lesser baroque but a *different thing* — freed from European art-phase anxiety because Brazil never internalized that linear history.

AC pieces are single ESM or Lisp files with hook exports (`boot`, `paint`, `act`, `beat`, `sim`). No build step, no dependency graph, no framework inheritance (unless you opt into a `system` like `nopaint`). This looks "underdeveloped" next to a React app. But the constraint is the point — a different structure of interspersion where a 3-line KidLisp sketch and a 9,000-line prompt system share the same publish pipeline and URL space.

### 4. Channel Broadcasting as Creolization

Flusser: diasporic culture is "a series of encounters, clashes, and adaptations between people."

AC's `channel` system — where chokidar file-watches push reload signals over WebSocket to every subscribed device, and Redis pub/sub relays code across session-server instances — makes this literal. Your piece code arrives on someone else's screen and runs in their runtime context. The code migrates; the execution is local.

### 5. Post-Historical URL Space

Flusser: Brazil is "beyond baroque, beyond Prague, beyond history, beyond any European labelling of art phases."

Every AC piece occupies the same flat URL namespace: `aesthetic.computer/notepat` is a musical instrument, `/chat` is a social space, `/@bash/hub` is someone's published remix, `/$abc123` is a hashed KidLisp snippet. The URL doesn't encode what category of software it is. There are no app store genres. The prompt is a text input that resolves names to running code.

### 6. Origin Story Parallels

Flusser's philosophizing was rooted in concrete exile — processing familial death in a new culture, building intellectual life from the margins of Brazilian academia, working at a transistor factory while writing philosophy.

AC was born from the margins of art education and indie software: No Paint users sending PNGs by email, COVID-era improvisation, art students wanting to publish experiments without friction. The "underdeveloped" constraints (single-file pieces, minimal APIs, mobile-first defaults, manual contribution workflows that eventually demanded a platform) are the source of the system's creative architecture, not limitations to be overcome.

---

## Technical Details Referenced

| AC Component | Role | Flusser Parallel |
|---|---|---|
| Piece system (`.mjs` / `.lisp` hook exports) | Single-file sandboxed programs sharing one runtime | Coexisting temporalities — heterogeneous complexity in one framework |
| `$` API object | Unified surface for graphics, audio, input, network, state | The "new man" operating across all historical layers simultaneously |
| `source` → `publish` → `@handle` | Fork, remix, republish under your identity | Migrant as divine curator of cultural elements |
| `channel` + Redis pub/sub + chokidar | Live code broadcasting across devices | Creolization as encounter/adaptation between people |
| Flat URL namespace (`/piece`, `/@handle/piece`, `/$code`) | No genre/category encoding | Post-historical dissolution of software categories |
| KidLisp alongside JavaScript | Concurrent languages, same sandbox, same publish path | Multiple historical layers coexisting without hierarchy |
| Web Worker isolation + BIOS compositing | Pieces run in worker, main thread handles DOM/canvas/audio | Lived experience (worker) vs. mediated colonial logic (display layer) |
