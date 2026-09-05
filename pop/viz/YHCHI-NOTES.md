# YHCHI design brief — for the wannadash lyric reel

Research on Young-Hae Chang Heavy Industries (Young-hae Chang + Marc Voge,
Seoul), 2026-09-05, commissioned by @jeffrey for the lyric-reel visual
strategy ("we may need to storyboard different words and utterances / have
different 'views' we cut between"). Companion to `lyric-reel.py`.

Research base: the Yoo/dichtung-digital interview (2005), Thom Swiss's Iowa
Review Web interview (2002), Jessica Pressman's *Digital Modernism* chapter on
DAKOTA, the Electronic Literature Directory, Art Papers, Rhizome's Net Art
Anthology, Honest Ulsterman, Vice, and yhchang.com.

## 1. Core typographic/temporal strategy

**One font, forever.** Everything is set in Monaco — the 1984 Apple monospaced
programmer font — almost always ALL CAPS. They picked it partly because "they
liked the way the name sounded." Their own artist statement: a "simple
technique that shuns interactivity, graphics, photos, illustrations, banners,
colors, and all but the Monaco font." Text is the *only* visual element. Their
motto on constancy, from the 2005 interview: "One computer program, one
recipe. Simplicity is a virtue."

**Color.** Default is black text on white (DAKOTA is "large and clear, black
on a white background"), with white-on-black inversions. Deviations are rare
and load-bearing: CUNNILINGUS IN NORTH KOREA is "black and white — with
occasional FLASHes of red"; WA'AD sets white text against a dark purple
starscape; the 2017 SAMSUNG remake changed to a white background *with a new
soundtrack and a new rhythm* — i.e., for them a background change is a
re-scoring, not a reskin.

**Sizing and density.** Text fills the browser frame. Art Papers: "groupings
of one or two words that fill up the entire screen; they may be followed by a
long paragraph, then maybe a brief pause." Scale is their emphasis dial — in
OPERATION NUKOREA "differing text-size manages to convey horror, hysteria and
a very dark humour." So: giant one-to-two-word slams alternating with dense
multi-line panels; nothing in between dominates.

**Time is authored, not offered.** No pause, rewind, or seek. Their words: "We
would like our own work to exert a dictatorial stranglehold on the reader."
Reading time, direction, and duration are determined by the piece; the
viewer's only power is "clicking away." Pieces run "from one minute to 25
minutes that fill up the browser." Speed deliberately borders on illegibility
so words become concrete perceptual objects, not just carriers of meaning.

**Cinema, not web.** Every piece opens with a film-leader countdown (their
"signature countdown," which differs across language versions). They aspired
to make net art "as entertaining as TV"; they called interactivity "laughable,
not unlike channel surfing." The pieces are text *movies*: opening credits,
editing rhythms, loops.

**Language re-scoring.** Key craft detail: "It's hard to fit a different
language into the same musical and rhythmic mold as the original piece, so we
often compose different music to go with the translation." The text-music bind
is that tight — change the words, recompose the score.

## 2. Editing grammar

- **Replacement-in-place montage.** No scrolling, no tweening, no easing. A
  screen of text is hard-replaced by the next, in the same frame position.
  Pressman reads this as Flash renovating Ezra Pound's "super-position" —
  textual montage where meaning arises from the *cut* between two screens,
  exactly like film editing.
- **Every visual transient sits on an audio transient.** "Texts flash by...
  in perfect rhythm with the soundtrack." In DAKOTA, drum intensity directly
  maps to the speed and density of word presentation. This is why the cuts
  feel musical: nothing changes on screen without an audible event
  underwriting it.
- **Syncopation against an established grid.** CUNNILINGUS "proceeds in
  rhythmic syncopation" — they lock a pulse first, then place text off it.
  Syncopation only reads as such because the grid was established.
- **Acceleration + density as the tension arc.** DAKOTA "gradually
  accelerates with the music until it tests the reader's attention, reflexes,
  and reading comprehension... switching between one and two lines per
  'flash' as the piece reaches its conclusion," escalating until it is
  "punctuated by the exuberant hollering of band members." Tension = cut-rate
  × words-per-cut, ridden along the music's intensity envelope.
- **The held frame is a beat too.** They pair bursts with silences — Art
  Papers notes they hold "the static image of purple space for a few seconds
  too long." Long paragraphs are followed by "a brief pause." Holds after
  barrages are where meaning lands.
- **Repetition with structural variation.** BUST DOWN THE DOORS! re-runs the
  same raid/execution story with the pronoun swapped (second → first → third
  person), recasting the viewer as victim, executioner, witness. Loops aren't
  a limitation; they're re-reads with new frames.
- **Aging gracefully = slowing down.** Their later "sober" period trades the
  "brilliant, complex, and explosive... drumming of Art Blakey and Max Roach"
  for calmer self-composed scores — proof the grammar scales down in tempo
  without losing identity.

## 3. Notable works

- **DAKOTA (2002)** — the canonical one. Pound's Cantos I–II retold as a
  drunken Dakota road trip, cut to Art Blakey's frenetic Afro-Drum Ensemble
  track "Tobi Ilu." Words flash on drum gestures; the piece accelerates past
  readable and ends in one-to-two-line barrages. Subject of Pressman's "Speed
  Reading" chapter.
- **BUST DOWN THE DOORS! (2000)** — countdown opening; midnight raid and
  execution narrated in flashing words; on each loop the pronouns shift,
  changing who *you* are in the story.
- **THE STRUGGLE CONTINUES (2000)** — among the first five works ever on
  yhchang.com; "the struggle" turns out to be "the stark-naked struggle for
  love"; fast, funny text against deliberately incongruous jazz.
- **CUNNILINGUS IN NORTH KOREA (2003)** — Kim Jong-Il propaganda voice; B/W
  with red flashes; driven by Nina Simone's "Sea-Lion Woman"; heavily
  syncopated.
- **SAMSUNG / SAMSUNG MEANS TO COME (1999)** — corporate love-paean quoting
  *Lolita*'s opening; exists in many language versions with different music
  and countdowns; 2017 remake = new background, new score, new rhythm.
- **OPERATION NUKOREA** — the slow register: solo piano (silent-film feel),
  text-size doing the emotional work.
- **WA'AD** — the color exception: white text on purple starscape,
  Chopin-esque piano, overlong held frames.

## 4. Actionable principles for the lyric reel

Context: current renderer = horizontally scrolling "train" of per-character
tiles through a center focus, curved rails, motion blur, per-character
utterance lighting. The v2 editorial cut (race/impact/poly/field views +
spatial wipes) was rejected 2026-09-03 because the cuts fought the song
timing — principles 2–4 below are the diagnosis of that failure.

1. **Add a second register: the SLAM slide.** YHCHI's whole language is
   discrete; ours is continuous. The power move is the *contrast*. Promote
   hook words, downbeat landings, and phrase-final words out of the train
   into full-frame hard-cut slides: one or two words, sized to frame width,
   flat background, zero internal motion. The cut itself is the animation —
   any easing or motion inside a slide kills it.
2. **Cut on the transient, hold through the sustain.** A slide appears
   exactly on the sung onset (we already have utterance timings) and holds
   for the note's duration. Melisma = one held slide, not many. Rest =
   blackout or held empty frame. This single rule is most of "musical, not
   random."
3. **Three-level rhythmic hierarchy.** View changes (train ↔ slam ↔
   dot-explosion) on phrase/bar boundaries; slide changes within a view on
   beats/utterance onsets; per-character lighting on sub-beats. Never cut a
   *view* on a sub-beat — that's what makes cuts feel arbitrary. YHCHI
   effectively does this with section-pauses vs. word-flashes.
4. **Establish the grid, then syncopate.** Run 4–8 straight on-beat cuts
   before placing anything off-beat. An early/late slide only reads as swing
   once the pulse is proven. Sparse off-grid "red flash" moments (single
   word, single accent color, one frame early) are chord changes, not
   decoration.
5. **Density is the tension arc.** Map cuts-per-second and words-per-slide to
   the song's intensity envelope, DAKOTA-style: verse = train (continuous,
   legible), pre-chorus = quickening slam inserts, chorus climax = one-to-
   two-word barrage slightly faster than comfortable reading, then a hard
   hold on silence. Let the climax outrun readability on purpose — the reel
   loops, and YHCHI's loops reward re-watching.
6. **One typeface, scale as the only emphasis.** Keep the current font but
   forbid mixed weights/faces within a view. Emphasis = scale jump, ALL
   CAPS, or repetition — never color-per-word, never italics. Reserve palette
   inversion (black-on-white ↔ white-on-black) for section boundaries so it
   reads as structure.
7. **Open with a leader countdown.** 3-2-1 synced to the count-in or first
   drum hits, in the same typeface. It's YHCHI's signature, it frames what
   follows as a *movie*, and it teaches the viewer the cut-grid before the
   lyrics start.
8. **Storyboard as named views with musical triggers**, not as a timeline of
   effects: `COUNTDOWN` (intro), `TRAIN` (default verse flow), `SLAM` (1–2
   beat full-frame inserts on accented words), `EXPLOSION` (dot-scatter
   reserved for one moment per section — a cymbal crash or phrase release,
   since it's the most entropic view), `HOLD/BLACKOUT` (rests, and 1–2 beats
   *after* a barrage — the YHCHI "few seconds too long"). A section picks a
   dominant view; accents punch through it; each view change lands on a bar
   line.
9. **Repetition with variation on the loop.** When the hook recurs,
   re-render it varied — inverted palette, doubled scale, or a swapped word —
   the BUST DOWN THE DOORS! pronoun trick applied to a chorus.
10. **Dictatorial time.** Never stretch a slide beyond its beat "so people
    can read it." YHCHI's tempo discipline — trusting the music over the
    reader — is precisely what makes the work feel confident. If a lyric
    doesn't fit its note, cut the words, not the timing.

## Sources

- [Wikipedia — YHCHI](https://en.wikipedia.org/wiki/Young-Hae_Chang_Heavy_Industries)
- [Yoo, "Interview with YHCHI," dichtung-digital 35, 2005 (mediarep PDF)](https://mediarep.org/items/dda9c679-5202-47aa-83e3-485e85084257)
- [Electronic Literature Directory — Dakota](https://directory.eliterature.org/individual-work/4835)
- [Pressman, "Speed Reading," *Digital Modernism* (OUP 2014)](https://academic.oup.com/book/2852/chapter/143424711)
- [Art Papers — YHCHI](https://www.artpapers.org/young-hae-chang-heavy-industries/)
- [Vice — "Web Art Duo YHCHI Speak Out"](https://www.vice.com/en/article/web-art-duo-y0ung-hae-chang-heavy-industries-speak-out/)
- [Honest Ulsterman — YHCHI](https://humag.co/article/1322/young-hae-chang-heavy-industries)
- [Rhizome Net Art Anthology — Samsung](https://anthology.rhizome.org/samsung)
- [Sample Reality — Dakota viewing account](http://samplereality.com/gmu/digital/2012/09/10/we-drank-and-insulted-each-others-mothers/)
- [E.L.E. E-CYCLOPEDIA — Cunnilingus in N0rth K0rea](http://gantercourses.net/ecyclopedia/2013/03/22/cunnilingus-in-n0rth-k0rea/)
- [I ❤️ E-Poetry — Bust Down the Doors!](https://iloveepoetry.org/?p=11426)
- [Jeffrey Moro, E.LIT/NET.ART — The Struggle Continues](https://elit.jeffreymoro.com/gallery/struggle-yhchi/)
- [M+ Magazine — YHCHI acquisition](https://www.mplus.org.hk/en/magazine/yhchi-acquisition/)
- [yhchang.com](https://yhchang.com/)
- Swiss, "Distance, Homelessness, Anonymity, and Insignificance," *The Iowa Review Web*, 2002 (quoted via secondary sources).
