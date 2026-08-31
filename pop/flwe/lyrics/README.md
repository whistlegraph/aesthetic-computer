# flwe lyrics — the canonical sheet

`flower-eater.txt` is the settled lyric for "Flower Eater", built by
cross-reading all eight take transcripts in `../analysis/whisper-raw/`
against the primary word-level transcript (`../analysis/transcript.json`,
take **6992837952212569350**, the 2021-08-05 "behind the scenes" spine).
Where the takes disagree the sheet keeps the **primary take's reading** in
the line and carries every variant in a bracketed inline note, so one
glance settles each ruling.

Convention follows `pop/cult/lyrics/`: plain text, a lowercase section
tag per stanza, one sung line per line.

## Take key

| short | post id | date | role |
| ----- | ------- | ---- | ---- |
| **primary** | 6992837952212569350 | 2021-08-05 | the spine; every canonical reading |
| sick-bed | 6948629412728360198 | 2021-04-08 | quietest room |
| premiere | 6949031877718117638 | 2021-04-09 | first full take |
| 🌻 take | 6949568150962703621 | 2021-04-10 | |
| excerpt | 6949737524520602885 | 2021-04-11 | verses 2–3 only |
| all-the-way | 6950816151547022598 | 2021-04-14 | cleanest "Mystery flower" |
| Providence | 6975681078543551749 | 2021-06-20 | tiny-recital, crowd noise; song at ~67–127 s |
| live | 6977277752525344005 | 2021-06-24 | live, song only |

## Variant table (needs @jeffrey's ruling)

| line | primary reading | variants | heard in |
| ---- | --------------- | -------- | -------- |
| v1 L4 | **Hold up** with both of my hands | "hold of/off" | sick-bed, all-the-way, live — vs "hold up" in premiere, 🌻, Providence, primary. Likely one word either way; the split is clean by take, not by date. |
| v2 L2 | **I am hatin' green now** | "I am angry now" | sick-bed, all-the-way, live (3 takes) |
| | | "I am hangry now" | premiere |
| | | "I am hungry now" | 🌻 take, excerpt |
| | | *(primary alone hears "hatin' green"; every April/June take hears a hunger/anger word. Whisper confidence is lowest here of any line — four mutually exclusive hearings of the same two syllables. The spoken outro's "girl who eats flowers" leans hungry/hangry; "hatin' green" would be the primary inventing words no other take supports.)* | |
| v3 L1 | Now **I think I wanna** have you every day | "I think I have to have you" | premiere, 🌻 take |
| | | "I know I wanna have you" | excerpt |
| v3 L2 | I'll spit **twee** of your seeds | "three" | sick-bed, Providence, live (twee = the child-voice "three"; premiere heard "dwee" — same sound) |
| outro L1 | **Harvest moon** I'll come collect your spawn | "For this moon" | live, Providence ("for this moon" — phonetically close to "harvest moon" under crowd noise; all four April takes + primary hear "Harvest moon") |
| outro L2 | **They're** gonna be all mine | "You're gonna be all mine" | live only |

### Not real variants (ASR artifacts, no ruling needed)

- "**Mr. E flower**" (excerpt, Providence) = whisper splitting
  "Mystery flower" into its homophone; same sung syllables.
- "flower **either**" (live) for the final "flower eater".
- "spit twee **off** your seeds" (🌻 take) for "of".
- "cuz" vs "'cause" — spelling only; the sheet keeps the primary's "cuz".

## Melody-chart pointers

`../analysis/melody-chart.json` phrases (index → beat span at ~110 BPM,
anchor 18.62 s in the primary) carry the sheet's lines like this:

| chart phrase | beats | carries |
| ------------ | ----- | ------- |
| 0 "Looked for so long…" | 0–11.5 | v1 L1 |
| 1 "Left with my arms…" | 12–20 | v1 L2 |
| 2 "My lollipop…" | 21–28 | v1 L3 |
| 3 "Hold up with both…" | 29–35 | v1 L4 |
| 4 "Mystery flower…" | 36–44.5 | v2 L1 |
| 5 "I am hatin green now" | 45–51 | v2 L2 |
| 6 "I'm gonna take you…" | 51–64 | v2 L3 |
| 7 "Yum yum yum" | 65–66.5 | v2 L4 |
| 8 "Now I think… your seeds" | 68–82 | v3 L1 **and** v3 L2 (one chart phrase, two sheet lines; the break falls after "day", beat ~78) |
| 9 "Bury them in the ground" | 82–86.5 | v3 L3 |
| 10 "Harvest moon… spawn They're" | 87–95.5 | outro L1 (+ the pickup "They're" that opens outro L2) |
| 11 "gonna be all mine… flower eater" | 96–112 | outro L2–L4 (the first "looping every day" ends L2; the next two are L3; "flower eater" is L4, the 5→1 cadence) |

Take-quality context (which take to dub from, per phrase) is
`../analysis/takes.json`; its `better_elsewhere` flags — "Mystery flower"
(all-the-way) and "Bury them in the ground" (excerpt) — are quality
verdicts, not lyric variants.
