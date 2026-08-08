# 07 · Meter, entrainment, groove — where the grid stops

**Shelf:** perception · **Sources:** `london-2012`, `butler-2006`, `iyer-2002`, `danielsen-2010`, `sethares-2007`, `rahn-1996`

Shelves 01–06 describe *notated* timelines: onsets on an idealised grid, with
properties that are exact. This shelf is the correction, and it should be read
before any claim is made that a measured property explains why something sounds
good.

## Entrainment limits (London)

A listener entrains to periodicities in a bounded window — roughly a **100 ms
floor** and a **2 s ceiling** for a felt beat, with the strongest pull near
500–700 ms. Outside that window a period is still *countable* but is not *felt*
as a beat.

Two direct consequences for the lanes:

1. **The cycle must land inside the window to be heard as a cycle.** At 120 BPM a
   16-pulse cycle is 2 s — right at the ceiling. Slower, and the ring stops being
   a rhythm and becomes a sequence of events.
2. **Precession has a speed limit.** `pop/minitek/c/hypnotek.c` sweeps rotation
   rates and reports that the ear hears the *rate of phase change*, not the
   64-bar realignment period. London gives the reason and the bound: past some
   rate the listener re-parses rather than tracks, and the identity of the
   necklace is lost. That threshold is measurable and worth measuring.

## Metric ambiguity (Butler)

In electronic dance music specifically, the same onset set supports multiple
metric interpretations, and producers exploit the switch — "turning the beat
around" is a compositional move, not an error. Since every syncopation measure in
[04](04-syncopation.md) takes the metric hierarchy as an *input*, ambiguity means
those numbers are interpretation-relative. Report the assumed downbeat alongside
any syncopation figure.

## Microtiming (Iyer, Danielsen)

The grid discards the microsecond-scale deviations that carry groove. Danielsen's
**beat bins** make the point sharply: a beat is a region of time, not a point, and
where inside the region a stroke falls is a musical decision with a name.

So a maximally even rhythm rendered dead on the grid is not "the most grounded
version" of anything — it is one quantised sample of a family. **Nothing in this
platter measures groove.** Every property here is invariant to the timing
detail that performers actually vary.

The tooling should therefore keep a **microtiming layer that is explicitly
separate** from the necklace layer: the necklace decides *which pulse*, an offset
table decides *where in the bin*. Merging them would make the measured properties
meaningless.

## Signal side (Sethares)

Recovering periodicity from audio rather than assuming a grid. Out of scope for
generation, relevant if a lane ever wants to analyse a recording — e.g. reading a
timeline out of a whistlegraph take.

## The methodological caution (Rahn)

Rahn names the risk this whole platter runs: applying European-derived formalism
to African-derived practice and mistaking the model for the music. Euclidean
rhythms are a striking *description* of a large family of world timelines. They
are not an account of why those timelines exist, how they are taught, what they
are for, or what a good performance of one is.

**This paragraph belongs in the ethics/limitations section of any paper carved
from this platter**, and in the `bracelet` thesis. The honest claim is: these
measures separate patterns reliably and predict some listener judgements. The
dishonest claim is that they explain the music.

## tools

Mostly not tools — this shelf constrains the others. What it does yield:

- `entrainment_check(n, bpm) -> {cycle_ms, pulse_ms, in_window, warnings}` —
  called before rendering, so a lane cannot silently write a cycle nobody can
  hear as a cycle.
- `precession_rate_limit(n, bpm) -> rotations_per_phrase` — the bound past which
  necklace identity is lost.
- `microtiming(rhythm, profile) -> offsets_ms[]` — a **separate layer**, never
  folded into onset positions.
