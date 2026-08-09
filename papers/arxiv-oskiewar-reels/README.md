# Take the Psychology Out of Posting

System paper for the `@oskiewar` reel factory — the marketing output function
that turns a date into a finished Instagram Reel with no human choosing what to
post.

Source: `reels.tex` + `references.bib` + `figures/`. Build with the `/papers`
stack (paper id `arxiv-oskiewar-reels`, registered in `papers/cli.mjs`).

The system it describes is `xbox/live/marketing/`; the operator's manual is
`xbox/live/MARKETING.md`. Every measurement in §8 and §9 was taken from the
three reels staged in `tmp/oskiewar-reels/queue/` with `ffprobe`/`ffmpeg` at the
time of writing; the determinism experiment in §7 is the operator's run and is
labelled as such.

Figures are regenerated from the current reels (all PNG32/RGBA):

- `reel-frame.png` — one native 1080×1920 frame, slot 660 at t = 14 s.
- `slidecop.png` — the chosen cover beside the same frame rebuilt from the
  108×192 review thumbnail.
- `trim.png` — four head frames and four tail frames showing that the reel opens
  on the previous round's result card and closes on the next round's intro.

Platform claims cite Meta-owned documentation read on 8 August 2026. Stage four
has never been run against the live API, because the account does not exist yet;
that is stated in the abstract rather than buried, and §12.4 lists it as the
principal limitation.
