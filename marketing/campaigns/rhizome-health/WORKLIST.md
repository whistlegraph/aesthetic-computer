# Rhizome Health — worklist

Satirical AC campaign: **Rhizome Health**, a fictional health-insurance
arm of Rhizome.org, covers every AC user with a `@handle`.

## Now

- [x] Pull real Rhizome brand identity (mark, wordmark, palette)
  - rhizome.org sits behind a Cloudflare challenge; recovered the real
    identity via the Wayback Machine snapshot + fontsinuse
  - mark = lime-green `#1FD61F` radiating roots on black (the real
    favicon — saved as `refs/rhizome-mark-favicon.png`)
  - wordmark = "Rhizome" in U001 grotesk; palette black / lime / cyan
    `#64ffff` / grey `#c1c0c0`
- [x] Build the `Rhizome Health` sub-brand logo lockup
  - `refs/rhizome-health-logo.png` — green root-mark + wordmark +
    tagline, rendered from the real identity (SVG → rsvg-convert)
- [x] Write `brand-brief.md`, `cover-prompt.txt`, this worklist
- [ ] Generate `gens/v1.png` — landscape exam-room tableau
- [ ] Review v1; check the Rhizome mark renders as true lime-green
      radiating roots and the handle-card reads "@jeffrey"

## Variants to consider

- **v2** — portrait crop if a vertical poster format reads better
  (`--size 1024x1536`)
- **v3** — tighter two-shot on jeffrey + the handle-card hand-off, for
  an identity-grounded portrait
- a matching **public-health poster** emblem (prompt-only, `--no-jeffrey`)
  if jas wants a graphic companion piece

## Generate

```bash
node marketing/bin/gen-promo.mjs marketing/campaigns/rhizome-health \
  --size 1536x1024 --variant v1
```

## Rules carried in

- Peer horizontality — jeffrey is the patient, off-center, same
  eye-line as the intake clinician; never the hero.
- Colored-pencil + gouache house style; no living-artist names with
  figures in frame.
- Wellness-checkup framing — no needles / syringes / blood (keeps the
  moderation surface safe; two-figure scene should pass cleanly).
- No recursive screens; jeffrey's chartreuse butterfly Neo is his
  alone; USB-stick vape as a varying prop; diegetic light only.
- Memory cap — 8 GB machine: image gens run concurrency=1.
