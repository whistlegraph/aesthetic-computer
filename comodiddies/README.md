# comodiddies/

comedy × commodity × diddy × play. sci-fi products that don't exist
but should — each one shipped with a completely straight face: a
typeset product sheet, a sung-or-spoken story video, an AC piece, a
campaign. the joke is that nothing is a joke.

a separate marketing pipeline that lifts off the original `/marketing`
stack, the same way `/pop` lifted the songs off it — `marketing/`
keeps the one-off campaigns and shared bins (`gen-promo.mjs` etc.);
a comodiddy is a *numbered product* with its own lane here.

## anatomy of a comodiddy

```
comodiddies/
  <slug>/                  # one lane per product
    product/               # the faux sheet: README (concept), sheet.tex → sheet.pdf
    gens/                  # campaign image gens (colored-pencil house style)
    story/                 # the story video: vo + build.mjs + chrome.mjs + motion/
    *-prompt.txt           # gen prompts
```

- **product sheet** — AC house style (YWFT Processing + Berkeley Mono +
  pals stamps, same language as `bills/invoices/`); build with
  `tectonic -X compile sheet.tex` from `product/`
- **story video** — VO-aligned cut (`story/build.mjs`), Seedance 2.0
  motion shots via the `pop/lib/motion-pipeline.mjs` harness
  (`story/gen-motion.mjs` driver), side-stamp chrome pass
- **AC piece** — `<slug>.mjs` in the disks dir links the product sheet
  from the prompt
- prose follows [`papers/VOICE.md`](../papers/VOICE.md) — lowercase,
  fragments, em-dashes; concept docs read like notebook pages, not
  spec sheets

## index

1. [twofa](twofa/product/README.md) — the 2FA Brush: an electric
   toothbrush that's also a hardware security key
   ([sheet](twofa/product/sheet.pdf), [story](twofa/story/))
