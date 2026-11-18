# Digest

Opened 2026-11-18. The question that opened it: does coloring a pricing tier
change which plan people buy?

## 1. Color sets reading order; it has not been shown to sell

**Grade: strong for attention, none for purchase.**

- `orquin-2021-eye-movements-meta` pools 122 effects on where people look while
  choosing. Central position ρ = .43, surface size ρ = .35, visual salience
  ρ = .13. Salience is the smallest of the three, and the outcome is gaze.
- `milosavljevic-2012-saliency-bias` set salience by brightness, not hue. The
  bias appears when people decide fast, are distracted, or have weak preferences.
- `hunt-1995-von-restorff`: the isolation effect is about what is remembered
  from a list. Using it to predict what is bought is extrapolation.
- No peer-reviewed randomized test of a colored pricing tier with purchase as
  the outcome was found. `cxl-2016-highlighted-plan` is the nearest: a
  highlighted plan was fixated sooner and chosen more often, in hypothetical
  choices on one product.

## 2. Which hue: thin, and red contradicts itself

**Grade: weak.**

- `puccinelli-2013-red-prices` (red prices raise perceived savings, for men,
  under low involvement) and `ye-2020-red-price-red-flag` (one red price lowers
  perceived savings) point in opposite directions and share an author.
- `bazley-2021-visual-finance`: in eight experiments red on financial displays
  signals loss. The effect is absent in colorblind participants and muted in
  China. This is the one hue reading that held across studies.
- `labrecque-2012-exciting-red` is ratings of invented logos, not behavior.
- `steele-2014-mehta-zhu-replication` and `gnambs-2020-red-performance`: the
  best-known red-versus-blue result failed direct replication, and the pooled
  red-and-performance effect has no evidential value after bias correction.
- `hagtvedt-2017-saturation-size` and `nng-visual-hierarchy`: weight comes from
  saturation and lightness against the surroundings, not from the hue.

## 3. Labels and defaults carry the measured effects

**Grade: moderate to strong.**

- `cai-2009-observational-learning`: showing the five most popular dishes
  raised their demand 13–18% in a randomized field experiment; listing dishes
  without the popularity claim did less.
- `bookwala-2022-most-popular-label`: a "most popular" label raised selection
  about 13%; an expert's-choice label did not.
- `jachimowicz-2019-defaults-meta`: defaults, d = 0.68 over 58 studies,
  stronger when the default reads as an endorsement.
- Both label studies are on food. No software replication was found.

## 4. Decoys are weaker than their reputation

**Grade: strong null for decoys; moderate for compromise.**

- `frederick-2014-limits-of-attraction`: 19 studies with realistic stimuli, no
  decoy effect in any.
- `pinger-2016-compromise-menu`: becoming the middle option is worth about five
  points of share over 88,000 real choices, and nothing for people ordering
  alone.

## 5. The boundary

- `ftc-2022-dark-patterns` names "false hierarchy": contrasting visual
  prominence used to steer a selection. `cma-2022-online-choice-architecture`
  calls choice architecture neutral and harmful when it misleads.
- `luguri-2021-dark-patterns`: mild steering roughly doubled uptake of a paid
  plan without backlash. That is the reason to hold the line on truth, not a
  reason to cross it.
- A "most popular" badge is a factual claim. "Recommended" is the seller's
  advice.

## 6. Limits any palette must meet

- `w3c-2023-wcag-22`: text 4.5:1, interface marks 3:1, and color never the only
  signal.
- `birch-2012-color-deficiency`: about 8% of men of European descent. Red
  against green and green against amber collapse; indigo against amber holds.

## 7. What pricing pages do (observed 2026-11-18)

See [`pricing-pages.csv`](pricing-pages.csv). The common recipe is a neutral
page, identical cards, and one plan marked by a pill, a border, a tint or the
only filled button. A hue per plan is rare and appears on consumer pages.
`saasframe-2026-pricing-pages` and `lazyweb-2026-badge-prevalence` give
floor counts for text labels across larger samples; neither measures color.

tools:
- Mark one plan. Use fill, border and a text label together, so the mark
  survives without color.
- Put the effort into position, size and a true label before choosing a hue.
- Keep price numerals neutral. Keep red for danger.
- Check any tier palette at 4.5:1 in both themes and under red-green
  color-blindness simulation.
- Say "most popular" only when the sales data says so.
- When a count of pages matters, render the pages. Markup alone misses images,
  hover states and script-applied styles.
