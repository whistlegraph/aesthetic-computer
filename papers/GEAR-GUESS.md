# Gear Guess

A gear guess is a dated, sourced procurement hypothesis. It says what we would
buy now, why, what could change the answer, and how the object enters the
studio. It is firmer than a research note and more honest than a buyer's guide.
Prices move. Ports lie by omission. The guess keeps both facts visible.

## Shape

Source lives in `papers/gear-guess-<slug>/<slug>.tex` and loads
`papers/ac-paper-gear-guess.sty`.

Every gear guess contains these moves, in this order:

1. **Best guess** — one concrete recommendation, including capacity/config.
2. **Price rail** — observed or budgeted prices, an as-of date, and a walk-away
   threshold. Estimated prices must say that they are estimates.
3. **Alternatives** — the next two credible choices and the condition under
   which either becomes the better answer.
4. **Compatibility catches** — ports, protocols, filesystems, thermals,
   warranties, and platform-specific limits that can erase the headline spec.
5. **Day-one score** — a checkable setup and operating sequence. Buying the
   object is not the end of the paper.
6. **Source shelf** — direct URLs, publisher/vendor names, and the date checked.
7. **Canonical object plate** — product photography from the manufacturer's
   own page or press kit, linked back to that page. Keep the exact image-asset
   URL in `figures/product-images.md`; do not silently substitute retailer
   thumbnails, generated renders, or an image of a neighboring SKU.
8. **Buy link layer** — images and product names always resolve to canonical
   manufacturer pages; `BUY NOW` resolves through the instance's
   `buy-links.tex`. This keeps image provenance separate from commerce.

The title block labels the artifact `GEAR GUESS`, not “review” or “buyer's
guide.” A guess is allowed to expire. Put the date on page one and say what
would change it.

## Posture

- Prefer one opinionated answer over a cloud of technically acceptable ones.
- Separate measured speed, manufacturer claims, and studio estimates.
- Name the weak link in the system, not just the fastest component.
- Treat current prices as observations, never timeless facts.
- Link primary product/spec pages. Add an independent measurement when a
  manufacturer's performance claim matters to the decision.
- Show the objects. Canonical manufacturer images are part of the evidence,
  not decoration; captions identify them as manufacturer images.
- Make purchase links explicit. Affiliate links set `\gearaffiliatetrue`, which
  adds `(paid link)` beside each button and the required page-level disclosure.
  Never add a tracking tag that does not belong to the publisher.
- End with a use score: labels, mounts, folders, verification, and backup.

## Reusable TeX surface

```tex
\documentclass[10pt,letterpaper]{article}
\usepackage{../ac-paper-gear-guess}

\gearsetshorttitle{short running title}
\hypersetup{pdftitle={Title — a gear guess}}

\begin{document}
\gearGuessTitle
  {title}
  {the one-line job this gear has to do}
  {22 July 2026}
  {guess 1}

\begin{bestguess}
The concrete answer and the sentence that makes it legible.
\end{bestguess}

\gearproduct
  {figures/product.png}
  {Exact product name}
  {preferred}
  {\$000 observed}
  {https://manufacturer.example/product}
  {\buyExactProduct}

\section{buy this}
\begin{gearcard}{component}
\gearprice{Exact model}{\$000}
Why it belongs in the system.
\end{gearcard}

\begin{gearwarning}
The catch that could change the answer.
\end{gearwarning}

\gearsource{Source name}{checked 22 July 2026}{https://example.com}
\end{document}
```

The format is single-column US Letter, print-friendly, cream-backed, and built
with XeLaTeX through the paper stack. Shared colors, Processing display faces,
links, headers, cards, warnings, prices, checkboxes, and source rows live in the
style file; each instance owns only its decision and evidence.
