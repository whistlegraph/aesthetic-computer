---
title: "KidLisp '26: A Minimal Lisp for Generative Art on a Social Platform"
tags:
  - JavaScript
  - Lisp
  - generative art
  - creative coding
  - domain-specific language
authors:
  - name: @jeffrey
    orcid: 0009-0007-4460-4913
    affiliation: 1
affiliations:
  - name: Independent Researcher / Aesthetic Computer
    index: 1
    ror: null
date: 3 March 2026
bibliography: paper.bib
header-includes:
  - |
    ```{=latex}
    \usepackage{listings}
    \definecolor{klfn}{RGB}{0,136,170}
    \definecolor{klform}{RGB}{119,51,170}
    \definecolor{klrepeat}{RGB}{170,0,170}
    \definecolor{klnum}{RGB}{204,0,102}
    \definecolor{klstr}{RGB}{170,120,0}
    \definecolor{klcmt}{RGB}{102,102,102}
    \definecolor{klmath}{RGB}{0,136,0}
    \definecolor{klvar}{RGB}{204,102,0}
    \definecolor{klembed}{RGB}{0,136,0}
    \definecolor{klgray}{RGB}{119,119,119}
    \newcommand{\kn}[1]{\textcolor{klnum}{#1}}
    \newcommand{\kt}[1]{\textcolor{klstr}{#1}}
    \newcommand{\kv}[1]{\textcolor{klvar}{#1}}
    \newcommand{\ke}[1]{\textcolor{klembed}{\textbf{#1}}}
    \newcommand{\km}[1]{\textcolor{klmath}{#1}}
    \lstdefinelanguage{kidlisp}{
      morekeywords=[1]{wipe,ink,line,box,circle,tri,plot,flood,shape,zoom,scroll,spin,blur,contrast,embed,layer,width,height,frame,time,wiggle,melody,mic,suck,sort,form,trans,cube,move,scale,hop,overtone,resolution,write,text,type,stamp,paste,point,poly,noise,fade},
      morekeywords=[2]{def,let,if,cond,once,later,lambda,do},
      morekeywords=[3]{repeat},
      morekeywords=[4]{random,sin,cos,tan,floor,ceil,round,abs,sqrt,min,max},
      sensitive=true,
      morecomment=[l]{;},
      morestring=[b]",
      escapeinside={|}{|},
    }
    \lstset{
      language=kidlisp,
      basicstyle=\ttfamily\small,
      keywordstyle=[1]\color{klfn}\bfseries,
      keywordstyle=[2]\color{klform}\bfseries,
      keywordstyle=[3]\color{klrepeat}\bfseries,
      keywordstyle=[4]\color{klmath},
      commentstyle=\color{klcmt}\itshape,
      stringstyle=\color{klstr},
      breaklines=true,
      frame=single,
      rulecolor=\color{klgray!30},
      backgroundcolor=\color{klgray!5},
      xleftmargin=0.5em,
      xrightmargin=0.5em,
      aboveskip=0.5em,
      belowskip=0.5em,
    }
    ```
---

# Summary

KidLisp is a minimal Lisp dialect for generative art and interactive audio-visual
experiences, running inside Aesthetic Computer (AC), a browser-based creative
computing platform. Programs are stored in a shared database, assigned short
alphanumeric codes (e.g., `$cow`, `$27z`), and are instantly executable at a URL.
A tree-walking evaluator (~15,000 lines of JavaScript) provides 118 built-in
functions spanning drawing, color, transformation, math, animation, audio, and
input --- with no file I/O, networking, or general string manipulation --- making
it safe to execute arbitrary user-submitted code in the browser. As of March 2026,
59 authors have created over 16,000 programs on the platform. The source code is
available at `https://github.com/digitpain/aesthetic-computer` under the MIT
license.

# Statement of need

Creative coding environments like Processing [@reas2007processing], p5.js
[@mccarthy2015p5js], Scratch [@resnick2009scratch], and Sonic Pi
[@aaron2016sonic] have significantly lowered the barrier to computational
expression. However, they still require familiarity with general-purpose language
syntax (Java, JavaScript, Ruby, or block-based visual grammar) and operate as
standalone tools --- users must install software, manage files, and share work
through external channels.

KidLisp targets the remaining gap: users who want to produce visual and sonic
output with zero boilerplate, share it instantly, and build on each other's work
through direct composition. A complete animated program can be a single line:

```{=latex}
\begin{lstlisting}
(ink |\kt{rainbow}|) (repeat |\kn{100}| |\kv{i}| (circle (wiggle width) (wiggle height) |\kn{10}|))
\end{lstlisting}
```

Programs are shareable by pasting a short code into the AC prompt or visiting a
URL. Embedding other programs is a first-class operation --- `($cow)` renders the
program stored under code `cow` as a composable layer, enabling collaborative
creative work without copying source code.

The choice of Lisp syntax --- often perceived as a barrier to entry
[@mccarthy1960recursive] --- is deliberate. KidLisp's parenthesized expressions
map directly to visual operations: `(circle 50 50 20)` draws a circle. There are
no imports, no semicolons, no variable declarations required. The syntax *is* the
interface. This inverts the common framing of Lisp as an expert's language and
positions it as a minimal notation for visual thinking. \autoref{fig:mondrian}
shows a complete program alongside its output.

![A KidLisp program rendering a Mondrian-style composition using six lines of code. The `box` function draws filled rectangles; `ink` sets the active color; `stroke` and `outline` switch to border-only rendering. Programs like this are stored and shared via short codes on the platform.\label{fig:mondrian}](figures/card-mondrian.png){ width=45% }

# State of the field

Among browser-native creative coding tools, Hydra [@hydra2019] is closest in
spirit --- live-coded visuals in a browser --- but uses JavaScript method chaining
and lacks persistent storage or social features. Strudel [@roos2023strudel]
provides pattern-based live coding for music and visuals but targets experienced
live-coders rather than first-time programmers. Racket's `2htdp/image` library
[@racket2018] uses S-expressions for educational graphics but targets computer
science pedagogy, not generative art. Fluxus [@fluxus2007] used Scheme for
live-coded 3D visuals but is no longer maintained.

KidLisp is distinguished by three properties in combination: (1) Lisp syntax
mapped to a canvas runtime with 118 graphics, audio, and math primitives; (2)
persistent social storage with short-code addressing and cross-program embedding;
and (3) a deliberately constrained function set designed for generative art safety
--- no arbitrary code execution, no system access, no imports. No existing
creative coding DSL or live-coding environment has been published as peer-reviewed
software in JOSS or a comparable venue, making this submission the first in its
category.

# Software design

## Evaluator

The evaluator (`kidlisp.mjs`) parses S-expressions into an AST and walks it,
dispatching to built-in functions that map to HTML Canvas 2D, Web Audio API, and
WebGL operations. Key design decisions:

- **Flat programs**: No user-defined functions beyond `def`/`let`. Abstraction is
  achieved by embedding other programs rather than defining procedures --- a
  social abstraction mechanism where reuse means referencing another author's
  published work.
- **Deterministic rendering**: Given the same random seed and frame count, output
  is identical, enabling reproducible generative art.
- **Temporal syntax**: Temporal operators schedule expressions without explicit
  loops: `2.5s (wipe red)` executes after 2.5 seconds; `1s... (spin 5)` repeats
  every second; `3s! (write "Done")` fires exactly once at 3 seconds.
- **Chaos mode**: Invalid or random input triggers artistic visual output rather
  than error messages. A confidence-scored detector examines word recognition
  rate, special character ratio, and parenthesis balance to decide whether input
  is code or chaos.

## Storage and distribution

Each program is stored with a short code (generated via `nanoid`), a SHA-256
content hash for deduplication, usage counters, and an optional creator ID linked
to the platform's handle system. Short codes are generated by a source-aware
algorithm that first attempts to derive a meaningful code from program content
(e.g., extracting dominant function names or color keywords), falling back to
random generation if all inferred codes are taken. A single REST endpoint supports
five query modes: store, retrieve, batch retrieve (for resolving embedded layers),
paginated feeds, and corpus-wide function usage analytics. Programs can optionally
be syndicated to ATProto (Bluesky) on creation.

## Performance

The evaluator implements multi-level caching (RAM, IndexedDB, network) for
embedded program resolution, buffer pooling (up to 8 reusable offscreen buffers),
and an auto-density system that scales pixel density between 0.5x and 4x to
maintain 30--55 FPS on varying hardware.

## Development environment

The KidLisp IDE at `kidlisp.com` provides a Monaco-based editor with syntax
highlighting that mirrors the color scheme of this paper's code listings, a live
preview pane, and a "slide mode" for dragging numeric literals to adjust values in
real time. A stage mode hides all chrome for live performance contexts.

# Example: Composition through embedding

A key differentiating feature is the composition graph. The program below embeds
two other user-created programs as layers:

```{=latex}
\begin{lstlisting}
(wipe |\kt{black}|)
(ink |\kt{rainbow}| |\kn{64}|)
(repeat |\kn{200}| |\kv{i}| (circle (wiggle width) (wiggle height) (wiggle |\kn{20}|)))
(|\ke{\$27z}|)   ; embed another author's piece as a layer
(|\ke{\$cow}|)   ; embed a second piece on top
\end{lstlisting}
```

`$27z` and `$cow` are resolved at runtime, rendered into offscreen buffers, and
composited onto the canvas. This enables collaborative layering --- users build on
each other's work without forking or copying source code. The resulting dependency
graph is queryable through the storage API. \autoref{fig:gallery} shows additional
examples spanning math, turtle graphics, and generative patterns.

![Four KidLisp programs from the platform corpus, each rendered as a card with source code below the output. From left to right: a parametric spiral using `plot` with trigonometric functions; a recursive fractal tree using turtle graphics; a golden-angle sunflower pattern; and the Mondrian composition from \autoref{fig:mondrian}. All programs are shareable via short codes.\label{fig:gallery}](figures/card-gallery.png){ width=95% }

# Research impact statement

The corpus of 16,000+ programs with hit counters, timestamps, and author
attribution constitutes a dataset for studying creative coding practices at
scale. The `?stats=functions` API endpoint provides function usage analytics
weighted by program popularity, enabling quantitative analysis of which primitives
users gravitate toward. The embedding graph --- which programs reference which
others --- offers a social network of creative influence amenable to graph
analysis.

KidLisp programs have been minted as NFTs on the Tezos blockchain through the
platform's Keeps system, creating a verifiable record of creative output with
on-chain provenance. The platform has been used in educational and live
performance settings.

The evaluator's chaos mode --- which produces deterministic visual output from
arbitrary text input --- represents an unusual approach to error handling in
creative environments that may interest programming language researchers studying
how error boundaries shape creative exploration.

The 118-function API is documented in the project's `kidlisp/README.md` and
`kidlisp/COMPLETE_API_MAP.md`. The test suite (`spec/kidlisp-spec.js`) validates
parsing, evaluation, timing syntax, and URL encoding. Tests can be run via
`npm run test:kidlisp`. Installation and development instructions are provided in
the repository README.

# AI usage disclosure

The KidLisp evaluator was developed with assistance from large language models
(Claude, GPT, Gemini) for code generation, refactoring, and debugging. This paper
was drafted with LLM assistance. All code and prose were reviewed, edited, and
validated by the human author, who made all design decisions.

# Acknowledgements

Written and produced by @jeffrey. Thanks to the 59 KidLisp authors on
Aesthetic Computer for creating the corpus and providing feedback on language
design.

# References
