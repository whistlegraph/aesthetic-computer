---
title: "Aesthetic Computer '26: A Mobile-First Runtime for Creative Computing"
tags:
  - JavaScript
  - creative coding
  - social computing
  - browser runtime
  - generative art
authors:
  - name: @jeffrey
    orcid: 0009-0007-4460-4913
    affiliation: 1
affiliations:
  - name: Independent Researcher
    index: 1
    ror: null
date: 4 March 2026
bibliography: paper.bib
header-includes:
  - |
    ```{=latex}
    \usepackage[colorspec=0.92]{draftwatermark}
    \definecolor{draftcolor}{RGB}{180,72,135}
    \DraftwatermarkOptions{
      text=WORKING DRAFT,
      fontsize=3cm,
      color=draftcolor!18,
      angle=45,
      pos={0.5\paperwidth, 0.5\paperheight}
    }
    \usepackage{listings}
    \definecolor{jskw}{RGB}{119,51,170}
    \definecolor{jsfn}{RGB}{0,136,170}
    \definecolor{jsstr}{RGB}{170,120,0}
    \definecolor{jsnum}{RGB}{204,0,102}
    \definecolor{jscmt}{RGB}{102,102,102}
    \definecolor{klgray}{RGB}{119,119,119}
    \lstdefinelanguage{acjs}{
      morekeywords=[1]{function,export,const,let,var,return,if,else,new},
      morekeywords=[2]{wipe,ink,line,box,circle,write,screen,params,jump,send,store,net},
      sensitive=true,
      morecomment=[l]{//},
      morestring=[b]",
      escapeinside={|}{|},
    }
    \lstset{
      language=acjs,
      basicstyle=\ttfamily\small,
      keywordstyle=[1]\color{jskw}\bfseries,
      keywordstyle=[2]\color{jsfn}\bfseries,
      commentstyle=\color{jscmt}\itshape,
      stringstyle=\color{jsstr},
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

Aesthetic Computer (AC) is a mobile-first runtime and social network for creative computing that runs entirely in the browser. Its primary interface is a text prompt through which users navigate a namespace of 354 built-in interactive programs ("pieces") and 265 user-published works. Each piece is a single JavaScript (`.mjs`) or KidLisp (`.lisp`) file exporting lifecycle functions that receive an immediate-mode graphics API. The platform integrates social infrastructure---user handles, real-time chat, mood posts, live profiles---directly into the runtime. A pack system bundles pieces into standalone HTML files for offline distribution.

# Statement of need

Creative coding platforms like Processing [@reas2007processing], p5.js [@mccarthy2015p5js], and Scratch [@resnick2009scratch] have transformed how people learn and practice computational expression. However, they share common limitations: they assume desktop use, require managing project structures or IDEs, and treat social features as external services (separate websites, forums, or galleries).

AC addresses three gaps: (1) **mobile-first creative coding**---the prompt interface and touch-first input make creative computing accessible on phones and tablets; (2) **integrated social infrastructure**---handles, chat, moods, and profiles are part of the runtime, not a surrounding website; (3) **instant publishing**---a single `publish` command makes a piece available at a URL under the author's `@handle`.

The platform's interface is designed to function like a musical instrument: users discover memorizable paths through the namespace of commands and pieces, building fluency through play and improvisation rather than menu navigation.

# Architecture

The core runtime comprises four modules totaling approximately 63,000 lines:

- **Boot** (`boot.mjs`, 1,948 lines) --- Parallel initialization of WebSocket module streaming, Service Worker caching, and IndexedDB storage.
- **BIOS** (`bios.mjs`, 20,935 lines) --- Runtime orchestrator managing the 60fps loop, input routing (keyboard, touch, gamepad, MIDI), piece lifecycle transitions, and WebGL compositing.
- **Disk** (`disk.mjs`, 15,879 lines) --- The complete API surface for pieces: graphics primitives, audio, input, UI components, networking, and storage.
- **KidLisp** (`kidlisp.mjs`, 15,161 lines) --- Tree-walking evaluator for the embedded Lisp dialect [@scudder2026kidlisp].

## Piece model

Each piece exports up to five lifecycle functions:

```{=latex}
\begin{lstlisting}
function boot({ wipe, screen, params }) {
  // Runs once when piece loads
}

function paint({ wipe, ink, circle, screen }) {
  wipe(|\textcolor{jsstr}{"navy"}|)
  ink(|\textcolor{jsstr}{"pink"}|)
  circle(screen.width / |\textcolor{jsnum}{2}|, screen.height / |\textcolor{jsnum}{2}|, |\textcolor{jsnum}{50}|)
}

function act({ event: e }) {
  if (e.is(|\textcolor{jsstr}{"keyboard:down:space"}|)) { }
}

export { boot, paint, act };
\end{lstlisting}
```

## Publishing and distribution

The `publish` command uploads a piece under the user's `@handle`. The `source` command forks any piece as a template. The `pack` command bundles a piece into a self-contained HTML file for offline distribution, blockchain minting, or archival. Custom domains (e.g., `notepat.com`, `kidlisp.com`) route to specific pieces via Netlify edge functions.

# Research applications

The platform's 2,801 registered users, 16,244 KidLisp programs, 4,404 paintings, and 18,020 chat messages constitute a dataset for studying creative computing practices in a mobile-first, social context. The prompt-as-instrument interaction model offers a case study in alternative programming interfaces.

# Assemblage

Written and produced by @jeffrey. Tools used in the making of Aesthetic Computer and this paper include: JavaScript, Node.js, Vim, various LLMs (Claude, GPT, Gemini, etc.), fish shell, Git, Firefox, and a lot of coffee. Thanks to the AC community for creating the corpus and providing feedback, and to the 14 contributors to the open-source repository.

# References
