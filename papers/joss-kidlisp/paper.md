---
title: 'KidLisp: A Minimal Lisp for Generative Art'
tags:
  - JavaScript
  - Lisp
  - generative art
  - creative coding
  - domain-specific language
authors:
  - name: Jeffrey Alan Scudder
    orcid: 0009-0007-4460-4913
    affiliation: 1
affiliations:
  - name: Independent Researcher / Aesthetic Computer
    index: 1
    ror: null
date: 3 March 2026
bibliography: paper.bib
---

# Summary

KidLisp is a minimal Lisp dialect with 118 built-in functions designed for creating generative art and interactive experiences. It runs inside the Aesthetic Computer platform, a mobile-first runtime and social network for creative computing. KidLisp programs are shareable by short three-character codes and executable at a URL, enabling a social layer of creative expression built on a functional programming foundation.

The language provides primitives across 12 categories --- drawing, color, transformation, math, animation, text, audio, input, system, flow control, data structures, and composition --- while maintaining a minimal syntax accessible to non-programmers. Programs are written as S-expressions and evaluated in real-time within a browser-based canvas environment.

# Statement of need

Creative coding environments typically require users to learn a general-purpose programming language (JavaScript, Python, Java) before they can produce visual or sonic output. This creates a high barrier for artists, musicians, educators, and young learners who want to explore computational expression without first mastering software engineering concepts.

Existing creative coding languages like Processing, p5.js, and Sonic Pi have lowered this barrier significantly, but they remain syntactically complex for beginners and do not embed natively in a social platform. KidLisp addresses this gap by providing:

- **Minimal syntax**: S-expressions with no required boilerplate, imports, or class definitions
- **Immediate visual output**: Every valid expression produces visible results on a canvas
- **Social distribution**: Programs are stored in a database and shareable by short codes (e.g., `$cow`, `$27z`)
- **Embeddable composition**: Programs can embed other programs, enabling collaborative layering

KidLisp is used by 59 active authors who have collectively created over 16,174 programs on the Aesthetic Computer platform, demonstrating adoption outside of traditional programming communities.

# State of the field

KidLisp occupies a specific niche among creative coding tools. Processing [@reas2007processing] and p5.js [@mccarthy2015p5js] provide comprehensive creative coding environments but use Java and JavaScript syntax respectively. Sonic Pi [@aaron2016sonic] uses Ruby-like syntax focused on music. Hydra [@hydra2019] provides a similar live-coding canvas for visuals but uses JavaScript method chaining rather than Lisp syntax.

Among Lisp-family languages, Racket [@racket2018] offers the `2htdp/image` library for educational graphics, but targets a computer science pedagogy audience rather than generative art practitioners. Fluxus [@fluxus2007] used a Scheme-like language for live-coded 3D visuals but is no longer maintained.

KidLisp differentiates itself through the combination of Lisp syntax, a browser-native runtime with no installation, social sharing by short codes, and a design philosophy that prioritizes immediate visual feedback over computational generality. Its 118 functions are chosen specifically for generative art workflows --- the language has no file I/O, no networking, no string manipulation beyond display --- making it safe to run untrusted user programs in the browser.

# Software design

KidLisp's evaluator (`kidlisp.mjs`, approximately 50KB) is implemented as a single JavaScript ES module. The evaluation model is straightforward: parse S-expressions into a tree, walk the tree, and execute built-in functions that map directly to HTML Canvas 2D operations, Web Audio API calls, and mathematical computations.

Key design decisions:

- **No user-defined functions or variables beyond `let`**: This keeps programs flat and readable, encouraging composition through embedding rather than abstraction.
- **Deterministic rendering**: Given the same random seed and frame count, a KidLisp program always produces the same output, enabling reproducible generative art.
- **Embedding via `embed` and `layer`**: Programs reference other programs by their short codes, creating a composition graph. The `layer` function supports opacity and blend modes between embedded programs.
- **Animation via `frame` and `time`**: Built-in bindings provide the current frame count and elapsed time, enabling animation without explicit loop constructs.

The storage backend (`store-kidlisp.mjs`) provides a REST API for creating, retrieving, and listing programs. Each program is assigned a unique three-character alphanumeric code and stored in MongoDB.

# Research impact statement

KidLisp has been used as pedagogical infrastructure in creative computing courses and workshops. Its corpus of 16,174 user-created programs represents a dataset of creative coding practices that could support research in computational creativity, programming language usability, and creative computing education.

The platform's social features --- embedding, remixing, and sharing by short code --- provide a naturalistic setting for studying how non-expert programmers learn and build on each other's work, a research area of growing interest in computing education [@resnick2009scratch].

# AI usage disclosure

The KidLisp evaluator was primarily written by the author. Claude (Anthropic) was used as a development assistant for debugging, test writing, and documentation during development. This paper was drafted with AI assistance.

# Acknowledgements

KidLisp is part of the Aesthetic Computer platform. Thanks to the AC community for creating 16,174+ programs and providing feedback on language design.

# References
