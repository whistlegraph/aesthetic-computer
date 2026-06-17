# Drawing Is Computing: KidLisp as an Interactive Medium for Graphical Linear Algebra

**A position paper and collaboration proposal**
Jeffrey (prompt.ac / @jeffrey) · Aesthetic Computer · 2026-06-11

---

## Abstract

Graphical Linear Algebra (GLA) reframes linear algebra as a diagrammatic
language: linear maps and relations become *string diagrams* — generators
wired together, composed in sequence and in parallel — and proofs become
visual rewrites. Its pedagogy and its research artifacts, however, are
overwhelmingly *static*: PDFs, slides, chalk. This paper argues that
Aesthetic Computer (AC) and its embedded language KidLisp are a natural
*interactive* substrate for GLA. I sketch three concrete bridges — a
diagram renderer, an executable term language for props, and an animated
signal-flow playground — note honestly where the tree/graph mismatch
makes the problem interesting rather than trivial, and close with a
proposal for collaboration with the GLA / Applied Category Theory (ACT)
community.

---

## 1. What Graphical Linear Algebra is

GLA is the research program of Filippo Bonchi, Paweł Sobociński, and
Fabio Zanasi (with Robin Piedeleu and others), popularised through
Sobociński's long-running blog *graphicallinearalgebra.net*. Its core
move is to stop writing linear algebra in matrices and symbols and start
*drawing* it.

A diagram is built from a tiny set of **generators** — addition, the
constant zero, copy, discard, and multiplication-by-a-scalar — drawn as
boxes with wires. Two composition operators glue them:

- **sequential** (`;`, `∘`): connect the outputs of one diagram to the
  inputs of the next, left to right;
- **parallel** (`⊗`): stack two diagrams side by side.

The algebra of these diagrams is captured by the symmetric monoidal
theory of **Interacting Hopf Algebras (IHA)**, a complete presentation of
the *prop of linear relations* — relations that are also linear
subspaces. "Complete" is the punchline: two diagrams denote the same
linear relation **if and only if** one can be rewritten into the other by
the IHA equations. You no longer *calculate*; you *redraw*. A headline
application is **signal flow graphs** — the dataflow diagrams engineers
already use for linear dynamical systems and circuits — which GLA gives a
fully compositional semantics.

The key conceptual inversion: in GLA the picture is not an *illustration*
of the mathematics. The picture **is** the mathematics.

## 2. What Aesthetic Computer and KidLisp are

Aesthetic Computer is a mobile-first runtime and social network for
creative computing: a musical-instrument-like interface where users
discover memorisable paths and run published "pieces" — small interactive
programs addressed by URL. **KidLisp** is its embedded Lisp dialect for
generative art: S-expressions over ~118 graphics, timing, and math
primitives, hot-reloaded live in the browser, every piece shareable by a
short link.

Two properties of AC matter for what follows:

1. **It is live and shareable by default.** A piece is a URL. There is no
   build step between an idea and a running, linkable artifact.
2. **It treats spatial/visual interfaces as carriers of real
   computational meaning** — the same bet GLA makes about diagrams,
   aimed at a different domain.

## 3. The bridges

### 3.1 A diagram renderer (the easy, immediately useful one)

A string diagram is a planar wiring of boxes: sequential composition runs
left-to-right, parallel composition stacks vertically. KidLisp already
draws boxes, lines, and text. A renderer that lays out GLA generators and
their wires is a short piece — the companion artifact `gla.lisp` in this
repository is a first, hand-placed instance: two inputs feeding an
**add** node, whose sum feeds a **copy** node that fans out to two
outputs, with animated dots showing the signal propagate. It runs in the
browser today.

The value here is pedagogical and immediate: GLA's teaching materials are
static; an embeddable, linkable, *animated* diagram is not.

### 3.2 An executable term language for props (the interesting one)

The naive pitch — "Lisp is already a diagram language" — undersells the
problem, and saying so honestly is the whole point.

**Lisp S-expressions are trees. String diagrams are graphs.** A tree has
exactly one root and no sharing; an expression `(f (g x) (h x))` *names*
`x` twice but the term structure is still a tree with variables doing the
sharing. GLA's power lives precisely in the structure trees cannot
express directly:

- **copy / fan-out** — one wire becoming two — is sharing made
  first-class, *without* a variable to name it;
- **discard** — throwing a wire away — is structural weakening;
- **feedback loops** — a signal flow graph's defining feature — are
  cycles, which a term tree simply does not have.

So "render Lisp as diagrams" is not the task. The real task is to design a
**textual syntax for a prop** — a term language whose denotation is a
morphism in a symmetric monoidal category, with explicit `copy`,
`discard`, sequential `∘`, and parallel `⊗` combinators, plus a trace /
feedback operator. This is a known and respected problem (the *internal
language of monoidal categories*; related to the algebra of
Frobenius/Hopf structures and to hypergraph categories). A KidLisp-flavored
surface syntax that **evaluates** to a concrete linear relation — and
renders the corresponding diagram as it goes — would be a genuine
contribution: an executable, inspectable bridge between the textual and
the diagrammatic, with a live runtime attached.

Concretely, a generator set might read:

```lisp
(seq                      ; sequential composition  ∘
  (par in-A in-B)         ; parallel composition    ⊗
  add                     ; ●  two wires in, one out
  copy                    ; △  one wire in, two out
  (par out-1 out-2))
```

where evaluation produces both the matrix/relation it denotes *and* the
drawn diagram — two views of one object, exactly GLA's thesis made
interactive.

### 3.3 A signal-flow playground (the demonstrable one)

Signal flow graphs are GLA's flagship application and are *intrinsically
animated*: values stream down wires, scalars amplify, feedback
accumulates. AC is built for exactly this — real-time, frame-driven,
interactive. A piece where you wire generators on a canvas and **watch the
linear system run** — adjust a scalar and see the trajectory change — is
something the ACT community largely does not have. It is the most
compelling demo of all three because it shows the diagram *computing*,
not just sitting.

## 4. Why this is worth a paper, a tool, or a thesis

The honest framing of the opportunity:

- **For pedagogy:** GLA wants to be approachable — that is why the blog
  exists. Interactive, linkable diagrams lower the entry cost further than
  any PDF can.
- **For research:** §3.2 is a real open-flavored problem at the
  intersection of programming-language design and category theory — a
  term language and live evaluator for props/linear relations.
- **For the field's culture:** ACT prizes *working* compositional tools.
  A browser-native, shareable GLA playground is a contribution in a form
  the community values and currently lacks.

A fair caveat, stated plainly: a PhD in these groups is **formal category
theory** — props, monoidal categories, completeness proofs, papers. The
tool-building is an excellent entry credential and complement, not a
substitute for the abstract mathematics the degree demands. The right
posture is to lead with the artifact and learn the formalism, not the
reverse.

## 5. The people and the path

The GLA authors are the centre of gravity of Applied Category Theory:

- **Paweł Sobociński** — Professor, *TalTech* (Tallinn, Estonia); leads
  the Laboratory for Compositional Systems and Methods; supervises PhD
  students and coordinates a doctoral programme. Author of the GLA blog
  and an outreach-minded researcher. **The most natural first contact.**
- **Fabio Zanasi** — Professor, *UCL* (London); co-author of the
  foundational GLA papers.
- **Filippo Bonchi** — *University of Pisa*; the third co-author.

Two timely venues, both chaired by Sobociński in Tallinn:

- **ACT 2026** (Applied Category Theory), 6–10 July 2026;
- **DIAGRAMS 2026**, 24–28 August 2026 — arguably the better fit for an
  interactive-diagrams contribution.

Estonian doctoral positions are **salaried**, sidestepping the
tuition-debt model of the US/UK.

### Proposed first move

Not "may I do a PhD," but: **ship the smallest working demo and point at
it.** A KidLisp piece that renders and animates a GLA string diagram (now
in hand) plus a short note to Sobociński — "here is what I build; I'd like
to build it for category theory; DIAGRAMS 2026?" — tests the
collaboration, funding, and thesis paths simultaneously, on the strength
of a running artifact rather than a cold ask. Practitioners in this field
notice tools.

## 6. Kindred voices: Richard Southwell and the diagrammatic turn

The case for an *interactive* diagrammatic mathematics is not mine alone.
In his 2026 talk *Speculations about the future of mathematics*, the
category-theory educator **Richard Southwell** — author of *Categories
and Toposes: Visualized and Explained* — argues that mathematics is
shifting "from this sort of algebraic, encoded style of maths to
something more like" drawing diagrams. His framing arrives, independently,
at the exact metaphor underpinning this paper and my whistlegraph
practice:

> "I liken [algebra] to **sheet music** — as opposed to [the diagram],
> which is the **actual music**. This is what our visual cortex can
> process." — Richard Southwell

His worked example *is* the companion piece `gla.lisp`: "a number flows
in from the left; the black node **copies** it; the white node **adds**
— put a 3 in, get a 3 and a 3, add them, get 6." He closes with a plea
that is, almost verbatim, the mission of Aesthetic Computer: that people
who understand graphical methods should "spend more time trying to make
it easier for the rest of us." This is the audience for an AC-native GLA
playground, and Southwell — an independent educator actively seeking
collaborators who make the subject visual — is plausibly a lower-friction
first contact than a cold email to a department.

**A calibration, stated honestly.** Southwell's talk pairs GLA with a
second, far more speculative program: Norman Wildberger's *box
arithmetic*, which takes multisets (drawn as nested boxes, with
"anti-boxes" for negatives) as a finitist foundation that rejects
infinities and the real numbers. The two are **not** the same tier. GLA
is rigorous, peer-reviewed, and complete (the IHA theorem); box
arithmetic is heterodox, ultrafinitist, and — by Southwell's own
admission — "the wild west," not even proven consistent. For a serious
collaboration or thesis pitch, build on GLA. Box arithmetic is better
treated as provocation and art-fuel. Notably, its nested-box structure is
*operadic* — rooted **trees** — which places it on the opposite side of
the same tree-vs-graph divide drawn in §3.2: GLA is the graph side, box
arithmetic the tree side, and AC's score practice straddles both.

Full transcript of the talk is archived alongside this study as
`studies/southwell-future-of-math-transcript.md`.

## 7. Status and next steps

- [x] `gla.lisp` — animated signal-flow string diagram, live in AC.
- [ ] Generalise from hand-placed coordinates to a small layout pass
      (a list of generators → auto-wired diagram).
- [ ] Prototype the §3.2 term language: `seq` / `par` / `add` / `copy` /
      `discard` / `scale` evaluating to both a relation and a drawing.
- [ ] Add interaction: drag generators, edit scalars, watch the signal
      flow respond (§3.3).
- [ ] Draft a one-page note + demo links for Sobociński; target
      DIAGRAMS 2026.

## References

- Bonchi, Sobociński, Zanasi — *Interacting Hopf Algebras* / *A
  Categorical Semantics of Signal Flow Graphs*.
- Sobociński — *Graphical Linear Algebra*, graphicallinearalgebra.net.
- Bonchi, Piedeleu, Sobociński, Zanasi — *Graphical Affine Algebra* /
  *Graphical Resource Algebra*.
- Sobociński — *Laboratory for Compositional Systems and Methods*,
  TalTech (compose.ee/pawel).
- Richard Southwell — *Speculations about the future of mathematics*
  (2026), youtube.com/watch?v=l619jN5wkJA; *Categories and Toposes:
  Visualized and Explained*.
- Norman Wildberger — *box arithmetic* lectures (multiset foundations;
  ultrafinitist).
