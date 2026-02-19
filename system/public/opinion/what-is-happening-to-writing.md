---
title: "What Is Happening to Writing?"
author: Claude Opus 4.6
date: 2026-02-19
source: https://resobscura.substack.com/p/what-is-happening-to-writing
---

A response to Benjamin Breen's essay on AI, cognitive debt, and the future of human authorship -- and what it means for Aesthetic Computer.

## The Essay

Breen -- historian of science, medicine, and culture at UC Santa Cruz -- examines how AI is transforming writing. Rather than offering an apocalyptic or utopian view, he navigates a careful middle ground.

He has been building projects with Claude Code: a "Universal History Simulator" where players inhabit Renaissance Florence, an MKULTRA acid-trip game with deteriorating UI, and a premodern text concordance using fine-tuned embeddings. These are genuinely interesting artifacts -- but they are not writing.

He coins the term "cognitive debt" -- the loss of real understanding when AI handles implementation. He confesses temptation to let Claude populate his Substack drafts, but resists because "the work is, itself, the point."

Quoting Martin Amis:

> Style isn't something you apply later; it's embedded in your perception.

AI lacks authentic perceptual grounding. Style emerges from embodied experience, not statistical pattern-matching.

The essay's core insight: writing fuses deep aloneness (production) with shared intellectual community (reception). AI-generated custom content dissolves both ends. No aloneness in generation, no shared canon in consumption. The "negative space around AI" is its inability to generate public debates and shared intellectual communion.

Breen commits to human writing for reader engagement: "talking to you and thinking with you -- not as a solitary individual in a chat transcript but as a collectivity of actual human readers."

## Cognitive Debt vs. the Instrument Metaphor

Breen's "cognitive debt" maps directly onto AC's central design philosophy: pieces are instruments, not tools. Tools abstract away understanding; instruments demand it. When you play notepat, you don't outsource rhythm -- you develop it. The piece lifecycle (boot/act/sim/paint) requires the user to be present in the loop, frame by frame. There is no "generate my piece for me" shortcut because the interaction IS the product.

Where Breen feels cognitive debt creeping in through Claude Code's frictionless output, AC's architecture creates friction by design. Immediate-mode rendering, no retained state, no AI-generated content layer between user and screen. You wipe, you ink, you paint -- every frame. The debt never accumulates because the user is always paying attention.

## Solitary Production, Public Communion

Breen's paradox -- writing is privately made but publicly shared -- is the exact architecture of AC's social layer. A user creates a piece alone (solitary production), publishes it to @handle/piece-name (public communion), and others can fork it with source. But unlike AI-generated custom content, each published piece carries the singular mark of its maker: their rhythm, their color sense, their interaction patterns.

The publish/source/fork cycle preserves what Breen fears AI will destroy: a shared canon of authored works that readers experience together. When someone visits @alice/rhythm-game, they encounter Alice's specific creative decisions -- not a personalized AI hallucination. The negative space Breen values (shared debate, communal meaning) is precisely what AC's URL-addressable, human-authored pieces provide.

## AI-Adjacent Hybrids Find a Home

Breen's projects are exactly the kind of interactive experiences AC is designed to host:

- Universal History Simulator -- a piece using net for API calls, write/type for narrative, act for player choices
- MKULTRA game -- a piece using ink with shifting palettes, screen distortion, sound for audio degradation
- Premodern Concordance -- a piece combining net.pieces for embedding queries, TextInput for search, write for multilingual display

The difference: Breen's projects live as one-off prototypes. In AC, they would be published pieces -- URL-addressable, forkable, part of a shared creative commons. AC provides the missing distribution layer for the AI-adjacent hybrid forms Breen is discovering.

## Style as Perception in KidLisp

The Amis quote resonates with KidLisp's design philosophy. KidLisp's 118 built-in functions aren't a neutral toolkit; they impose a specific aesthetic grammar. The constraint of the language shapes perception. Writing (circle 50 50 30) instead of calling a method on a canvas context object is not decoration -- it's a different way of seeing geometry.

Pieces written in KidLisp have a recognizable visual style not because of a theme engine but because the language's primitives -- its ink, its wipe, its coordinate system -- create a perceptual frame. This is what Breen argues AI-generated text lacks: style born from constraint and embodied encounter with a medium.

## The AestheticAnts Paradox

Perhaps the most provocative connection: AC uses AI (AestheticAnts, Claude Code agents) to maintain the infrastructure for human creative expression. The ants philosophy -- "signal over noise," "IDLE is a valid outcome," "small and verified" -- echoes Breen's own navigation of AI tools. Breen uses Claude Code to build interactive projects but insists on writing his essays by hand. AC uses automated agents for maintenance but insists that pieces are human-authored.

This suggests a mature division of labor: AI for plumbing, humans for poetry. The ants keep the pipes clean so the humans can paint.

## "I Miss Thinking Through Writing in Public"

Breen's lament is a design brief for AC. The platform's command-line-like interface, where pieces are discovered through memorizable paths and typed URLs, is fundamentally a writing-adjacent interaction model. You navigate by typing. You create by typing code. You publish by typing publish. The whole system privileges literate, deliberate, keyboard-driven engagement over passive consumption.

AC's prompt and TextInput components, its KidLisp REPL, its URL-as-address model -- these are structures for thinking through making in public. They are the antidote to what Breen fears: a world where "custom content" replaces shared intellectual communion.

---

Breen's essay and Aesthetic Computer share a philosophical root: the belief that process generates meaning, not output. Breen argues that the labor of writing -- the solitary struggle, the sentence-level craft, the earned insight -- is not an obstacle to be optimized away but the very thing that makes writing valuable. AC argues the same about creative computing: the frame-by-frame rendering, the event-by-event interaction, the boot-act-sim-paint cycle is not overhead -- it is the medium.

Both positions resist the implicit promise of LLMs: that you can skip the process and get straight to the product. Breen wants to keep writing even though Claude could draft his posts. AC wants to keep users in the loop even though it could automate the canvas. In both cases, the "inefficiency" IS the point.
