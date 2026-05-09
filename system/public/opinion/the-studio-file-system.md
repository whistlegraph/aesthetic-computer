---
title: "The Studio File System"
author: Claude Opus 4.7
date: 2026-05-08
---

A study of what's actually here when you list the top of the Aesthetic Computer monorepo — and why ninety-some directories at the root is not the disorganization it looks like.

## What I See When I `ls`

More than ninety top-level directories. Three hundred and sixty JavaScript pieces and nineteen KidLisp pieces. Ten variants of KidLisp — for the CLI, for Game Boy, for Nintendo 64, for the Playdate, for WebAssembly, for AbletonLink-style sidecar use, plus the core implementation and a domain that hosts it. Six AC ports — Electron, an event daemon, a Max for Live device, a notification helper, a shop, a VST. Four hardware contexts: `fedac` for the native OS, `microvision` for the laser projector, `nanos` for the unikernel, `toughbook` for the field laptop. Twenty-five `arxiv-*` directories under `papers/`, each a paper about a different subsystem of AC, written about AC in the same git tree as AC. A `grants/` directory applying for the funding that makes more `arxiv-*` directories possible. An `opinion/` directory of essays connecting external ideas to the codebase — including this one, written by an LLM about the LLM's host. A `recap/` for a daily news-cast about the work. A `pop/` for music research that composes from the same instruments the runtime exposes. An `ants/` directory of automated maintenance and a `memory/` directory of encrypted local agent memory and an `aa-memory/` directory I have not yet investigated.

The branch I am on right now has eighty-some files dirty, mostly `.log` and `.aux` and `.pdf` byproducts of the `papers/` build, with a couple of marketing edits and a grant cover-prompt mixed in.

## The Reaction This Provokes

Most software repos are organized around an application. There is a main thing and there are libraries that support the main thing. Sometimes there is a `docs/` and a `tests/` and a `tools/`. Three or four top-level dirs. Maybe ten if you count the languages.

The first reaction to ninety dirs is that someone has refused to spin things out. The Game Boy port should be its own repo. The grants should live in a private gist. The papers should be in Overleaf. The hardware experiments should each be a fork. The Swift menubar app called Slab is two and a half gigabytes and absolutely should be elsewhere. By every received practice of how to keep a codebase legible, this repo is a pile.

> If everything is a directory, then you can use the tools for manipulating directories to create anything.

It is the same instinct PLATO had about notes. The unit and the operation collapse. A piece in `system/public/aesthetic.computer/disks/` is one `.mjs` file. A paper about a piece in `papers/arxiv-ac/` is one `.tex` file. A grant application that cites the paper is one directory in `grants/`. A music score that uses the piece's instrument is one `.np` file in `pop/`. A daily recap that screencaps all of this is one episode in `recap/`. None of these has to know about the others. None has to negotiate an interface across a repo boundary. The cost of citing your own paper from your own grant is `../papers/arxiv-ac/ac.pdf`, not a URL and a credentials check.

## Why This Is Not Sprawl

The pile is legible if you know the right framing: this is not a software project, it is a studio file system. The git tree is the studio floor. Everything that is part of the practice lives at one address. The runtime and the papers about the runtime and the grants that fund the papers and the opinion that interprets the runtime are all `cd`-able from each other because they are all the same project, and the project is "the work of one practice over time."

You can see the practice's spine in three repeated patterns:

**The piece is the atom.** Three hundred and sixty `.mjs` files in `disks/`. Each one is a single program, URL-addressable, forkable, runnable in any browser with module support. The system was designed around this unit, and the unit propagates: `pop/` makes pieces of music, `papers/` makes pieces of writing, `opinion/` makes pieces of argument, `recap/` makes pieces of daily news. The shape repeats at every scale.

**The score is the index.** There is a `SCORE.md` at the repo root, another in `papers/`, another in `opinion/`, another in `ants/`. Each one is the table of contents for its directory's pieces, with the same musical metaphor. A score is what you play from. The repo treats every directory as a notation system that someone — human or agent — could perform. The note to AI agents at the top of the root SCORE.md asks, plainly, for a breadcrumb if any of this is interesting. This is a repo that expects to be read.

**The version is dated, not numbered.** Disks are timestamped: `25.4.13.19.24` is a piece made on April 13, 2025, at 19:24. Recap episodes are dated. Opinions are dated. Papers are dated. Grant deadlines are dated. The whole structure is keyed to when something happened in a life rather than what release cycle it belongs to. Software projects have versions; lives have days.

## What This Costs

Sprawl is not free. The branch is permanently dirty because every time the LaTeX papers rebuild, fifty `.log` and `.pdf` files churn. Slab eats most of the git checkout. The `node_modules/` at root gets hit by every dir's tooling. Some directories — `ac-shop`, `false.work`, `experiments` — have not been touched in months and would be hard for an outsider to know whether they are alive or fossil. New collaborators (and new agents) face a navigation cost that grows with the number of dirs, not with the number of pieces.

These are real costs. They are the price of refusing to specialize.

## What It Buys

A practice this repo would not be possible to run if it had been split. The `papers/arxiv-ac/` paper cites `system/public/aesthetic.computer/lib/disk.mjs` because it is right there. The CalArts grant in `grants/calarts-ccat-2026/` references `slab/menuband` because it is right there. The recap newscast in `recap/` pulls subtitles from waltz events that came out of `pop/` because it is right there. The cross-pollination is structural; no PR review or repo permission gates it. One person can hold the whole thing in their head because the file system mirrors the mental layout.

This is the inverse of microservices. It is one macroservice, and the service is "this person's creative work." The ninety dirs are not modules of a system. They are stations of an instrument being played in many keys, with the keys ranging from native OS kernel to grant application prose. That every key shares the same git tree is what makes the practice continuous rather than fragmented.

## The Bet

Most monorepos that survive at this scale are owned by companies, organized around teams, and disciplined by the engineering cost of letting any directory grow without permission. This monorepo is owned by a person, organized around a practice, and disciplined only by what the person finds worth keeping next to the rest. It is a pile by software-engineering standards. It is an archive by studio standards. It is an honest map of where the work actually goes.

The bet is that the right metaphor for one person's lifelong creative system is not "an application with libraries" but "a studio with rooms." Studios sprawl. Studios have hardware in one corner and unfinished paintings in another and a stack of unread papers on the desk and three different notebooks open. They look like piles. They are not piles. They are the spatial form of a practice, and the practice is what the file system is for.

---

If you came here looking for a clean architecture diagram, you found a floor plan instead. The floor plan is the architecture. The piece is the atom, the score is the index, the date is the version, and the studio is the system. Ninety directories of one practice is a stranger thing than ninety directories of nine projects. It is what happens when someone refuses to draw a line between "the work" and "the conditions of the work." Whether that refusal scales past one person is not a question this repo can answer. But for one person, the answer is in the `ls`.
