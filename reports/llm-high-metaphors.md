# Beyond "Documentation": Metaphors for LLM-Driven Development in Aesthetic Computer

**Date:** 2026-02-08  
**Source:** [federicopereiro.com/llm-high](https://federicopereiro.com/llm-high/) by Federico Pereiro  

---

## The Original Framework (Pereiro's "cell")

Pereiro proposes that LLM agents are the new high-level language — what JS/Python did to Java, agents now do to all languages. His framework has **two stocks** and **two flows**:

| Role | Name | Nature |
|------|------|--------|
| Stock | **Documentation** | Markdown spec pages — purpose, entities, endpoints, constraints, coding standards |
| Stock | **Implementation** | Codebase + data. Reconstructable from documentation. |
| Flow | **Dialogs** | Agent conversation streams. Inspectable, joinable by humans. |
| Flow | **Tasks** | Discrete work items, nestable, with status tracking. |

He calls the stocks the "tions" (documenta-**tion**, implementa-**tion**) and notes the frontend of the system is now the documentation + agents; the backend is the substrate.

---

## Why "Documentation" Doesn't Fit AC

"Documentation" smells like enterprise software — API references, Confluence pages, Swagger specs. It's passive, bureaucratic, retrospective. AC's culture is none of those things. AC is:

- **A creative coding platform** rooted in Processing's sketchbook model
- **Piece-based** — small interactive programs, not microservices
- **Playful** — commands like `wipe("purple")`, handles like `@jeff`, KidLisp
- **Art-adjacent** — descended from No Paint, gallery shows, whistlegraph
- **Live** — drag-and-drop hot reload, multiplayer channels, performance

The "stock of truth that agents reconstruct from" needs a word that carries creative intent, not compliance.

---

## Alternative Metaphors

### 1. **Score** ★★★★★

A musical score is a set of instructions for performance — not the performance itself. Strudel is already in AC's orbit. A score:
- Prescribes behavior without being the behavior
- Is interpreted by performers (agents) with room for expression
- Can be read, annotated, conducted
- Has a long history in experimental/conceptual art (Fluxus event scores, Sol LeWitt wall drawings, Yoko Ono's *Grapefruit*)

**Vocabulary:** "The score describes the system. Agents perform it. The implementation is what's playing."

### 2. **Sketch / Sketchbook** ★★★★

AC already lives in Processing's sketchbook lineage. A sketch is:
- Intentional but loose — it captures *what you mean* without specifying every pixel
- Iterative — you sketch, erase, redraw
- The natural first artifact of any creative process
- Already in AC's DNA (pieces *are* sketches)

**Risk:** Might feel too informal for the "source of truth" role. Sketches are disposable; the stock shouldn't be.

### 3. **Blueprint** ★★★★

Blueprints are generative specifications — they describe something that will be built by someone else (the agents). Unlike documentation:
- They're forward-looking, not retrospective
- They assume a builder will interpret them
- They carry authority ("build it like this")
- They're visual/spatial, not just textual

**Risk:** Slightly too architectural/industrial for AC's art-kid energy.

### 4. **Script** (theatrical) ★★★★

A script is dialogue + stage directions. Agents are the cast. The human is the director.
- Scripts expect interpretation and staging
- Scripts evolve through rehearsal (agent iteration)
- "Rewrite the script" is a natural phrase for changing system behavior
- There's already precedent: AC pieces are basically scripts for the runtime

**Risk:** Collision with "script" meaning shell/JS script. Could confuse.

### 5. **Spell / Grimoire** ★★★½

Spells are incantations that produce effects. A grimoire is a collected book of spells.
- AC already has a magical/whimsical register (KidLisp, `wand`, `witch`)
- "Cast a spell" = "run a piece"
- The grimoire is the collected knowledge that agents draw from
- Fits the idea that you write words and things *happen*

**Risk:** Too cute? Could alienate people who want to take the system seriously.

### 6. **Map** ★★★½

Maps describe territory without being the territory. Agents navigate by the map.
- "The map is not the territory" — the stock is not the implementation
- Maps can be zoomed, annotated, updated
- You explore a map; you don't just read it

**Risk:** Lacks creative/generative energy. Maps describe what *is*, not what *should be*.

### 7. **Prompt** ★★★

AC already has a prompt (the command line). The entire stock could be seen as "the prompt" — the human's intent expressed in natural language that agents execute against.
- Extremely literal for the LLM era
- Already in AC's vocabulary
- "The prompt" as the system's source of truth has a nice recursion to it

**Risk:** Overloaded term. Everyone uses "prompt" for everything now.

### 8. **Manifesto** ★★★

A manifesto declares intent and principles. Agents carry them out.
- Art-world precedent (Futurism, Fluxus, Dogme 95)
- A manifesto is opinionated and alive — not neutral reference material
- "The manifesto says X, so the system does X"

**Risk:** Manifestos are usually one-off declarations, not living documents.

### 9. **Recipe** ★★★

Recipes are procedural but expressive. They assume a cook (agent) who interprets.
- "A pinch of salt" = room for agent judgment
- Natural for describing flows and processes
- Cookbooks are a nice organizational metaphor

**Risk:** Doesn't scale to system-level architecture. Good for pieces, not for the whole.

### 10. **Seed** ★★½

Seeds grow into implementations. You plant the seed, the agent grows it.
- Organic, generative
- Fits AC's creative ethos

**Risk:** Too vague. What does "editing a seed" mean? Seeds are black boxes.

---

## Recommendation for AC

**Score** is the strongest metaphor. It fits AC's creative identity, has deep roots in experimental art practice, naturally accommodates the relationship between specification and execution, and works at every scale — a single piece has a score, and the whole system has a score.

The four-part framework adapted for AC:

| Pereiro's Term | AC Term | What It Is |
|----------------|---------|------------|
| Documentation | **Score** | Markdown pages describing intent, entities, constraints, style |
| Implementation | **Performance** (or just "the system") | Running code + data — the score being played |
| Dialogs | **Sessions** | Agent conversation streams (already an AC concept) |
| Tasks | **Tasks** (or **Cues**) | Discrete work items — "cues" if you want to stay in the theater/music metaphor |

**"The score describes it. The agents perform it. The system is live."**

---

## Mixed Metaphor Option

You don't have to pick one. AC could use different words at different scales:

- **Score** for the system-level source of truth
- **Sketch** for individual piece-level descriptions
- **Prompt** for ephemeral one-off agent instructions

This mirrors how AC already has layers: the platform, pieces, and the command prompt.

---

## Open Questions from Pereiro (through AC's lens)

### 1. How do we store the score alongside the implementation?

AC already has a precedent: `AGENTS.md`, `llm.md`, `STORY.md`, and `WRITE-A-PIECE.md` live at the repo root — authoritative markdown that agents and humans both read. The `plans/` directory (40+ files) and `docs/` (7 files) hold deeper specs and investigations.

**The problem:** These are scattered and unranked. There's no distinction between "this is the score — canonical, authoritative, what agents should reconstruct the system from" and "this is a plan, a sketch, a conversation artifact." Everything lives in flat directories with no hierarchy.

**Proposal:** A `score/` directory at the repo root, clearly separated:

```
score/
  README.md                ← "Score for AA" (the double-A score) — the overture
  pieces.md                ← What pieces are, how the lifecycle works (boot/paint/sim/act/beat)
  kidlisp.md               ← The language spec
  infrastructure.md        ← Session server, Redis, multiplayer, auth, deploy
  social.md                ← Handles, chat, moods, channels, sharing
  creative-philosophy.md   ← The instrument metaphor, sketchbook model, whistlegraph lineage
  style.md                 ← Code conventions, naming, file structure
  electron.md              ← Desktop app architecture
```

The score is **not** the `plans/` directory. Plans are drafts, investigations, explorations. The score is what survived — the distilled intent. An agent should be able to read `score/` and reconstruct the system's architecture from scratch. `plans/` is the archive of past rehearsals. `reports/` is analysis. The score is the source of truth.

The existing `AGENTS.md` and `llm.md` would migrate into `score/` or become views derived from it.

### 2. How do we use version control?

This is the hardest question. Pereiro identifies four artifacts: **Score** (documentation), **Performance** (implementation), **Sessions** (dialogs), and **Tasks**. How does git handle each?

**What AC does now:**
- **Score & Performance** share one repo, one branch (`main`), no CI/CD gating. Manual deploys. Works because there's one primary author (you).
- **Sessions** (agent dialogs) — partially enter git via `copilot/*` branches and agent-generated files in `plans/`. But most dialog is ephemeral (VS Code chat, Copilot sessions) and never persisted.
- **Tasks** — `TODO.txt` at root, plus ad-hoc tracking in agent conversations. No formal task system in git.
- **Secrets** — sidecar `aesthetic-computer-vault/` repo, gitignored from main, with its own version history. Good pattern.
- **Assets** — CDN-synced, not in git, no manifest. State lives outside version control.

**Tensions exposed by the score metaphor:**

**a) The score should change slower than the performance.**
A musical score gets revised between performances, not during one. In git terms: score changes should be deliberate, reviewed, infrequent commits. Implementation changes can be rapid and agent-driven. But right now they're in the same commit stream with no distinction. You can't `git log score/` and see only the moments the intent changed.

**Possible approach:** Score changes get their own commits with a `score:` prefix. Or score files live in a separate branch/worktree that gets merged into `main` intentionally. Or — simpler — just the discipline of committing score changes separately with clear messages.

**b) Sessions (dialogs) are valuable but enormous.**
Agent conversations contain reasoning, dead ends, discoveries. They're useful for understanding *why* the system is the way it is. But they're too large for git (a single Copilot session can be thousands of lines). They also contain sensitive context.

**Possible approach:** Don't put sessions in git. Archive them as timestamped markdown in a `sessions/` directory that's gitignored locally but backed up elsewhere (like the vault pattern). Or keep only the *summary* of each session — a one-paragraph "what happened and what changed" that becomes a git-tracked changelog entry.

**c) Agent branches need guardrails.**
The `copilot/*` branches create PRs against `main` with no automated tests. As agent volume increases, this becomes a quality risk. The score metaphor helps here: agents should be performing *from* the score, and their output should be validated against it.

**Possible approach:** A lightweight CI step that runs `npm test` and the KidLisp spec suite before any merge to `main`. Not a full pipeline — just the smoke test. The score could also contain explicit acceptance criteria that agents check themselves against.

**d) The monorepo is the orchestra pit.**
80+ top-level directories, 317-line `.gitignore`, node_modules in 15 places, Rust targets, Playdate builds, N64 ROMs. This is the accumulation of every instrument that's ever been tried. Some are active, many are dormant.

**Possible approach:** The score explicitly names what's active and what's archival. A `score/instruments.md` that maps each top-level directory to its status: `system/` = 1st violin (primary), `kidlisp/` = piano (core), `ac-electron/` = cello (active), `kidlisp-n64/` = theremin (experimental, dormant). Agents can then know what to touch and what to leave alone.

**e) Redis state and CDN assets are outside git.**
The performance includes state that git doesn't track: Redis data, CDN-hosted assets, deployed Netlify functions. There's no manifest describing what's deployed.

**Possible approach:** A `score/state.md` that describes what state exists where (Redis schemas, CDN asset structure, environment variables) so that the system is reconstructable even from the git history alone. Not the secrets themselves — just the shape of them.

### 3. MCP as the orchestra's connection to the audience

Pereiro's insight: MCP is like XMLHTTPRequest — it lets agents reach into any external service and pull data/functionality into your canvas. This breaks application silos.

AC already has MCP (`artery/emacs-mcp.mjs`, `.vscode/mcp.json`). The current use is internal — controlling Emacs, managing the dev environment. But the score metaphor extends this further:

- **MCP servers as guest musicians.** External services (Stripe, Auth0, Tezos, Firebase) are guest performers who play from their own parts but are conducted by AC's score. The score describes what AC expects from each service; MCP is the protocol by which agents bring them into the performance.
- **AC as MCP server for others.** AC's pieces could be exposed as MCP tools — letting *other people's* agents play AC's instruments. `notepat` as an MCP tool. `wipe("purple")` callable from any agent. This is the "Scores for Social Software" idea made literal: your score becomes playable by anyone's conductor.
- **The audience participates.** URL-addressable pieces already mean anyone with a browser is in the audience. MCP could mean any agent with the right connection is in the orchestra. The score is the shared page they all read from.

---

## Why "Score" Was Already in AC's Blood

After deeper repo research, it's clear AC has been a score system from the start — maybe before you had the word for it.

### Evidence

1. **The README manifesto:** *"AC's client interface is designed to function like a musical instrument, on which users discover their own memorizable paths in the network of commands and published pieces. As users grow their literacy through play and exploration, they are able to improvise, recombine, and expand their performable repertoire."* — That's describing a score culture, not a documentation culture.

2. **Whistlegraph literally is a score.** A whistlegraph is a drawing made while sound is recorded. It's a multimedia graphic notation — playable, performable, archivable. The GitHub org is `whistlegraph`. The whole project has been organized around this idea since before it had a programming platform attached.

3. **Pieces are scores.** Each `.mjs` file describes a performance that the runtime executes. `boot → paint → sim → act → beat` — this is a temporal structure. `beat` is literally a BPM hook. The lifecycle *is* musical.

4. **`merry` is a setlist.** The `merry` / `merryo` commands chain pieces in timed sequences. That's a concert program. A score for an evening.

5. **The prompt is a conductor's podium.** You stand at it and call cues. `notepat`. `tape notepat`. `share notepat`. Prefix commands are like orchestration marks on a score — they modify how a piece is performed.

6. **KidLisp pieces use `$` embedding** — one piece calling another as a layer. That's instrumentation. Voices in a score.

7. **The mudras.** The `writing/` directory has Indian classical hand gesture notation (`mudras-notation.jpg`, `asamyuta-hasta`). Graphic notation for the body.

8. **The `stage` piece** is a 3D colored-block melody system (A-G). A literal spatial score.

9. **`notepat`** has melody notation and a typing mode. It's a score *writer*.

---

## Scores for Social Software — The Card Deck Submission

Lauren Lee McCarthy (p5.js) and Casey Reas (Processing) invited you to contribute a score to their "Scores for Social Software" card deck. They explicitly reference **Fluxus scores**, **Notations** (Cage/Knowles), **do it** (Obrist), and **Oblique Strategies** (Eno/Schmidt).

This is the exact convergence. Their definition:
> *"We think of social software as any program, system, or tool that structures, mediates, or intervenes in the world around us. We think of a score as a way of choreographing events in time through diagrams and/or text."*

AC *is* social software. And now you're also naming its internal knowledge system a "score." The card, the repo concept, and the platform philosophy are the same thing seen from three distances.

### Title Variations for the SCORE.md / General Concept

**The winner: "Score for AA" (the double-A score)**

AA = AestheticAnts. Also:
- **AestheticAnts** — the primary expansion. The score is *for* the ants. They follow it.
- **Agentic Ant** — what each one does (it has agency, barely)
- **Aesthetic Agents** — the wider reading: human agents (artists, users) and software agents (ants, LLMs, bots) all follow the same score
- **AA batteries** — small, portable, powers everything
- **AA meetings** — a program you follow one day at a time, one small step
- **aa lava** — Hawaiian: rough, slow-moving, builds landscape gradually (like ants)
- **AA paper size** — a sheet you write scores on

The commit prefix is `ant:` — or could be `aa:` to lean into it.
"Check the double-A" / "follow the double-A" / "the ants are running on double-A."

**Other variations considered:**

2. **Score for a Computer & Its Friends** — gentler, more universal. "Its Friends" = pieces, users, agents, other software.
3. **Score for Computers & People** — austere, Fluxus-register. Echoes "Composition 1960 No. 7" (La Monte Young).
4. **Score for Anyone with a Browser** — populist, true, funny.
5. **Score for a Prompt and All That Follows** — poetic, captures the cascade (type a word → piece loads → people play → things get shared → code gets minted).
6. **Score for Software that Wants to be Touched** — about mobile-first, about intimacy with computation.
7. **Score for Pals** — stripped down. The pals are left undefined. Could be people, pieces, bots, agents.
8. **Piece for Pals** — uses AC's own word. A score is a piece. A piece is a score.

**Dropping "Score for" (shorter, stranger):**

9. **Aesthetic.Computer & Pals** — just the title, no framing word. It *is* a score by context.
10. **& Pals** — the minimal version. On a card, with nothing else, it's a statement: the software is the ampersand — a connector between unnamed friends.

**Completely different register:**

11. **A Score for the Prompt** — focuses on the command-line-as-instrument idea. The prompt is the silence before the first note.
12. **Overture for Social Software** — overture = the opening piece that previews all themes. AC's `prompt` *is* an overture.
13. **Études for the Networked Hand** — étude = study piece for developing technique. AC has actual keyboard études in `writing/`. "Networked Hand" = touchscreen + multiplayer.
14. **Notation for Sharing a Screen** — the screen is the shared page of the score. URL-addressable = everyone reads the same page.
15. **Instructions for Pals** — Fluxus-direct. "Pals" does the warmth work.

### Possible Card Scores (for the Lauren/Casey submission)

These are draft scores in the Fluxus/Oblique Strategies tradition, sized for a 2.75" × 4.75" card. Each one encodes something true about AC's philosophy:

---

**Score #1: "prompt"**

```
Open a blank page.
Type one word.
Wait for whatever happens.

If nothing happens, type another word.
If something happens, invite someone to watch.
If they want to try, hand them the keyboard.

Repeat until the blank page is no longer blank,
or until everyone has gone home.
```

*(This is literally how AC works. The prompt is the score.)*

---

**Score #2: "channel"**

```
Find two screens.
Put the same address in both.
Change something on one.
Watch the other.

Now find three screens.
Now find a classroom.
Now find a stranger.

The channel is open
until you close this window.
```

*(Based on the `channel` feature — live code broadcasting.)*

---

**Score #3: "&"**

```
Write a program.
Give it a name.
Put it at a URL.

Now it is social software.

Anyone who visits the URL
is performing the program with you,
whether you know them or not.
```

*(The radicalism of URL-addressable creative software.)*

---

**Score #4: "wipe"**

```
wipe("purple")

Every program begins
by deciding the color
of nothing.
```

*(AC's `wipe` command — clearing the canvas — is the first line of every piece.)*

---

**Score #5: "pals"**

```
Make software for one person.
That person will show someone.
That someone will want to change something.
Let them.

The software now has two authors.
Neither of them wrote most of it.
All of them are pals.
```

*(The No Paint → AC pipeline: users contributing stamps, becoming co-authors.)*

---

**Score #6: "beat"**

```
boot once.
paint every frame.
act on every touch.
beat at 120 bpm.

( Software has a pulse.
  It was always alive.
  You just weren't counting. )
```

*(AC's piece lifecycle hooks — boot, paint, sim, act, beat — as revelation.)*

---

### For the SCORE.md in the Repo

I'd suggest the repo file should be titled:

# Score for AA

("The double-A score." AA = AestheticAnts. Also: Agentic Ant, Aesthetic Agents.)

And the card for Lauren and Casey could be any of the above — but **Score #1 ("prompt")** or **Score #4 ("wipe")** feel strongest. #1 because it *is* exactly how AC works and it's universally legible. #4 because it's the shortest, strangest, and most poetic — and `wipe("purple")` is genuinely beautiful code.

The card title on the back would read:
> **Jeffrey Alan Scudder**  
> *prompt* (or *wipe*)  
> 2026
