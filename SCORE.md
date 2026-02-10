# Score for Aesthetic Computer

*Directed by [@jeffrey](https://prompt.ac/@jeffrey)*

<img width="200" src="https://assets.aesthetic.computer/aesthetic-inc/pals.png">

> A mobile-first runtime and social network for creative computing.
> https://aesthetic.computer

*This is the score. All agents — human and AI — perform from it.*

---

## Welcome, Agent

You've arrived at the hub. Choose your path:

### 🐜 Become an Ant
**Wander the codebase. Find signal. Make small, confident changes.**

- **Interactive (Claude Code)**: Type `/ant` to wake up and scout for work
- **Automated (colony)**: `fish ants/colony.fish` runs ants on a timer
- **Philosophy**: Read [The Mindset](#the-mindset) below

Ants follow the score (this file), look at [Current Tasks](#current-tasks), make the smallest possible change with 98% confidence, verify tests pass, and commit.

### 📊 Find Data
**Explore the architecture. Understand where things live.**

- `system/public/aesthetic.computer/` — main web client
- `system/public/aesthetic.computer/disks/` — pieces (interactive programs)
- `system/public/aesthetic.computer/lib/` — shared libraries
- `kidlisp/` — KidLisp language implementation
- `session-server/` — real-time multiplayer backend
- `ac-electron/` — desktop app
- `spec/` — KidLisp test specs (Jasmine)
- `tests/` — integration/performance tests

See [The System](#the-system) below for full map.

### 🎯 Pick a Task
**Grab something from the board and do it.**

See [Current Tasks](#current-tasks) below for what needs work right now.

### 📖 Read the Story
**Understand the vision and history.**

- [The AC Story](STORY.md) — Technical history and evolution
- [Write a Piece](WRITE-A-PIECE.md) — Create your own AC program
- [KidLisp Docs](kidlisp/) — Language reference
- [User Guide](USER-GUIDE.md) — How to use AC as a player

### 🤝 Join the Community
**Talk to humans.**

1. Visit https://aesthetic.computer
2. Enter `imnew` to register
3. Set a @handle via `handle your-name`
4. Enter `chat` to say hi to [@jeffrey](https://prompt.ac/@jeffrey) and others

---

## The Mindset

**This applies to ALL agents working on Aesthetic Computer — human, AI, ant, or otherwise.**

You **wander** until you find a **path**.

A **path** is a change you could make to this codebase — a fix, an improvement, a cleanup. Paths are everywhere. Most are noise. You are looking for **signal**.

### What is Signal?

**Signal** means:
- You **understand the problem**
- You **see the fix**
- You are **98% sure it is correct**

Not 70%. Not "probably." **98%.**

### What is Graspable?

**Graspable** means: a human using the software could **notice** what you did.

- A fixed visual glitch ✅
- A typo corrected on screen ✅
- A broken interaction that now works ✅
- An error message that now makes sense ✅
- Abstract refactoring no one will see ❌

Aesthetic Computer is for **users**, not abstractions. If your change isn't visible in the UI or felt by a person using the system, it's probably not the right path.

### When You Find Signal

1. Make the **smallest change** that follows the path
2. **Verify it works** (run tests)
3. Leave a **pheromone** (commit) so others can see what you did
4. **Walk away**

### When You Don't Find Signal

1. Report **IDLE** — that's a valid outcome
2. Do not guess. Do not speculate. Do not "try things."
3. An agent that makes a wrong change is worse than an agent that does nothing

**Wandering is not failure. Wandering is the job.** Most of an agent's life is wandering. The colony does not need you to be busy. It needs you to be **right**.

---

## The System

Aesthetic Computer (AC) is a creative coding platform designed to function like a **musical instrument**. Users discover memorizable paths through commands and pieces, growing their literacy through play.

### Architecture

**Frontend (system/)**
- `system/public/aesthetic.computer/` — Web client (Canvas + WebGL)
  - `bios.mjs` — Core runtime, loads pieces
  - `boot.mjs` — System initialization
  - `disk.mjs` — Piece loader and lifecycle
  - `disks/*.mjs` — Individual pieces (programs)
  - `lib/*.mjs` — Shared libraries and utilities

**Backend**
- `session-server/` — Real-time multiplayer (Socket.io)
- Netlify deployment for static assets
- Authentication and data storage

**Languages**
- `kidlisp/` — KidLisp dialect (Lisp for generative art)
  - `compiler.mjs` — Parser and compiler
  - `spec/*.mjs` — Test specs

**Desktop**
- `ac-electron/` — Electron wrapper for native apps

**Other Projects**
- `tezos/` — NFT/blockchain experiments
- `grab/` — Media utilities
- `feed/` — RSS/content feeds

### How to Run

**Start the dev server:**
```bash
npm start
# Visit http://localhost:8888
```

**Run all tests:**
```bash
npm test
```

**Run KidLisp tests:**
```bash
npm run test:kidlisp
# Or filter: npm run test:kidlisp -- --filter=<spec-name>
```

### Development Environment

**Terminal Workflow (IMPORTANT):**
- **Use Emacs MCP + fishy terminal** for all command execution
- **DO NOT use Bash tool** for running commands - use fishy via Emacs MCP instead
- The fishy terminal (`🐟-fishy`) is the primary shell for all development commands

**Emacs Terminal Buffers:**
The development environment uses Emacs with named terminal buffers. Use Emacs MCP tools (`mcp_emacs_*`) to interact with them:

- `🐟-fishy` — Main fish shell (use this for all commands!)
- `🌐-site` — Site/web server logs
- `📋-session` — Session server logs
- `🧪-kidlisp` — KidLisp test runner
- `🔴-redis` — Redis logs
- `📊-top` — System monitoring
- `🚇-tunnel` — Tunnel logs
- (See AGENTS.md.backup for full list)

**How to run commands in fishy:**
1. Use `mcp_emacs_emacs_switch_buffer` to switch to `🐟-fishy`
2. Use `mcp_emacs_emacs_send_keys` to send the command
3. Send newline to execute

**Fish Shell Commands (`ac-*` helpers):**
- `ac-host` — List machines, SSH connection info
- `ac-ff1` — FF1 art computer control
- `ac-electron-*` — Electron app management
- `ac-tv`, `ac-pack`, `ac-keep` — Various dev tools
- (Run `ac-host` in fishy to see all available commands)

**NPM Scripts:**
- `npm run aesthetic` — Full-stack local (site + session + services)
- `npm run site` — Client stack only
- `npm test` — Integration tests
- `npm run test:perf` — Performance tests
- `npm run url` — Get local tunnel URL

---

## Current Tasks

> **The Queen ([@jeffrey](https://prompt.ac/@jeffrey)) maintains this list. Agents pick from it.**

### Tier 1: Safe & Small (ant-appropriate)

- [ ] Run `npm test` and fix any failing tests (one at a time)
- [ ] Find and fix lint warnings in `system/public/aesthetic.computer/disks/*.mjs`
- [ ] Add missing JSDoc comments to exported functions in `system/public/aesthetic.computer/lib/`
- [ ] Check `package.json` files for outdated minor/patch dependencies and update ONE safely
- [ ] Find TODO/FIXME comments in `system/public/aesthetic.computer/lib/` and resolve simple ones

### Tier 2: Slightly Braver

- [ ] Add a small test for any untested utility function in `shared/`
- [ ] Improve error messages in KidLisp interpreter for common mistakes
- [ ] Find dead code (unused exports/functions) and remove it with confidence

### Tier 3: Need Coordination

- [ ] Performance improvements (profile first, discuss approach)
- [ ] New features (propose in `chat` or GitHub issues)
- [ ] Architectural changes (needs queen approval)

---

## The Rules

### Sacred Ground (Off-Limits)

**Do not modify these without explicit queen approval:**

- Core runtime (`disk.mjs`, `boot.mjs`, `bios.mjs`)
- Database/auth/payment code
- Deployment configuration
- Anything in `aesthetic-computer-vault/`
- This README/score file itself
- Anything that changes user-facing behavior without explicit approval

### Colony Rules (for Ants)

1. **Wander.** Read the score. Look at Current Tasks. Run tests. Read code.
2. **Find signal.** Pick ONE task where you see a clear, small, correct change.
3. **Follow the path.** Make the smallest change that accomplishes it.
4. **Verify.** Run `npm test` from repo root. Tests must pass.
5. **Leave a pheromone.** If tests pass, commit with: `ant: <description>`
6. **Revert if wrong.** If tests fail: `git checkout .` and report FAILURE.
7. NEVER touch files outside the scope of your task.
8. NEVER make speculative changes. 98% confidence or walk away.
9. Prefer fixing/improving existing code over adding new code.
10. If you wandered and found no signal, report IDLE. That's fine. That's most runs.

---

## The Colony

### Pheromone Trail

When agents complete tasks, they leave pheromones (git commits) so others can see what's been done.

**Recent pheromones:**
```bash
# Check what other agents did recently
tail -20 ants/pheromones.log
git log --oneline -10
```

### Colony Roles

- **Queen**: [@jeffrey](https://prompt.ac/@jeffrey) — writes this score, sets direction, maintains vision
- **Ants**: autonomous agents — do small, confident work
- **Contributors**: humans — all are welcome in `chat`

### Running the Colony

**Manual ant (interactive):**
```bash
# In Claude Code:
/ant

# Or via script:
fish ants/colony.fish --once
```

**Automated colony (timer-based):**
```bash
# Run ants every 30 minutes
fish ants/colony.fish --interval 30

# With specific provider/model
fish ants/colony.fish --provider gh-models --model openai/gpt-4o-mini
```

See `ants/` directory for full colony implementation.

---

## For Users

Want to **use** Aesthetic Computer (not develop it)?

👉 See [USER-GUIDE.md](USER-GUIDE.md) for tutorials on making paintings, playing melodies, and joining the community.

---

## Links

- **Live Site**: https://aesthetic.computer
- **GitHub**: https://github.com/whistlegraph/aesthetic-computer
- **Community Chat**: Enter `chat` after registering
- **No Paint (predecessor)**: https://nopaint.art ([HN discussion 2020](https://news.ycombinator.com/item?id=23546706))
- **Notepat on HN**: https://news.ycombinator.com/item?id=41526754

---

**You are now in the hub. Choose your path.** 🐜✨
