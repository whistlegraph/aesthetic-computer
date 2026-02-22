# Score for Aesthetic.Computer & Pals

*Directed by [@jeffrey](https://prompt.ac/@jeffrey)*

<img width="200" src="https://assets.aesthetic.computer/aesthetic-inc/pals.png">

> Implemented on aesthetic.computer and its various properties of this monorepo.

*This is the score. All agents — human and AI — perform from it.*

---

## The Mindset

**This applies to ALL agents working on Aesthetic Computer — human, AI, or otherwise.**

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

- A fixed visual glitch
- A typo corrected on screen
- A broken interaction that now works
- An error message that now makes sense
- Abstract refactoring no one will see — not graspable

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

#### Emacs & Development Environment
- `ac-aesthetic` — Connect to aesthetic emacs UI (alias for `aesthetic-now`)
- `ac-emacs-restart` — Kill and restart emacs daemon
- `ac-emacs-full-restart` — Restart emacs and reconnect UI
- `ac-emacs-kill` — Kill emacs daemon
- `ac-emacs-status` — Check emacs daemon health
- `ac-emacs-logs` — View emacs logs
- `ac-emacs-health-check` — Verify emacs config loaded correctly
- `ac-restart` — Restart all AC tabs/processes (calls emacs `ac-restart`)
- `ac-crash-diary` — View emacs crash log
- `ac-emacs-crash-monitor` — Background process that monitors emacs

#### Core Development
- `ac-artery` — Start artery development server
- `ac-artery-dev` — Start artery in dev mode
- `ac-site` — Start site server
- `ac-session` — Start session server
- `ac-url` — Get local tunnel URL
- `ac-views` — View stats
- `ac-watch` — Watch and rebuild (alias for `npm run watch`)
- `ac-repl` — Start REPL

#### KidLisp Tools
- `ac-st` — KidLisp source tree viewer (`ac-st cow`, `ac-st $cow`, `ac-st cow --source`)

#### Testing & Debugging
- `ac-test-tabs` — Test tab functionality
- `ac-diagnose` — Run diagnostics
- `ac-profile-start` — Start performance profiling
- `ac-profile-stop` — Stop performance profiling
- `ac-profile-report` — Generate profile report
- `ac-watch-cpu` — Monitor CPU usage
- `ac-dev-log` — View development logs
- `ac-dev-logs` — View all dev logs
- `ac-dev-log-clean` — Clean old logs
- `ac-dev-log-new` — Create new log

#### Deployment & Distribution
- `ac-pack` — Package for distribution
- `ac-unpack` — Unpack distribution
- `ac-ship` — Deploy/ship changes
- `ac-keep` — Save state/backup
- `ac-keeps` — List saved states
- `ac-keep-test` — Test keep functionality

#### Media & Recording
- `ac-tv` — TV mode
- `ac-record` — Start recording
- `ac-pix` — Image utilities
- `ac-media` — Media server

#### Services & Infrastructure
- `ac-servers` — Start all servers
- `ac-tunnel` — Start tunnel
- `ac-chat-system` — Start chat system
- `ac-chat-sotce` — Start sotce chat
- `ac-chat-clock` — Start clock chat
- `ac-stripe-print` — Stripe print service
- `ac-stripe-ticket` — Stripe ticket service
- `ac-logger` — View netlify function logs
- `ac-oven` — Oven service
- `ac-offline` — Offline mode

#### Authentication & Tokens
- `ac-login` — Login to AC
- `ac-token` — Manage auth tokens

#### Other Tools
- `ac-host` — List machines, SSH connection info
- `ac-cdp-tunnel` — CDP tunnel
- `ac-cdp-status` — CDP status
- `ac-extension` — Build VSCode extension

**Quick Start:**
```fish
ac-aesthetic          # Connect to development UI
ac-emacs-full-restart # Restart everything
ac-restart            # Restart AC services only
```

**NPM Scripts:**
- `npm run aesthetic` — Full-stack local (site + session + services)
- `npm run site` — Client stack only
- `npm test` — Integration tests
- `npm run test:perf` — Performance tests
- `npm run url` — Get local tunnel URL

**Notation:**
- compush — commit, push

---

## Current Tasks

> **The Queen ([@jeffrey](https://prompt.ac/@jeffrey)) maintains this list. Agents pick from it.**

### Opener Storage (automatic — @jeffrey's sessions)

When @jeffrey is working, the first prompt of each session is stored to the `openers` MongoDB collection automatically via `ants/store-opener.mjs`. Post-commit hooks also log commit messages there. No ant action needed — this is infrastructure, not a task.

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

---

## For Users

Want to **use** Aesthetic Computer (not develop it)?

See [USER-GUIDE.md](USER-GUIDE.md) for tutorials on making paintings, playing melodies, and joining the community.

---

## Links

- **Live Site**: https://aesthetic.computer
- **GitHub**: https://github.com/whistlegraph/aesthetic-computer
- **Community Chat**: Enter `chat` after registering
- **No Paint (predecessor)**: https://nopaint.art ([HN discussion 2020](https://news.ycombinator.com/item?id=23546706))
- **Notepat on HN**: https://news.ycombinator.com/item?id=41526754

---

## Embodiments

Different agents perform from this score in different ways.

- **AestheticAnts** — Automated AI colony that makes small, confident changes. See `ants/` for colony rules and implementation.
- **Human contributors** — Welcome in `chat`. Read the mindset, pick a task, follow signal.
- **@jeffrey (the queen)** — Writes and maintains this score.

---

## Resources

- [The AC Story](STORY.md) — Technical history and evolution
- [Write a Piece](WRITE-A-PIECE.md) — Create your own AC program
- [KidLisp Docs](kidlisp/) — Language reference
- [User Guide](USER-GUIDE.md) — How to use AC as a player

### Join the Community

1. Visit https://aesthetic.computer
2. Enter `imnew` to register
3. Set a @handle via `handle your-name`
4. Enter `chat` to say hi
