# Score for Aesthetic.Computer & Pals

As implemented in this monorepo through the direction of [@jeffrey](https://prompt.ac/@jeffrey).

<img width="200" src="https://assets.aesthetic.computer/aesthetic-inc/pals.png">

---

## Front Door

<!-- stats:start -->
351 built-in pieces (333 JS + 18 KidLisp), ~78 API endpoints.<br>
2796 registered handles, 265 user-published pieces, 4386 paintings, 14677 KidLisp programs, 18010 chat messages, 20 prints ordered.<br>
*Last refreshed: Feb 27, 2026*
<!-- stats:end -->

Visit https://aesthetic.computer — press the top left of the screen or type any key to activate the prompt.

Enter names of built-in pieces like `notepat`, `boyfriend`, or `list` for a scrollable index. User-published pieces live at handles like `@bash/hub`.

Every piece is URL addressable (e.g. https://aesthetic.computer/notepat). Generate QR codes with `share notepat`.

**Getting started:**
1. Enter `imnew` to register
2. Verify your email
3. Set a @handle via `handle your-name`
4. Enter `chat` to say hi

**Recipes:** See [USER-GUIDE.md](USER-GUIDE.md) for making paintings, playing melodies, and joining the community.

**Links:**
- **GitHub**: https://github.com/whistlegraph/aesthetic-computer
- **No Paint (predecessor)**: https://nopaint.art ([HN 2020](https://news.ycombinator.com/item?id=23546706))
- **Notepat on HN**: https://news.ycombinator.com/item?id=41526754

---

## Back Door

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

## Resources

- [The AC Story](STORY.md) — Technical history and evolution
- [Write a Piece](WRITE-A-PIECE.md) — Create your own AC program
- [KidLisp Docs](kidlisp/) — Language reference
- [User Guide](USER-GUIDE.md) — How to use AC as a player

---

## Ant Guidance

The ant-specific mindset and rules now live in [`ants/mindset-and-rules.md`](ants/mindset-and-rules.md).

---

## Embodiments

Different agents perform from this score in different ways.

- **AestheticAnts** — Automated AI colony that makes small, confident changes. See `ants/` for colony rules and implementation.
- **Human contributors** — Welcome in `chat`. Read the score, pick a task, follow signal.
- **@jeffrey (the queen)** — Writes and maintains this score.
