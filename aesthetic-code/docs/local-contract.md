# Local contract

Aesthetic Code requires no Aesthetic Code server.

- No runtime account, telemetry, analytics, cloud sync, or hosted control plane.
- Configuration, session records, memory, and credentials remain on machines
  controlled by the user.
- Peer discovery and control use an explicitly configured LAN or private
  tailnet. They never create a public listener.
- Licenses are signed files verified offline. The application does not phone
  home to remain usable.
- Updates are user-initiated. An update check may be enabled separately without
  transmitting workspace or session data.

## Account and publishing boundary

Signing in talks to Auth0 (`hi.aesthetic.computer`) and `aesthetic.computer`
only when the user runs `/login`, and stores the result in `~/.ac-token`, the
shared Aesthetic Computer session file. Publishing sends one piece's source
and the session token to `aesthetic.computer` only when the user runs
`/publish`. The engine bridge never receives the token; publishing is an
interface action, not an agent tool.

Auto-publish (`--autopublish`, `AESTHETIC_CODE_AUTOPUBLISH=1`, `/autopublish`)
is the one way that becomes repeated rather than per-command: with it on, the
interface publishes the session's piece a couple of seconds after every save,
and the last save is flushed on the way out. It is off by default and has to be
turned on per session or per environment, because it writes to a public route
under the user's own handle. It changes when publishing happens, not who does
it — still the interface, still the same one file, and the token still never
reaches the bridge. The agent cannot turn it on, and the transcript keeps one
line naming the URL each publish went to.

## Live piece boundary

The session's piece is pushed to `aesthetic.computer/run` on a private code
channel every time its file changes. The session server retains the last
message a channel received, so a phone that scans the code later still receives
the piece without the interface re-announcing it. That request carries
the piece's source and the channel token, and nothing else: no account token, no
workspace paths, no conversation. The channel token is random per session and is
never reused.

Pushing is the interface's own action, on a file the user can see, and stops
when the session ends. It is the one thing Aesthetic Code sends without being
asked each time, so it is worth stating plainly: while the interface is open,
the piece on screen is repeatedly leaving the machine.

## Inference boundary

The terminal interface is always Aesthetic Code. Engines are internal bridges,
not alternate client interfaces or command shortcuts.

Two bridges exist, both remote: Claude Code in headless stream-json mode (the
default, on `claude-opus-5`) and Codex app-server. `--backend` and
`/backend` choose between them and `--model` and `/model` name the model. The
interface labels remote inference before a prompt is sent. Provider terms
govern that traffic, and each bridge signs in with its own vendor's existing
credentials on this machine; Aesthetic Code stores no key of its own.

Neither bridge inherits the user's own agent configuration. Codex is started
with `on-request` approvals and a `workspace-write` sandbox regardless of what
`~/.codex/config.toml` says; Claude is started with `--setting-sources ""` and
`--strict-mcp-config`, so the user's allow-lists, hooks and MCP servers are not
in the session. On both, an approval is answered in this terminal and nowhere
else, and an `a` — allow for the session — is held in memory for the life of
the session rather than written to a settings file.

### Why the Claude bridge opens on Opus and not Fable

Fable is the model this bridge was built for and the one it should default to.
The account it runs on cannot currently bill it: `claude --model
claude-fable-5-1` answers `out_of_credits`, with the seven-day overage already
spent, and `--fallback-model` does not rescue that. The harness selects and
requests Fable correctly — it fails at the provider, not here — but a default
that greets every session with a red error line is not a default, so the bridge
opens on `claude-opus-5` instead.

`/model claude-fable-5-1` still reaches for Fable at any time. When the credits
are back, `DEFAULT_CLAUDE_MODEL` in `src/claude-server.mjs` and the matching
string in `bin/aesthetic` go back to Fable and this section comes out.

### The sandbox gap on the Claude bridge

The two bridges are not equivalent on containment, and the difference is worth
stating rather than papering over.

Codex runs commands under an operating-system sandbox: writes are confined to
the workspace and `networkAccess` is false, so an approved command still cannot
reach the network without a second, explicit escalation.

Claude Code has no equivalent sandbox. On that bridge Aesthetic Code confines
the file tools to the workspace, removes WebFetch and WebSearch, and routes
every prompt to this terminal — and Claude does prompt before a command that
touches the network — but the prompt is the whole boundary. A shell command the
user approves runs with the user's own privileges and can reach the network.
Read-only commands are auto-approved by Claude's own classifier, as reads
inside the sandbox are on the Codex bridge.

Aesthetic Code therefore does not claim that agent tools are network-isolated
on the Claude bridge. Where that matters, `--backend codex` is the bridge with
a kernel behind its approvals.

A future local bridge must route inference only to a loopback or explicitly
configured private endpoint, reject known cloud model names, and disable
telemetry, feedback, browser integration, WebFetch, and WebSearch surfaces.

Local inference will not by itself impose an operating-system network sandbox
on shell commands run by the agent. A command the user approves can still
access the network. Strict offline enforcement is required before the product
may claim that arbitrary agent tools are network-isolated.
