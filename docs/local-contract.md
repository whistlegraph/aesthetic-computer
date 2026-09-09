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

## Live piece boundary

The session's piece is pushed to `aesthetic.computer/run` on a private code
channel every time its file changes, so that a phone that scanned the QR code
can run it. That request carries the piece's source and the channel token, and
nothing else: no account token, no workspace paths, no conversation. The
channel token is random per session and is never reused.

Pushing is the interface's own action, on a file the user can see, and stops
when the session ends.

## Inference boundary

The terminal interface is always Aesthetic Code. Engines are internal bridges,
not alternate client interfaces or command shortcuts.

The current bridge uses Codex app-server and remote inference. The interface
labels this before a prompt is sent. Provider terms govern that traffic.

A future local bridge must route inference only to a loopback or explicitly
configured private endpoint, reject known cloud model names, and disable
telemetry, feedback, browser integration, WebFetch, and WebSearch surfaces.

Local inference will not by itself impose an operating-system network sandbox
on shell commands run by the agent. A command the user approves can still
access the network. Strict offline enforcement is required before the product
may claim that arbitrary agent tools are network-isolated.
