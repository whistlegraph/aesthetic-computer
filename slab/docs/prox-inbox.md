# prox inbox — messages between live agent sessions, no keystrokes

Every session prox names as `host:name` has a mailbox on the machine that
runs it. Senders drop one JSON line; the session reads it at its next turn
boundary, or at once over a socket when a live harness is listening. Nothing
on this path types into a terminal. Module: `slab/bin/prox-inbox.mjs`
(dependency-free ESM; also a CLI). Consumer side: `slab/docs/inbox-drain.md`.

## paths

`SLAB_HOME` defaults to `~/.local/share/slab` (same as Easel).

```
$SLAB_HOME/inbox/                       0700
  <session_id>/                         0700   session_id = ledger entry id (Claude session_id, Codex rollout id)
    messages.jsonl                      0600   pending queue, append-only
    inbox.sock                                 optional; exists only while a harness listens (Easel pro)
    log.jsonl                           0600   delivered messages, appended by the drainer, last 500 lines
```

A session id must match `^[A-Za-z0-9._-]{1,180}$`; anything else is refused
before it becomes a path.

Unix socket paths cap at 104 bytes on macOS. `~/.local/share/slab/inbox/<uuid>/inbox.sock`
is ~83 bytes for a short username; a `SLAB_HOME` under `/var/folders/…` or a
long home path can push past the cap, at which point a listener fails to bind
(`EINVAL`) and delivery simply stays on the file path.

## message

One JSON object per line:

```json
{ "v": 1, "id": "<uuid>", "ts": 1790283480000,
  "from": "neo:sip", "to": "neo:fotos", "to_id": "<session_id>",
  "text": "look at the diff", "urgency": "queue", "kind": "message" }
```

- `text`: required, 1–8000 characters; longer is rejected, never truncated.
- `urgency`: `queue` (default) delivers at the next turn boundary; `urgent`
  lets a socket-listening harness interrupt its running turn.
- `from`: required, the sender's `host:name` (prox_send defaults to
  `<thisHost>:prox`). `to` is informational; `to_id` is the address.
- Missing `v`, `id`, `ts`, `kind` are filled in on receipt.

## local delivery precedence

`deliverLocal(message)`:

1. If `inbox.sock` exists: connect (200 ms timeout), write the line + `\n`,
   wait up to 1 s for a one-line JSON ack `{"ok":true}`. Result `via: "socket"`.
2. On any socket failure (no listener, stale file, refused, timeout): append
   to `messages.jsonl`. Result `via: "file"`.

A message is never lost to a harness that is restarting: the file is the
floor. `appendMessage(message)` skips the socket on purpose.

## remote delivery

`POST http://<owner ip>:5252/send` with the message as the JSON body. The
owner (Swift menubar `LedgerHTTPServer` on Macs, `prox-worker.mjs` on
headless hosts) runs the local precedence above and answers

```json
{ "ok": true, "via": "socket" | "file", "id": "<uuid>" }
{ "ok": false, "error": "..." }
```

Bad `to_id` or `text` is a 400 on the worker; the menubar answers 200 with
`ok:false` like its other routes. The route only writes into the inbox tree;
it never focuses, pastes, or signals anything.

## drain semantics (consumers)

- `drain(sessionId)`: rename `messages.jsonl` → `messages.draining.<ts>` (the
  rename is the claim; a sender appending afterwards lands in a fresh file),
  parse, append to `log.jsonl` capped to the last 500 lines, delete the
  draining file, return the messages. Missing dir or file → `[]`. Draining
  files left by a drainer that died are picked up on the next drain.
- `peek(sessionId)`: read without consuming.
- `stamp(message, now?)`: what the model sees —
  `[inbox from neo:sip · 2026-09-23 17:58] look at the diff`. The clock is the
  receiver's local zone, the moment is `ts` (send time), `now` only fills in
  for a line without one. An urgent message reads
  `[inbox from neo:sip · 2026-09-23 17:58 · urgent] stop`.

## surfaces

- MCP (`slab/bin/prox-mcp.mjs`): `prox_send { handle, text, urgency?, by? }`
  resolves the handle like `prox_poke` (refuses ambiguity), delivers locally
  or POSTs `/send`. `prox_inbox { handle?, consume? }` peeks (or drains) a
  local session's queue; without `handle` it uses the caller's own session
  from `AGENT_SESSION_ID` / `CLAUDE_SESSION_ID`.
- CLI: `node slab/bin/prox-inbox.mjs deliver <session_id> --from host:name --text "..." [--urgency urgent]`,
  `peek <id> [--stamped]`, `drain <id> [--stamped]`.

## the rule

No keystroke injection. A consumer takes messages at a turn boundary (hook
drain of `messages.jsonl`) or over its own `inbox.sock`; a sender never
touches the receiving terminal.
