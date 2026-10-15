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
  ```
  [inbox from neo:sip · 2026-09-23 17:58 · reply: prox_send handle="neo:sip"]
    │ look at the diff
  ```
  The clock is the receiver's local zone, the moment is `ts` (send time), `now`
  only fills in for a line without one. An urgent message adds `· urgent` before
  the reply hint. The `reply:` hint appears for any real sender and is left off
  the anonymous `<host>:prox` fallback and `loopboy:` event sources. Every body line is indented under `│`, so
  no line of a body can pass for a second `[inbox from …]` header, and the
  sender label is stripped of newlines, quotes and brackets. `from` is still
  self-reported — `/send` is unauthenticated.

## surfaces

- MCP (`slab/bin/prox-mcp.mjs`): `prox_send { handle, text, urgency?, by? }`
  resolves the handle like `prox_poke`, delivers locally or POSTs `/send`. An
  ambiguous handle is an error listing each candidate's status and age — nothing
  is sent. `from` defaults to the calling session's own `host:name`, else
  `<host>:prox`; `by` overrides. The result echoes what the receiver will see,
  and for a file drop to a session that is not mid-turn says it is read at its
  next hook or `prox_receive` call. A queued receipt does not mean the agent has read the message. `prox_inbox { handle?, consume? }` peeks
  (or drains) a local session's queue; without `handle` it uses the caller's own.
- Who is calling. prox runs as one shared HTTP daemon, so its own env says
  nothing about a caller. In order: the `x-slab-prompt-session-id` header a
  harness forwards; the env (`AGENT_SESSION_ID` / `CLAUDE_SESSION_ID` /
  `SLAB_PROMPT_SESSION_ID`) only for a per-session stdio child; then, for a
  loopback HTTP caller that forwards nothing (Claude Code), the process owning
  its end of the socket (`lsof`), matched — itself or an ancestor — against the
  session markers' `agent_pid`/`claude_pid`. Unknown callers fall back to `<host>:prox`.
- CLI: `node slab/bin/prox-inbox.mjs deliver <session_id> --from host:name --text "..." [--urgency urgent]`,
  `peek <id> [--stamped]`, `drain <id> [--stamped]`.

## receiving without terminal input

Use `prox_send {handle, text}` to send and `prox_receive {timeoutSeconds: 30}`
to wait in the receiving agent. The wait consumes only the calling session’s
inbox and returns through MCP; it needs no Loopboy binding and works for
Claude, Codex, and Aesel. The maximum wait is 55 seconds. When a reply matters,
keep receiving until the peer acknowledges it. A sender’s file receipt proves
queueing, not reading. Clients with cached tool lists can use `prox_inbox`.

An idle agent that is not waiting receives at its next lifecycle hook or inbox
call. Slab never resumes it by submitting user input. `prox_wake` is retained
only as an inbox alias for old MCP clients. Direct `POST /wake` requests are
rejected by the menubar; its keyboard and Screen/PTY wake code is removed.
`prox_artifact_ready {handle, artifacts}` queues completed paths through the
same inbox, including notifications from Paper MCP.

## the rule

[Loopboy event sources](loopboy.md) add a declared subscription to this same
inbox. `prox_launch` accepts the binding at creation, and arbitrary watcher code
publishes through `prox_loopboy_emit`. Existing sessions can enter or leave the
mode without replacing their process or conversation.

No keystroke injection. A consumer takes messages at a turn boundary (hook
drain of `messages.jsonl`) or over its own `inbox.sock`; a sender never
touches the receiving terminal.


## deployment

On a Mac with an existing prox launch agent, run
`python3 slab/bin/install-prox-inbox.py`. It installs a versioned MCP release,
restarts only prox, checks its tool list, and updates the Slab communication
rule in the user’s Claude and Codex instructions. It preserves other settings
and does not install or trust lifecycle hooks. Install the updated menubar too
to retire `/wake` at the receiver; `deploy-host.sh` accepts `SLAB_PREBUILT` so
one guarded release build can serve the fleet.

The 2026-10-08 rollout installed both services on Blueberry, Neo, Frisbee,
Panda, Chicken, and Poorslice. Each host passed an isolated inbox send/receive
check and rejected `/wake`; Blueberry also sent through `prox_send` to test
inboxes on each of the five peers and received matching file receipts.
Jasellite’s headless worker was upgraded to support `/send`, with write access
limited to its inbox directory; its queued receipt, drain, and refusal of
`/wake` were verified separately. It has no terminal wake path.
