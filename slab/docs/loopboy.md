# Loopboy event sources

A Loopboy is a mode of a prox session bound to one event/source pair. Any local
watcher can compute its own conditions and publish an update. The binding is
metadata; Slab does not evaluate code or start commands from it.

## Create with a source

Call `prox_launch` on the owning host:

```json
{
  "host": "blueberry",
  "agent": "codex",
  "prompt": "Watch network activity. Receive updates with prox_loopboy_wait.",
  "loopboy": {
    "event": "fishery",
    "source": "studio",
    "description": "New visits and engagement milestones across the studio",
    "config": {"pollSeconds": 60, "windowMinutes": 5},
    "expiresAt": "2026-10-15T07:00:00Z"
  }
}
```

The expiry above is an example: use the desired future deadline or omit it.
`event` and `source` are lowercase keys of 1–40 letters, digits, underscores, or
hyphens, starting with a letter or digit. The pair identifies the route;
`fishery:studio` and `build:studio` are separate sources. One live session owns
each pair, and a session has one binding. `config` is an optional JSON object
of at most 4096 bytes, stored for producer inspection. It does not schedule or
configure the producer automatically. `description` is at most 240 characters.

Creation passes the source through the Claude, Codex, or Aesel launcher and
registers the new live session. Custom creation requires an updated local
menubar and launchers. Cross-host creation with a binding is refused; configure
the route on the host that owns the session.

For an existing prox, call `prox_bind_notification` with its `handle` and the
same `event`, `source`, and optional metadata. This preserves its process,
window, name, session ID, and conversation. Unbind before changing to another
source. Rebinding the same source creates a new generation, ending an old wait.

## Publish and receive

When the watcher's own code detects a change, call `prox_loopboy_emit`:

```json
{
  "event": "fishery",
  "source": "studio",
  "text": "New anonymous visit reached 30 seconds of visible activity.",
  "data": {"kind": "engagement", "visibleSecondsAtLeast": 30}
}
```

Any language can call the local MCP HTTP endpoint, using JSON-RPC `tools/call`
with the tool name and arguments. The producer runs independently; it can poll,
consume a stream, watch a file, or evaluate custom conditions. It should deduplicate
and batch before emitting. Text is required, at most 6000 characters; optional
JSON data is at most 1200 bytes and is included in the inbox text. The existing
local MCP access boundary applies; source names are routing keys, not credentials.

Emission verifies the live local owner, matching mode and route, generation,
and expiry. It queues an ordinary prox inbox message and attempts a visual poke.
A receipt confirms queueing, not reading. A missing poke does not invalidate the
durable message. Producers should retain failed deliveries for retry.

The bound session calls `prox_loopboy_wait` with `timeoutSeconds` from 0 to 55.
Optional `event` and `source` assert the expected binding. The wait receives the
session's normal inbox, including peer messages, and rechecks its binding every
poll. Continue waiting after an empty result. An agent that is not waiting reads
at its next inbox call or lifecycle hook; the protocol never submits terminal
input, changes focus, resumes a process, or replies automatically.

## Exit and compatibility

`prox_unbind_notification {handle}` removes the route and writes a durable OFF
mode. Expiry also stops new delivery and waiting. Neither closes the prox, stops
the external producer, nor removes already queued inbox messages.

Version 2 routes store `event`, `source`, `routeKey`, `bindingId`, the owner, and
optional metadata in `~/.config/slab/loopboy.json`. Mode records live under
`~/.local/share/slab/state/loopboy-modes/`. An explicit OFF, corrupt, or expired
mode defeats stale launch markers. Each binding has a fresh generation so an
old waiter cannot silently follow a replacement binding.

Existing `contact` bindings and `loopboyContact` creation arguments still mean
`event: "imessage"`. Their registry keys remain the contact name. Custom keys
use `event:source`. Custom bindings cannot consume the private iMessage feed;
legacy `wake` and `autoRespond` flags never authorize terminal input.

Install the menubar with `slab/menubar-swift/install.sh`, then update the prox
daemon with `python3 slab/bin/install-prox-inbox.py`. Cached MCP clients may need
to refresh their tool list to expose `prox_loopboy_emit` and the new arguments.
