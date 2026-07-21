# Lobby, matchmaking, rooms, and spectators

**Status: vertical slice plus plan.** Local `TRAIN` / freefight is implemented.
Handled users can use the current `FIND` queue, direct WebRTC rollback path,
and private chat/laklok challenges in local development. Guest `VS` room
protocol operations exist without their final UI. Watch, public listings,
ranked/tournament policy enforcement, shared coordinator state, and production
relay infrastructure remain plans. Nothing in this document provisions or
activates global online services.

The Menu Fighter front door has four primary actions:

```text
FIND MATCH     WATCH MATCH     VS CODE     LOCAL PLAY
```

The public lobby is available to every signed-in Aesthetic Computer user who
has claimed a handle. It does **not** publish every currently online AC user.
Presence is opt-in: a handled user appears only after entering Menu Fighter and
choosing an available or public state.

## Identity gates

| Activity | AC handle required? | Public record? |
| --- | --- | --- |
| Local play on one device | no | no |
| Same-Wi-Fi private play | no | no |
| Join/create unlisted `VS` room | no | no, unless handled host opts in |
| Browse public lobby | yes | presence only while in lobby |
| Find public match | yes | yes |
| Challenge a handled user | yes | invitation and match result |
| Watch a public match | yes | spectator count and moderated identity |
| Ranked play | yes | rating and result |
| Tournament play | yes | signed match record |

Guests in private rooms receive ephemeral labels such as `P1` and `P2`. They
cannot enter ranked/tournament queues, appear in the public lobby, use public
spectator chat, publish results, or claim ratings. A guest may sign in without
destroying the room.

## Lobby screen

The screen is a live list, not a single global queue. Its sections are:

1. **Find Match** — quick match controls and available handled players.
2. **Watch Match** — public matches currently accepting spectators.
3. **Invites** — incoming challenges, reconnects, and room invitations.
4. **My Room** — create, join, share, or close a `VS` room.

A handled user's explicit presence state is one of:

```text
browsing | available | queued | invited | ready | fighting | watching | away
```

Only `available`, `queued`, `fighting` when public, and `watching` when public
appear to other users. Closing the piece, losing the socket, or missing presence
heartbeats removes the listing after a short grace period.

### Available-player row

Show handle, region bucket, connection-quality estimate, mode sought, ranked or
casual, controller-ready status, and challenge availability. Do not expose IP
addresses, exact location, browser version, hardware details, or private room
membership.

### Live-match row

Show player handles, score/round, mode, approximate region, tournament badge,
spectator count, and whether joining mid-match is allowed. Never make spectators
connect to players directly.

## Find Match

`FIND MATCH` opens a small choice rather than a configuration wall:

- **Quick Casual** — widest compatible search; rating is not affected.
- **Ranked** — certified build and platform, tighter network limits, rating.
- **Tournament** — organizer-issued event and match credentials only.
- **Challenge** — select one currently available handle.

The queue ticket contains account id, handle, mode, build/protocol/rules hashes,
coarse region and network preflight, crossplay certification flags, current
rating band, block/mute constraints, and creation time. It never contains an IP
address in public state.

Matching expands deliberately over time: same quality band and region first,
then adjacent skill and region buckets while never crossing the selected mode's
network ceiling. Crossplay is on by default. A platform may enter ranked or
tournament matchmaking only while its exact build/runtime version is certified.

After a candidate pair is found:

1. both clients receive a short-lived match proposal;
2. both run direct and relay route probes;
3. the coordinator chooses an allowed route and symmetric input delay;
4. both see route quality and choose ready;
5. hashes, seats, seed, rules, and credentials are locked;
6. the match starts only after startup synchronization succeeds.

Declining or timing out returns the other player to the front of the queue.
Repeated dodging becomes a matchmaking cooldown only in ranked/tournament play.

## Challenges

A handled user marked `available` can receive a challenge. The invitation shows
challenger, casual/ranked eligibility, proposed rules, and measured quality.
Accepting creates a private ready room. Decline can be silent; blocking prevents
future presence, challenge, chat, and spectator interactions between the pair.

## `VS` rooms

`VS CODE` creates or joins an unlisted room. The display form is an eight-symbol
Crockford-style code grouped for speech, for example:

```text
VS 7K3M-PQ9D
```

The room also has a share URL/QR containing the full capability token. Codes are
random, single-room, rate-limited, expire after ten idle minutes before a match,
and cannot be enumerated through any listing endpoint. Joining places the guest
in a waiting seat; the host approves them before any peer information is
exchanged. A room can rotate its code without ending the current match.

Private rooms support:

- two player seats plus invited spectators;
- host-selectable spectator permission and delay;
- rematch, swap sides, and return-to-room;
- handled or guest players;
- casual rules only when either player is a guest;
- no public result unless both handled players opted in before readying.

A short `VS` code requires the rendezvous service but match traffic can still
be direct P2P. The share URL's longer token prevents the human-friendly code
from becoming the sole security boundary.

## Local play

`LOCAL PLAY` immediately opens controller assignment on one device. No network,
account, room, or service is required. Keyboard and controllers are sampled into
the same shared input bitmasks used online. Local results never affect rating.

## Same-Wi-Fi play

`SAME WIFI` lives under Local Play and does not require handles.

- Native macOS and Xbox hosts advertise/discover a Menu Fighter service on the
  LAN and show nearby rooms after host approval.
- When internet is available, every surface can use an anonymous short-lived
  rendezvous code; ICE should select the direct LAN candidate, so gameplay does
  not leave the network.
- A browser cannot depend on unrestricted LAN discovery. The web fallback uses
  rendezvous signaling, or an advanced offline QR exchange of the complete
  offer/answer. An actually short offline code is impossible without a local or
  internet rendezvous service that maps it to connection data.

LAN mode still performs build/rules hashing and the deterministic handshake.
It does not need accounts, ratings, public presence, or a remote match record.

## Watch Match

`WATCH MATCH` lists only public, handled matches whose players or tournament
rules allow viewers. A spectator connects to the spectator service, never to a
fighter's peer connection. This prevents viewer count from changing fighter
latency or exposing player network information.

The coordinator provides:

- the immutable match header: build/rules hashes, seed, seats, characters;
- a verified checkpoint or the input stream from frame zero;
- confirmed inputs after a configurable delay;
- score, connection state, and signed result.

The spectator runs the same deterministic sim locally and can catch up faster
than real time without rendering every intermediate frame. Public matches use a
small delay; tournament organizers can increase it to prevent coaching. Viewer
chat uses handled AC identity and existing moderation/block primitives, and is
fully separate from match-control packets.

## Server state model

Replace the current “first two roster entries are players” prototype with these
explicit objects:

```text
Presence      handle -> visible state + expiry
QueueTicket   handled account -> mode + compatibility + preflight
Room          room id -> visibility + seats + invite/capability policy
Match         match id -> immutable header + route + lifecycle
Spectator     match id + handled account -> cursor + permissions
Result        match id -> verified inputs/checksums/outcome/signatures
```

Ephemeral presence, queues, rooms, invitations, and socket routing belong in
Valkey with TTLs. Durable completed results, tournament evidence, ratings, and
moderation records belong in the existing durable database/object storage.

## Protocol families

Reliable lobby/control messages:

```text
fight:presence:set       fight:lobby:snapshot
fight:queue:join         fight:queue:leave       fight:match:proposal
fight:challenge:create   fight:challenge:reply
fight:room:create        fight:room:join         fight:room:approve
fight:ready:set          fight:match:start       fight:match:control
fight:watch:join         fight:watch:leave       fight:watch:stream
fight:result:submit      fight:result:verified
```

Every mutating message uses the authenticated server identity when available;
the server never trusts a client-supplied handle. Guest room credentials are
scoped capabilities. Payloads carry protocol version, request id, and monotonic
room/match revision so reconnects and duplicate messages are safe.

Per-frame input packets stay outside this control channel and use the versioned
rollback packet codec described in `NETCODE.md`.

## Abuse and privacy defaults

- Public presence is opt-in and disappears promptly.
- Only handled users can search, be searched, chat publicly, or watch publicly.
- Challenges, room joins, and code attempts are rate-limited by account and
  network signals.
- Blocks are enforced server-side before presence, matchmaking, invitations,
  chat, and spectator membership are returned.
- Private room codes and capability tokens are never logged in plaintext.
- Tournament evidence has a published retention period and access log.
- Spectator delay and visibility are locked by event policy once a tournament
  match begins.
