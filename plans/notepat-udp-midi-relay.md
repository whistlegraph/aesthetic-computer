# Notepat UDP MIDI Relay Plan

## Overview

Let running `ac-native` `notepat` sessions automatically publish note events to the Aesthetic Computer network, so a Max for Live device or `notepat.com`-based device can subscribe to those events and play them back live.

The important twist is identity:

- every note stream should be attributable to a `@handle`
- we should also preserve `machineId` so multiple devices owned by the same person can be distinguished
- subscribers should be able to filter by `@handle`, by `machineId`, or by "everyone"

This would make an `ac-native` machine feel like a network instrument source, while Ableton / Max acts like a remote listener, mirror, recorder, or re-player.

## Why This Shape

We already have the right footholds:

- `fedac/native/src/udp-client.c` already sends compact UDP packets from `ac-native`
- `fedac/native/src/js-bindings.c` already injects the current `handle` into the UDP client
- `system.machineId` already exists in native
- `notepat.mjs` already has a clean note-on / note-off seam for outgoing MIDI events
- `ac-m4l/AC Notepat.amxd` and related M4L assets already exist

The part that does not exist yet is a note-event relay path and a subscription model on the receiver side.

## Recommended Architecture

Use a hybrid model:

1. `ac-native` sends note events to a UDP relay endpoint for low-latency ingest.
2. The relay normalizes those events and republishes them to WebSocket subscribers.
3. Max for Live / `notepat.com` devices subscribe over WebSocket by `@handle` or `machineId`.

This is better than raw UDP end-to-end because:

- browser-based receivers do not want to deal with raw UDP directly
- WebSocket subscription/filtering is much easier to debug in M4L and web UIs
- the relay can enrich events with metadata, timestamps, and presence
- we can keep the native sender lightweight

## Architecture

```text
┌──────────────────────┐
│ ac-native / notepat  │
│ on Yoga / AC machine │
└──────────┬───────────┘
           │
           │ UDP note events
           │ handle + machineId + midi
           ▼
┌──────────────────────────────┐
│ session-server MIDI relay    │
│ - UDP ingest                 │
│ - presence / last-seen map   │
│ - WS fanout                  │
│ - handle + machine filters   │
└──────────┬───────────────────┘
           │
           │ WebSocket subscribe:
           │ notepat:midi:subscribe
           ▼
┌──────────────────────────────┐
│ Max for Live / AC Notepat    │
│ or notepat.com device        │
│ - choose source handle       │
│ - optional machine filter    │
│ - playback / monitor UI      │
└──────────────────────────────┘
```

## Current Repo Footholds

### Native sender

- `fedac/native/pieces/notepat.mjs`
  - already computes note-on / note-off centrally
  - already knows `system.usbMidi`, `system.udp`, and `system.machineId`
- `fedac/native/src/js-bindings.c`
  - already exposes `system.udp`
  - already exposes `system.machineId`
  - already sets UDP identity from the config `handle`
- `fedac/native/src/udp-client.c`
  - currently supports fairy packets only
  - already has background send / recv thread

### Receiver side

- `ac-m4l/AC Notepat.amxd`
- `ac-m4l/AC-Notepat.maxpat`
- `ac-m4l/ac-ws-server.js`
- `plans/kidlisp-m4l-integration.md`

These give us a plausible place for a handle-selectable network note receiver.

## Event Model

### Minimum event payload

Each note event should carry:

- `type`: `note_on` or `note_off`
- `note`: MIDI note number
- `velocity`: 0-127
- `channel`: MIDI channel
- `handle`: performer handle without `@`, if known
- `machineId`: native machine identity
- `piece`: usually `notepat`
- `ts`: sender timestamp in ms

### Suggested normalized relay JSON

```json
{
  "type": "notepat:midi",
  "event": "note_on",
  "note": 60,
  "velocity": 109,
  "channel": 0,
  "handle": "jeffrey",
  "machineId": "ac-1234abcd",
  "piece": "notepat",
  "ts": 1710000000000
}
```

### Optional later fields

- `source`: `ac-native`
- `wave`: current notepat wave mode
- `usbMidi`: whether it was also forwarded to a local USB MIDI host
- `session`: optional room / channel
- `pressure`: for aftertouch-like experiments

## UDP Protocol Plan

### Phase 1

Add new packet types to `fedac/native/src/udp-client.c`:

- `0x10` = note on
- `0x11` = note off
- `0x12` = heartbeat / source status

Suggested binary shape:

```text
[1 type]
[1 note]
[1 velocity]
[1 channel]
[1 handle_len][N handle_bytes]
[1 machine_len][N machine_bytes]
[4 timestamp_ms_u32]
```

This keeps the packet compact while still carrying identity.

### Alternative

If we want to move faster and accept a slightly fatter payload, we could send short JSON strings over UDP first, then tighten to binary later. That would reduce implementation friction in the relay.

Recommendation:

- binary if we want to keep it native-first and stable long-term
- JSON if we want the fastest prototype

My recommendation is:

- prototype with JSON-over-UDP
- switch to binary once the event schema settles

## Relay / Server Plan

The relay should do four jobs:

1. accept note events from native senders
2. maintain a live source index
3. expose source discovery to listeners
4. fan events out to subscribers

### Source index

Maintain a map keyed by `handle + machineId`:

```js
{
  "jeffrey:ac-1234abcd": {
    handle: "jeffrey",
    machineId: "ac-1234abcd",
    lastSeen: 1710000000000,
    lastEvent: "note_on",
    piece: "notepat"
  }
}
```

### WebSocket API

Add a small family of messages:

```json
{ "type": "notepat:midi:sources" }
```

Response:

```json
{
  "type": "notepat:midi:sources",
  "sources": [
    { "handle": "jeffrey", "machineId": "ac-1234abcd", "piece": "notepat", "lastSeen": 1710000000000 }
  ]
}
```

Subscribe:

```json
{ "type": "notepat:midi:subscribe", "handle": "jeffrey" }
```

Or:

```json
{ "type": "notepat:midi:subscribe", "handle": "jeffrey", "machineId": "ac-1234abcd" }
```

Or:

```json
{ "type": "notepat:midi:subscribe", "all": true }
```

Broadcast event:

```json
{
  "type": "notepat:midi",
  "event": "note_on",
  "handle": "jeffrey",
  "machineId": "ac-1234abcd",
  "note": 60,
  "velocity": 96,
  "channel": 0,
  "ts": 1710000000000
}
```

## Native Sender Changes

### `notepat.mjs`

Extend the current note send helpers so note events can also go to the network:

- note on -> UDP relay send
- note off -> UDP relay send
- all notes off -> optional cleanup event

Suggested behavior:

- default off until user opts in
- per-boot toggle in prompt or `notepat`
- persist in `/mnt/config.json`

Suggested config key:

```json
{ "udpMidiBroadcast": true }
```

### `js-bindings.c`

Expose one small higher-level helper on `system.udp`, for example:

- `system.udp.sendMidi(event, note, velocity, channel, machineId, piece)`

That keeps the piece code clean and avoids reimplementing packet shape in JS.

### `udp-client.c`

Add:

- note packet encoding
- light send queue / coalescing
- heartbeat support

The heartbeat should announce:

- `handle`
- `machineId`
- `piece=notepat`
- `broadcast=true`

every few seconds while the piece is active.

## Max for Live / notepat.com Receiver Plan

### UX

The receiver device should have:

- source mode: `all`, `handle`, or `handle + machine`
- handle selector
- machine selector
- monitor list showing live note events
- armed / muted toggle
- optional MIDI passthrough to Live track

### Playback behavior

For the first version:

- incoming `note_on` -> emit MIDI note on in Max
- incoming `note_off` -> emit MIDI note off in Max

Later:

- optional quantization
- optional octave remap
- optional channel remap
- optional monitor-only mode

### Good first target

Build this into `AC Notepat.amxd` or a sibling device such as:

- `AC Notepat Relay.amxd`

That keeps the "instrument" device separate from the "remote listener" device until the UI settles.

## Filtering By Handle

This is the core product behavior.

### Rules

- if `handle` is selected, accept events from all machines owned by that handle
- if `handle + machineId` is selected, accept only that machine
- if `all` is selected, accept all public broadcasts

### Why both handle and machineId matter

`@handle` alone is not enough if:

- someone has two AC machines running at once
- one machine is a practice keyboard and one is a stage performer
- we want to mirror a specific machine into a specific Ableton set

So `handle` should be the human-facing grouping key, while `machineId` is the precise routing key.

## Presence and Discovery

The receiver should be able to see active sources without typing blindly.

Recommended behavior:

- native sender heartbeats every 5-10 seconds
- relay expires sources after 15-20 seconds of silence
- M4L receiver requests source list on load and periodically refreshes

Displayed label:

```text
@jeffrey  ac-1234abcd  notepat  seen 2s ago
```

## Logging and Observability

We should keep a paper trail because this will be hard to debug live.

### Native

Log to `/mnt/ac-native.log`:

- UDP relay connected
- UDP note-on / note-off send errors
- broadcast enabled / disabled

### Relay

Log:

- source registration
- malformed packets
- subscriber counts
- fanout counts by handle

### M4L / receiver

Expose a small event monitor:

- last event time
- last source handle
- last note
- dropped note count if any

## Privacy / Safety

Because note streams can become ambient network presence, this should be opt-in.

### Recommendation

- sender broadcasting is off by default
- enabling it is explicit in prompt / config
- receiver subscribes explicitly; nothing auto-plays by surprise

Optional later:

- `public` vs `friends` vs `private`
- signed session tokens
- authenticated per-handle subscriptions

## Phased Implementation

### Phase 1: Native -> relay prototype

- [ ] Add `udpMidiBroadcast` config flag
- [ ] Add note send hook in `fedac/native/pieces/notepat.mjs`
- [ ] Add UDP note packet support in `fedac/native/src/udp-client.c`
- [ ] Add relay listener endpoint
- [ ] Log received events server-side

Success condition:

- notes from one `ac-native` machine appear in relay logs with `handle` and `machineId`

### Phase 2: Relay -> WebSocket subscribers

- [ ] Add `notepat:midi:sources`
- [ ] Add `notepat:midi:subscribe`
- [ ] Add source expiry / heartbeat tracking
- [ ] Broadcast normalized note events over WebSocket

Success condition:

- a browser console can subscribe to `@handle` and print incoming notes

### Phase 3: Max for Live receiver

- [ ] Create or fork `AC Notepat` receiver device
- [ ] Subscribe by `handle`
- [ ] Emit Live MIDI notes
- [ ] Show source / last-note monitor

Success condition:

- notes played on `ac-native` trigger a Live instrument from the selected handle

### Phase 4: Refinement

- [ ] machine-level filtering
- [ ] monitor-only mode
- [ ] quantize / transform options
- [ ] optional recording into Live clips
- [ ] note-pressure / CC extension

## Open Questions

### 1. Should the relay live inside the existing session server?

Recommendation:

- yes for the first version

Reason:

- it already handles realtime identity-oriented traffic
- it already has WebSocket clients
- it is the easiest place to hang `handle`-filtered subscriptions

### 2. Should the receiver use raw UDP too?

Recommendation:

- no for the first version

Reason:

- browser / M4L environments are easier over WebSocket
- UDP ingest + WS fanout already gets most of the benefit

### 3. Should we mirror every note or only notes that go to USB MIDI?

Recommendation:

- every note played in `notepat`, independent of USB MIDI state

Reason:

- network relay and local USB MIDI solve different problems
- it is more musically useful if network relay does not depend on a cable

### 4. Should this be public performance presence?

Recommendation:

- start opt-in and semi-private
- add public browsing later if it feels magical

## First Concrete Build Slice

If we want the smallest useful first slice:

1. add `udpMidiBroadcast` toggle in native prompt
2. send JSON note events from `notepat` over UDP
3. add relay logging + source list in session server
4. build a tiny browser subscriber page that filters by `@handle`
5. only then move into Max for Live playback

That gets us from idea to audible proof fast, without overcommitting to a packet format or Max UI too early.
