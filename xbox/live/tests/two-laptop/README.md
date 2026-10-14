# two-laptop versus

Drives real oskiewar seats on two Macs (neo and blueberry) and measures the
rollback lane between them: seat times, the lane forming, pings, rollbacks,
stalls, desyncs, leave and join. 26.10.14

- `web-vs-web.mjs <base> <room|bare> [seconds]` — two headless Chrome tabs
  (neo CDP :9227, blueberry via the :9225 tunnel), both botted through
  `bot-harness.mjs`; the host bot is cross-fed from the challenger's bridge.
- `mac-host-vs-web.mjs [room] [seconds]` — the Mac app on blueberry hosts
  (launched with `OSKIEWAR_ROOM` and `OSKIEWAR_NETLOG=net.log`), a headless
  tab on neo takes the chair.
- `mac-vs-mac.mjs [room] [seconds]` — the Mac app on both; blueberry hosts,
  neo takes the chair; both seats pressed by `keybot` (CGEvent key posts to
  the app's pid, built with `swiftc -O -o keybot keybot.swift` on each Mac).
- `serve.mjs` — serves `xbox/live` the way lith does (`/<room>` answers
  mac-test.html, shared modules from `system/public`) so a build under test
  can be played before it is deployed.
- `relay-observe.mjs <room>` — one viewer socket on the relay: status and
  the first state frame, with its age.

The apps' net logs land in
`~/Library/Containers/computer.aesthetic.oskiewar/Data/tmp/net.log` (the app
is sandboxed): one line a second of `__oskiewarNetStats` plus every telemetry
event, which is how "Can't find variable: performance" was found.
