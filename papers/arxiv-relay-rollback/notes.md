# Rollback Over a Relay — notes

Consulted on 2026-11-08, core revision f2d1f744c2, vault revision 0cedfd08 (oskiewar build 462).

## Sources read
- session-server/oskiewar-live-manager.mjs (1,432 lines): MAX_NET_BYTES, LANE_FRESH_MS, laneEligible, relayInput, relayNet, offerLane, freshLane, datagram, geckosChannel, bindDatagramSocket, publish path.
- session-server/oskiewar-relay.mjs, session-server/relay/deploy-relay.sh.
- xbox/live/round-room.mjs: OSKIEWAR_RELAYS (nj = session-server.aesthetic.computer, sfo = relay-sfo.aesthetic.computer), chooseRelayHost.
- xbox/live/oskiewar.js: NET_* constants (lines ~12373-12391), netRollback, netPrune, netSendInputs, netTick, NET_DESYNC_COOLDOWN_MS, hello/start packets.
- xbox/live/oskiewar-multiplayer.md parts two to five.
- duel/net.mjs, duel/connection.mjs, duel/match.mjs (input/step/snapshot), duel/client.mjs (send cadence, snapshot coalescing, reconcile), duel/server.mjs (60 Hz step, snapshot per tick), duel/README.md.
- Core commit 8badd09b27 (West Coast relay) message and stat.

## Test runs (Node v24.22.1, one Mac)
- `node --test session-server/oskiewar-live-manager.test.mjs` → 43/43.
- `node --test duel/test/*.test.mjs` → 64/64 (12 files).
- `node --test xbox/live/tests/oskiewar-netplay.test.mjs` → 26 pass / 16 fail; full failure list and first assertion of each recorded in the paper. All sync assertions passed; failures are fixture drift (map label DOJO, untimed versus rounds, gun-arena jump, versus-lobby opening, coach map spawn columns, head pumping, versus deadline in recordings).
- Not run: netplay-browser.mjs, production fight harness (no headless Chrome on this machine).

## Live probes
- https://relay-sfo.aesthetic.computer/health → {"ok":true,"region":"sfo","commit":"5a08d88c47","rooms":1,"lane":{"udp":8891,"geckos":true,"in":117222,"out":117081,"refused":100}}.
- HTTPS /oskiewar-ping to both relays answered 200 but TCP connect from this Mac took 2.3–4.2 s to either; discarded as a relay measurement.

## Figures
- fig:topology and fig:dool are TikZ diagrams drawn from the source; no screenshots.
