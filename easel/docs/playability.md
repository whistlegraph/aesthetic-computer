# Playtesting inside the Aesel preview

Status: implementation plan, grounded in the current checkout. No game has been driven or declared playable by this exploration.

Use the existing preview as the execution surface. The loop is **observe its pixel buffer → send bounded virtual controls → observe completed frames → assert progress**. Keep the same piece, logical resolution, runtime, and preview window the author sees. No second browser or separate game runner.

## Existing parts to reuse

| Existing code | Contribution and current limit |
| --- | --- |
| `easel/src/frame-capture-script.mjs`, `preview-frame.mjs` | `ac_frame` reads the software canvas or preserved WebGL composite, returns PNG plus color/edge/blank statistics and optional local OCR. WebGPU is explicitly unsupported. Requested source revision is not yet proof of rendered revision. |
| `easel/desktop/frame-capture.cjs`; native `PromptPreview` capture bridge | Already identify the preview guest and verify channel/revision. File polling is currently 200 ms on desktop, 250 ms in the native overlay; this is a useful compatibility path, not the final fast loop. |
| `slab/bin/puppet-mcp.mjs`, `puppet.mjs` | Existing action vocabulary: key, stroke, gesture, cursor; resident daemon holds warm CDP connections. Adapt these primitives to the explicitly bound preview target. Do not use the default “most recent page” target. The MCP key operation is a press, so bounded holds need a small extension. |
| `artery/arena-probe.mjs` | Existing CDP key down/up descriptors, bounded movement sequences, and runtime performance evidence. Reuse its hold/release behavior rather than inventing keyboard synthesis. |
| `system/public/aesthetic.computer/lib/disk.mjs` (`Robo`, `$api.robo`) | Existing virtual pen dispatches `touch:1`, `draw:1`, `lift:1` through AC's `act`, with a separate robot pen. It does not automatically update the hardware `$api.pen`, and is not a complete keyboard/gamepad injector. |
| `lib/pen.mjs`, `keyboard.mjs`, `gamepad.mjs`, `gamepad-mappings.mjs` | Keep the actual AC input normalization, coordinate transforms, key polling, gamepad button edges, axes and deadzones. Gamepad currently polls `navigator.getGamepads()` every 8 ms; no test-pad provider was found in this path. |
| `disks/gameboy.mjs`, BIOS `handleGameboyInput` | Existing AC virtual buttons and per-frame joypad state already feed WasmBoy. Exercise those controls; do not create another emulator. |
| `tests/browser/ac-harness.mjs` | Useful precedent for state hooks, seeded configuration and evidence. Its separate Chrome launch is unsuitable for this preview-only feature. |
| `easel/src/runtime-feedback.mjs` | Existing bounded runtime errors, warnings and source context can accompany each observation. |
| `slab/bin/frame-mcp.mjs`, `slab/lib/frame-tape.mjs` | Existing observe/action/evidence conventions and recorded tapes. Whole-desktop capture/OCR is for shell verification, not the game loop. |

Paths beginning `lib/` or `disks/` above are under `system/public/aesthetic.computer/`.

## First executable slice

Add an Aesel-scoped adapter for the existing Puppet action vocabulary, callable through Aesel's existing MCP server alongside `ac_frame`. Bind it to the current preview guest using the frame bridge's channel/revision checks. Desktop can dispatch through the guest's existing browser input facility; native Slab needs an equivalent bridge into AC's input path. Report unsupported capabilities explicitly until both adapters exist.

Proposed tool: `ac_play`, a scoped adapter over existing input primitives, not a second control stack. One request batches a short action segment and its resulting observation. Proposed shape, not an existing tool:

```json
{
  "channel": "jeffrey/example",
  "revision": "<source hash>",
  "afterFrame": 120,
  "actions": [
    {"kind": "key", "key": "ArrowRight", "phase": "down"},
    {"kind": "wait", "frames": 6},
    {"kind": "key", "key": "ArrowRight", "phase": "up"}
  ],
  "observe": {"image": true, "ocr": false}
}
```

Start with keys and single-pointer press/move/release, capped at 32 actions and two seconds per segment. Map logical piece pixels to the actual preview canvas, independent of hover enlargement. Reuse trusted browser input for ordinary keyboard/pointer interaction; use Robo where the piece already supports robot events. Validate actual held-key and polling behavior rather than claiming synthetic `act` events simulate all controls.

Use a short exclusive automation lease on this preview. Human input, navigation, piece revision change or cancellation stops the segment. Release every held key/button/pointer in `finally` and on timeout/disconnect. A watchdog must release state even if the requesting agent vanishes. Never send OS/global key chords, target another app, or silently click the chat/footer. Reload/restart is an explicit scenario operation because it changes the user's current play state. Tests can exercise networked pieces, so the first rollout should target local single-player drafts without publishing, purchase or account-changing actions.

Return the observed frame ID, boot ID, actual rendered source hash when available, input acknowledgments, elapsed times, pixels/statistics and new runtime errors. Record `unverified` when loaded revision or frame identity cannot be established; do not quietly pass the scenario.

## Make the loop quick

Keep the existing preview guest warm. Add a resident in-memory request/reply channel to the current desktop/native preview bridge; retain private files for compatibility and saved evidence. Avoid a browser handshake, new process, filesystem poll, full-window screenshot or OCR on every action.

The preview should publish a monotonic **completed paint** ID. Wait for that counter to advance after accepted input; `requestAnimationFrame` alone is not proof that the AC worker finished a simulation or paint. Return only the latest frame, with bounded backpressure, rather than queuing obsolete frames behind model inference.

Compute changed-pixel bounds/hash and coarse statistics beside the buffer; encode PNG only when the caller requests pixels or a checkpoint needs evidence. Current capture creates PNG and RGBA, then the client encodes PNG again: remove that duplicate work in the hot path. Reuse the 1 MP cap. Sample visual observations at decision boundaries; execute known short action sequences locally, without a model call per frame. Measure input acknowledgment → next completed paint → observation round-trip p50/p95 before promising a latency target.

## Determinism and end states

BIOS exposes `acPAUSE` / `acRESUME`, but the inspected path does **not** expose exact “advance N simulation ticks” control. Waiting for six browser frames is therefore a real-time probe, not a deterministic replay. Add stepping at the existing AC simulation/paint boundary before advertising deterministic tests; account for worker completion, gamepad polling, async assets, audio clocks and network traffic. Never call a piece's `sim` separately beside the real runtime.

Offer an optional development-only piece test hook that reports `ready`, `playing`, `won`, `lost`, score/lives and restart generation. It must observe actual game state; tests must not force a win or write private gameplay variables. Pin source hash, seed, initial state, logical size and input sequence. If time/randomness/network cannot be controlled, label the run nondeterministic and preserve the replay trace.

| Scenario | Assertion and evidence |
| --- | --- |
| Boot/start | Ready frame appears; declared start control transitions into play; no new fatal runtime error. |
| Controls | Each declared movement/action causes its expected state change, with before/after frames. Animation alone is not proof of responsive controls. |
| Win | A legitimate recorded input path reaches the game's declared win state; corroborate with its rendered end screen or score. |
| Loss | An intentional failure path reaches loss; controls do not leave the game stuck. |
| Restart | The actual restart control returns to initial score/lives and a fresh play generation; repeat one control probe. |
| Stability | Bounded seeded action exploration catches crashes, stuck inputs and impossible transitions, with a reproducible failing prefix. |

Games without a state hook can still receive visual/OCR smoke tests. “You win” pixels are useful evidence, not universal proof of correct game logic. Terminal-state checks should combine declared requirements, runtime state and rendered evidence; a generated hook can be wrong too. Results distinguish pass, fail, unsupported and inconclusive. Playability coverage does not establish fun, fairness, accessibility, audio quality or every possible ending.

Save a private `.easel/playtests/<run>/` manifest, JSONL input/frame trace, selected PNG checkpoints and runtime errors. Associate them with the tested source revision; keep current transcript-sharing policy separate from any new telemetry. No extra vision provider call is required: requested pixels return to the current model, and deterministic assertions run locally.

## Delivery order

1. Existing-preview keyboard/pointer batch + capture + automatic release, using current Puppet/Artery semantics and `ac_frame` evidence. Demonstrate start, movement, loss and restart on a disposable local fixture loaded in the preview.
2. Completed-paint acknowledgments, verified loaded source hash, resident transport and latency measurements. Add gamepad provider injection through AC's existing normalization; adapt Game Boy virtual controls. Keep native/desktop capabilities explicit.
3. Exact simulation stepping, seed/state hook contract and reusable win/loss/restart scenarios. Replay failing traces against each revision and let the agent inspect only decision-point frames.

This exploration changed documentation only. It did not drive the user's game, run paid inference, or claim end-state validation already exists.
