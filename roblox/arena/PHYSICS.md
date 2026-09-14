# Multiplayer physics research — 2026-09-13

Use server throw validation and swept collision decisions, local effects, and
owner-side character impulse for this prototype. Avoid repeated ownership
switches and server CFrame corrections during normal movement. Client ownership
is responsive but is not a complete anti-cheat boundary.
[Network ownership](https://create.roblox.com/docs/physics/network-ownership).

An impulse is delta velocity multiplied by assembly mass. Roblox permits server
impulses on automatically client-owned parts too; it is incorrect to assume they
always fail. A client impulse cannot move a server-owned part.
[BasePart documentation](https://create.roblox.com/docs/reference/engine/classes/BasePart).

Spherecasts catch travel between frames, but do not report an initial overlap.
Current throws start ahead of the character and exclude it. Spawn-in-obstacle
handling remains a limitation to cover before adding walls or arbitrary maps.
[WorldRoot](https://create.roblox.com/docs/reference/engine/classes/WorldRoot).

Roblox documents server authority with prediction, rollback and resimulation.
It requires fixed simulation, Input Actions and shared BindToSimulation code;
verify availability in Studio before a separate migration. It is not a drop-in
switch for this Heartbeat/RemoteEvent controller.
[Server authority](https://create.roblox.com/docs/projects/server-authority),
[security and availability](https://create.roblox.com/docs/scripting/security/network-ownership).

## Acceptance tests

Record 6–10 seconds of the actual Roblox window at 30 fps, beginning before a
throw. Inspect contact, expanding blast, airborne motion and recovery. Confirm
window bounds and focus; reject footage of lobby or other windows as evidence.
Pair footage with root displacement and velocity: a follow camera can conceal
motion. Measure standing, approaching, retreating, edge-radius hits, repeated
blasts and reset during stagger.

Use Studio Server & Clients with two clients, then Network Simulator latency,
jitter and loss. Set explicit zero values for baseline; even Ideal Fiber adds
delay. Solo bot ownership cannot establish multiplayer quality.
[Testing modes](https://create.roblox.com/docs/studio/testing-modes),
[Network Simulator](https://create.roblox.com/docs/studio/network-simulator).

Use a short MicroProfiler dump to separate frame stalls from network delay;
network detail is in saved dumps. Headless Luau here reports IsRunning false;
it verifies logic and engine objects, not motion or frame timing.
[Network profiling](https://create.roblox.com/docs/performance-optimization/microprofiler/network).

## Applied in version 13

Shared `GrenadeMath.position` now drives server sweeps and frame-local client
rendering using `GetServerTimeNow`. Launch packets carry projectile ID, initial
position, velocity, gravity, server time and round. Predicted throws retain their
visual and exponentially decay the correction, avoiding the old object swap.
Blast IDs remove the corresponding visual. Throws carry bounded running momentum;
blast recovery preserves velocity without forcing a GettingUp animation.
[Render step API](https://create.roblox.com/docs/reference/engine/classes/RunService),
[server clock API](https://create.roblox.com/docs/reference/engine/classes/Workspace).

65 headless checks pass, including shared trajectory math and inherited momentum.
Visual frame pacing and two-client latency remain separate acceptance checks.
