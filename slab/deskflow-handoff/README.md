# Deskflow controller handoff

Neo and Blueberry are both eligible Deskflow controllers. Touching the physical
trackpad on whichever one is currently a client promotes it to server and moves
the other fleet machines to it. Chicken and Panda remain clients.

The physical-touch signal comes from MacPal's `PhysicalTrackpad` wrapper around
macOS's private `MultitouchSupport` framework. Remote Deskflow motion does not
produce those contact frames, so moving the shared pointer cannot accidentally
claim control.

Installed components:

- `~/.local/bin/deskflow-role-runner` — launchd entry point; starts the core in
  the role named by `~/.config/slab/deskflow.json`.
- `~/.local/bin/deskflow-set-role` — atomically updates role/client target and
  restarts the one launchd-managed core.
- `~/.local/bin/deskflow-claim-control` — controller-only fan-out to the other
  three machines.
- `~/.local/bin/deskflow-role-watchdog` — health check that follows the current
  role instead of assuming a permanent server or client.

The installer also provisions both eligible servers' TLS fingerprints on every
client and all three client fingerprints on each controller. A TCP socket alone
is not considered a successful handoff; Deskflow must complete its mutual trust
check.

Controller-to-controller commands use pinned Tailscale addresses. That peer is
switched synchronously; Chicken and Panda retarget in detached jobs so their SSH
latency never delays the local confirmation sound and flash.

The new server and old controller restart concurrently, with 50 ms readiness
polling and a persistent SSH control socket. This avoids serially paying two
core-start and SSH handshakes on every touch.

MacPal records the horizontal trackpad motion made during the role change. Once
the new server is live it replays one small HID edge delta in the original
direction, so the first side bump is not swallowed. Client starts also pulse
macOS user activity briefly; this wakes headless displays before Deskflow asks
for their screen shape.

Every claim carries a nanosecond generation. Role setters reject older
generations, so delayed background work from a previous touch cannot override a
newer touch on the other controller.

`install.sh` installs one machine. `deploy.fish` installs the four-machine
Neo/Blueberry/Chicken/Panda topology from Neo.
