# Deskflow controller handoff

Neo and Blueberry are both eligible Deskflow controllers. Touching the physical
trackpad on whichever one is currently a client promotes it to server and moves
the other fleet machines to it. Chicken and Panda remain clients.

The physical-touch signal comes from MacPal's `PhysicalTrackpad` wrapper around
macOS's private `MultitouchSupport` framework. Remote Deskflow motion does not
produce those contact frames, so moving the shared pointer cannot accidentally
claim control.

The shared logical pointer is canonically named `unipointer`; its wire record is
`unipointer-state`. See `UNIPOINTER.md`. The name is intentionally independent
of Neo, Blueberry, and Deskflow so other seat software can adopt the same
continuity contract.

Installed components:

- `~/.local/bin/deskflow-role-runner` — launchd entry point; starts the core in
  the role named by `~/.config/slab/deskflow.json`.
- `~/.local/bin/deskflow-set-role` — atomically updates role/client target and
  restarts the one launchd-managed core.
- `~/.local/bin/deskflow-claim-control` — controller-only fan-out to the other
  three machines.
- `~/.local/bin/deskflow-role-watchdog` — health check that follows the current
  role instead of assuming a permanent server or client.
- `~/.local/bin/deskflow-seat-ready` — wakes the local panel and repairs an
  unhealthy core; `--fleet` reconciles and wakes the full controller/client
  topology. An Aqua LaunchAgent runs the local mode after every login. On
  macOS it also brings `awdl0` down to prevent Apple Wireless Direct Link from
  time-slicing the Wi-Fi radio and making the remote cursor stutter. Install
  `deskflow-awdl.sudoers` at `/etc/sudoers.d/deskflow-awdl` (mode `0440`) so
  that workaround is non-interactive. AirDrop, Sidecar, and other AWDL features
  remain unavailable until AWDL is restored or the Mac restarts.

The installer also provisions both eligible servers' TLS fingerprints on every
client and all three client fingerprints on each controller. A TCP socket alone
is not considered a successful handoff; Deskflow must complete its mutual trust
check.

Deskflow transport uses each machine's stable Tailscale address. On the Fuser
Wi-Fi this keeps Chicken and Panda pointer latency far steadier than the direct
access-point route. Control and wake SSH still use `.local` hostnames on the
shared seat LAN, and Neo's existing `computer.aesthetic.deskflow-tailscale-ensure`
agent heals a stopped tailnet before it can strand the clients. The peer
controller is switched synchronously; Chicken and Panda retarget in detached
jobs so their SSH latency never delays the local confirmation sound and flash.

The new server and old controller restart concurrently, with 50 ms readiness
polling and a persistent SSH control socket. This avoids serially paying two
core-start and SSH handshakes on every touch.

MacPal records the full trackpad gesture made during the role change. Once the
new server has restored the active destination it replays that two-axis HID
motion, so the first wiggle is not swallowed. Client starts also pulse macOS
user activity briefly; this wakes headless displays before Deskflow asks for
their screen shape.

The yielding controller also reports its active screen inside the same SSH call
that changes its role. The new controller waits only for that one client to
reconnect, traverses the shortest path through the same 2×2 screen geometry,
restores the cursor's captured client-local coordinates, and replays the full
trackpad gesture. A handoff therefore preserves both destination and position:
touching Blueberry while Neo is driving Chicken continues driving Chicken from
the same point.

Every claim carries a nanosecond generation. Role setters reject older
generations, and each host serializes the generation check through core restart,
so simultaneous trackpad touches cannot complete out of order or leave both
controllers in server mode.

`install.sh` installs one machine. `deploy.fish` installs the four-machine
Neo/Blueberry/Chicken/Panda topology from Neo.
