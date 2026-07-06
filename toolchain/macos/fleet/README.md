# Fleet Mac config

Small idempotent scripts to keep @jeffrey's Macs (neo, blueberry, chicken, panda)
consistently configured. Run from any host that can `ssh` the targets.

## Stats (menu-bar system monitor)

`stats-shared.plist` is the canonical [exelban/Stats](https://github.com/exelban/stats)
config — it mirrors neo's menu bar: **CPU / RAM / GPU / Disk** as `mini` widgets +
**Network** as `speed`; Sensors and Battery off. Machine-specific keys (`remote_id`,
status-item/window positions) are excluded so each Mac lays out naturally.

```bash
bash stats-sync.sh all              # neo chicken panda
bash stats-sync.sh chicken panda    # specific hosts
```

Each run imports the config, registers Stats as a login item (launches at boot),
and opens it. Stats' own auto-updater keeps versions at parity once it's running.

Login-at-startup uses a classic LaunchServices login item, because Stats 3.x's
"Start at login" toggle is SMAppService-backed and not reliably settable from the
CLI. Both mechanisms launch the same single-instance app, so there's no conflict.

## SSH mesh

`fleet-keys.pub` holds every fleet Mac's public identity key
(`jas@aesthetic` laptop, `jas-neo`, `chicken-to-neo`, `panda-to-neo`,
`blueberry-to-neo`). `ssh-mesh.sh` merges them into a host's `authorized_keys`
(dedup by key blob, never removes) so any fleet Mac can reach any other.

```bash
bash ssh-mesh.sh all
```

> Tailscale SSH's *server* is Linux-only, so it can't mesh these Macs — classic
> `authorized_keys` distribution is the durable path. The fleet is reachable over
> the tailnet by name/IP (e.g. blueberry = `100.79.75.53`); the tailnet gives the
> network path, `authorized_keys` gives the auth.

## Blueberry bootstrap

Blueberry does **not** trust the fleet key yet and isn't SSH-reachable, so it can't
be pushed to. `blueberry-join.sh` is a self-contained one-shot that adds the fleet
keys to its `authorized_keys` and configures Stats. It's staged on neo, so run this
**on blueberry** (blueberry already holds a key to neo):

```bash
ssh neo 'cat ~/blueberry-join.sh' > /tmp/bj.sh && bash /tmp/bj.sh
```

After that, blueberry joins the mesh and `ssh-mesh.sh` / `stats-sync.sh` work on it
like any other host.
