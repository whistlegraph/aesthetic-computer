# BTX mining ops scripts

Operational helpers for the BTX (btxchain) mining setup. See agent memory
`jas-nzxt-btx-mining` for the full architecture (GPU miner on `jastow`,
wallet + full node on `jasellite`).

**No secrets live here.** Credentials are read at runtime from separate
`chmod 600` env files on each box.

## Scripts

- **`btx-notify.py`** — send an email from jasellite via Gmail SMTP.
  Usage: `btx-notify.py "subject" "body"`. Reads `~/.config/btx-notify.env`
  (`SMTP_SERVER`, `SMTP_USER`, `SMTP_PASS`, `NOTIFY_TO`). Python stdlib only.

- **`btx-sync-watch.py`** — cron-driven watcher on jasellite (`*/10 * * * *`).
  Emails once when the `btxd` node finishes initial block download
  (`initialblockdownload=false`), and once when the `miner-rewards` wallet
  first shows a nonzero balance. Uses sentinel files
  (`~/.btx-synced-notified`, `~/.btx-paid-notified`) so it fires once each.

- **`jas-nzxt-disk-guard.sh`** — legacy-named, one-shot, idempotent disk
  self-maintenance for the `jastow` Fedora miner (44G root partition, runs hot on docker +
  osbuild artifacts). Caps the systemd journal at 500M, installs a weekly
  docker/package-cache prune timer, and an hourly disk-guard that prunes when
  `/` crosses 90%.

- **`ac-disk-clean.sh`** — reusable `jastow` disk inventory and safe-clean
  command. It reports by default, requires `--apply` before deleting known
  regenerable caches, protects the active ChromiumOS/VM build tree, and can
  install itself with a weekly systemd timer via `--install`.

- **`btx-miner-guard.sh`** — pauses `btx-miner.service` while Will is logged
  in or native/ChromeOS compilation is active, then resumes after ten quiet
  minutes. The accompanying systemd timer checks every 30 seconds.

## Deploy

These run on the remote boxes, not in this repo. Copy to the target and run:

```bash
scp toolchain/btx/btx-notify.py toolchain/btx/btx-sync-watch.py jas@jasellite:~/
scp toolchain/btx/jas-nzxt-disk-guard.sh me@jastow:/tmp/ && ssh me@jastow bash /tmp/jas-nzxt-disk-guard.sh
```
