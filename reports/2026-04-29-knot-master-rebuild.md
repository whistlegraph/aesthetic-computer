# Knot rebuild — 2026-04-29

Rebuilt self-hosted Tangled knot on `knot.aesthetic.computer` from master HEAD. No new tag has shipped since the 2026-04-17 v1.13.0-alpha cutover (see `2026-04-17-knot-spindle-upgrade.md`); this picks up post-tag commits on master.

## Summary

| | before | after |
|---|---|---|
| Knot source | `v1.13.0-alpha` tag (`c3f60dc1`), built 2026-04-17, rebuilt 2026-04-23 | master HEAD `3ff418dad639a9755b9ef7509ed948578d89be8b` ("blog: templatize topbar, add link to go to blog index") |
| Binary sha256 | `1c7b842e2c3223a142b2343d39353f7bd1d80eebf12da6b3b3a4e9b869dc9c6a` | `63c1a9ec3e8c6f036c35e36c6e9a1a0b66e8ccd9d31ac69fec1930db958a34c5` |
| Binary size | 52,224,872 B | 52,246,384 B (+21,512 B) |
| Service downtime | — | ~3 s (`systemctl stop` → install → `systemctl start`) |
| Schema migrations | — | none ran (no DB delta between v1.13.0-alpha tag and master HEAD) |
| `repo_keys` rows | 1 | 1 (unchanged) |
| Repo on disk | `did:plc:fjfqgw6uxfpfzsojsxwaug6k/` | unchanged |

## Why master, not a tag

`git ls-remote --tags https://tangled.org/@tangled.org/core` shows `v1.13.0-alpha` is still the newest tag. The migration doc only covers v1.13+, which we're already past. User asked for an upgrade; chose master HEAD over a no-op rebuild of the same tag.

## Run log

1. **Pre-flight** — captured SHA, service uptime (Apr 23 22:52 UTC, 6 days), DB sizes, `repo_keys` count, `git ls-remote` (HEAD `daac06cf...`). No errors in journalctl tail.
2. **Backup** → `/root/knot-upgrade-backup-20260429-235704/` on droplet:
   - `knot.bin.prev` (52 MB) + `knot.bin.prev.sha256`
   - `.knot.env` (preserved perms 600 git:git)
   - `knotserver.service`
   - `knotserver.db.bak` (via `sqlite3 .backup`, WAL-consistent)
3. **Build** — on droplet, fresh clone:
   ```
   cd /tmp && git clone https://tangled.org/@tangled.org/core tangled-core-upgrade
   cd tangled-core-upgrade && git checkout 3ff418dad639a9755b9ef7509ed948578d89be8b
   CGO_ENABLED=1 go build -o /tmp/knot-new ./cmd/knot
   ```
   Build clean, no warnings. New binary 52,246,384 B.
4. **Cutover** — `systemctl stop knotserver` → `cp -a /usr/local/bin/knot /usr/local/bin/knot.prev.20260429-235759` → `install -m 0755 /tmp/knot-new /usr/local/bin/knot` → `systemctl start knotserver`. Service active on PID 31507 within 3 s.
5. **Verify** —
   - `curl -sI http://localhost:5555/` → 405 (expected; knot has no `GET /`)
   - `curl -sI https://knot.aesthetic.computer/` → 405 (Caddy + knot still wired)
   - `git ls-remote git@knot.aesthetic.computer:aesthetic.computer/core` → `main daac06cf...` (refs preserved, SSH AuthorizedKeysCommand still works, motd "aesthetic computer knot" prints)
   - `SELECT COUNT(*) FROM repo_keys` → 1 (unchanged)
   - `tail -100 /home/git/log/knot.log` → file does not exist (knot logs to stderr → journalctl); `journalctl -u knotserver` clean
6. **Cleanup** — `rm -rf /tmp/tangled-core-upgrade /tmp/knot-new`.

## deploy.sh change

[at/knot/deployment/deploy.sh](../at/knot/deployment/deploy.sh):

- `KNOT_VERSION` default bumped from `v1.13.0-alpha` to the SHA `3ff418dad639a9755b9ef7509ed948578d89be8b` so a future reinstall reproduces what's running. When v1.14 tags, switch back to a tag string.

## Rollback

On droplet:
```
systemctl stop knotserver
cp /usr/local/bin/knot.prev.20260429-235759 /usr/local/bin/knot
systemctl start knotserver
```
No DB rollback needed (no schema changes were applied this round). Older `knot.v1.12.prev` is still there too if a pre-DID rollback is ever required (it would need the `repo_keys` row removed and the on-disk repo path restored — see Apr 17 report).

## Binaries on disk

```
/usr/local/bin/knot                          52,246,384  master 3ff418da   (current)
/usr/local/bin/knot.prev.20260429-235759     52,224,872  v1.13.0-alpha     (Apr 29 rollback)
/usr/local/bin/knot.v1.12.prev               51,829,032  v1.12.0-alpha     (pre-DID-migration)
```
