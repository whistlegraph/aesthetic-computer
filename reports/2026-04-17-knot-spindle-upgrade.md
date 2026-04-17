# Knot upgrade — 2026-04-17

Upgraded self-hosted Tangled knot on `knot.aesthetic.computer` per https://docs.tangled.org/migrating-knots-and-spindles.html.

## Summary

| | before | after |
|---|---|---|
| Knot version | `v1.12.0-alpha.0.20260331173301-12ffd7bde019` (master HEAD on 2026-03-31) | `v1.13.0-alpha` (tag `c3f60dc1`) |
| Spindle | not installed | not installed (nothing to do) |
| Binary | `/usr/local/bin/knot` (51,829,032 B, 2026-03-31) | `/usr/local/bin/knot` (52,184,608 B, 2026-04-17) |
| Repo on disk | `/home/git/repositories/did:plc:k3k3wknzkcnekbnyde4dbatz/core` | `/home/git/repositories/did:plc:fjfqgw6uxfpfzsojsxwaug6k` |
| Repo DID in PLC | (none — repos identified by owner/name) | `did:plc:fjfqgw6uxfpfzsojsxwaug6k` (newly minted via plc.directory) |

Only one repo (`aesthetic.computer/core`) to migrate. No spindle unit on the droplet, so only the knot was touched.

## What v1.13 actually changes

`docs/migrating-knots-and-spindles.html` says little beyond "upgrade to the latest tag, click retry in dashboard." The real content is in the codebase (`knotserver/migrate.go`, commits between `v1.12.0-alpha..v1.13.0-alpha`):

1. **Repo DID assignment.** Each repo now gets its own `did:plc:...`, minted against `plc.directory` (override with `KNOT_SERVER_PLC_URL`, default `https://plc.directory`). Stored in a new `repo_keys` table.
2. **On-disk rename.** Legacy layout `repositories/<ownerDid>/<name>` → new layout `repositories/<repoDid>`. The empty owner dir is removed.
3. **RBAC rewrite.** Casbin policies scoped by `ownerDid/name` are rewritten to be scoped by `repoDid`.
4. **DB migrations** (run automatically on first boot): `add-owner-did-to-repo-keys`, `add-repo-name-to-repo-keys`, `add-unique-owner-repo-on-repo-keys`, `add-key-type-and-nullable-signing-key`.
5. **`didAssign` event** emitted to appview so `tangled.org/aesthetic.computer/core` updates its pointer without a user "retry" click.

Migration runs async in a goroutine at startup (`go migrateReposOnStartup` in `knotserver/server.go:92`) — the server is reachable before it finishes. For a single repo it took 88 ms end-to-end; one PLC call per repo.

## Run log

1. **Pre-flight** — SSHed to droplet (key: `~/aesthetic-computer/aesthetic-computer-vault/home/.ssh/aesthetic_pds` for `root`, not the `tangled` key which is for the `git` user). Confirmed running knot is `v1.12.0-alpha` pseudo-version, no spindle, one repo at `did:plc:k3k3wknzkcnekbnyde4dbatz/core` (464 M working copy, 260 K DB).
2. **Backup** → `/root/knot-upgrade-backup-20260417-074032` on the droplet:
   - old binary `knot.bin.v1.12-prelaunch` (51 MB) + sha256
   - `.knot.env` + systemd unit
   - `knotserver.db.bak` via `sqlite3 .backup` (WAL-consistent)
   - `repos-refs.txt` (show-ref output per bare repo — first attempt missed because repos don't have `.git` suffix; re-ran with a broader find).
3. **Build** — on the droplet (already has Go 1.24.1):
   ```
   git clone https://tangled.org/@tangled.org/core /tmp/tangled-core-upgrade
   git checkout v1.13.0-alpha        # c3f60dc1
   CGO_ENABLED=1 go build -o /tmp/knot-v1.13 ./cmd/knot
   ```
4. **Cutover** — `systemctl stop knotserver` → `mv /usr/local/bin/knot /usr/local/bin/knot.v1.12.prev` → `install -m 0755 /tmp/knot-v1.13 /usr/local/bin/knot` → `systemctl start knotserver`. Downtime ≈ 2 s.
5. **Migration** (from `/home/git/logs/knot.log`):
   ```
   07:44:27 migration applied successfully migration=add-owner-did-to-repo-keys
   07:44:27 migration applied successfully migration=add-repo-name-to-repo-keys
   07:44:27 migration applied successfully migration=add-unique-owner-repo-on-repo-keys
   07:44:27 migration applied successfully migration=add-key-type-and-nullable-signing-key
   07:44:27 starting legacy repo migration count=1
   07:44:27 rewrote RBAC policies old=did:plc:k3k3wknzkcnekbnyde4dbatz/core new=did:plc:fjfqgw6uxfpfzsojsxwaug6k count=6
   07:44:27 migrated repo repoDid=did:plc:fjfqgw6uxfpfzsojsxwaug6k
   07:44:27 legacy repo migration complete migrated=1 total=1 duration=87.882925ms
   ```
6. **Verification:**
   - `git ls-remote git@knot.aesthetic.computer:aesthetic.computer/core` → `main 1147a4a...`, `codex/native-whistle-notepat b23d664...` (same refs as pre-upgrade).
   - `git clone --depth 1` + `git push --dry-run origin main` → "Everything up-to-date" (auth + write path confirmed without actually writing).
   - `GET https://tangled.org/aesthetic.computer/core` → 200, body contains correct commit hash. `HEAD` returns 405 (nginx `allow: GET` only — not a regression).
   - `GET https://knot.aesthetic.computer/` → 405 (knot has no `GET /` handler; expected).
   - DB: `SELECT * FROM repo_keys` → one row, `did:plc:fjfqgw6uxfpfzsojsxwaug6k | did:plc:k3k3wknzkcnekbnyde4dbatz | core | k256`.

## Gotchas

- **Two SSH keys in the vault.** `~/aesthetic-computer/aesthetic-computer-vault/home/.ssh/tangled` is the `git@knot.aesthetic.computer` push key (ed25519, comment `aesthetic-computer-tangled-2026`). `aesthetic_pds` is the `root@<droplet>` admin key. Using `tangled` for root gets `Permission denied (publickey)` — not obvious from the filename.
- **Repo bare dirs have no `.git` suffix.** Knot lays repos out as `repositories/<did>/<name>` (v1.12) or `repositories/<repoDid>` (v1.13+), never `.git`. Any backup or scanning script that relies on `find -name "*.git"` will come back empty; match on `HEAD` presence instead.
- **Two `context deadline exceeded` errors from `/xrpc/sh.tangled.repo.languages`** within the first minute after restart. Query duration was ~1 s each, looks like a warm-up hiccup on a 464 M working tree; no recurrence after the first two requests and no effect on git reads/writes. Watch for this if it starts happening regularly — may be worth raising the language-analysis timeout.
- **Migration needs outbound HTTPS to `plc.directory`.** Fine here (no egress restrictions), but worth knowing: if PLC rate-limits (429/5xx) the migration retries with exponential backoff up to 5 min and 20 attempts before giving up. A firewalled droplet would hang the migration goroutine.
- **Docs are thin.** The migration guide just says "upgrade and click retry." The actual list of DB migrations, on-disk rename, and PLC interaction is only documented in `knotserver/migrate.go`. Worth reading before upgrading a larger knot.

## Rollback (if needed later)

On the droplet:
```
systemctl stop knotserver
cp /root/knot-upgrade-backup-20260417-074032/knot.bin.v1.12-prelaunch /usr/local/bin/knot
# Restore repo path: mv /home/git/repositories/did:plc:fjfqgw6uxfpfzsojsxwaug6k \
#                      /home/git/repositories/did:plc:k3k3wknzkcnekbnyde4dbatz/core
# Restore DB: sqlite3 /home/git/database/knotserver.db ".restore /root/.../knotserver.db.bak"
systemctl start knotserver
```
The DB rollback also removes the `repo_keys` row, so the v1.12 binary won't see the minted repo DID as orphaned.

## deploy.sh changes

[at/knot/deployment/deploy.sh](at/knot/deployment/deploy.sh):

- Added `KNOT_VERSION="${KNOT_VERSION:-v1.13.0-alpha}"` at the top so reinstalls pin to a tag instead of whatever master happens to be that day. Override with `KNOT_VERSION=... ./deploy.sh` (or set `KNOT_VERSION=""` for master HEAD).
- `build_knot()` now `git fetch --tags && git checkout "$KNOT_VERSION"` after the clone, and copies the existing binary to `/usr/local/bin/knot.prev.<timestamp>` before replacing it, so a future in-place upgrade has a one-line rollback. Uses `install -m 0755` instead of `mv` + `chmod` for atomicity.

No other changes needed — `.knot.env` already has `KNOT_SERVER_OWNER` (we're past the v1.7 `KNOT_SERVER_SECRET` → `KNOT_SERVER_OWNER` change), and `PLC_URL` defaults to `https://plc.directory`.
