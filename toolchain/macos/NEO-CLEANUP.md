# Cleaning Neo

Keep **20 GiB free** for swap, updates, and interactive work. Run host commands
on Neo; its SSH login shell is fish, so send multi-line Bash explicitly:

```bash
ssh -o BatchMode=yes -o ConnectTimeout=20 neo /bin/bash -s <<'REMOTE'
df -h /System/Volumes/Data
"$HOME/.local/bin/cleaner" --apply
df -h /System/Volumes/Data
REMOTE
```

Cleaner handles regenerable caches. It does not remove worktrees, project
dependencies, models, agent histories, or Photos libraries. An inaccessible
cache must be reported and skipped so the rest of the cleanup can finish.
Snapshot thinning and remote-backed media pruning remain explicit options;
neither belongs in unattended cleanup. See [SCORE.md](SCORE.md#cleaner).

## Keeping the space free

- Keep voice evaluation and intermediate audio on Poorslice; run video/browser
  rendering and retain bulk outputs on Panda. Neo can keep the controlling
  sessions and source edits. Verify remote outputs before removing local copies.
- Neo's `computer.aesthetic.cleaner` job runs daily at 03:15 with
  `ProcessType=Background`, `LowPriorityIO=true`, and `Nice=10`.
- Install the hourly warning with
  `bash toolchain/macos/disk-space-watch.sh --install` on Neo. It records free
  space in `~/.local/share/slab/disk-space/latest.json`, marks `low-space` below
  20 GiB, and notifies at most once per 24 hours. It never deletes anything.

The Cleaner installer currently writes a **weekly** schedule. After reinstalling
it on Neo, restore the daily schedule and background priority:

```bash
python3 - <<'PY'
import pathlib, plistlib
path = pathlib.Path.home() / "Library/LaunchAgents/computer.aesthetic.cleaner.plist"
config = plistlib.loads(path.read_bytes())
config.update(StartCalendarInterval={"Hour": 3, "Minute": 15},
              ProcessType="Background", LowPriorityIO=True, Nice=10)
path.write_bytes(plistlib.dumps(config))
PY
launchctl bootout "gui/$(id -u)/computer.aesthetic.cleaner"
launchctl bootstrap "gui/$(id -u)" "$HOME/Library/LaunchAgents/computer.aesthetic.cleaner.plist"
```

Run that block in Bash on Neo. If the job is already unloaded, `bootout` can
report it missing; still run `bootstrap`. Inspect the installed plist and
`~/Library/Logs/cleaner.log` after changing the schedule.

## Reviewing stale checkouts

Commit age alone does not establish that a checkout is unused. Inspect the
registered worktrees, then each candidate:

```bash
git -C "$HOME/aesthetic-computer" worktree list --porcelain
# Set candidate to one exact checkout being reviewed.
git -C "$candidate" status --porcelain=v1 --untracked-files=all
git -C "$candidate" ls-files --others --ignored --exclude-standard
git -C "$candidate" log -1 --format='%h %cs %s'
git -C "$candidate" rev-list --count main..HEAD
```

Check process command lines **and** open working directories, plus references
in `~/Library/LaunchAgents`, `~/.config/slab`, and `~/.local/bin`. For example,
the old `aesthetic-computer-slab-sync` checkout still supplied the Emacs MCP
service; `ac-jev-workshop` still had process references. Both were retained.

Before removing an approved candidate:

1. Record its absolute path, HEAD, branch, and merge status against the current
   main branch. Preserve unmerged commits and local edits; do not classify them
   as disposable because the checkout is old.
2. Review ignored and untracked files. Preserve unique QA scripts, logs, images,
   and build artifacts in a recovery archive. Verify saved regular files by
   checksum and saved symlinks by link target; do not follow dependency or vault
   symlinks into shared trees.
3. Recheck HEAD, edits, extras, and live users immediately before removal.
   Unlink only the reviewed, saved extras, then use
   `git -C "$HOME/aesthetic-computer" worktree remove "$candidate"` without
   `--force`. Stop if Git refuses. Keep branches and commits.
4. Record restore commands: `git worktree add` at the retained branch or exact
   detached commit, followed by extraction of the saved extras into that tree.
   Verify the checkout disappeared and its commit still exists.

Fuser checkouts under `~/Developer/fuser-*` require Jeffrey's explicit approval
even when merged. Do not include worktree removal in Cleaner or a timer.

## Retiring Neo's local Photos library

This is an explicitly requested, one-time removal of the **local library
bundle**, not deletion of photos through the Photos app. Photo deletions inside
a synced library can propagate to iCloud. Apple's supported device-only controls
are described in [Turn off iCloud Photos](https://support.apple.com/en-us/102179);
its [library relocation guide](https://support.apple.com/en-us/108345) also
distinguishes retiring a local library from deleting individual photos.

The terminal recovery on 2026-09-21 used this scope:

1. Jeffrey confirmed the photos originated on his phone and authorized removal
   of Neo's local copy. Missing cloud IDs in SQLite were **not** treated as proof
   that photos had failed to sync.
2. Close Photos and quiesce the current user's photo-library writers while
   copying/removing the bundle. Restore or restart those services on every exit;
   do not leave a daemon suspended or disable its job permanently.
3. Preserve `originals/`, `internal/`, `database/Photos.sqlite`, its `-wal` and
   `-shm` sidecars when present, and `database/DataModelVersion.plist` in a private,
   compressed archive on Panda. Match the sender and receiver SHA-256 digests
   before deleting the local library.
4. Remove the approved `~/Pictures/Photos Library.photoslibrary` bundle as a
   whole. Do not edit its SQLite records or selectively delete files inside a
   library that will remain in use. Verify it is gone, release old service file
   handles, and measure free space again.

That archive is **limited recovery data**, not a complete usable Photos library
or a backup of all full-resolution iCloud originals. Extract it separately to
recover/import local originals; keep the database and WAL together for metadata
recovery. Do not open the partial bundle as a live library. If a complete local
backup is required, preserve the entire closed library instead.

Avoid copying every rebuildable preview before an authorized retirement: Neo's
library occupied about 20.3 GiB but held only about 1.7 MiB under `originals/`.
The useful recovery archive was 1.2 GiB. A full preview transfer was abandoned as
unnecessarily slow, and its incomplete copies were removed after verification.
Re-enabling Photos locally can rebuild its storage; prefer Optimize Mac Storage
if a new synced library is wanted. Photos retirement is never scheduled cleanup.

## Verified recovery, 2026-09-21

| Action | Observed result |
| --- | --- |
| Repaired Cleaner | Continued past a denied cache; reclaimed about 1.6 GiB |
| Removed two idle Swift `.build` caches | About 516 MiB reclaimed |
| First five approved, merged worktrees | 16.59 GiB reclaimed |
| Four further approved worktrees | 7.45 GiB reclaimed |
| Retired local Photos library | Final filesystem reading: about 43.5 GiB free |

The second batch was `ac-aesel-feed-live`,
`ac-worktrees/oskiewar-fighter-generation`, `ac-worktrees/oskiewar-xbox`, and
`ac-worktrees/tape-upload-fix`. Their ten regular extras and symlink targets were
saved before removal. These are historical outcomes, not a standing deletion
list. Filesystem free-space deltas can differ from `du` and change during work.

Recovery records on Neo are under `~/.local/share/slab/recovery/20260921/`:
`worktree-cleanup-plan.json`, `four-checkouts/receipt.json`,
`four-checkouts/restore.sh`, and `photos/{receipt.json,RESTORE.txt}`.
Panda holds `~/Backups/neo-photos-20260921/Local-originals-and-database.tar.gz`
with its checksum, receipt, and recovery instructions. Keep private archives and
per-host receipts out of Git.
