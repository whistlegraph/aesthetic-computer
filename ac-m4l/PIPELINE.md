# Notepat.com M4L Build & Distribution Pipeline

Versioned build stream for the `notepat.com.amxd` Max for Live device.
Modeled after the `ac-os` OTA pattern (`fedac/native/ac-os`): every
lith deploy that touches an amxd input rebuilds from the deployed
commit, versions the artifact by git hash, and pushes to DO Spaces as
the long-term archive.

## Layout

### Local (repo + lith filesystem)

```
system/public/m4l/
├── notepat.com.amxd                ← current alias (committed)
└── notepat.com/
    ├── latest.json                 ← manifest (committed, drives staleness check)
    └── <git-hash>.amxd             ← immutable versioned build (gitignored)
```

### DO Spaces archive

```
s3://assets-aesthetic-computer/m4l/notepat.com/
├── notepat.com.amxd                ← root alias (non-cached)
├── latest.json                     ← rolling pointer (no-cache)
└── <git-hash>.amxd                 ← immutable (max-age=31536000,immutable)
```

CDN: `https://assets.aesthetic.computer/m4l/notepat.com/<hash>.amxd`

### User-facing permalink

```
https://notepat.com/amxd
└─ Caddy rewrite → /m4l/notepat.com.amxd (lith-served)
   with Content-Disposition: attachment
```

## Commands

- `npm run notepat:build` — build to local paths only
- `npm run notepat:build:desktop` — also drop a copy on `~/Desktop`
- `npm run notepat:publish` — build + sync to DO Spaces from dev machine
  (requires `AWS_ACCESS_KEY_ID` / `AWS_SECRET_ACCESS_KEY` or
  `DO_SPACES_KEY` / `DO_SPACES_SECRET` in env)
- `fish lith/deploy.fish` — deploys lith; rebuilds the amxd on the
  server (with `--if-stale`) and pushes to DO Spaces (with
  `--sync-spaces`) when `spaces/.env(.gpg)` is available in the vault

## `--if-stale`

Skips the rebuild when no amxd-input file changed since the last
successful build. Tracked inputs (`INPUT_PATHS` in
`ac-m4l/build-notepat.mjs`):

- `system/public/aesthetic.computer/disks/notepat-remote.mjs`
- `system/public/aesthetic.computer/bios.mjs`
- `system/public/aesthetic.computer/lib/`
- `oven/bundler.mjs`

Compares:

1. Committed diff (`prev.piece_git…HEAD`) under those paths
2. Uncommitted working-tree changes under those paths (only when dirty)

If both are empty → skip. Otherwise rebuild and print the offending
files.

## Staleness UX

`bios.mjs` fetches `/m4l/notepat.com/latest.json` on DAW bridge setup,
compares `packGit` (from the offline bundle) against `piece_git`. The
notepat-remote piece surfaces `UPDATE AVAILABLE` inside its
case-doors readout when the two differ.

## Future options

These are tracked for later — nothing urgent.

### 1. Move the archive to `releases-aesthetic-computer`

The ac-os OTA already lives in
`s3://releases-aesthetic-computer/os/`, with `builds/<slug>.vmlinuz`
for versioned kernels and `latest-manifest.json` for the rolling
pointer. Mirroring that layout for m4l would strictly parallel the
OS pattern:

```
s3://releases-aesthetic-computer/m4l/notepat.com/
├── builds/<git-hash>.amxd
├── notepat.com.amxd
└── latest.json
```

Requires:
- Caddy rule for `releases.aesthetic.computer` (or update the piece
  to fetch from the `releases-*` CDN subdomain)
- Migrate existing archived builds from `assets-*/m4l/notepat.com/`
  (or leave them — they stay reachable under the old path)

### 2. Garbage-collect old `<hash>.amxd` builds on S3

Every deploy with an amxd-input change adds a new immutable
`<hash>.amxd` object. After a few dozen commits the bucket will
accumulate obsolete versions. Options:

- A scheduled cron on lith (or as part of deploy.fish) that keeps
  the latest N builds + anything under 30 days old, deletes the rest.
- Mirror ac-os's `builds/` subdirectory with slug names + retention
  policy.

### 3. Version-browser page under `/ableton`

All published versions sit at predictable paths, so a
`ListObjectsV2` against the bucket can power a "browse past
releases" section on the ableton piece. Users could roll back to a
specific hash if a recent build regressed, or compare SHA-256s when
something looks off.

Would need:
- Backend endpoint listing `m4l/notepat.com/<hash>.amxd` objects
  (either a cached JSON or a live S3 list)
- UI in `disks/ableton.mjs` to render the list + per-version
  download buttons linking to the S3/CDN URLs

### 4. Auto-prune `system/public/m4l/notepat.com/` on lith

Similar to (2) but for the local filesystem on lith. Currently
`<hash>.amxd` files accumulate in `/opt/ac/system/public/m4l/
notepat.com/` (they're gitignored so they don't sync down). Not
user-visible unless someone guesses the URL structure, but eats
disk. A `find -mtime +30 -delete` in deploy.fish would handle it.

### 5. Multi-device generalization

`build-notepat.mjs` is hardcoded to `notepat-remote`. If another
piece gets promoted to a flagship amxd, the script should generalize
to a list (like `ac-m4l/devices.json` already has). Would refactor
into `ac-m4l/build-device.mjs <piece-name>` and update the deploy
step to iterate devices flagged `featured: true`.

## Credential sources

| What | Vault path | Used by |
|------|-----------|---------|
| `AWS_ACCESS_KEY_ID` + `AWS_SECRET_ACCESS_KEY` | `aesthetic-computer-vault/spaces/.env.gpg` | `build-notepat.mjs --sync-spaces`, `assets:sync:up` |
| `DO_TOKEN` | `aesthetic-computer-vault/oven/deploy.env.gpg` | `doctl` commands, `ac-lith` droplet lookup |
| `ART_SPACES_*`, `AT_BLOBS_SPACES_*` | `aesthetic-computer-vault/oven/.env.gpg` | `oven/baker.mjs`, `oven/grabber.mjs` |

The deploy flow decrypts `spaces/.env.gpg` on-the-fly, scp's to
`/tmp/notepat-spaces.env` on lith for the lifetime of the build step,
and removes it. S3 keys don't persist in `/opt/ac/system/.env`.

## DO Spaces inventory

As of 2026-04-24 the account has 10 buckets in `sfo3`:

- `art-aesthetic-computer`
- `assets-aesthetic-computer` ← **m4l archive lives here**
- `at-blobs-aesthetic-computer`
- `pals-aesthetic-computer`
- `pix.nopaint.art`
- `private-aesthetic-computer`
- `releases-aesthetic-computer` ← **ac-os OTA target**
- `sotce-aesthetic-computer`
- `user-aesthetic-computer`
- `wand-aesthetic-computer`

Inspect with:

```bash
TMP_ENV=$(mktemp)
gpg --batch --pinentry-mode loopback \
  -d aesthetic-computer-vault/spaces/.env.gpg > "$TMP_ENV"
set -a; source "$TMP_ENV"; set +a

aws s3 ls --endpoint-url "$SPACES_ENDPOINT"
aws s3 ls "s3://assets-aesthetic-computer/m4l/notepat.com/" \
  --endpoint-url "$SPACES_ENDPOINT"

rm -f "$TMP_ENV"
```
