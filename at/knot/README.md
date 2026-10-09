# Tangled Knot — knot.aesthetic.computer

Self-hosted [Tangled](https://tangled.org) knot server co-located on the PDS
droplet (`at.aesthetic.computer`). Provides decentralized git hosting under
AC's ATProto identity.

## Current deployment — 2026-10-08

Production runs **v1.16.1-alpha**, upstream commit
`1d379a324497da39a27e49453c72615607a9b199`, built with Go 1.25.9.
The public version endpoint advertises `knot-acl`:

```bash
curl --fail https://knot.aesthetic.computer/xrpc/sh.tangled.knot.version
git ls-remote git@knot.aesthetic.computer:aesthetic.computer/core refs/heads/main
```

The upgrade preserved all Git refs; SQLite integrity, HTTPS and SSH reads,
and the Tangled repository page were checked. The service now uses
`GOMEMLIMIT=768MiB`, `MemoryHigh=1536M`, and `MemoryMax=2G` after four kernel
OOM kills in the preceding day. These limits protect the shared PDS host;
longer observation is still needed to assess stability under load.

Consistent PDS and repository backups plus the previous Knot binary,
configuration, and database are stored privately on Blueberry under
`~/.local/share/aesthetic-computer/backups/at-20261008/`. The droplet retains
its rollback files in `/root/ac-at-upkeep-20261008/rollback/`.

Signed in as `aesthetic.computer` at
[Tangled's knot dashboard](https://tangled.org/settings/knots) and completed
**Retry knot verification** after the upgrade. The AppView now reports
**Verified** rather than **Needs upgrade**.
See the [upstream migration guide](https://tangled.org/tangled.org/core/blob/master/docs/DOCS.md).

## Prerequisites

1. PDS droplet running at `at.aesthetic.computer` (165.227.120.137)
2. Vault file `aesthetic-computer-vault/at/knot.env` with:
   ```
   KNOT_OWNER_DID=did:plc:your-did-here
   ```
   Find your DID at https://tangled.org/settings
3. Tangled account with SSH key added at https://tangled.org/settings/keys

## Deploy

The script below bootstraps a host; it overwrites configuration and is not an
in-place production upgrade procedure. For an existing knot, back up its
database, repositories, binary and configuration, build the pinned release
separately, and stop the service only for the final snapshot and binary swap.
Preserve the existing Caddy/PDS configuration and allow for database rollback.

```fish
cd at/knot/deployment
fish deploy.fish
```

This will:
- Install Go + build deps on the droplet
- Create `git` user with SSH AuthorizedKeysCommand
- Build the knot binary from source
- Deploy systemd service + environment
- Configure Caddy reverse proxy (TLS auto via Let's Encrypt)
- Create `knot.aesthetic.computer` DNS via Cloudflare

## After Deploy

1. Verify: `curl https://knot.aesthetic.computer/`
2. Register knot at https://tangled.org/settings/knots — click verify
3. Create repo on Tangled, selecting `knot.aesthetic.computer` as host
4. Push: `git remote add tangled git@knot.aesthetic.computer:aesthetic.computer/core && git push tangled main`

## Unify Repo History

Stitch the four predecessor repos into one continuous timeline:

```bash
# Dry run first
./unify-repo-history.sh --dry-run

# For real
./unify-repo-history.sh
```

Requires `git-filter-repo` (`pip install git-filter-repo`).

## Architecture

```
knot.aesthetic.computer (Caddy :443)
  └─ reverse proxy → localhost:5555 (knot public API)
  └─ websocket → localhost:5555/events

SSH :22
  └─ Match User git → knot keys (AuthorizedKeysCommand)
  └─ git push/pull via SSH

/home/git/
  ├─ .knot.env          # config
  ├─ repositories/      # bare git repos, keyed by repository DID
  ├─ database/          # knotserver.db (SQLite)
  └─ logs/              # knot.log (service stdout/stderr)
```

Co-hosts with PDS — same droplet, separate subdomain.

## Files

```
at/knot/
├─ README.md
├─ unify-repo-history.sh       # graft 4 repos → one history
├─ deployment/
│  ├─ deploy.sh                # main deployment script
│  ├─ deploy.fish              # fish wrapper (loads vault env)
│  └─ configure-dns.mjs        # Cloudflare A record setup
└─ infra/
   ├─ Caddyfile                # reverse proxy config
   └─ knotserver.service       # systemd unit
```
