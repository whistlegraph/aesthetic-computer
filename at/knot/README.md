# Tangled Knot — knot.aesthetic.computer

Self-hosted [Tangled](https://tangled.org) knot server co-located on the PDS
droplet (`at.aesthetic.computer`). Provides decentralized git hosting under
AC's ATProto identity.

## Prerequisites

1. PDS droplet running at `at.aesthetic.computer` (165.227.120.137)
2. Vault file `aesthetic-computer-vault/at/knot.env` with:
   ```
   KNOT_OWNER_DID=did:plc:your-did-here
   ```
   Find your DID at https://tangled.org/settings
3. Tangled account with SSH key added at https://tangled.org/settings/keys

## Deploy

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
4. Push: `git remote add tangled git@knot.aesthetic.computer:aesthetic-computer.git && git push tangled main`

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
  ├─ repositories/      # bare git repos
  ├─ database/          # knotserver.db (SQLite)
  └─ log/               # knot.log
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
