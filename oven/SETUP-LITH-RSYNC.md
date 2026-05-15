# SETUP: oven → lith rsync for papers

One-time provisioning so `oven/papers-builder.mjs` can rsync built PDFs to
lith instead of committing them to git.

## Why

Phase 1 already stopped tracking xelatex outputs in source dirs
(`papers/arxiv-*/*.pdf`). Phase 2 stops the oven from committing the
**deployed** PDFs at `system/public/papers.aesthetic.computer/*.pdf` —
174 files that churn binary blobs into git on every build. They go to
lith via rsync over SSH instead.

## Architecture

```
oven box                                        lith box
  ┌─────────────────────────────┐               ┌─────────────────────────────────┐
  │ papers/cli.mjs publish      │               │ /opt/ac/system/public/          │
  │   → builds PDFs into        │   rsync       │   papers.aesthetic.computer/    │
  │   system/public/papers...   │ ──ssh────▶    │   *.pdf, platter.html, ...      │
  │                             │               │                                  │
  │ git commit metadata.json    │   push        │ webhook on push → cache purge   │
  │ + BUILDLOG.md only          │ ──git────▶    │ Caddy serves the dir            │
  └─────────────────────────────┘               └─────────────────────────────────┘
```

Cache purge still works because the small metadata commit triggers lith's
existing GitHub webhook → `lith/webhook.sh` → per-file CDN purge.

## One-time SSH key setup

The vault's `home/.ssh/id_rsa` is your laptop's identity. For oven we want
a **dedicated** keypair — generated locally, public half authorized on lith,
private half copied to oven. Rotating the laptop key shouldn't break oven.

From your laptop:

```fish
# 1. Generate a dedicated ed25519 keypair (no passphrase — oven runs as root)
set keypath /tmp/oven-to-lith
ssh-keygen -t ed25519 -f $keypath -N "" -C "oven@aesthetic.computer → lith"

# 2. Authorize the public key on lith (write-access to /opt/ac/system/public/papers.aesthetic.computer/)
ssh -i ~/.config/sops/age/ac-lith/id_rsa root@lith.aesthetic.computer "cat >> /root/.ssh/authorized_keys" < $keypath.pub

# 3. Copy the private key to oven (find OVEN_HOST from oven/deploy.fish)
set oven_host (set -q OVEN_HOST; and echo $OVEN_HOST; or echo "<oven droplet IP>")
scp $keypath root@$oven_host:/root/.ssh/oven-to-lith
ssh root@$oven_host "chmod 600 /root/.ssh/oven-to-lith"

# 4. Pre-seed oven's known_hosts so rsync doesn't prompt on first run
ssh root@$oven_host "ssh-keyscan -t ed25519 lith.aesthetic.computer > /root/.ssh/oven-known-hosts"

# 5. Wipe the local copy
rm $keypath $keypath.pub

# 6. Smoke test from oven
ssh root@$oven_host "rsync -av --dry-run -e 'ssh -i /root/.ssh/oven-to-lith -o UserKnownHostsFile=/root/.ssh/oven-known-hosts' /tmp/ root@lith.aesthetic.computer:/tmp/oven-smoke/"
```

If step 6 prints rsync output without errors, the key works.

## Deploy

After verifying the key, sync the updated oven source and restart:

```fish
oven/sync-source.sh   # rsyncs oven/*.mjs to /opt/oven/ac-source/
ssh root@$oven_host "systemctl restart oven"
```

Then trigger a papers build:

```fish
curl -X POST https://oven.aesthetic.computer/papers-build
```

Watch the logs at `oven.aesthetic.computer/papers-build/<id>?logs=true` —
you should see `RSYNC: pushing PDFs + platter to root@lith.aesthetic.computer`
followed by per-file transfer lines, then the small metadata commit + push.

## After this is verified working

Then it's safe to do the local cleanup commit:

```fish
echo "system/public/papers.aesthetic.computer/*.pdf" >> .gitignore
echo "system/public/papers.aesthetic.computer/platter.html" >> .gitignore  # also a build artifact
echo "system/public/papers.aesthetic.computer/index.html"   >> .gitignore  # also a build artifact
git ls-files system/public/papers.aesthetic.computer/ \
  | grep -E '\.(pdf|html)$' \
  | xargs git rm --cached
git commit -m "gitignore: stop tracking papers deploy artifacts (oven rsyncs them)"
git push
```

lith's webhook fires on this push, lith `git pull` removes the PDFs from
disk, and the next oven cycle (~60s later) restores them via rsync.
Expect a brief flicker on `papers.aesthetic.computer` URLs in the
interval — if that's unacceptable, trigger a manual oven rebuild
immediately after the push.

## Rollback

If rsync starts misbehaving, revert `oven/papers-builder.mjs` to the
pre-Phase-2 version (`commitAndPushPDFs`), sync-source + restart oven.
PDFs will start re-entering git again, but everything keeps working.
