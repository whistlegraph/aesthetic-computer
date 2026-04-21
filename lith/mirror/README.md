# lith mirror — knot ↔ GitHub bidirectional sync

Keeps `main` in lockstep between the two remotes for
`aesthetic.computer/core`:

- **knot** (`knot.aesthetic.computer:aesthetic.computer/core`, Tangled) —
  what `lith` deploy pulls from.
- **GitHub** (`whistlegraph/aesthetic-computer`) — what `session-server`
  deploy pulls from, plus Claude/tools usage.

Runs as a systemd timer on `lith.aesthetic.computer`, every 60 seconds.
Idempotent: exits 0 when the tips match, pushes the ahead side to the
behind side otherwise, and exits 2 with a warning when the tips truly
diverged (requires manual merge).

## Files in this directory

- [`mirror.sh`](./mirror.sh) — the bidirectional sync script.
- [`ac-mirror.service`](./ac-mirror.service) — systemd oneshot unit.
- [`ac-mirror.timer`](./ac-mirror.timer) — every-60s trigger.

## First-time setup

On the lith host:

```sh
# 1. Bare clone (fetched over anon HTTPS; push goes via SSH keys below).
mkdir -p /opt/ac-mirror
git clone --bare https://knot.aesthetic.computer/aesthetic.computer/core \
  /opt/ac-mirror/core
cd /opt/ac-mirror/core
git remote rename origin knot
git remote set-url --push knot git@knot.aesthetic.computer:aesthetic.computer/core
git remote add github https://github.com/whistlegraph/aesthetic-computer.git
git remote set-url --push github git@github.com:whistlegraph/aesthetic-computer.git

# 2. SSH keys (ed25519).
#    /root/.ssh/knot_push    ← copy of the vault's home/.ssh/tangled
#                              (the key registered on @jeffrey's
#                              Tangled account, allowed to push).
#    /root/.ssh/github_mirror ← fresh ed25519 keypair generated on lith;
#                              public half registered as a repo deploy key
#                              with write access on
#                              whistlegraph/aesthetic-computer,
#                              encrypted private copy in
#                              aesthetic-computer-vault/lith/mirror/.
# Both files must be mode 600.

# 3. Pin host keys to avoid interactive prompts.
ssh-keyscan -t ed25519,rsa knot.aesthetic.computer >> /root/.ssh/known_hosts
ssh-keyscan -t ed25519,rsa github.com             >> /root/.ssh/known_hosts
sort -u /root/.ssh/known_hosts -o /root/.ssh/known_hosts

# 4. Install the script + units and enable the timer.
install -m 755 mirror.sh         /opt/ac-mirror/mirror.sh
install -m 644 ac-mirror.service /etc/systemd/system/ac-mirror.service
install -m 644 ac-mirror.timer   /etc/systemd/system/ac-mirror.timer
systemctl daemon-reload
systemctl enable --now ac-mirror.timer
```

## Observe / debug

```sh
systemctl list-timers ac-mirror.timer
journalctl -u ac-mirror -n 50
# In sync = no output per run. A sync push logs one line:
#   2026-04-20T22:31:27+00:00 → knot behind; pushing <sha> to knot.
```

## Manual force

```sh
# Run immediately (the timer fires hourly-ish otherwise on boot).
systemctl start ac-mirror.service
```

## Divergent heads

If both sides received independent commits (true fork), the script exits
`2` and logs:

```
⚠️  divergent heads: knot=<sha> github=<sha> — skipping (manual resolution required)
```

Resolve by pulling both locally, merging with `git merge`, and pushing
the merge commit. The mirror will then see both sides equal the merge
tip and go back to green.

## Why this instead of GitHub Actions?

A `.github/workflows/mirror-to-knot.yml` was tried first but:

1. GitHub Actions is billing-locked on the repo at the moment —
   the workflow never fires.
2. Tangled knot *also* reads `.github/workflows/*.yml` as pipelines,
   and sent failure emails about them.

The systemd timer on lith is free, runs even when GitHub is unavailable,
and keeps all credentials on a host we already control.
