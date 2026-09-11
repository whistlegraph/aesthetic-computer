# session-server autopublish

The session server redeploying itself when its own code lands on `main`.

lith redeploys the moment a push arrives, because a GitHub webhook calls it.
This box has no such door — it sits behind DigitalOcean with nothing listening
but the game socket — so deploying it was a thing somebody had to remember to
do by hand. They did not always remember: a Mongo credential rotation once sat
undeployed long enough that chat died days later, and "is it deployed?" was a
question you could only answer by asking a person.

So the box watches instead of being told.

## What it does, every 60 seconds

1. `git fetch origin main`.
2. Nothing moved → exit.
3. Something moved but nothing under `session-server/` or `shared/` → exit.
   The repo is a monorepo and most of what lands in it has nothing to do with
   the process running here; restarting for a piece or a site change would drop
   every live chat and match for nothing. `shared/` counts because
   `session.mjs` imports it.
4. Otherwise run [`../deploy-remote.sh`](../deploy-remote.sh) — the same script
   `npm run session:publish` streams over SSH, with the same `npm ci`, the same
   150-second health gate on `:8889`, and the same automatic rollback to the
   previous commit if the server does not come up.

`flock -n` means a hand deploy and a timer tick never both restart the service:
the loser gives up rather than queueing, because whatever the winner lands will
be at or past this tip anyway and the next tick re-checks.

## How long it takes

Up to a minute of polling here, on top of up to a minute of lith's
knot→GitHub mirror ([`lith/mirror`](../../lith/mirror)), because this box pulls
from the GitHub side. **Two minutes from `compush` to a live relay, worst
case.** No key is shared anywhere and no inbound port is opened.

## Deploying by hand still works

`npm run session:publish` is unchanged and is still the right thing for a
deploy you want to watch, or for a ref that is not `main`. The timer only
removes the obligation, not the option.

## First-time setup

On the droplet (`root@157.245.134.225`):

```bash
cd /home/aesthetic-computer && git pull
install -m 644 session-server/autopublish/session-autopublish.service \
  /etc/systemd/system/session-autopublish.service
install -m 644 session-server/autopublish/session-autopublish.timer \
  /etc/systemd/system/session-autopublish.timer
systemctl daemon-reload
systemctl enable --now session-autopublish.timer
```

## Reading it

```bash
systemctl list-timers session-autopublish        # when it last ran, when next
journalctl -u session-autopublish -n 50          # what it decided and did
systemctl disable --now session-autopublish.timer  # stop watching
```

Proven end to end on 2026-09-11: a `session-server/` commit pushed to knot
reached the box and redeployed it with nobody touching the box.

A tick with nothing to do logs nothing. A tick that deploys logs the same
`RESULT=` line the hand path prints — `ok:`, `rolledback:`, or `fail:stale*`
when the GitHub mirror has not caught up yet, which is not an unwell server,
just an early look.
