# silo/datomic

Host-side infrastructure for the Datomic Pro instance that backs kidlisp (v1).
The Clojure sidecar that talks to this transactor lives at `/kidlisp-sidecar/`.

## Components on silo

- **Postgres** — Datomic storage backend. Datomic treats it as a KV blob store;
  the logical schema lives inside Datomic, not in Postgres.
- **Datomic transactor** — JVM process, single node, auto-restart via systemd.
- **pg_dump backup** — nightly, retained 14 days.

## Layout

    transactor/
      transactor.properties.template   merged with vault secrets at deploy
      datomic-transactor.service       systemd unit
    postgres/
      init.sql                         creates datomic db + role
      backup.fish                      pg_dump wrapper
    backup.cron                        cron entry (run nightly)
    deploy.fish                        installs/updates transactor + pg config

## One-time: add silo env vars

The silo dashboard needs to know where the sidecar lives and what admin
secret to present. The sidecar's `ADMIN_SECRET` was generated into
`aesthetic-computer-vault/kidlisp-datomic/sidecar.env.gpg`. Copy that value
into silo's existing env:

    cd /workspaces/aesthetic-computer/aesthetic-computer-vault
    fish vault-tool.fish unlock
    # ADMIN_SECRET=<hex>   ← read from kidlisp-datomic/sidecar.env
    # Append two lines to silo/.env:
    #   DATOMIC_SIDECAR_URL=http://127.0.0.1:8891
    #   DATOMIC_SIDECAR_ADMIN_SECRET=<hex>
    fish vault-tool.fish edit silo/.env
    fish vault-tool.fish lock

Silo proxies all `/api/datomic/*` requests to the sidecar with this header.
Sidecar binds to 127.0.0.1 on silo, so the URL is internal-only.

## End-to-end bring-up sequence

Run each step from your local terminal (not via Claude tool calls — gpg +
ssh keys need interactive agent access).

    # 1. Unlock vault
    fish aesthetic-computer-vault/vault-tool.fish unlock

    # 2. Append the two sidecar env vars to silo/.env
    fish silo/datomic/update-silo-env.fish

    # 3. Install JVM, Postgres, create datomic user + dirs on silo
    fish silo/datomic/bootstrap-silo.fish

    # 4. Download Datomic Pro (Apache 2.0) and ship the jar to silo
    #    The bootstrap script prints exact commands if the jar is absent.

    # 5. Deploy transactor config + systemd + backup cron
    fish silo/datomic/deploy.fish

    # 6. Build the Clojure sidecar uberjar and ship it
    fish kidlisp-sidecar/deploy.fish

    # 7. Redeploy silo to pick up the new env vars
    fish silo/deploy.fish

    # 8. Backfill Mongo kidlisp → Datomic (from silo, since sidecar is localhost)
    ssh -i aesthetic-computer-vault/home/.ssh/id_rsa root@silo.aesthetic.computer \
      "cd /opt/silo && CLIENT_SECRET=\$(grep CLIENT_SECRET /opt/kidlisp-sidecar/.env | cut -d= -f2) \
        node /opt/ac/system/backend/backfill-kidlisp-to-datomic.mjs"

    # 9. Verify the dashboard → datomic tab shows healthy transactor,
    #    schema installed, entity counts matching Mongo.

    # 10. Flip the feature flag to cut over.
    #     Edit aesthetic-computer-vault/lith/.env and add:  KIDLISP_DATOMIC=on
    #     Then:  cd lith && fish deploy.fish
    #     After this, store-kidlisp.mjs routes all traffic to Datomic.

    # 11. Lock the vault when done
    fish aesthetic-computer-vault/vault-tool.fish lock

## Individual runbook steps (reference)

- **First-time software install on silo**: handled by `bootstrap-silo.fish`
  (apt install temurin-21-jdk + postgresql, create datomic user, mkdir tree,
  create PG role + db, verify jar present).
- **Datomic jar acquisition**: not scripted — operator downloads
  `datomic-pro-<version>.zip` from https://docs.datomic.com/pro/ and scp's to
  `/tmp/` on silo. Bootstrap script prints the exact unzip/symlink commands.
- **Config rollout**: `deploy.fish` renders `transactor.properties.template`
  in `/dev/shm`, uploads to silo, restarts transactor via systemd.
- **Sidecar rollout**: `kidlisp-sidecar/deploy.fish` builds uberjar locally,
  decrypts `sidecar.env.gpg` into `/dev/shm`, scp's both to silo, restarts.
- **Backup cron**: installed at `/etc/cron.d/datomic-backup` by `deploy.fish`,
  runs 03:17 UTC daily, 14-day retention in `/var/backups/datomic/`.

## Ongoing ops

- **Logs**: `journalctl -u datomic-transactor -f`
- **Restart**: `systemctl restart datomic-transactor`
- **Backup now**: `fish /opt/datomic/backup.fish`
- **Restore**: stop transactor, `psql` the latest dump into a fresh db, restart.

## Non-goals

- No HA transactor in v1. Single node. If/when kidlisp traffic warrants it,
  Datomic Pro supports standby transactors with automatic failover.
- Not moving moods/paintings/handles/chat here — see root CLAUDE.md and the
  v1 plan.
