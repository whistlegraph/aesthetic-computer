# Lith recovery

DigitalOcean droplet `ac-lith` (`561482364`, `sfo3`) uses daily disk backups,
with seven days of retention and an 08:00–12:00 UTC start window. At the
current $24/month droplet price, daily backups add $7.20/month.
An on-demand snapshot is also copied to `nyc3` before maintenance.
Snapshots cost $0.06/GB/month; adding a region does not increase that charge.
See [backup pricing](https://docs.digitalocean.com/products/backups/details/pricing/)
and [snapshot pricing](https://docs.digitalocean.com/products/snapshots/details/pricing/).

`lith-backup-prepare.timer` runs at 07:30 UTC. It exports PostgreSQL roles and
every connectable non-template database into root-only `/var/backups/lith/`.
Custom-format exports are checked with `pg_restore --list` and checksummed
before publication. Completed exports older than seven full days are pruned
only after a successful run. The disk backup then includes these consistent
exports alongside the rest of the server. Failed exports fail the systemd
unit; monitor its status and the DigitalOcean backup inventory.

Install as root from `/opt/ac`:

```sh
install -m 755 lith/backup-prepare.sh /usr/local/sbin/lith-backup-prepare
install -m 644 lith/lith-backup-prepare.{service,timer} /etc/systemd/system/
systemctl daemon-reload
systemctl enable --now lith-backup-prepare.timer
systemctl start lith-backup-prepare.service
systemctl status lith-backup-prepare.service lith-backup-prepare.timer
```

For recovery, create a replacement droplet from the chosen snapshot or daily
backup. Keep it isolated from public traffic and outbound mail initially:
restored credentials and timers are live, including repository mirroring.
Stop Lith, mail, feed, IPFS, Oxigraph and mirror/sync timers until the clone is
ready. Check Caddy routing, SSH access, environment files, external MongoDB
and Spaces connectivity, and local databases before switching DNS.

If PostgreSQL needs logical recovery, select a completed export directory,
verify `sha256sum -c SHA256SUMS`, and restore roles from `globals.sql` only
into a fresh cluster. Create the target database and restore its `.dump`
with `pg_restore --exit-on-error`. Test into a uniquely named empty database
first; never use `--clean` against production as a validation step.
The globals file includes sensitive role information: keep all exports private.

These backups cover Lith's local disk. External MongoDB, Spaces, other
droplets, and attached volumes need their own backups. This is recovery
capacity, not automatic failover. Daily backups remain in the source region;
the additional-region snapshot is a manual checkpoint and does not refresh
with each daily backup. Refresh it before substantial maintenance and remove
superseded snapshots only after verifying the replacement.
