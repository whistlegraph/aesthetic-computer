#!/bin/bash
# heal-knot.sh — stop knot.aesthetic.computer wedging every quarter hour.
#
# Diagnosis (2026-10-10): the knot's cgroup is capped at MemoryHigh=1536M, and
# git-upload-pack for the multi-gigabyte core repo runs INSIDE that cgroup
# (a full clone takes ~3 min and gigabytes of pack memory). memory.events
# showed 213,905 "high" throttle events: once a clone is in flight the whole
# cgroup crawls, including `knot keys` which sshd calls for every git auth,
# so pushes hang and port 5555 stops answering. Not a leak — pack memory.
#
# Fix, applied as root over `ssh aesthetic-pds`:
#   1. cap git's pack memory in the bare repos (pack.windowMemory etc.), so a
#      clone of core costs a few hundred MB instead of gigabytes;
#   2. loosen the cgroup to what the 3.9 GB box can spare;
#   3. a watchdog timer that restarts knotserver when 5555 stops answering;
#   4. Restart=always so a crash never leaves it down.
# Run from a Mac:  bash toolchain/knot/heal-knot.sh
set -euo pipefail
ssh -o BatchMode=yes aesthetic-pds 'bash -s' <<'REMOTE'
set -euo pipefail
echo "== git pack memory caps on the bare repos"
for r in /home/git/repositories/*/ /home/git/repositories/*/*/; do
  [ -d "$r" ] || continue
  if git -C "$r" rev-parse --is-bare-repository >/dev/null 2>&1; then
    git -C "$r" config pack.threads 1
    git -C "$r" config pack.windowMemory 128m
    git -C "$r" config pack.deltaCacheSize 64m
    git -C "$r" config pack.packSizeLimit 512m
    git -C "$r" config core.packedGitWindowSize 32m
    git -C "$r" config core.packedGitLimit 256m
    git -C "$r" config core.bigFileThreshold 4m
    chown git:git "$r/config" 2>/dev/null || true
    echo "  capped $r"
  fi
done
echo "== cgroup: more room, and Restart=always"
mkdir -p /etc/systemd/system/knotserver.service.d
cat > /etc/systemd/system/knotserver.service.d/at-upkeep-memory.conf <<'UNIT'
[Service]
Environment=GOMEMLIMIT=768MiB
MemoryHigh=2600M
MemoryMax=3200M
Restart=always
RestartSec=3
UNIT
echo "== watchdog: restart when port 5555 stops answering"
cat > /usr/local/bin/knot-watchdog.sh <<'WD'
#!/bin/bash
code=$(curl -s -m 8 -o /dev/null -w "%{http_code}" http://127.0.0.1:5555/ || echo 000)
if [ "$code" = "000" ]; then
  logger -t knot-watchdog "port 5555 not answering (rss $(ps -o rss= -C knot | sort -n | tail -1 | tr -d ' ') kB); restarting knotserver"
  systemctl restart knotserver.service
fi
WD
chmod +x /usr/local/bin/knot-watchdog.sh
cat > /etc/systemd/system/knot-watchdog.service <<'UNIT'
[Unit]
Description=Restart the knot when it stops answering
[Service]
Type=oneshot
ExecStart=/usr/local/bin/knot-watchdog.sh
UNIT
cat > /etc/systemd/system/knot-watchdog.timer <<'UNIT'
[Unit]
Description=Knot health check every 30 s
[Timer]
OnBootSec=90
OnUnitActiveSec=30
AccuracySec=5
[Install]
WantedBy=timers.target
UNIT
systemctl daemon-reload
systemctl enable --now knot-watchdog.timer
systemctl restart knotserver.service
sleep 6
echo "== state"
systemctl show knotserver -p MemoryHigh -p MemoryMax -p Restart | tr '\n' ' '; echo
systemctl list-timers knot-watchdog.timer --no-pager | head -3
curl -s -m 5 -o /dev/null -w "knot http=%{http_code}\n" http://127.0.0.1:5555/
REMOTE
echo "== from here"
timeout 20 ssh -o BatchMode=yes -T git@knot.aesthetic.computer 2>&1 | head -1
