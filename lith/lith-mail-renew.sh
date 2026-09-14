#!/bin/sh
# lith-mail-renew — hand the Amail door its certificate.
#
# certbot keeps the Let's Encrypt cert for inbound.aesthetic.computer under
# /etc/letsencrypt, readable by root only. lith-mail runs as its own user, so
# after every issue/renewal (this is the certbot deploy hook) — and once at
# deploy — the pair is copied into /etc/lith-mail for that user, and the door
# is restarted to offer it. Idempotent; safe to run when there is no cert yet.
set -e
HOST=inbound.aesthetic.computer
LIVE=/etc/letsencrypt/live/$HOST
DEST=/etc/lith-mail

install -d -o lith-mail -g lith-mail -m 700 "$DEST"
if [ -f "$LIVE/fullchain.pem" ] && [ -f "$LIVE/privkey.pem" ]; then
  install -o lith-mail -g lith-mail -m 600 "$LIVE/fullchain.pem" "$DEST/fullchain.pem"
  install -o lith-mail -g lith-mail -m 600 "$LIVE/privkey.pem" "$DEST/privkey.pem"
  systemctl try-restart lith-mail 2>/dev/null || true
  echo "lith-mail: certificate for $HOST installed"
else
  echo "lith-mail: no certificate for $HOST yet"
fi
