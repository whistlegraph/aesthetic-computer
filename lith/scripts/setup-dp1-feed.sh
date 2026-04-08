#!/usr/bin/env bash
# setup-dp1-feed.sh — Install and configure dp1-feed-v2 on lith VPS
# Run once on the server to set up Go, Postgres, and the dp1-feed-v2 binary.
#
# Usage: bash setup-dp1-feed.sh
#
# Prerequisites: .env file at /opt/dp1-feed/.env with:
#   DP1_FEED_API_KEY=<your-api-key>
#   DP1_FEED_SIGNING_KEY_HEX=<64-hex-chars>
#   DP1_FEED_DATABASE_URL=<postgres-connection-string>

set -euo pipefail

LOG_TAG="[dp1-feed-setup]"
log() { echo "$LOG_TAG $*"; }

INSTALL_DIR="/opt/dp1-feed"
BUILD_DIR="/opt/dp1-feed-build"
GO_VERSION="1.24.2"

# --- 1. Install Go ---
if ! command -v go &>/dev/null || ! go version | grep -q "$GO_VERSION"; then
  log "Installing Go $GO_VERSION..."
  curl -fsSL "https://go.dev/dl/go${GO_VERSION}.linux-amd64.tar.gz" -o /tmp/go.tar.gz
  rm -rf /usr/local/go
  tar -C /usr/local -xzf /tmp/go.tar.gz
  rm /tmp/go.tar.gz
  export PATH="/usr/local/go/bin:$PATH"
  # Persist for future shells
  if ! grep -q '/usr/local/go/bin' /etc/profile.d/go.sh 2>/dev/null; then
    echo 'export PATH="/usr/local/go/bin:$PATH"' > /etc/profile.d/go.sh
  fi
  log "Go $(go version) installed."
else
  log "Go already installed: $(go version)"
fi

# --- 2. Install PostgreSQL ---
if ! command -v psql &>/dev/null; then
  log "Installing PostgreSQL..."
  apt-get update -qq
  apt-get install -y -qq postgresql postgresql-contrib
  systemctl enable postgresql
  systemctl start postgresql
  log "PostgreSQL installed."
else
  log "PostgreSQL already installed."
  systemctl is-active postgresql || systemctl start postgresql
fi

# --- 3. Create database and user ---
log "Setting up database..."
DB_PASS="${DP1_FEED_DB_PASSWORD:-$(openssl rand -hex 16)}"

sudo -u postgres psql -tc "SELECT 1 FROM pg_roles WHERE rolname='dp1feed'" | grep -q 1 || \
  sudo -u postgres psql -c "CREATE ROLE dp1feed WITH LOGIN PASSWORD '$DB_PASS';"

sudo -u postgres psql -tc "SELECT 1 FROM pg_database WHERE datname='dp1_feed'" | grep -q 1 || \
  sudo -u postgres psql -c "CREATE DATABASE dp1_feed OWNER dp1feed;"

log "Database ready. Password: $DB_PASS"

# --- 4. Create system user ---
if ! id dp1feed &>/dev/null; then
  useradd --system --no-create-home --shell /usr/sbin/nologin dp1feed
  log "Created dp1feed system user."
fi

# --- 5. Clone and build ---
log "Cloning dp1-feed-v2 and dp1-go..."
mkdir -p "$BUILD_DIR"

if [ -d "$BUILD_DIR/dp1-feed-v2" ]; then
  cd "$BUILD_DIR/dp1-feed-v2" && git pull --quiet
else
  git clone https://github.com/display-protocol/dp1-feed-v2.git "$BUILD_DIR/dp1-feed-v2"
fi

if [ -d "$BUILD_DIR/dp1-go" ]; then
  cd "$BUILD_DIR/dp1-go" && git pull --quiet
else
  git clone https://github.com/display-protocol/dp1-go.git "$BUILD_DIR/dp1-go"
fi

log "Building dp1-feed binary..."
cd "$BUILD_DIR/dp1-feed-v2"
CGO_ENABLED=0 go build -o dp1-feed ./cmd/server

# --- 6. Install binary and config ---
mkdir -p "$INSTALL_DIR/db"

cp "$BUILD_DIR/dp1-feed-v2/dp1-feed" "$INSTALL_DIR/dp1-feed"
cp -r "$BUILD_DIR/dp1-feed-v2/db/migrations" "$INSTALL_DIR/db/migrations"
cp /opt/ac/lith/dp1-feed-config.yaml "$INSTALL_DIR/config.yaml"

chown -R dp1feed:dp1feed "$INSTALL_DIR"

# --- 7. Create .env if not exists ---
if [ ! -f "$INSTALL_DIR/.env" ]; then
  cat > "$INSTALL_DIR/.env" <<ENVEOF
DP1_FEED_API_KEY=changeme
DP1_FEED_SIGNING_KEY_HEX=$(openssl rand -hex 32)
DP1_FEED_DATABASE_URL=\$(printf '%s://%s:%s@%s/%s' postgres dp1feed "\${DB_PASS}" localhost:5432 dp1_feed)
DP1_FEED_EXTENSIONS_ENABLED=true
DP1_FEED_PUBLIC_BASE_URL=https://feed.aesthetic.computer
ENVEOF
  chown dp1feed:dp1feed "$INSTALL_DIR/.env"
  chmod 600 "$INSTALL_DIR/.env"
  log "Created $INSTALL_DIR/.env — EDIT API_KEY and SIGNING_KEY_HEX before starting!"
fi

# --- 8. Install systemd service ---
cp /opt/ac/lith/dp1-feed.service /etc/systemd/system/dp1-feed.service
systemctl daemon-reload
systemctl enable dp1-feed

log ""
log "=== Setup complete ==="
log ""
log "Next steps:"
log "  1. Edit /opt/dp1-feed/.env with your real API_KEY and SIGNING_KEY_HEX"
log "  2. Start the service: systemctl start dp1-feed"
log "  3. Check status: systemctl status dp1-feed"
log "  4. Check logs: journalctl -u dp1-feed -f"
log "  5. Test: curl -s https://feed.aesthetic.computer/health"
log ""
log "To rebuild after updates:"
log "  cd $BUILD_DIR/dp1-feed-v2 && git pull && CGO_ENABLED=0 go build -o dp1-feed ./cmd/server"
log "  cp dp1-feed $INSTALL_DIR/dp1-feed && systemctl restart dp1-feed"
