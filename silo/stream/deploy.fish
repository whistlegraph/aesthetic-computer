#!/usr/bin/env fish
# Deploy MediaMTX streaming relay to silo.aesthetic.computer
#
# Usage:
#   fish deploy.fish            # Full deploy (download binary + config + systemd)
#   fish deploy.fish --config   # Config-only update (no restart unless binary changed)

set RED '\033[0;31m'
set GREEN '\033[0;32m'
set YELLOW '\033[1;33m'
set NC '\033[0m'

set SCRIPT_DIR (dirname (status --current-filename))
set VAULT_DIR "$SCRIPT_DIR/../../aesthetic-computer-vault"
set SSH_KEY "$VAULT_DIR/home/.ssh/id_rsa"
set SILO_HOST "silo.aesthetic.computer"
set SILO_USER "root"
set REMOTE_DIR "/opt/mediamtx"
set MEDIAMTX_VERSION "1.12.2"
set MEDIAMTX_URL "https://github.com/bluenviron/mediamtx/releases/download/v$MEDIAMTX_VERSION/mediamtx_v{$MEDIAMTX_VERSION}_linux_amd64.tar.gz"

set CONFIG_ONLY false
if contains -- --config $argv
    set CONFIG_ONLY true
end

if not test -f $SSH_KEY
    echo -e "$RED x SSH key not found: $SSH_KEY$NC"
    exit 1
end

echo -e "$GREEN-> Testing SSH connection to $SILO_HOST...$NC"
if not ssh -i $SSH_KEY -o StrictHostKeyChecking=no -o ConnectTimeout=10 $SILO_USER@$SILO_HOST "echo ok" &>/dev/null
    echo -e "$RED x Cannot connect to $SILO_HOST$NC"
    exit 1
end

# Upload config
echo -e "$GREEN-> Uploading mediamtx.yml...$NC"
ssh -i $SSH_KEY $SILO_USER@$SILO_HOST "mkdir -p $REMOTE_DIR"
scp -i $SSH_KEY -o StrictHostKeyChecking=no \
    $SCRIPT_DIR/mediamtx.yml \
    $SILO_USER@$SILO_HOST:$REMOTE_DIR/

if test $CONFIG_ONLY = true
    echo -e "$GREEN-> Config-only: reloading mediamtx...$NC"
    ssh -i $SSH_KEY $SILO_USER@$SILO_HOST "systemctl reload mediamtx 2>/dev/null || systemctl restart mediamtx"
    echo -e "$GREEN   Done.$NC"
    exit 0
end

# Download + install binary on remote
echo -e "$GREEN-> Installing MediaMTX v$MEDIAMTX_VERSION on $SILO_HOST...$NC"
ssh -i $SSH_KEY $SILO_USER@$SILO_HOST "
    cd $REMOTE_DIR
    if test -f mediamtx && ./mediamtx --version 2>&1 | grep -q '$MEDIAMTX_VERSION'; then
        echo 'Already at v$MEDIAMTX_VERSION, skipping download'
    else
        curl -fsSL '$MEDIAMTX_URL' -o /tmp/mediamtx.tar.gz
        tar -xzf /tmp/mediamtx.tar.gz -C $REMOTE_DIR mediamtx
        rm /tmp/mediamtx.tar.gz
        chmod +x $REMOTE_DIR/mediamtx
        echo 'Installed MediaMTX v$MEDIAMTX_VERSION'
    fi
"

# Create systemd service
echo -e "$GREEN-> Setting up systemd service...$NC"
ssh -i $SSH_KEY $SILO_USER@$SILO_HOST "
cat > /etc/systemd/system/mediamtx.service << 'UNIT'
[Unit]
Description=MediaMTX streaming relay (AC Native)
After=network-online.target
Wants=network-online.target

[Service]
Type=simple
ExecStart=/opt/mediamtx/mediamtx /opt/mediamtx/mediamtx.yml
ExecReload=/bin/kill -HUP \$MAINPID
Restart=on-failure
RestartSec=5
LimitNOFILE=65536

[Install]
WantedBy=multi-user.target
UNIT

systemctl daemon-reload
systemctl enable mediamtx
systemctl restart mediamtx
sleep 2
systemctl is-active mediamtx
"

set STATUS $status
if test $STATUS -eq 0
    echo -e "$GREEN   MediaMTX is running.$NC"
else
    echo -e "$RED x MediaMTX failed to start. Check logs:$NC"
    echo -e "$YELLOW   ssh -i $SSH_KEY $SILO_USER@$SILO_HOST journalctl -u mediamtx -n 30$NC"
    exit 1
end

# Open firewall ports
echo -e "$GREEN-> Opening firewall ports (9000/udp SRT, 1935/tcp RTMP, 8888/tcp HLS)...$NC"
ssh -i $SSH_KEY $SILO_USER@$SILO_HOST "
    if command -v ufw >/dev/null 2>&1; then
        ufw allow 9000/udp comment 'MediaMTX SRT'
        ufw allow 1935/tcp comment 'MediaMTX RTMP'
        ufw allow 8888/tcp comment 'MediaMTX HLS'
    elif command -v firewall-cmd >/dev/null 2>&1; then
        firewall-cmd --permanent --add-port=9000/udp
        firewall-cmd --permanent --add-port=1935/tcp
        firewall-cmd --permanent --add-port=8888/tcp
        firewall-cmd --reload
    else
        echo 'No firewall tool found — ports may already be open'
    fi
"

echo ""
echo -e "$GREEN Done. MediaMTX relay deployed to $SILO_HOST$NC"
echo -e "$GREEN   SRT push:  srt://$SILO_HOST:9000?streamid=publish:/<channel>$NC"
echo -e "$GREEN   RTMP pull: rtmp://$SILO_HOST:1935/<channel>$NC"
echo -e "$GREEN   HLS view:  http://$SILO_HOST:8888/<channel>$NC"
