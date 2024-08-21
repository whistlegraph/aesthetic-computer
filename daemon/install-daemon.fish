#!/usr/bin/env fish

# Get the current user's username
set username (whoami)

# Ensure the required directories exist
mkdir -p ~/.config/systemd/user

# Create the service file
echo "[Unit]
Description=Aesthetic Daemon Service

[Service]
ExecStart=/usr/bin/node /home/$username/aesthetic-computer/daemon/daemon.js
Restart=always
Environment=NODE_ENV=production
Environment=PATH=/home/linuxbrew/.linuxbrew/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
WorkingDirectory=/home/$username/aesthetic-computer/daemon/
ExecReload=/bin/kill -HUP \$MAINPID

[Install]
WantedBy=default.target" > ~/.config/systemd/user/aesthetic-daemon.service

# Create the path unit file
echo "[Unit]
Description=Watch aesthetic daemon.js for changes

[Path]
PathChanged=/home/$username/aesthetic-computer/daemon/daemon.js

[Install]
WantedBy=default.target" > ~/.config/systemd/user/aesthetic-daemon.path

# Reload the systemd manager configuration
systemctl --user daemon-reload

# Ensure the script has execute permissions
chmod +x /home/$username/aesthetic-computer/ssl-dev/fedora-install.fish

# Enable the service and the path unit
systemctl --user enable aesthetic-daemon.service
systemctl --user enable aesthetic-daemon.path

# Start the service and the path unit
systemctl --user start aesthetic-daemon.service
systemctl --user start aesthetic-daemon.path

# Restart the service if it is already running
systemctl --user restart aesthetic-daemon.service

echo "😈 Aesthetic Daemon systemd service installed and started successfully!"
