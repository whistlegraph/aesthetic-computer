#!/bin/bash

# Installation:
# chmod 755 autostart.sh
# cp autostart.service /etc/systemd/system/
# sudo systemctl enable autostart.service

node /home/georgica/aesthetic.computer/video-sync/server.js &
sleep 3
chromium-browser --kiosk /home/georgica/aesthetic.computer/video-sync/index.html 