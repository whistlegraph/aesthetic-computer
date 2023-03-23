#!/bin/bash

# Installation to run this file on boot:
# chmod +x autostart.sh
# sudo cp autostart.sh /etc/init.d/
# sudo update-rc.d autostart.sh defaults

node /home/georgica/aesthetic.computer/video-sync/server.js &
sleep 3
chromium-browser --kiosk /home/georgica/aesthetic.computer/video-sync/index.html 