#!/bin/bash

# Installation:
# sudo chmod 755 autostart.sh
# sudo cp autostart.desktop ~/.config/autostart/
#                           (may have to make this dir)
# reboot!

node /home/georgica/aesthetic.computer/video-sync/server.js &
sleep 3
chromium-browser --disable-pinch --kiosk /home/georgica/aesthetic.computer/video-sync/index.html 