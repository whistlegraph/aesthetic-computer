#!/bin/bash

# Installation:
# sudo apt-get install unclutter
# sudo chmod 755 autostart.sh
# sudo cp autostart.desktop ~/.config/autostart/
#                           (may have to make this dir)
# reboot!

unclutter -idle 0 &
node /home/georgica/aesthetic.computer/video-sync/server.js &
sleep 3
chromium-browser --disable-pinch --kiosk /home/georgica/aesthetic.computer/video-sync/index.html 