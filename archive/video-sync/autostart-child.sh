#!/bin/bash

# Installation:
# sudo apt-get install unclutter
# sudo chmod 755 autostart-child.sh
# sudo cp autostart-child.desktop ~/.config/autostart/
#                                 (may have to make this dir)
# reboot!

unclutter -idle 0 &
chromium-browser --kiosk '/home/georgica/aesthetic.computer/video-sync/index-child.html' 