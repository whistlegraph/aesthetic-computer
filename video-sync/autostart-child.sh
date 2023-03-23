#!/bin/bash

# Installation:
# sudo chmod 755 autostart-child.sh
# sudo cp autostart-child.desktop ~/.config/autostart/
#                                 (may have to make this dir)
# reboot!

chromium-browser --disable-pinch --kiosk '/home/georgica/aesthetic.computer/video-sync/index-child.html' 