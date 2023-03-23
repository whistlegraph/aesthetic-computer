#!/bin/bash

# Installation:
# chmod 755 autostart-child.sh
# cp autostart-child.service /etc/systemd/system/
# sudo systemctl enable autostart-child.service

chromium-browser --kiosk '/home/georgica/aesthetic.computer/video-sync/index-child.html' 