video-sync, 23.03.23.06.14
Created for Georgica Pettus' `Screenplay` work.

💾 Installing `video-sync` on each Raspberry Pi 2 systems:

  0. `cd` into this directory.

  1. Install NodeJS
    `curl -fsSL https://deb.nodesource.com/setup_lts.x | sudo -E bash -`
    `sudo apt install nodejs`
    
  2. Install dependencies
    `npm install`

  3. Start Scripts
    Follow the instructions in `autostart.sh` or `autostart-child.sh`.

  4. Networking (Setting up fixed IP addresses)
    `sudo vi /etc/dhcpcd.conf` and add the bottom entries to each...

    # Parent
    interface eth0
    static ip_address=192.168.1.1

    # Child
    interface eth0
    static ip_address=192.168.1.2

🌟 Future Improvements

  - [] `index.html` and `index-child.html` are mostly redundant.