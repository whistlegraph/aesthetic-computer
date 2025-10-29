# Secret Server Club - Exploration Notes

**Date:** October 29, 2025  
**Server Type:** Raspberry Pi (ARM64)  
**OS:** Debian-based Linux (Raspberry Pi OS)  
**Hostname:** ssc

## 🎭 Server Overview

This is a friend's local SSH server running on a Raspberry Pi, nicknamed the "Resonant Server" or "Secret Server Club."

## 👥 Users

The server has 7 user accounts (plus root):

1. **ssc** (UID 1000) - Primary/admin user with desktop environment
2. **whisper** (UID 1001)
3. **mtn** (UID 1002)
4. **ayz** (UID 1003)
5. **hal** (UID 1004)
6. **nat** (UID 1005)
7. **jeffrey** (UID 1006) - Our account!

All users have:
- Home directories with read permissions for others (`chmod 755`)
- Membership in `sudo` and `docker` groups
- Bash shell access

## 📂 Interesting Files & Notes

### User Messages
- `/home/ssc/Documents/memo.txt` - Collaborative memo with messages from users (whisper, alice mentions)
- `/home/nat/hiiinat.txt` - Friendly greeting file
- `/home/ssc/Documents/copyparty/files/SSCNotes1.md` - Markdown note about eating ASCII characters

### Scripts
Located in `/home/ssc/Documents/scripts/`:

1. **moongreeting.sh** - Displays moon phase and moonrise time with cute ASCII art
2. **secret-server-club.sh** - Random ASCII art banner generator (3 different styles)
3. **provision-users.sh** - User provisioning script for the "Resonant Server"
4. **provision-users-old.sh** - Previous version

## 🐳 Services Running

### Copyparty File Sharing
- Container: `copyparty/ac:latest`
- Port: `3923`
- Docker Compose config: `/home/ssc/Documents/copyparty/docker-compose.yml`
- Shared files: `/home/ssc/Documents/copyparty/files/`
- Content includes: images, audio files (music mixes), screenshots

### Network Services
- SSH on port 22
- RPC on port 111
- CUPS printing on port 631 (localhost)
- Tailscale VPN (listening on Tailscale IPs)

## 🎨 ASCII Art Styles

The server features multiple ASCII art styles for "SECRET SERVER CLUB":

1. **Classic monospaced** - Multi-line elegant font
2. **Asterisk/slash design** - Modern blocky style with `**` and `/*`
3. **Comma-dash style** - Simpler retro look with `,-` characters

## 💾 Storage

- Root partition: 917GB total, 7.9GB used, 872GB available (~1% usage)
- Boot firmware: 510MB (17% used)
- Plenty of space available!

## 🌙 Moon Phase Feature

The moon greeting script fetches:
- Current moon phase (e.g., "First Quarter")
- Moonrise time
- Displays with scattered star/moon glyphs: `✧ ･ﾟ ｡ ✦ ° ˚ ☾ ⋆`

## 📊 System Info

```
Linux ssc 6.12.47+rpt-rpi-2712 #1 SMP PREEMPT Debian 1:6.12.47-1+rpt1
Architecture: aarch64 (ARM 64-bit)
Device: Raspberry Pi (likely Pi 5 based on kernel version)
```

## 🎪 Server Culture

This appears to be a cozy community server where friends:
- Share files via copyparty
- Leave messages for each other
- Enjoy fun ASCII art and moon phase greetings
- Have a "secret server club" vibe

## 🔒 Security Notes

- User provisioning script sets home directories to `755` (readable by others)
- Users can explore each other's home directories
- Collaborative, trust-based environment
- SSH key support available in provisioning script

---

*Note: Connection credentials stored separately in `aesthetic-computer-vault/.env` (not committed to git)*
