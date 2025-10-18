# RDP Connection Guide for false.work UE5 Builder

## Quick Connect

**From your dev container:**
```bash
cd /workspaces/aesthetic-computer/false.work/unreal-builder/scripts
./connect-rdp.sh
```

This will show you the command for your host OS.

## Connection Details

```
IP: 136.114.176.135
Username: builduser
Password: (in aesthetic-computer-vault/false.work/ue5-builder.env)
```

## Connect from Host Machine

### 🪟 Windows
```cmd
mstsc /v:136.114.176.135
```
Or search for "Remote Desktop Connection" in Start menu.

### 🍎 macOS
```bash
open rdp://builduser@136.114.176.135
```
Or download **Microsoft Remote Desktop** from App Store (recommended).

### 🐧 Linux (Fedora/RHEL)

**Option 1: GNOME Connections (Recommended for GNOME desktops)**
```bash
# Install if needed
sudo dnf install gnome-connections

# Launch directly
gnome-connections rdp://136.114.176.135

# Or open via GUI:
# 1. Open 'Connections' app (search in Activities)
# 2. Click '+' for New Connection
# 3. Select 'RDP'
# 4. Enter: 136.114.176.135, builduser, password
# 5. Connect and save for future use
```

**Benefits:**
- Native GNOME app (clean UI)
- Simpler than Remmina
- Saves connections easily
- Modern and well-maintained

**Option 2: Remmina (Feature-rich alternative)**
```bash
# Already installed!
remmina -c rdp://builduser@136.114.176.135
```

Or launch Remmina and:
1. Click "+"
2. Protocol: RDP
3. Server: 136.114.176.135
4. Username: builduser
5. Password: (from vault)
6. Save and connect

**Option 3: xfreerdp (Command-line)**
```bash
# Already installed with Remmina
xfreerdp /v:136.114.176.135 /u:builduser /cert:ignore /scale:140
```

**Comparison:**

| Client | Best For | Pros | Cons |
|--------|----------|------|------|
| GNOME Connections | GNOME users | Simple, native, modern | Fewer advanced options |
| Remmina | Power users | Feature-rich, protocols | More complex UI |
| xfreerdp | Scripts/CLI | Fast, scriptable | No GUI |

## Troubleshooting

### "Cannot connect" / "Connection refused"

Check VM is running:
```bash
gcloud compute instances describe ue5-builder-falsework \
  --zone=us-central1-a \
  --format="value(status)"
```

If stopped, start it:
```bash
gcloud compute instances start ue5-builder-falsework --zone=us-central1-a
# Wait 60 seconds for boot
sleep 60
```

### "Password incorrect"

Get the current password:
```bash
cat ~/aesthetic-computer-vault/false.work/ue5-builder.env | grep VM_PASSWORD
```

Or reset it:
```bash
gcloud compute reset-windows-password ue5-builder-falsework \
  --zone=us-central1-a \
  --user=builduser
```

### "Certificate error" / "Security warning"

This is normal for first connection. Choose:
- ✅ "Accept" or "Trust"
- ✅ Check "Don't ask again for this host"

### Slow connection / lag

1. Check your internet speed
2. Try lower color depth in Remmina (Settings → Color depth → 16 bit)
3. Disable desktop effects in Windows on the VM

### Black screen after connect

Wait 10-20 seconds - Windows may be loading. If still black:
1. Disconnect
2. Wait 30 seconds
3. Reconnect

## Tips for Best Experience

### GNOME Connections Settings (Recommended)
- **Fullscreen:** Enable for best experience
- **Quality:** Automatic (it handles this well)
- **Remember connection:** Save for quick access
- **Keyboard shortcuts:** Automatically mapped

### Remmina Settings (If Using Remmina)
- **Resolution:** Fit window
- **Color depth:** 32-bit (or 16-bit for slower connections)
- **Compression:** Enable
- **Quality:** Good or Best
- **Sound:** Off (saves bandwidth)

### Windows VM Settings
Once connected, optimize for remote work:
1. Disable animations: Settings → System → About → Advanced system settings → Performance → "Adjust for best performance"
2. Disable Windows Search indexing: Services → Windows Search → Stop
3. Set power plan to "High Performance"

## Using RDP Files

If you have a `.rdp` file:

**Windows:**
```cmd
mstsc your-file.rdp
```

**Linux:**
```bash
xfreerdp your-file.rdp
```

## Shortcuts While Connected

### Windows Key Combinations
- `Ctrl+Alt+End` - Ctrl+Alt+Del on remote
- `Ctrl+Alt+Break` - Toggle fullscreen
- `Ctrl+Alt+Home` - Show connection bar

### Disconnect vs Logout
- **Disconnect:** Keeps your session running (recommended)
- **Logout:** Closes all programs

## Security Notes

- Always disconnect when done (don't leave it logged in)
- Use strong password (already set)
- VM automatically stops overnight if configured
- Firewall only allows RDP from internet (port 3389)

## Performance Monitoring

Once connected, check VM performance:
1. Open Task Manager (Ctrl+Shift+Esc)
2. Check CPU, RAM, Disk usage
3. During builds, CPU should be near 100% (this is good!)

## File Transfer

### To VM (from your machine):
- Drag and drop files (works in Remmina)
- Or use gcloud: `gcloud compute scp local-file ue5-builder-falsework:C:\`

### From VM (to your machine):
- Copy/paste text works
- Or use gcloud: `gcloud compute scp ue5-builder-falsework:C:\file local-path`

## Next Steps After Connecting

See: `NEXT-STEPS.md` for what to do inside the VM.
