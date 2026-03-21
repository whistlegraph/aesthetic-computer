# 1v1 Piece: Host Info & QR Code Display

## Overview

Add a persistent QR code and host IP address display to the 1v1 multiplayer FPS piece so other users can easily join the game by scanning. Additionally, pass host machine identity (from vault's `machines.json`) through the stack so we know if we're running on the MacBook, ThinkPad, Windows Tower, etc.

## Current State Analysis

### How QR Codes Work in AC

1. **QR Library**: `system/public/aesthetic.computer/dep/@akamfoad/qr/qr.mjs`
   - Usage: `import { qrcode as qr } from "../dep/@akamfoad/qr/qr.mjs"`
   - Generate: `cells = qr(url).modules` returns a 2D boolean array

2. **Example: share.mjs** ([share.mjs](../system/public/aesthetic.computer/disks/share.mjs))
   ```javascript
   import { qrcode as qr } from "../dep/@akamfoad/qr/qr.mjs";
   
   // In boot():
   url = net.lan ? net.lan : `https://${net.host}`;
   cells = qr(url).modules;
   
   // In paint():
   for (let y = 0; y < cells.length; y++) {
     for (let x = 0; x < cells.length; x++) {
       const black = cells[y][x];
       ink(black ? 0 : 255).box(ox + x * scale, oy + y * scale, scale);
     }
   }
   ```

3. **Example: artery-tui.mjs** ([artery-tui.mjs#L902](../artery/artery-tui.mjs#L902))
   - Uses `qrcode-terminal` npm package for terminal display
   - Reads `HOST_IP` from environment variable
   ```javascript
   async generateLanQrCode() {
     const hostIp = process.env.HOST_IP;
     const url = `https://${hostIp}:8888`;
     qrcode.generate(url, { small: true });
   }
   ```

### How Host Machine Identity Works

1. **Vault machines.json** ([machines.json](../aesthetic-computer-vault/machines.json))
   ```json
   {
     "machines": {
       "jeffrey-windows": { "label": "🪟 Jeffrey's Windows Tower", "emoji": "🗼" },
       "jeffrey-macbook": { "label": "🍎 Jeffrey's MacBook", "emoji": "💻" }
     },
     "detection": {
       "windows": { "defaultMachine": "jeffrey-windows" },
       "darwin": { "defaultMachine": "jeffrey-macbook" }
     }
   }
   ```

2. **Artery-TUI Detection** ([artery-tui.mjs#L802](../artery/artery-tui.mjs#L802))
   ```javascript
   detectHostInfo() {
     // SSH to host.docker.internal to detect OS
     const hostOS = execSync('ssh me@host.docker.internal "uname -s"');
     // Map to machine from vault
     const machines = JSON.parse(fs.readFileSync('aesthetic-computer-vault/machines.json'));
   }
   ```

3. **Environment Flow**
   - `entry.fish` sets `HOST_IP` environment variable
   - `devcontainer.env` in vault stores persistent settings
   - Container can SSH to `host.docker.internal` to detect host OS

### Current Networking in 1v1.mjs

- Uses session server for multiplayer state
- Has `net` API available in boot/act/paint
- `net.host` contains current hostname
- `net.lan` contains LAN URL if in dev mode

## Implementation Plan

### Phase 1: Display QR Code in 1v1 Piece ✅ IMPLEMENTED

**Files modified:**
- [system/public/aesthetic.computer/disks/1v1.mjs](../system/public/aesthetic.computer/disks/1v1.mjs)

**Changes:**

1. **Add QR code import and state** (at top of file)
   ```javascript
   import { qrcode as qr } from "../dep/@akamfoad/qr/qr.mjs";
   
   let joinQRCells = null;  // QR code cells for join URL
   let joinURL = null;      // The actual URL to display
   let hostMachineInfo = null; // Host machine identity
   ```

2. **Generate QR in boot()** (add to boot function)
   ```javascript
   // Generate join URL and QR code
   if (net.lan) {
     joinURL = `${net.lan}/1v1`;
   } else if (net.host.startsWith("localhost")) {
     joinURL = "https://local.aesthetic.computer/1v1";
   } else {
     joinURL = `https://${net.host}/1v1`;
   }
   joinQRCells = qr(joinURL).modules;
   
   // Store host machine info if available
   hostMachineInfo = net.devIdentity || null;
   ```

3. **Draw QR overlay in paint()** (add helper function)
   ```javascript
   function drawJoinQR(api, screen) {
     if (!joinQRCells) return;
     
     const { ink, box, write } = api;
     const hudFont = "MatrixChunky8";
     
     // Position in bottom-right corner
     const scale = 2;  // Small but scannable
     const size = joinQRCells.length * scale;
     const margin = 8;
     const ox = screen.width - size - margin;
     const oy = screen.height - size - margin - 20; // Leave room for text
     
     // Semi-transparent background
     ink(0, 0, 0, 180).box(ox - 4, oy - 4, size + 8, size + 28);
     
     // Draw QR code
     for (let y = 0; y < joinQRCells.length; y++) {
       for (let x = 0; x < joinQRCells.length; x++) {
         ink(joinQRCells[y][x] ? 0 : 255).box(
           ox + x * scale,
           oy + y * scale,
           scale
         );
       }
     }
     
     // Draw URL text below
     const displayIP = joinURL.replace('https://', '').replace('/1v1', '');
     ink(255, 255, 0).write(displayIP, { x: ox, y: oy + size + 4 }, 
       undefined, undefined, false, hudFont);
     
     // Show host machine label if available
     if (hostMachineInfo?.hostLabel) {
       ink(200, 200, 200).write(hostMachineInfo.hostLabel, 
         { x: ox, y: oy + size + 14 }, undefined, undefined, false, hudFont);
     }
   }
   ```

4. **Call from paint()** (at end of paint function, after all 3D rendering)
   ```javascript
   drawJoinQR({ ink, box, write }, screen);
   ```

### Phase 2: Pass Host Machine Identity Through Stack

**Goal:** Make the host machine name (MacBook, ThinkPad, etc.) available to pieces in dev/local mode.

**Files to modify:**
- [system/public/aesthetic.computer/lib/disk.mjs](../system/public/aesthetic.computer/lib/disk.mjs)
- [system/public/aesthetic.computer/bios.mjs](../system/public/aesthetic.computer/bios.mjs)  
- [session-server/session.mjs](../session-server/session.mjs)

**Approach Options:**

#### Option A: Session Server Provides Host Info (Recommended)

1. **Session server reads machines.json on startup**
   ```javascript
   // session.mjs - at startup
   let hostMachineInfo = null;
   try {
     const machinesPath = './aesthetic-computer-vault/machines.json';
     if (fs.existsSync(machinesPath)) {
       const machines = JSON.parse(fs.readFileSync(machinesPath, 'utf8'));
       // Detect current machine based on hostname or HOST_IP
       hostMachineInfo = detectCurrentMachine(machines);
     }
   } catch (e) {}
   ```

2. **Include in `dev:identity` message**
   ```javascript
   // session.mjs - when sending dev:identity
   pack("dev:identity", {
     letter,
     hostName: HOST_NAME,
     hostMachine: hostMachineInfo,  // { label, emoji, os }
     lanUrl: `https://${HOST_IP}:8888`
   })
   ```

3. **disk.mjs stores it in net.devIdentity**
   ```javascript
   // Already exists - receives dev:identity messages
   if (type === "dev:identity") {
     $commonApi.net.devIdentity = content;
   }
   ```

#### Option B: Direct Environment Variable

1. **entry.fish exports HOST_MACHINE_LABEL**
   ```fish
   # Detect host OS and look up in machines.json
   set -gx HOST_MACHINE_LABEL "🍎 Jeffrey's MacBook"
   ```

2. **Netlify dev server passes to client via initial page load**
   ```javascript
   // In index.html or boot sequence
   window.AC_HOST_MACHINE = "{{ HOST_MACHINE_LABEL }}";
   ```

### Phase 3: Toggle & Polish

1. **Add keyboard toggle** (in act function)
   ```javascript
   if (e.is("keyboard:down:q")) showJoinQR = !showJoinQR;
   ```

2. **Only show in local/dev mode** (in paint)
   ```javascript
   if (showJoinQR && (net.host.startsWith("localhost") || net.lan)) {
     drawJoinQR({ ink, box, write }, screen);
   }
   ```

3. **Consider different positions for mobile vs desktop**

## Testing Checklist

- [ ] QR code renders in bottom-right of 1v1 piece
- [ ] QR code is scannable with phone camera
- [ ] Scanning takes user to correct 1v1 session
- [ ] IP address is readable below QR code
- [ ] Host machine label shows (if running from known machine)
- [ ] QR hides when 'Q' is pressed
- [ ] Works in both local dev and production modes
- [ ] Doesn't interfere with gameplay or HUD elements

## Dependencies

- `@akamfoad/qr` - Already used in share.mjs, no new deps needed
- `MatrixChunky8` font - Already imported in 1v1.mjs for gamepad HUD

## Related Files

| File | Purpose |
|------|---------|
| [share.mjs](../system/public/aesthetic.computer/disks/share.mjs) | Reference implementation of QR code display |
| [artery-tui.mjs](../artery/artery-tui.mjs#L902) | HOST_IP detection and QR generation |
| [machines.json](../aesthetic-computer-vault/machines.json) | Host machine definitions |
| [entry.fish](../.devcontainer/entry.fish) | Container environment setup |
| [disk.mjs](../system/public/aesthetic.computer/lib/disk.mjs) | devIdentity message handling |
| [session.mjs](../session-server/session.mjs) | Session server dev mode features |

## Open Questions

1. Should QR always be visible or start hidden?
2. Best position that doesn't obscure gameplay?
3. Should the QR update if the session moves to a different room?
4. Include room ID in URL for direct join to specific match?

## Future Enhancements

- Animate QR code in/out with a pulse effect
- Add "SCAN TO JOIN" label with blink animation  
- Generate unique room codes for private matches
- Show player count / waiting status near QR
