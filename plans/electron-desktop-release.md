# Electron Desktop App Release Plan

## Overview
Release the Aesthetic Computer Electron app and create a download experience at `aesthetic.computer/desktop`.

---

## Phase 1: Commit & Push Current Changes ✅
- [x] Commit ac-electron zoom and width changes
- [x] Push to main

## Phase 2: Electron App Distribution
### Options for Hosting the App:
1. **GitHub Releases** (Recommended)
   - Use electron-builder's GitHub publish feature
   - Auto-updates support built-in
   - Free hosting for releases
   
2. **Netlify / Custom CDN**
   - Host DMG/ZIP in `system/public/assets/downloads/`
   - Simpler but no auto-update

### Build Artifacts:
- `Aesthetic Computer-0.1.0-universal.dmg` (macOS Universal)
- Future: Windows `.exe`, Linux `.AppImage`

### GitHub Release Setup:
1. Add `publish` config to `ac-electron/package.json`
2. Create GitHub personal access token for publishing
3. Run `npm run publish` to create release

---

## Phase 3: Create `desktop.mjs` Piece
A dedicated piece at `aesthetic.computer/desktop` that:
- Shows the Aesthetic Computer logo/branding
- Displays download button for macOS
- Shows "Coming Soon" for Windows/Linux
- Includes system requirements
- Links to GitHub releases page

### UI Elements:
- Title: "Aesthetic Computer Desktop"
- Subtitle: "The creative coding environment on your desktop"
- Download button (detects OS, shows appropriate download)
- Version number display
- GitHub link for all releases

---

## Phase 4: Update `prompt.mjs`
- Add `desktop` command to navigate to the desktop piece
- Similar to how other navigation commands work

---

## Phase 5: Future Enhancements
- [ ] Auto-update system (electron-updater)
- [ ] Code signing for macOS (Apple Developer account)
- [ ] Windows build & signing
- [ ] Linux AppImage/Snap/Flatpak
- [ ] In-app update notifications

---

## Release Checklist
- [ ] Version bump in package.json
- [ ] Build universal macOS app
- [ ] Create GitHub release with DMG
- [ ] Create desktop.mjs piece
- [ ] Update prompt.mjs with desktop command
- [ ] Test download flow end-to-end
- [ ] Announce release

---

## Technical Notes

### GitHub Release URL Pattern:
```
https://github.com/whistlegraph/aesthetic-computer/releases/download/v0.1.0/Aesthetic.Computer-0.1.0-universal.dmg
```

### OS Detection in desktop.mjs:
```javascript
const platform = navigator.platform.toLowerCase();
const isMac = platform.includes('mac');
const isWindows = platform.includes('win');
const isLinux = platform.includes('linux');
```

### Download URLs:
- macOS: `releases/download/electron-v{version}/Aesthetic.Computer-{version}-universal.dmg`
- Windows: (future) `releases/download/electron-v{version}/Aesthetic.Computer-Setup-{version}.exe`
- Linux: (future) `releases/download/electron-v{version}/Aesthetic.Computer-{version}.AppImage`
