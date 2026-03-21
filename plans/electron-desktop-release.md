# Electron Desktop App Release Plan

## Overview
Release the Aesthetic Computer Electron app and create a download experience at `aesthetic.computer/desktop`.

---

## Progress Log (December 27, 2025)

### ✅ Completed
1. **Electron App Features**
   - Independent zoom controls (Cmd+/- for front webview or back terminal)
   - Window size: 680×480
   - Alt+scroll window drag in all window types
   
2. **macOS v0.1.0 Release Published**
   - URL: https://github.com/whistlegraph/aesthetic-computer/releases/tag/v0.1.0
   - Universal DMG (Apple Silicon + Intel)
   - Unsigned (requires right-click → Open or `xattr -cr`)

3. **desktop.mjs Piece Created**
   - Platform detection (macOS/Windows/Linux/Fedora)
   - Download buttons for all platforms
   - Install instructions for each platform
   - Features & requirements sections
   - ⚠️ BUG: Scrolling broken, `e.screen` undefined in `act()`

4. **Prompt Commands Added**
   - `desktop`, `app`, `electron` all navigate to the desktop piece

5. **Short URL Redirects (netlify.toml)**
   - `aesthetic.computer/desktop/mac` → macOS DMG
   - `aesthetic.computer/desktop/win` → Windows exe
   - `aesthetic.computer/desktop/linux` → AppImage
   - `aesthetic.computer/desktop/fedora` → RPM
   - `aesthetic.computer/desktop/deb` → Debian package

6. **GitHub Actions Multi-Platform CI/CD**
   - Workflow: `.github/workflows/electron-release.yml`
   - Builds for macOS, Windows, Linux (AppImage, deb, rpm)
   - Uses Node 22 + Python 3.11 with setuptools
   - GH_TOKEN set as repository secret

7. **Package.json Updates (ac-electron)**
   - Added `homepage: "https://aesthetic.computer"`
   - Added `author`, `license` for Linux deb/rpm builds
   - Linux targets: AppImage, deb, rpm

8. **Repo Fixes**
   - Renamed `a*.mjs` to `a-star.mjs` (Windows compatibility)
   - Removed orphaned submodule refs (`feral-file/docs`, `feral-file/ffos-user`)

### 🔄 In Progress
- GitHub Actions builds for Windows/Linux (latest run pending)

### ❌ Known Issues
- `desktop.mjs` has broken scrolling (`e.screen` undefined in `act`)
- Need to simplify piece using `ui.Button` class like other pieces

---

## Phase 1: Commit & Push Current Changes ✅
- [x] Commit ac-electron zoom and width changes
- [x] Push to main

## Phase 2: Electron App Distribution ✅
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

## Phase 3: Create `desktop.mjs` Piece ✅
A dedicated piece at `aesthetic.computer/desktop` that:
- [x] Shows the Aesthetic Computer logo/branding
- [x] Displays download button for macOS
- [x] Shows download buttons for Windows/Linux
- [x] Includes system requirements
- [x] Links to GitHub releases page
- [x] Platform-specific install instructions
- [ ] FIX: Scrolling and button hover states broken

### UI Elements:
- Title: "Aesthetic Computer Desktop"
- Subtitle: "The creative coding environment on your desktop"
- Download button (detects OS, shows appropriate download)
- Version number display
- GitHub link for all releases
- One-liner install commands (curl)

---

## Phase 4: Update `prompt.mjs` ✅
- [x] Add `desktop` command to navigate to the desktop piece
- [x] Add `app` and `electron` aliases

---

## Phase 5: Short URL Redirects ✅
- [x] `aesthetic.computer/desktop/mac` → DMG
- [x] `aesthetic.computer/desktop/win` → exe
- [x] `aesthetic.computer/desktop/linux` → AppImage
- [x] `aesthetic.computer/desktop/fedora` → RPM
- [x] `aesthetic.computer/desktop/deb` → deb

---

## Phase 6: Future Enhancements
- [ ] Fix desktop.mjs scrolling and button hover states
- [ ] Auto-update system (electron-updater)
- [ ] Code signing for macOS (Apple Developer account)
- [ ] Windows code signing
- [ ] In-app update notifications

---

## Release Checklist
- [x] Version bump in package.json (0.1.0)
- [x] Build universal macOS app
- [x] Create GitHub release with DMG
- [x] Create desktop.mjs piece
- [x] Update prompt.mjs with desktop command
- [x] Set up GitHub Actions for Windows/Linux builds
- [x] Add short URL redirects in netlify.toml
- [ ] Fix desktop.mjs bugs
- [ ] Test Windows/Linux builds
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

### Download URLs (via Netlify redirects):
- macOS: `aesthetic.computer/desktop/mac`
- Windows: `aesthetic.computer/desktop/win`
- Linux: `aesthetic.computer/desktop/linux`
- Fedora: `aesthetic.computer/desktop/fedora`
- Debian: `aesthetic.computer/desktop/deb`

### One-liner Install Commands:
```bash
# Fedora
curl -L aesthetic.computer/desktop/fedora -o ac.rpm && sudo dnf install -y ac.rpm

# Debian/Ubuntu
curl -L aesthetic.computer/desktop/deb -o ac.deb && sudo dpkg -i ac.deb

# Linux AppImage
curl -L aesthetic.computer/desktop/linux -o ~/AC.AppImage && chmod +x ~/AC.AppImage
```

### GitHub Actions Workflow:
- File: `.github/workflows/electron-release.yml`
- Triggers: Push to `v*` tags or manual dispatch
- Builds: macOS (universal), Windows (x64), Linux (AppImage, deb, rpm)
- Artifacts uploaded to workflow, not auto-published to releases yet
