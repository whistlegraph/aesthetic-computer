# ThinkPad X1 Nano → Aesthetic Computer “single‑app” boot report (repo-first)

Date: 2026-01-23

## ✅ What exists in this repo

**ThinkPad setup (dev-oriented):**
- Fedora installation checklist and dev setup steps are in [writing/thinkpad.txt](writing/thinkpad.txt).
- A scripted dependency install for Fedora ThinkPads exists in [utilities/thinkpad.sh](utilities/thinkpad.sh) and [utilities/thinkpad.fish](utilities/thinkpad.fish).

**Electron app details:**
- Electron app entry points and build targets (AppImage/deb/rpm) are in [ac-electron/package.json](ac-electron/package.json#L1-L116).
- App start/restart wrapper is in [ac-electron/restart-electron.sh](ac-electron/restart-electron.sh).
- Electron window defaults are currently small, floating, frameless, and always-on-top (not fullscreen) in [ac-electron/main.js](ac-electron/main.js#L1034-L1067).
- Development/production mode notes are in [ac-electron/README.md](ac-electron/README.md#L1-L70).
- Linux build targets (AppImage/deb/rpm) are called out in [plans/electron-desktop-release.md](plans/electron-desktop-release.md#L169-L184).

**FF1 / Feral File integration docs (not OS):**
- FF1 device integration (DP‑1 protocol, proxy relay, device URLs) lives in [system/public/kidlisp.com/js/ff1.js](system/public/kidlisp.com/js/ff1.js#L1-L104) and [system/public/kidlisp.com/js/ff1.js](system/public/kidlisp.com/js/ff1.js#L265-L308).
- KidLisp + Feral File architectural docs exist in [kidlisp/docs/integration/x-ff1-art-computer.md](kidlisp/docs/integration/x-ff1-art-computer.md#L1-L120).

**FFOS / ffos-user mentions:**
- The only direct reference to ffos-user in this repo is a note about removing orphaned submodule refs in [plans/electron-desktop-release.md](plans/electron-desktop-release.md#L38-L41).

**No FF1 OS/bootable image docs were found** in this repo.

## ✅ What the FFOS repo actually is (feral-file/ffos)

Based on the public README, FFOS is **not a ready-to-install laptop OS**. It is a **build repository** for FF1 images:

- **Arch Linux ISO build repo** that uses an archiso profile (`archiso-ff1/`).
- **Depends on a separate ffos-user repository** for components and user data.
- **Builds via GitHub Actions** and uploads artifacts to **Cloudflare R2** storage.
- Produces ISO images named like `FF1-<branch>-<version>.iso`.

Source: https://github.com/feral-file/ffos (README)

## ✅ What the FFOS-USER repo actually is (feral-file/ffos-user)

Based on the public README, FFOS-USER is a **components + user data** repo that FFOS pulls into the ISO build. It contains:

- **components/**: daemons + UI that ship on the device (Go/Rust/JS)
   - `feral-controld`, `feral-setupd`, `feral-sys-monitord`, `feral-watchdog`
   - `launcher-ui`, `player-wrapper-ui`
- **users/**: user home data that becomes `/home/<user>` in the ISO
   - `users/feralfile/scripts/start-kiosk.sh`
   - `users/feralfile/.config/systemd/user/*.service` (includes `chromium-kiosk.service`)

Source: https://github.com/feral-file/ffos-user (README)

## Recommendations: “boot to Aesthetic Computer” on ThinkPad

### Option A — Fastest (Fedora + auto‑launch AC Electron)
**Goal:** GNOME still runs, but user auto‑logs in and the Electron app opens fullscreen/kiosk immediately.

1. **Install Fedora** (same baseline as [writing/thinkpad.txt](writing/thinkpad.txt)).
2. **Install AC Electron build** (AppImage/deb/rpm from [plans/electron-desktop-release.md](plans/electron-desktop-release.md#L169-L184)).
3. **Auto‑login** in GDM.
4. **Auto‑start Electron** using a user systemd service or a .desktop autostart entry.
5. **Force fullscreen/kiosk** by updating Electron window creation.
   - Current window uses a fixed size and `frame: false` (see [ac-electron/main.js](ac-electron/main.js#L1034-L1067)).
   - For kiosk: set `kiosk: true` and/or call `win.setFullScreen(true)` after load.

**Pros:** Minimal changes to OS. **Cons:** GNOME still present behind app.

### Option B — “Appliance” feel (minimal OS + kiosk session)
**Goal:** Boot into a dedicated session that runs only the Electron app.

- Use a minimal Fedora/Ubuntu install, create a dedicated user, and set the session to auto‑login into a custom X/Wayland session that starts Electron.
- Systemd user service can launch Electron and auto‑restart on exit.

**Pros:** Cleaner “single‑app” device. **Cons:** More OS config.

### Option C — Browser kiosk
**Goal:** Run AC as a full‑screen web experience.

- Launch Chromium in kiosk mode to https://aesthetic.computer.
- This is viable but lacks the Electron terminal/flip view features.

## Suggested Electron changes for kiosk mode
These are not implemented yet—just what would be needed:

1. **Fullscreen/kiosk window flags** in `openAcPaneWindowInternal()`.
2. **Disable dev UI and menus** in production kiosk build.
3. **Start in production mode** (connects to https://aesthetic.computer).

If you want, I can implement a kiosk toggle flag (e.g. `--kiosk`) and a Linux autostart unit.

## About FF1 OS / Feral File Art Computer
**Summary:** The FF1 OS is hosted in feral-file/ffos, but it’s an **ISO build pipeline**, not a ready-to-use laptop image.

**Implications for ThinkPad X1 Nano:**
- You’d need **ffos + ffos-user** access, plus Cloudflare R2 secrets and CI pipelines, to build images.
- You would likely need a **new archiso profile** tailored for a generic x86_64 laptop (ThinkPad), not FF1 hardware.
- You’d still have to **swap the FF1 services** for a kiosk/autostart setup that launches the Electron app.

**Likely ffos-user modification points for a ThinkPad “Aesthetic Computer” build:**
- `users/feralfile/scripts/start-kiosk.sh` → launch Electron instead of Chromium.
- `users/feralfile/.config/systemd/user/chromium-kiosk.service` → new `aesthetic-kiosk.service`.
- `components/launcher-ui` or `components/player-wrapper-ui` → replace launcher with single‑app boot or skip entirely.
- `feral-setupd` (if it enforces hardware onboarding) → bypass FF1‑specific provisioning paths.

## ✅ KidLisp + FF1 integration (current state in this repo)

**Where the integration lives:**
- FF1 device protocol client (DP‑1 playlist + relay proxy) is implemented in [system/public/kidlisp.com/js/ff1.js](system/public/kidlisp.com/js/ff1.js#L1-L308).
- KidLisp playback routes to FF1 via `sendKidLispCode()` in [system/public/kidlisp.com/js/playback.js](system/public/kidlisp.com/js/playback.js#L33-L140).
- FF1 integration proposal + UX narratives are documented in [kidlisp/docs/integration/x-ff1-art-computer.md](kidlisp/docs/integration/x-ff1-art-computer.md#L1-L120).

**Current mechanics:**
- KidLisp code is sent to AC first, stored, and a `codeId` is obtained.
- FF1 receives a DP‑1 playlist containing a `device.kidlisp.com/<codeId>` URL (optimized with `tv=true`, `nogap=true`, `nolabel=true`).
- Transport typically uses the session‑server proxy (CORS‑safe), with optional direct device IP and relay API key.

## ✅ How FF1 OS patching could improve the integration

These are OS‑level improvements inside FFOS/ffos-user that would make the AC + KidLisp experience tighter and more reliable on FF1 hardware:

### 1) Preinstall a “Kidlisp Channel” launcher
- **Goal:** Boot into a curated “Aesthetic Computer” mode without phone setup.
- **Where:** `components/launcher-ui` (new entry) + `users/feralfile/scripts/start-kiosk.sh`.
- **Effect:** Single‑tap entry into `device.kidlisp.com` or a dedicated “AC Channel” URL.

### 2) First‑class DP‑1 channel for AC
- **Goal:** Treat AC as a native FF1 channel, not just a URL cast.
- **Where:** `feral-controld` + `launcher-ui` to add a channel that always points at an AC playlist endpoint.
- **Effect:** Device persists the channel and can auto‑resume after reboot.

### 3) Offline fallback + cached last piece
- **Goal:** Avoid blank screen if Wi‑Fi drops.
- **Where:** `player-wrapper-ui` (or media player layer) to cache last successful URL and re‑display it when offline.
- **Effect:** Better gallery stability, fewer “dead boots.”

### 4) System‑level display settings for AC
- **Goal:** Guarantee kiosk‑safe rendering (TV density, no chrome, correct overscan).
- **Where:** OS‑level browser flags in `users/feralfile/.config/systemd/user/chromium-kiosk.service`.
- **Effect:** Hard‑lock display settings instead of relying on query params.

### 5) Deep link: QR → FF1 cast → AC piece
- **Goal:** Rapid, phone‑friendly pairing that casts directly into a piece.
- **Where:** `feral-setupd` (pairing) + `feral-controld` (cast) to accept QR deep links.
- **Effect:** “Scan → play” without manual config entry.

### 6) Remote health & telemetry for AC channel
- **Goal:** Detect failed loads and auto‑recover.
- **Where:** `feral-sys-monitord` + `feral-watchdog` to restart the player wrapper on failure and report error state.
- **Effect:** Self‑healing AC channel with fewer support events.

## Recommendation

If the target is **ThinkPad X1 Nano**, the FFOS path is heavy‑lift. The **fast path** is still Fedora + kiosk Electron. If the target is **FF1 hardware**, then the above OS patches are the right lever to tighten the KidLisp experience.

## Concrete FFOS/FFOS-USER patch plan (scoped)

### Phase 1 — Minimal “AC Channel” on FF1 (low risk)
**Goal:** Add a dedicated launcher entry that opens AC without touching core daemons.

**Changes (ffos-user):**
- `components/launcher-ui/` → add an **Aesthetic Computer** tile that opens a fixed URL (e.g. `https://device.kidlisp.com/` or `https://aesthetic.computer?tv=true&nogap=true&nolabel=true`).
- `users/feralfile/scripts/start-kiosk.sh` → ensure the launcher shows the new tile and can auto‑launch it when configured.

**Risks:** minimal; UI‑only.

### Phase 2 — Kiosk service hardening (medium risk)
**Goal:** Guarantee consistent rendering + recovery.

**Changes (ffos-user):**
- `users/feralfile/.config/systemd/user/chromium-kiosk.service` → create **`aesthetic-kiosk.service`** with fixed flags:
   - disable UI chrome, force fullscreen, lock density, disable screen blanking.
   - point to a pinned AC URL with `tv=true&nogap=true&nolabel=true`.
- Add a watchdog timer or restart policy for the kiosk service.

**Risks:** may affect other channels if misconfigured; needs testing on FF1 hardware.

### Phase 3 — Deep link + QR cast (medium/high)
**Goal:** “Scan → play” direct to KidLisp piece on FF1.

**Changes (ffos-user components):**
- `feral-setupd` → accept QR payloads that include a `kidlisp://code/<id>` or URL.
- `feral-controld` → translate deep link into DP‑1 playlist + cast.

**Risks:** touches pairing and connectivity flows.

### Phase 4 — Offline caching + resilience (medium)
**Goal:** Keep screen alive if network drops.

**Changes (ffos-user components):**
- `player-wrapper-ui` (or UI layer) → cache last good URL and reload on failure.
- `feral-sys-monitord` / `feral-watchdog` → detect render stall and restart the wrapper.

**Risks:** requires careful loop‑avoidance to prevent restart storms.

### Phase 5 — AC channel telemetry (optional)
**Goal:** Track performance and failures for the AC channel.

**Changes:**
- Add lightweight status pings (success/failure, time‑to‑render).
- Wire to existing FF1 logging paths.

**Risks:** needs agreement on metrics + privacy.

## Acceptance checklist
- AC tile visible in launcher.
- Single‑tap launch into AC full‑screen.
- Auto‑resume after reboot.
- No input chrome, no system UI leaks.
- On network loss, last image persists.
- On crash, kiosk restarts within 5s.

## Running FFOS on a ThinkPad (realistic outlook)

**Short answer:** Possible but heavy. FFOS is tuned for FF1 hardware and build pipelines. For ThinkPad, the fastest and safest path is still **standard Linux + kiosk Electron**.

**What it would take to run FFOS on ThinkPad:**
- Build a new archiso profile tailored for generic x86_64 laptops (drivers, power, display stack).
- Replace FF1 hardware services (pairing, watchdog expectations) with laptop‑appropriate equivalents.
- Swap the default kiosk target to AC Electron or AC web view.

**Risks:**
- Hardware mismatch (GPU/driver stack, display output assumptions).
- Lost time on build pipeline + R2 credentials.
- Maintenance burden vs. using a standard distro.

**Recommendation:** Use Fedora/Ubuntu + auto‑login + kiosk AC. Reserve FFOS for FF1 devices.

## Local FFOS build pipeline (this repo)

- Added a containerized local build flow at [utilities/ffos-build/README.md](utilities/ffos-build/README.md).
- **No submodules** are used; FFOS/FFOS-USER are cloned into a local cache.
- Cache is ignored in git to avoid Netlify build interference.
- Added an **AC kiosk overlay** (service + script) under [utilities/ffos-build/overlays/ffos-user](utilities/ffos-build/overlays/ffos-user).

## AC for education (what it enables)

**Why it fits classrooms:**
- Immediate visual feedback (KidLisp) → fast learning loops.
- Low‑barrier syntax encourages beginners.
- Shareable outputs ($code links, QR) support group critique and portfolios.

**Suggested education modes:**
- **Kiosk lab mode**: AC auto‑boots into a curated set of KidLisp prompts.
- **Lesson mode**: preloaded prompts + locked toolset to reduce distraction.
- **Gallery mode**: show‑only playback of student works on shared screens.

**Operational notes:**
- Use dedicated student accounts or device‑local sessions.
- Favor browser kiosk if deployment must be locked down tightly.

**Practical reality:** For a ThinkPad, a minimal Linux install + auto‑login + kiosk Electron is far faster and lower‑risk than forking FFOS.

## Next steps I can take
- Add a `--kiosk` flag to the Electron app and set full‑screen behavior.
- Add a systemd user unit + autostart `.desktop` for Linux.
- Draft a minimal Fedora kiosk setup guide customized for ThinkPad X1 Nano.
- Review additional FF1/Feral File GitHub repos if you share the URLs/owners.
