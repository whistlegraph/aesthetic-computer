# Aesthetic Computer iOS — Progress

Per-submission notes for the iOS app and iMessage extension in this directory.
Append a new section on top for each App Store build.

---

## 1.1 (4) — 2026-09-11 (submitted)

Builds 1.1 (2) and 1.1 (3) were uploaded in April and May but never attached
to an App Store version, and TestFlight let them expire. This build carries
everything both of them had plus the June push change, and it ships through a
lane instead of by hand.

### Shipped
- **Firebase removed.** Push tokens come straight from APNs
  (`didRegisterForRemoteNotificationsWithDeviceToken`) and reach the runtime
  as `kind: "apns"` via `/api/register-push-token`; the server fans out
  itself. No SDK, no GoogleService plist, smaller binary
  (commit `2a5816a281`).
- **fastlane lane** at `apple/fastlane/` — the Menu Band / oskiewar shape:
  `fastlane ios meta` (creates the 1.1 version + metadata), `shots`,
  `build` (cloud-signed archive + App Store export to
  `apple/build/export/aesthetic.computer.ipa`), `upload`, `privacy`, and
  `ship`, which hands the last mile to `slab/menuband/bin/asc.mjs submit
  --app aestheticcomputer --platform IOS`. Metadata seeded from the live 1.0
  listing in `fastlane/metadata/ios/en-US/`.

### Submission notes
- `fastlane ios meta` ran 2026-09-11: version 1.1 created (PREPARE_FOR_SUBMISSION,
  id `c2212b58-deec-424e-a60d-5ac2ee3c7260`), description/keywords/URLs/What's New
  landed, and all eight screenshot sets (iPhone 6.7 + 5.5, iPad Pro 12.9 ×2, and
  their iMessage twins) were inherited from 1.0 — no `shots` run needed.
- **Blocker 2026-09-11:** `fastlane ios build` fails at destination resolution —
  "iOS 26.5 is not installed. Please download and install the platform".
  Xcode 26.6 kept the iPhoneOS 26.5 SDK but has zero simulator runtimes, and
  Xcode 15+ needs the platform even for device archives. Fix is
  `xcodebuild -downloadPlatform iOS` (~8 GB on disk); the build lane now
  preflights this.
- **Archive host = blueberry** (Xcode 26.6 + iOS 26.5 runtime already installed,
  39 GiB free) — neo would have to download the 8.5 GB platform first. Sync the
  lane files with `rsync --relative apple/fastlane apple/PROGRESS.md
  apple/aesthetic-computer-Info.plist apple/aesthetic.computer.xcodeproj/project.pbxproj
  blueberry:~/aesthetic-computer/`, then `ssh blueberry bash -c "cd
  ~/aesthetic-computer/apple && fastlane ios build"` (login shell is fish). First
  run died at CodeSign with `errSecInternalComponent` — the ssh session cannot
  reach the login keychain; fix is the blueberry recipe: `security
  unlock-keychain` + `security set-key-partition-list -S apple-tool:,apple:,codesign:
  -s` (interactive, @jeffrey's password), then rerun.
  **Unlock state is per audit session**: an unlock in one `ssh -t` does nothing
  for the next ssh connection, so unlock and archive in the SAME session:
  `ssh -t blueberry 'security unlock-keychain ~/Library/Keychains/login.keychain-db; cd ~/aesthetic-computer/apple && fastlane ios build'`.
- **Export ran on neo, not blueberry.** Blueberry's archive succeeded but its export
  produced nothing; the `.xcarchive` was tar-piped to `apple/build/` on neo and
  exported with `xcodebuild -exportArchive … -exportOptionsPlist
  ExportOptions-AppStore.plist -allowProvisioningUpdates` + the S4TQKG6U99 key.
  Signing is a **Cloud Managed Apple Distribution** certificate (SHA1 D25B0254…,
  expires 2027-04-23) — cloud-managed, so it never appears in
  `security find-identity`, and `/v1/certificates` does not list it either.
  Verified in the ipa: aps-environment=production, get-task-allow=false,
  ITSAppUsesNonExemptEncryption=false, app + appex both build 4.
- **deliver gotcha:** it validates `./fastlane/metadata` and `./fastlane/screenshots`
  on every call even with `skip_metadata`/`skip_screenshots`, and rejects the
  `ios/` platform subfolder as an unknown locale — so `metadata_path` and
  `screenshots_path` live in `deliver_defaults`, not per lane.
- **Uploaded 2026-09-11 13:04** via `fastlane ios upload` ("Successfully uploaded
  package to App Store Connect"). Poll `asc.mjs status --app aestheticcomputer
  --platform IOS` until build 4 (dated today) reads VALID, then `fastlane ios ship`.
- **Build 4 VALID on ASC 2026-09-11 10:05 PT** (id `09702f94-172a-46ac-b4bd-9c55cee7910d`,
  usesNonExemptEncryption=false from the plist key, minOS 15.0). Waiting on
  `fastlane ios ship`.
- **SUBMITTED 2026-09-11 16:00 ET via `fastlane ios ship`** — version 1.1 and
  reviewSubmission `3645eb9f-9254-4d77-9f5c-db9210da77e2` both WAITING_FOR_REVIEW.
  Three 409s on the way, all fixed in `slab/menuband/bin/asc.mjs`:
  (1) `submit` picked the highest build NUMBER across every train — the 2023
  1.0 train reached build 5, so it tried to attach an EXPIRED build; now filters
  `preReleaseVersion.version` + `expired=false`, sorted by upload date.
  (2) Apple's expanded age-rating questionnaire had ten unanswered fields
  (advertising, ageAssurance, parentalControls, gunsOrOtherWeapons,
  healthOrWellnessTopics, lootBox, messagingAndChat, userGeneratedContent,
  socialMedia, socialMediaAgeRestricted). They live on the **editable appInfo**
  (`556bc291…`, created with version 1.1); the live 1.0 appInfo's declaration is
  locked. Declared honestly per the operator: chat, UGC, social = yes (not
  age-restricted); ads, loot boxes, health, parental controls, age assurance =
  no; guns = none. Rating stayed 4+.
  (3) `submit` opened a fresh reviewSubmission on every retry; it now reuses an
  open READY_FOR_REVIEW shell and skips an item already present. One empty
  shell (`f60158d8…`) is orphaned — Apple refuses to cancel an empty one.
- `CURRENT_PROJECT_VERSION` bumped 3 → 4 on both targets (3 was consumed by
  the expired upload).
- Order: `fastlane ios meta` → `fastlane ios build` → `fastlane ios upload`
  → `node slab/menuband/bin/asc.mjs status --app aestheticcomputer
  --platform IOS` until build 4 reads VALID → `fastlane ios ship`.
- What's New lives in `fastlane/metadata/ios/en-US/release_notes.txt`.

---


## 1.1 (3) — 2026-04-30 (uploaded, never submitted; build expired)

Reliability pass before this build ships. Field reports of the app sitting
on the boot.mjs animation indefinitely (5,000s+) with no way to recover
short of force-quit. Root cause was structural — there was no host-side
detection of a stuck JS runtime, no failure path on `WKNavigationDelegate`,
and the network monitor cancelled itself on the first satisfied path so
mid-session drops were invisible.

### Shipped
- **Long-lived `AppNetworkMonitor`** (replaces the one-shot monitor in
  `ContentView`). Stays alive for the whole process and publishes online↔
  offline transitions; flipping back online auto-reloads.
- **Boot watchdog.** `BootStatus` runs a 5s-poll timer that flips
  `stalled = true` if no JS heartbeat has arrived for 25 seconds. Stalled
  state surfaces a SwiftUI overlay with a "Reload" button anchored at the
  bottom — covers the "stuck on boot animation" failure mode.
- **`WKNavigationDelegate`** wired up. Provisional + final navigation
  failures populate `BootStatus.lastError` so the same overlay can show a
  proper error string ("Cannot reach aesthetic.computer — …") instead of
  a frozen UI.
- **JS heartbeat** in `boot.mjs`. Every 1s during boot, then every 8s
  after `acHIDE_BOOT_LOG` fires, posts `{type:"boot:heartbeat"|"boot:ready"}`
  through the existing `iOSApp` message channel. Lets the host distinguish
  "slow load" from "deadlocked".
- **Pull-to-refresh** on the WebView's scroll view (UIRefreshControl
  attached in `makeUIView`). Recovery is now one gesture.
- **Scene-phase reload.** Returning the app from >5min of background
  triggers a fresh load — the WebView often holds a stale runtime
  (timed-out sockets, half-loaded modules) after long sleeps.
- **`offline.html` retry button.** Posts `{type:"reload-online"}` to the
  host, which force-loads the live URL even if the path monitor still
  reads offline (cell handoffs lag a few seconds).
- **Reload gating in `updateUIView`.** Tracks `lastLoadedKey` on the
  Coordinator so reloads only fire on real URL/trigger changes, not on
  every SwiftUI re-render (the previous code rebust+reloaded each time).

### Wiring summary
- iOS: `apple/aesthetic.computer/ContentView.swift` —
  `AppNetworkMonitor`, `BootStatus`, watchdog, nav delegate, overlay,
  pull-to-refresh, scene phase.
- iOS: `apple/aesthetic.computer/html/offline.html` — retry button.
- Runtime: `system/public/aesthetic.computer/boot.mjs` — IIFE that posts
  `boot:heartbeat`/`boot:ready` via `webkit.messageHandlers.iOSApp`.

### Submission notes
- Same bundle IDs / team / Apple ID as 1.1 (2).
- Bump `CFBundleVersion` to `3` before archiving.
- What's New text: "Recovery from hung loads — reload from a stuck boot
  screen, automatic retry when the network reconnects, pull down to
  refresh."

---

## 1.1 (2) — 2026-04-23

First update since 1.0 (1) shipped as RC4 in late 2024. Focus: per-device push
notifications, replacing the slow topic-broadcast model.

### Shipped
- **FCM device-token bridge.** `MessagingDelegate` hands the token to the WebView
  via `evaluateJavaScript(window.iOSReceivePushToken(...))` with retry until the
  AC runtime is ready. The runtime POSTs `/api/register-push-token` against the
  logged-in user so the backend can target individual devices.
- **`tell` command.** `tell @handle message` from the AC prompt calls `/api/tell`,
  which writes to the `tells` Mongo collection and sends a direct-token FCM push
  (sub-second latency vs. minutes for topic broadcasts). Deliberately one-way
  for v1 — no reply UI, no thread.
- **Firebase iOS SDK 10.19 → 11.15.** 10.19's prebuilt xcframeworks were missing
  `CFBundleShortVersionString` / `MinimumOSVersion` in their Info.plists, which
  Apple's submission validator began rejecting in late 2024. Dropped the
  now-removed `FirebaseAnalyticsSwift` product (merged into `FirebaseAnalytics`
  in 11.x).
- **Account-deletion cascade.** `delete-erase-and-forget-me` now clears
  `push-tokens` and `tells` alongside existing `moods` cleanup.

### Wiring summary
- iOS: `apple/aesthetic.computer/aesthetic_computerApp.swift` — `deliverPushTokenToWebView`
- Runtime: `system/public/aesthetic.computer/bios.mjs` — `window.iOSReceivePushToken`,
  `iOSTryRegisterPushToken`, `iOSUnregisterPushToken`
- Runtime: `system/public/aesthetic.computer/boot.mjs` — login-drain + logout-unregister hooks
- Backend: `system/netlify/functions/register-push-token.mjs`, `tell.mjs`
- Prompt: `system/public/aesthetic.computer/disks/prompt.mjs` (`tell` handler)

### Build/submission notes
- Bundle IDs: `aesthetic.computer` (main), `aesthetic.computer.aesthetic` (iMessage ext).
- Team: `FB5948YR3S`. Apple ID for distribution: `me@jas.life`.
- App-specific password lives at `aesthetic-computer-vault/apple/app-specific-password.env.gpg`.
- CLI archive + export + upload works via `xcodebuild archive -allowProvisioningUpdates`
  → `xcodebuild -exportArchive` → `xcrun altool --upload-app`. See `/tmp/ac-ios-*.log`
  from this session for reference commands.
- Submit-for-review (What's New, export compliance, etc.) still manual in App Store
  Connect — requires an ASC API key `.p8` to automate. Drop one in
  `aesthetic-computer-vault/apple/appstoreconnect/` next time to close the loop.

### Known gaps / candidates for 1.2
- `tells` inbox piece — right now the push notification *is* the UI. A
  `tells.mjs` piece + deep-link from the notification tap-through (Swift already
  forwards `userInfo["piece"]` → `iOSAppSwitchPiece`) would show the conversation
  log.
- `mood` notifications still go via FCM topic (`mood.mjs:346`). Convert to
  per-follower direct sends once a follower graph exists.
- App Store screenshots are stale — proposal: extend `oven/` with an `app-shots.mjs`
  driving `xcrun simctl` across the required device sizes to auto-capture a
  curated set of pieces.
- ASC API key for unattended "Submit for Review" (see above).
