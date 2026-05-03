# Aesthetic Computer iOS — Progress

Per-submission notes for the iOS app and iMessage extension in this directory.
Append a new section on top for each App Store build.

---

## 1.1 (3) — 2026-04-30 (in progress)

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
