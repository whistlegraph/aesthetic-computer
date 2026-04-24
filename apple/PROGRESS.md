# Aesthetic Computer iOS — Progress

Per-submission notes for the iOS app and iMessage extension in this directory.
Append a new section on top for each App Store build.

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
