# Oskiewar platforms

The current game lives in `xbox/live/oskiewar.js`. Browser hosts use
`xbox/live/mac-test.html`. `xbox/classic` is the explicitly archived game,
not another current release. Build directories are generated output.

`node xbox/tools/oskiewar-manifest.mjs` inventories the complete browser
runtime, including imports, fonts, and theme assets. The game hash identifies
shared game logic; the release hash also identifies shell and module changes.
Native host binaries remain platform-specific and require their own builds.

| Surface | Source and delivery | Account character |
| --- | --- | --- |
| Web / mobile browser | Lith serves the shared runtime; tabs check the release hash | Saved @jeffrey fighter in private practice, subject to live consent and expiry |
| iOS app | Production WKWebView; complete shared runtime bundled for offline use | Account controls currently disabled in the app |
| Mac native | JavaScriptCore + Metal; bundle staged from shared runtime | No account login in this host yet |
| Mac WebView test app | Loads production directly; never injects a stale local game | Same as web |
| Steam | Unmodified shared web shell in Electron; offline packaged mode | No account login in packaged mode |
| Xbox native | Shared game source through Device Portal | Separate native host; web account UI unavailable |

Build and verify:

```sh
node xbox/tools/oskiewar-manifest.mjs --write
sh xbox/tools/build-macos-app.sh          # --install also replaces local app
xcodegen generate --spec apple/oskiewar/project.yml --project apple/oskiewar
xcodebuild -project apple/oskiewar/Oskiewar.xcodeproj -scheme Oskiewar \
  -sdk iphonesimulator -configuration Debug -derivedDataPath xbox/builds/ios \
  CODE_SIGNING_ALLOWED=NO build
node xbox/steam/shell/stage.mjs
node xbox/tools/oskiewar-release.mjs status
```

Use `npm run oskiewar:deploy` from the intended committed deployment checkout.
The receipt separates web, iOS-web, installed Mac, staged iOS/Steam artifacts,
and Xbox. A staged simulator bundle does not mean the App Store or a physical
phone has been updated. Bundle verification checks every shared asset.

Run `system/tests/oskiewar-platform.browser.mjs` on Poorslice for desktop/touch
boot and module-only update behavior. Debug iOS builds accept `--offline
--smoke-test`; their Documents/runtime-smoke.json reports the loaded release,
game screen, and JavaScript errors. Store builds do not include those switches.
