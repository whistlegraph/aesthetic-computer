# macOS

Start from `macos/fight-runner/`, the existing Swift/AppKit `WKWebView` host with
native GameController polling. The first product shell should load
`/menu-fighter`, consume the icon master, and add signing/notarization,
fullscreen and window policy, URL/room-code deep links, preferences, and update
delivery.

Keep WebRTC in the shared page for the first release so web and macOS exercise
the same network path. Add native transport only if profiling reveals a reason.
