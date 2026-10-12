# MIME.ac

The iPhone app for watching what everyone makes across Aesthetic Computer, KidLisp and Whistlegraph, one piece per screen. Creation happens in those apps; this one only watches: N apps for making, one for watching.

The feed is the same ranked network feed as https://mime.ac (`GET /api/mime?feed=1&seed=…`, `system/backend/mime-feed-engine.mjs`), with a random seed per launch, so each launch gets its own order.

- `Sources/Feed.swift`: the feed model and how each post draws (image, video, live runtime, packed HTML, words, document).
- `Sources/Reel.swift`: the "swipe up" splash, the vertical pager, and the cards. The code sits top-left and is the Interact control, the handle bottom-left, the MIME type bottom-right. The wordmark follows the first swipe from its banner into a top-right watermark.
- `Sources/PieceRuntime.swift`: one live web runtime for the card in view. KidLisp and pieces load aesthetic.computer, Whistlegraph pieces load `/mime-play`. Autoplay is allowed and the runtime is asked to start audio. A non-persistent website data store keeps pieces from seeing any session.
- `Sources/PalText.swift`: MacPal's title lettering (macpal/Sources/PalCore.swift), with outlines under fills.
- `Sources/Wordmark.swift`: the MIME wordmark, drawn from the same letter paths as mime.ac's SVG.
- `Artwork/icon.html`: the app icon's source. Render it at 1024×1024 into `Resources/Assets.xcassets/AppIcon.appiconset/icon.png`.

XcodeGen generates `Mime.xcodeproj` from `project.yml`. Bundle `computer.aesthetic.mime`, team `FB5948YR3S`.

Native SwiftUI and the website share `system/public/mime-layout.json`, served
at `https://aesthetic.computer/mime-layout.json`. Build 2 reads schema 1 when
opening or foregrounding the app. Font sizes, spacing, captions and optional
titles can change through a web deploy. Invalid settings retain the last
valid layout; offline launches use that cached layout or the bundled JSON.
The global code always remains top-left, handle bottom-left, type bottom-right.
New native controls or schema capabilities require another app build.

After committing and pushing main, `bash apple/mime/release-mime.sh device`
deploys and verifies the website/config, then builds, installs and launches
the native app on the connected iPhone (`DEVICE` selects it). Use `testflight`
to archive/upload instead, or `web` for production presentation updates only.
Both releases use the same source revision.

## TestFlight

From this directory: `fastlane ios status`, `fastlane ios build`, `fastlane ios upload`, `fastlane ios distribute` (which makes the private **MIME.ac Friends** group if needed), and `fastlane ios invite email:<email> name:<first-name>`. These use the same Admin API key as AC, Whistlegraph, Oskiewar and Menu Band.

The first app record needs an Apple ID login once, in the terminal (two-factor included): `FASTLANE_USER=<Apple ID> fastlane ios create`. Apple's public API cannot create apps.
