# MIME.ac

The iPhone app for watching what everyone makes across Aesthetic Computer, KidLisp and Whistlegraph, one piece per screen. Tap a supported MIME type at bottom-right to create a new work directly in MIME. AC pieces, PNG paintings, Doom WADs and Roblox rooms use Whistlegraph’s shared native editor and bundled Aesel engine, with AC sign-in, prompts, drawings, voice, projects, version history, undo and export. Other media types remain read-only.

The feed is the same ranked network feed as https://mime.ac (`GET /api/mime?feed=1&seed=…`, `system/backend/mime-feed-engine.mjs`), with a random seed per launch, so each launch gets its own order.

- `Sources/Feed.swift`: the feed model and how each post draws (image, video, live runtime, packed HTML, words, document).
- `Sources/Reel.swift`: the loading wordmark, vertical pager, and cards. The first piece loads behind MIME; the lettering breaks apart when its content is ready, revealing the feed without a swipe. The code sits top-left and is the Interact control, the handle bottom-left, and the MIME type bottom-right. Supported type labels open creation after sign-in. A small wordmark stays top-right.
- `Sources/PieceRuntime.swift`: one live web runtime for the card in view. KidLisp and pieces load aesthetic.computer, Whistlegraph pieces load `/mime-play`. Autoplay is allowed and the runtime is asked to start audio. A non-persistent website data store keeps pieces from seeing any session.
- `Sources/MimeCreation.swift`: keeps the existing Whistlegraph editor warm and presents it as a smaller card over the paused feed. Each type tap starts a new v0 project; dragging its grabber down and Projects’ “Explore mimes” return to the same feed position. Local work lives in MIME’s own app container; AC account-backed piece synchronization uses the existing service.
- `Sources/PalText.swift`: MacPal's title lettering (macpal/Sources/PalCore.swift), with outlines under fills.
- `Sources/Wordmark.swift`: the same Comic Relief Bold lettering and four letter colors as mime.ac. Handle and MIME type share a baseline on both interfaces.
- `Artwork/icon.html`: the app icon's source. Render it at 1024×1024 into `Resources/Assets.xcassets/AppIcon.appiconset/icon.png`.

XcodeGen generates `Mime.xcodeproj` from `project.yml`. Bundle `computer.aesthetic.mime`, team `FB5948YR3S`.

Native SwiftUI and the website share `system/public/mime-layout.json`, served
at `https://aesthetic.computer/mime-layout.json`. Build 2 reads schema 1 when
opening or foregrounding the app. Font sizes, spacing, captions and optional
titles can change through a web deploy. Invalid settings retain the last
valid layout; offline launches use that cached layout or the bundled JSON.
The global code always remains top-left, handle bottom-left, type bottom-right.
New native controls or schema capabilities require another app build.
Build 8 slides the editor up as an opaque card, with the wooden artwork frame,
a chalk icon between Type and Talk, and image resolution under Brain → Canvas.
The account and engine initialize behind the feed; reloads conceal the HTML
controls until native preview styling is ready. Doom WADs autoplay through the same
`preview=1&mime=1` player mode used by the website.
Build 3 adds the automatic loading transition. `loadingBreakupSeconds` adjusts
its duration from production (0.15–2 seconds, default 0.8); older schema-1
configs remain valid. Reduced Motion uses a brief fade. Images, documents,
video readiness and runtime first-frame signals release the loading screen;
failed or empty feeds provide a retry instead of trapping the user.

After committing and pushing main, `bash apple/mime/release-mime.sh device`
deploys and verifies the website/config, then builds, installs and launches
the native app on the connected iPhone (`DEVICE` selects it). Use `testflight`
to archive/upload instead, or `web` for production presentation updates only.
Both releases use the same source revision.

## TestFlight

From this directory: `fastlane ios status`, `fastlane ios build`, `fastlane ios upload`, `fastlane ios distribute` (which makes the private **MIME.ac Friends** group if needed), and `fastlane ios invite email:<email> name:<first-name>`. These use the same Admin API key as AC, Whistlegraph, Oskiewar and Menu Band.

The first app record needs an Apple ID login once, in the terminal (two-factor included): `FASTLANE_USER=<Apple ID> fastlane ios create`. Apple's public API cannot create apps.
