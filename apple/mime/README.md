# MIME.ac

The iPhone app for watching what everyone makes across Aesthetic Computer, KidLisp and Whistlegraph, one piece per screen. Tap a supported MIME type at bottom-right to create a new work directly in MIME. AC pieces, PNG paintings, Doom WADs and Roblox rooms use Whistlegraph’s shared native editor and bundled Aesel engine, with AC sign-in, prompts, drawings, voice, projects, version history, undo and export. Other media types remain read-only.

The feed is the same ranked network feed as https://mime.ac (`GET /api/mime?feed=1&seed=…`, `system/backend/mime-feed-engine.mjs`), with a random seed per launch, so each launch gets its own order.

- `Sources/Feed.swift`: the feed model and how each post draws (image, video, live runtime, packed HTML, words, document).
- `Sources/Reel.swift`: the loading wordmark, vertical pager, and cards. The first piece loads behind MIME; the lettering breaks apart when its content is ready, revealing the feed without a swipe. The code sits top-left and is the Interact control, the handle bottom-left, and the MIME type bottom-right. Supported type labels open creation after sign-in. A small wordmark stays top-right.
- `Sources/PieceRuntime.swift`: one live web runtime for the card in view. KidLisp and pieces load aesthetic.computer, Whistlegraph pieces load `/mime-play`. Autoplay is allowed and the runtime is asked to start audio. A non-persistent website data store keeps pieces from seeing any session.
- `Sources/MimeCreation.swift`: presents the shared Whistlegraph editor as a full-screen mode. The app root owns its persistent session. Feed, a left swipe across the header or from the right edge, and Projects’ “Explore mimes” return to the same feed position even during generation. Tapping the MIME wordmark resumes the current work; a type tap starts a new v0 project when idle and resumes existing work when busy. Local work lives in MIME’s own app container; AC account-backed piece synchronization uses the existing service.
- `Sources/PalText.swift`: MacPal's title lettering (macpal/Sources/PalCore.swift), with outlines under fills.
- `Sources/MimeRemix.swift`: imports the displayed public AC source, PNG pixels or classic Doom WAD as a separate project, with its source version and hash recorded in v0.
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
Build 9 slides the full-screen editor right into place; swipe left to return to
the feed. Its header and safe areas share the editor’s current color. Navigation
does not wait for inference or an artwork frame. The wooden frame, chalk icon
between Type and Talk, and Brain → Canvas image resolution remain shared with
Whistlegraph. The account and engine initialize behind the feed; reloads conceal
the HTML controls until native preview styling is ready. Doom WADs autoplay through the same
`preview=1&mime=1` player mode used by the website.
Build 10 gives the editor explicit playback ownership. Leaving for the feed
suspends its Web Audio contexts and media, including late render/speech resumes
and new preview frames. Reopening resumes the existing work. Native story
narration stops on exit; generation keeps running.
Build 11 puts a tappable version beside the global identifier for editable AC
pieces, PNGs and classic Doom WADs. It opens a separate v0 with a “Stole from”
citation; subsequent edits start at v1. The current project is retained. Public
Whistlegraph source requests pin the displayed version and refuse a changed
publication. The website uses the same badge and hands remixing to the app via
`mime://remix`; it does not yet host the editor. Native letters now share the
web's character hash, colors, motion period and easing, pausing offscreen and
under Reduce Motion. WAD imports retain maps and custom asset bytes, omit
byte-identical Freedoom assets from saved versions, and rebuild one complete
sprite namespace when exported.
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
