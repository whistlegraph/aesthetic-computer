# MIME.ac

The iPhone app for watching what everyone makes across Aesthetic Computer, KidLisp and Whistlegraph, one piece per screen. Tap a supported MIME type at bottom-right to create a new work directly in MIME. AC pieces, PNG paintings, Doom WADs and Roblox rooms use Whistlegraph’s shared native editor and bundled Aesel engine, with AC sign-in, prompts, drawings, voice, projects, version history, undo and export. Other type labels resume the existing editor workspace.

Public chat messages containing a recognized YouTube video link show its poster and an inline YouTube player. The chat identifier, author, source and accompanying words remain intact. Playback pauses offscreen; unavailable embeds retain a YouTube link.

The feed is the same ranked network feed as https://mime.ac (`GET /api/mime?feed=1&seed=…`, `system/backend/mime-feed-engine.mjs`), with a random seed per launch, so each launch gets its own order.

- `Sources/Feed.swift`: the feed model and how each post draws (image, video, live runtime, packed HTML, words, document).
- `Sources/Reel.swift`: the loading wordmark, vertical pager, and cards. The first piece loads behind MIME; the lettering breaks apart when its content is ready, revealing the feed without a swipe. The code sits top-left, the handle bottom-left, and the MIME type bottom-right. The center of a playable card accepts input immediately. Held input hides the labels toward their corners; after release, one idle second and the return animation unlock the 48-point top/bottom swipe strips. The outer 8 points always allow a feed swipe, including during cooldown. Edge contact glows before navigation. Reduced Motion fades the labels. Supported type labels open creation after sign-in. A small wordmark stays top-right.
- `Sources/PieceRuntime.swift`: one live web runtime for the card in view. KidLisp, registered pieces and Whistlegraph pieces share `/mime-play`. Autoplay is allowed and the runtime is asked to start audio. A non-persistent website data store keeps credentials out of pieces. The shared player receives only the current public handle, exposed through `handle()` and cleared on sign-out.
- `Sources/MimeCreation.swift`: presents the shared Whistlegraph editor as a full-screen mode. The app root owns its persistent session. The arrow or left swipe beside the preview, the right-edge swipe, and Projects’ “Explore mimes” return to the same feed position even during generation. Tapping the MIME wordmark resumes the current work; a type tap starts a new v0 project. Local work lives in MIME’s own app container; AC account-backed piece synchronization uses the existing service.
- `Sources/MimeWorkQueue.swift`: saves remix and new-work intents until the editor can open them. AC turns already accepted by the server can be parked immediately; jobs running on the phone keep their engine until finished. Queued entries remain available after navigation or relaunch, with removal and retry from the feed.
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
Build 14 shows the provisional version in pulsing orange, followed by distinct
stage symbols and a single line of arriving source. Tapping the stages hides or
shows the source; Stop remains at the right. The version number stays fixed
through the turn, and motion stops offscreen and under Reduce Motion.
Remixing, new projects, and project/type switching can leave a server turn
running. Parking preserves its journal and detaches the old viewer before any
late result can touch the next project. The existing server queue schedules
generation across projects; PNG, WAD and Roblox generation still run locally.
Build 15 plays saved Oskiewar rounds in the feed. Their existing `ow-…` IDs
remain the global identifiers. Web and app share a filter over the newest 1,000
rounds: both fighters must supply input, with sustained combat and usable frames
in that round. Up to 200 enter the feed; active @handle rounds get priority.
Idle rounds retain their permalinks. Playback uses the recorded checkpoint viewer,
pauses offscreen, and does not enter a live match or publish another replay.
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

Mood and chat cards show their meaning as `mood` and `chat`; their underlying
media type remains `text/plain`. The + button resumes the shared Whistlegraph
editor. Website creation links use `mime://create`; a supported `type` opens a
new project in that medium. Unsupported types open the existing workspace.
There is no separate web or native feed uploader.

## TestFlight

From this directory: `fastlane ios status`, `fastlane ios build`, `fastlane ios upload`, `fastlane ios distribute` (which makes the private **MIME.ac Friends** group if needed), and `fastlane ios invite email:<email> name:<first-name>`. These use the same Admin API key as AC, Whistlegraph, Oskiewar and Menu Band.

The first app record needs an Apple ID login once, in the terminal (two-factor included): `FASTLANE_USER=<Apple ID> fastlane ios create`. Apple's public API cannot create apps.

NES ROM posts use the shared `/nes-player/` runtime with touch, keyboard and
standard gamepad controls. Aesel’s existing file-publishing path recognizes `.nes` and returns a
`nes~` permalink. The runtime
pauses and releases held buttons offscreen; gamepad and keyboard activity also
hold the corner-label cooldown. NES changes require a new native app release.
