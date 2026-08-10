# Publishing oskiewar to retail Xbox

Research current as of **2026-08-07**. Every load-bearing claim below is sourced
to a Microsoft-owned page listed at the bottom. Where Microsoft's own documents
contradict each other — and on this topic they do, repeatedly — both readings are
given rather than one picked silently.

## Summary

There is one route to a retail Xbox console, and it is ID@Xbox plus a GDK port.
Microsoft states plainly that "UWP based games are no longer accepted in the Xbox
Store," which closes the platform the current app is built on. Store Policy
10.13.1 requires Xbox console games to go through ID@Xbox; the only documented
alternative, the Xbox Creators program, has no surviving enrollment page. Program
fees are now **$0** — both the $19 individual and $99 company Partner Center fees
were waived — so the cost is entirely engineering time plus devkit hardware.
Realistic calendar time from a standing start to concept approval is four to six
weeks; the port and certification work behind it is measured in months.

The port is not a recompile. `xbox/native-bios/App.cpp` is C++/CX against
`CoreWindow`, `ApplicationData`, `Windows.Gaming.Input`, `Windows.Web.Http`,
`Windows.Networking.Sockets`, `Windows.Devices.Midi`, and WinRT `StorageFile`.
GDK titles are plain x64 Win32 with a different API surface for every one of
those. The QuickJS core, the renderer's HLSL, the DSP, and `oskiewar.js` port
cleanly; the platform layer is a rewrite.

Two things would fail certification even after a clean port. The game is not in
the package — only `smoke_piece.js` ships, and `oskiewar.js` arrives through the
`live-piece.js` sideload path, which cannot exist on retail. And the app has zero
Xbox platform integration: no achievements (mandatory, minimum ten and 1000
gamerscore), no Xbox user model, no privilege checks, and it displays AC
`@handles` where XR-046 requires gamertags.

If the goal is "people who own an Xbox can play oskiewar this year," the honest
answer is that the Store is not that path. It already runs in Microsoft Edge on
retail consoles today at zero cost and zero certification. The Store is worth
doing, but as a deliberate multi-month project, not a packaging step.

## Where the app stands today

`AestheticComputer.NativeBios`, display name `oskiewar`, version 1.0.0.38.

- **Target:** UWP. `ApplicationType=Windows Store`, `AppContainerApplication=true`,
  `TargetDeviceFamily Windows.Universal`, min 10.0.17763.0. C++/CX (`CompileAsWinRT`),
  toolset v143, x64 only.
- **Runtime:** QuickJS-ng v0.15.1 vendored, 124 bound globals, D3D11 renderer at
  a fixed 1920×1080 backbuffer scaled to output, XAudio2.
- **Shipped piece:** `smoke_piece.js`, twelve lines — wipe, four button colors, a
  beep. `oskiewar.js` (5,549 lines, the actual game) is not in the package.
- **Piece delivery:** `PollLivePiece()` stats `LocalState\live-piece.js` every
  500 ms and hot-swaps on change. `xbox/tools/live.mjs` writes that file over
  Device Portal. The signed-manifest OTA design in `xbox/ota/` has no UWP adapter
  yet, so today's network code-fetch risk is design-stage, not shipped.
- **Network:** `api/mood`, `api/chat-messages`, `api/handle-colors`,
  `api/clock`, `api/piece-log`, `api/oskiewar-replays`,
  `wss://session-server.aesthetic.computer/oskiewar-live`. QR codes to
  `oskiewar.com`. Raw UDP on 51337 (MIDI inlet) and 51338 (OSC broadcast to
  255.255.255.255).
- **Content surfaces:** AC community `@handles`, per-handle mood strings, and
  latest public system-chat text for `@jeffrey`, `@fifi`, `@oskie`, `@sat`.
  A photo-disc service that recursively scans mounted removable volumes.
- **Analytics:** `oskiewar-analytics.mjs` → PostHog, aggregated server-side under
  a single fixed `distinctId`, not per-user.
- **CI:** AppVeyor produces a `SideloadOnly` x64 MSIX signed with a self-signed
  `CN=AestheticComputerDev` certificate. No Store packaging mode exists.

## The routes

### 1. ID@Xbox + GDK — the only path to retail consoles

Store Policy 10.13.1: "Game products that target Xbox consoles … must use Xbox
network services through the ID@Xbox program." Concept approval is the gate into
the program, not a step after it: "An approved game concept is required for all
XBOX games."

Cost is $0 in fees. Registration as an Xbox partner, the ID@Xbox application, and
Partner Center enrollment are all free; devkit hardware is bought through the
Entertainment Developer Store and its price is not published. Microsoft's own
timeline table gives NDA signature in 20 minutes to 3 business days, concept
review at 10–15 business days ("up to 3 weeks"), and Partner Center verification
at 3–5 business days, summarized as "around two weeks" — which does not reconcile
with its own three-week concept figure. Plan four to six weeks.

The GDK is public and needs no NDA to install. You can build against it from day
one and apply in parallel.

Trade-off: it costs a full platform-layer rewrite plus every Xbox integration the
app currently lacks. It is also the only option that puts oskiewar in front of a
console audience with a storefront page.

### 2. Microsoft Store on PC, self-serve — real, but does not reach consoles

New in 2026: a Win32/GDK path to the Microsoft Store with no ID@Xbox and no
concept approval. Microsoft is explicit that "Xbox services are not required to
publish a PC-only game using this path," and equally explicit that "If you plan to
publish to Xbox consoles in the future, you'll need to enroll in the appropriate
managed Xbox program at that time."

This is the cheapest way to learn Store certification for real — IARC, privacy
policy, packaging, the submission loop — on a product that ships. It is worth
doing first, and it answers none of the console question.

### 3. Non-game UWP app on the Xbox device family — closed by categorization

Partner Center's Xbox device-family checkbox is documented as self-checkable "if
your app is not a game." That does not help: Store Policy 10.1 requires that game
products "must be categorized as a game in our Store." Calling oskiewar an app to
route around the game gate is the thing the policy exists to stop.

Two further reasons not to pursue this. The `upload-app-packages` doc describing
that checkbox still names the Xbox Live Creators Program as a live option, which
dates it; whether the checkbox still functions for new submissions is not
documented anywhere and could not be verified. And 10.13.4 bars products on Xbox
consoles from enabling "general browsing of the operating system, file systems or
attached physical media file structures" — which the photo-disc feature is close
to by design.

### 4. Xbox Developer Mode — what exists today, and a dead end for distribution

The Dev Mode app is free (the $19 was the Partner Center individual fee, now
waived). The activation terms cap it: "Your use for the Purpose stated above is
limited to three Xbox One consoles at any one time," for testing and
"demonstrating your applications." A console in Dev Mode cannot run retail games.
The primary Dev Mode doc is now archived under `/previous-versions/`.

For an installation, a gallery, a demo table, or the studio, this is entirely
adequate and already works. It reaches nobody's living room.

### 5. Microsoft Edge on retail consoles — zero cost, available now

Retail Xbox consoles ship a Microsoft Edge app. oskiewar already runs on the web
at `oskiewar.com`, so any retail console can reach it today with no program, no
certification, and no port. Distribution is a URL, which is weak, and gamepad
behavior in Edge on Xbox for an arbitrary web game is not something I could verify
from a primary source — the Xbox support page is JavaScript-rendered and returned
no body. Worth an hour on a retail console to measure input latency and gamepad
mapping before dismissing it.

## What blocks certification

Ordered by how much work each represents, not by severity. Every one of these is
current-state, not hypothetical.

**The platform is closed.** "Please note that UWP based games are no longer
accepted in the Xbox Store. Please use the ID@XBOX program." Microsoft has not
published a formal UWP *deprecation* — the platform guide says only "Although
still supported, UWP is not under active development" — but for Xbox games the
door is shut. Everything below assumes the GDK port happens first.

**C++/CX is the wrong dialect, not just the wrong platform.** `App.cpp` uses
`ref new`, `String^`, `TypedEventHandler`, and `create_task` throughout, against
`CoreApplicationView`, `ApplicationData::Current->LocalFolder`,
`Windows.Gaming.Input`, `Windows.Web.Http.HttpClient`,
`Windows.Networking.Sockets`, `Windows.Devices.Midi`, WinRT `StorageFile`, and
Windows Imaging. GDK titles are standard x64 Win32. Every one of those needs a
replacement — `XGameSave`/`XPersistentLocalStorage`, `GameInput`, `libHttpClient`
or WinHTTP, Winsock. The QuickJS engine, the six HLSL shaders, the audio graph,
and `oskiewar.js` are portable; `App.cpp`'s platform layer is not.

**The game is not in the package.** CI packages `smoke_piece.js`. `oskiewar.js`
reaches the console only through `PollLivePiece()` reading
`LocalState\live-piece.js`, written by Device Portal, which does not exist on
retail. A Store build ships a twelve-line color demo. `oskiewar.js` has to be baked
in as a package asset, and `PollLivePiece()` has to be compiled out of release
builds — not merely inert, removed.

**Out-of-band code delivery is barred on console.** The applicable rule is not
the one `xbox/ota/README.md` anticipates. Microsoft has no "sandboxed interpreter"
policy — that construction is Apple's guideline 2.5.2, and citing it against a
Microsoft submission would be citing the wrong regulator. What applies is
10.2.5: "any products offered on Xbox consoles must be submitted using supported
package types … such products and in-product offerings must be installed and
updated only through the Microsoft Store." That is categorical for console, and it
retires the OTA lane there. XR-009 quotes the older phrasing more bluntly still:
"installed, serviced, and updated only through the Store."

10.2.2 is the softer, intent-based rule and would govern a PC build: a product
"must not attempt to fundamentally change or extend its described functionality …
through any form of dynamic inclusion of code." Note what that implies —
compliance is a function of the *store listing*, not the packaging. A listing that
describes oskiewar as a fighting game and then hot-loads new pieces violates it. A
listing that describes an Aesthetic Computer runtime whose whole point is running
AC pieces is a different conversation. On console the question is moot; on PC it
is worth having deliberately.

**Achievements are mandatory and do not exist.** "All games targeting Xbox
consoles are required to have Achievements." XR-055 sets the floor at 10
achievements and 1000 gamerscore at launch, no single achievement over 200 GS, all
achievable. Achievements require *full* Xbox services, which require concept
approval — so this is downstream of ID@Xbox and cannot be prototyped early.
XR-060 makes them permanent: once published, an achievement cannot be removed and
its unlock rules cannot change. Design them once, carefully.

**No Xbox user model, and the wrong display name.** XR-112 requires establishing
an active user before any profile-related action and handling suspend and
constrained modes. `App.cpp` has no `Suspending`/`Resuming` handlers at all —
that is both an XR-112 and an XR-001 (Title Stability) exposure. XR-046: "titles
must use the gamertag as their primary display name." oskiewar's roster is built
on AC `@handles` with per-handle colors — `@JEFFREY`, `@FIFI`, `@OSKIE`, `@SAT` —
rendered as the player name. That is a direct conflict, and it is a design
problem, not a string swap: the handle colors are the visual identity of the
character select. XR-047 additionally requires gamercard access wherever display
names are enumerated.

**The community-chat surface is UGC with none of the machinery.** `App.cpp` polls
`api/chat-messages` and renders public system-chat text and handle colors from an
online community. XR-018 requires content guidelines published in-product or on
the site, an in-product report path or proactive text filtration, and the ability
to remove content at Microsoft's request. Non-gamertag usernames are explicitly
"subject to proactive filtration and/or reporting requirements." XR-045 requires
checking `XPRIVILEGE_USER_CREATED_CONTENT` (247) and `XPRIVILEGE_COMMUNICATIONS`
(252) before showing any of it. None of these exist in the codebase.

Worth knowing before scoping this: the Forbidden Terms List that XR-018 names as
the "minimum baseline for blocking" lives at
`learn.microsoft.com/gaming/xbox-nda-docs/…`, which 404s without NDA credentials.
The requirement is public; the wordlist you must implement is not. Microsoft's
`StringService` API satisfies the proactive-detection requirement automatically —
that is the cheap way through, and it is GDK-only.

**Self-hosted multiplayer is allowed; ignoring the Xbox session layer is not.**
This is better news than expected. XR-067 permits a title's own multiplayer state
functionality provided it records player interactions through MPSD *or* the
Multiplayer Activity Recent Player feature. The session server and the
`oskiewar-live` WebSocket relay can stay. What has to be added: XR-064 (joinable
sessions surfaced through the Xbox shell), XR-124 (in-game invitations via the
Xbox platform), XR-070 (Xbox friends list as primary, never persisted on game
servers), XR-045 privilege checks including `CrossNetworkPlay` (185).

Open question worth asking Microsoft directly: oskiewar is two players on one
console, and its network layer is one-way spectator publishing plus a replay
upload. Whether that constitutes "synchronous network play" under 10.13.2 — which
would require Xbox network integration and ID@Xbox approval regardless — is
genuinely unclear from the policy text. The spectator relay is cross-network by
construction, which pulls XR-007 in as well.

**The photo-disc feature is close to a prohibited behavior.** 10.13.4 bars
products on Xbox consoles from enabling "general browsing of the operating system,
file systems or attached physical media file structures." `PhotoDiscService`
recursively scans mounted removable volumes for seven image formats, inventories
up to 4,096 files, displays them, and `discCopy` writes numbered copies to
`LocalState/photo-cd`. The manifest declares `removableStorage` and the `optical`
device capability. The mitigations already in place are real — WinRT handles stay
native, JS never sees a path — but the *user-visible behavior* is browsing
attached physical media, and that is what the policy names. This is the feature I
would cut from a console build first. It is also the one most likely to draw a
manual reviewer's attention.

**The LAN UDP lanes will not survive.** Port 51337 accepts MIDI over a private
network; 51338 broadcasts OSC to 255.255.255.255. These are studio instruments and
they are excellent, but XR-132 governs service access limitations, and a retail
console title opening a broadcast UDP socket is not a normal shape. Expect to gate
both behind a build flag. Nothing of the game depends on them.

**Packaging, signing, and metadata.** CI emits `SideloadOnly` with a self-signed
certificate. Store submission needs an `.msixupload`, a reserved name in Partner
Center, and Store association. Separately, XR-022 binds the title to Microsoft's
terminology list — which no longer contains "Xbox Live" at all; the term is "Xbox
network." Any UI or copy carrying the old name fails. XR-017 also warns that
rating elements left in the manifest are "cause for certification rejection."

**Rating and privacy are cheap but not free.** IARC is mandatory and free at
point of use — a questionnaire in Partner Center at first submission. Microsoft
publishes no mapping from violence content to a rating tier, so the questionnaire
is the only authority. Note the metadata ceiling in 11.1: screenshots, trailer,
and description "may contain only content that would merit a rating of PEGI 12,
ESRB EVERYONE 10+, or lower," whatever the game itself rates. The store page has
a tighter bar than the game.

`system/public/privacy-policy.html` exists and is dated February 2026, but it is
seven sentences and does not cover what 10.5.1 requires: how information is
"used, stored and secured," the "types of parties to whom it is disclosed" (it
says "analytics" without naming PostHog), or "the controls that users have over
the use and sharing of their information." 10.13.11 goes further for Xbox — an
independent-controller privacy statement, no personalized advertising from Xbox
user data, no social-graph storage, and a standing obligation to check Microsoft's
deleted-account list **at least every 30 days**. That last one is an operational
commitment with a recurring cost, not a document.

**Accessibility is not a gate.** Contrary to the usual assumption: there is no
accessibility XR, and the Xbox Accessibility Guidelines say so themselves — they
are "best practices" and "aren't intended to act as a checklist to validate any
type of compliance or legal requirements." The Store's "Accessible" tag is an
optional self-declaration. Do the work because it is right; do not budget it as
certification.

## Next steps

Nothing before step 4 requires a decision about the port, and nothing before step
4 costs money.

1. **Measure Edge on a retail console.** One hour. Load `oskiewar.com` on a retail
   Xbox, test gamepad mapping and latency. If it holds up, that is a shipping
   answer to "people can play it on their own Xbox" available this week, and it
   changes how urgent everything below is.

2. **Bake `oskiewar.js` into the package and delete the sideload path from release
   builds.** One to two days. This is required for every route, is independently
   correct, and turns the MSIX into an actual game rather than a shell. Keep
   `PollLivePiece()` behind a debug configuration so the dev loop survives.

3. **Register at `storedeveloper.microsoft.com`.** $0, same day. Use this entry
   point specifically — Microsoft states it is "the only supported entry point for
   the new flow" and that Partner Center, Xbox, or Visual Studio "will show the
   legacy flow," whose pricing Microsoft does not publish. Choose a **company**
   account: the Xbox-specific docs say business account, and conversion from
   individual to company is not supported. Verification is 2–5 business days.
   Use a new dedicated personal MSA; onboarding explicitly forbids reusing one.

4. **Install the GDK and spike the platform layer.** One to two weeks. Public, no
   NDA, `winget`. The question to answer is narrow: does QuickJS plus the D3D11
   renderer plus `oskiewar.js` boot under GameCore x64? If it does, the port is
   bounded and the rest is API substitution. If it does not, that is the moment to
   reconsider.

5. **Ship the PC build to the Microsoft Store.** Two to four weeks after the port
   spike. Self-serve, no concept approval. This buys real certification
   experience — IARC, privacy policy, packaging, review turnaround — on a product
   that ships, and it de-risks the console submission. It is also the point at
   which the 10.2.2 listing-description question for the OTA lane has to be
   answered on purpose.

6. **Apply to ID@Xbox with a concept.** $0. NDA in 20 minutes to 3 days, concept
   review 10–15 business days. Up to ten concepts can be in review at once.
   Requires a legally registered business name and a tax ID — see the open
   questions below if there is no entity. Submit while step 5 is in flight; the
   two do not block each other.

7. **On approval: design achievements, wire the Xbox user model, and rebuild the
   name layer.** One to two months. Ten achievements and 1000 gamerscore, designed
   once because XR-060 makes them permanent. Gamertag as primary display name,
   which means rethinking how the AC handle identity survives — probably as a
   secondary badge rather than the player name. Privilege checks. Suspend and
   resume. Multiplayer Activity reporting for the spectator lane.

8. **Cut or gate the console-hostile features, then submit.** Remove photo-disc
   from the console build. Gate the UDP MIDI and OSC lanes behind a build flag.
   Add UGC reporting or `StringService` filtration for the chat feed. Publish
   content guidelines. Rewrite the privacy policy against 10.5.1 and 10.13.11, and
   stand up the 30-day deleted-account check as a scheduled job.

Total: **$0 in program fees**, devkit hardware unpriced, and realistically four to
six months of engineering for a solo developer, most of it in steps 4, 7, and 8.

## Open questions for Microsoft

Worth asking a rep before committing, in roughly this order.

1. **Does ID@Xbox accept a sole proprietorship?** The public hub lists exactly
   three hard requirements — 18+, sign an NDA, be in a supported country — and
   mentions no entity. The onboarding form demands a "legally registered business
   name with suffix (for example, LLC, GmbH, or SRL)" and a "registered business
   number, such as a tax ID." The FAQ item literally titled "Do I need to be an
   established studio to develop for XBOX?" is JavaScript-rendered and could not
   be read. This is the single biggest unknown for a solo developer and it gates
   everything from step 6 on.

2. **What does "limited Xbox services" actually include?** Microsoft documents
   only what it excludes — publishing and achievements. Whether sign-in, presence,
   leaderboards, or multiplayer sessions are available pre-approval is not
   published anywhere. It determines how much of step 7 can be started early.

3. **Is a two-player-local game with one-way spectator publishing "synchronous
   network play" under 10.13.2?** The answer decides whether the current spectator
   architecture can survive as-is or has to move onto Xbox network services.

4. **Is the Xbox Creators program actually dead?** Store Policy 7.19 — updated
   2026-07-30 — still offers it: "Optionally, you may publish your game product to
   console without integration of Xbox network Services through the Xbox Creators
   program." Every enrollment URL 404s or redirects to ID@Xbox, and its only
   technology was UWP. No retirement announcement exists on any Microsoft domain.
   The overwhelmingly likely reading is stale policy text, but it is the one thing
   in this document that would change the recommendation if it were wrong, so it
   is worth one direct question.

5. **Does `StringService` cover the AC chat feed?** If it satisfies XR-018's
   proactive-detection requirement for text pulled from a third-party community
   feed, the moderation work collapses from a project to an integration.

## What could not be verified

Stated plainly rather than smoothed over.

- No formal Xbox Live Creators Program retirement announcement exists. Its death
  is inferred from dead URLs and the UWP closure.
- No formal UWP deprecation notice exists, and no UWP-to-GDK migration guide was
  found on any Microsoft domain.
- Devkit pricing through the Entertainment Developer Store is not public.
- The legacy Partner Center registration flow's price is not stated; that it is
  still $19/$99 is inference.
- Whether Partner Center's Xbox device-family checkbox still functions for new
  submissions is undocumented and untestable without an account.
- Gamepad behavior for arbitrary web games in Edge on Xbox — the support page is
  JavaScript-rendered and returned no readable body.
- The Forbidden Terms List and the Xbox Game Store Policy PDF are NDA-gated; the
  404 was confirmed but the contents were not read.

Several live, indexed Microsoft pages are demonstrably stale and should not be
trusted on this topic: `/windows/uwp/gaming/concept-approval` (2017), the body —
as opposed to the banner — of the UWP-on-Xbox FAQ, and the Xbox section of
`upload-app-packages`. There is also a widely-cited Microsoft Q&A answer on this
exact question that carries Microsoft's own AI-generated disclaimer. None of it
was used as evidence here.

## Sources

All accessed 2026-08-07.

**Programs and onboarding**
- ID@Xbox hub — https://developer.microsoft.com/en-us/games/partner
- Xbox partner onboarding overview — https://learn.microsoft.com/en-us/gaming/game-publishing/onboarding/overview
- Register as an Xbox partner — https://learn.microsoft.com/en-us/gaming/game-publishing/onboarding/onboarding-register
- Join ID@Xbox — https://learn.microsoft.com/en-us/gaming/game-publishing/onboarding/onboarding-join-id-at-xbox
- Submit game concepts — https://learn.microsoft.com/en-us/gaming/game-publishing/publishing-processes/managed-creators/publishing-processes-game-concepts
- Configuring Xbox services — https://learn.microsoft.com/en-us/gaming/game-publishing/concepts/xbox-services
- Open a developer account — https://learn.microsoft.com/en-us/windows/apps/publish/partner-center/open-a-developer-account
- Individual registration fee waived — https://learn.microsoft.com/en-us/windows/apps/publish/whats-new-individual-developer
- Company registration fee waived — https://learn.microsoft.com/en-us/windows/apps/publish/whats-new-company-developer
- What's new in game publishing (PC self-serve path) — https://learn.microsoft.com/en-us/windows/apps/publish/whats-new-game-publishing
- GDC 2026: what's changed in Xbox development — https://developer.microsoft.com/en-us/games/articles/2026/03/gdc-2026-press-start-get-your-pc-game-ready-for-xbox-in-one-day/

**UWP status**
- UWP on Xbox FAQ (the "no longer accepted" note) — https://learn.microsoft.com/en-us/windows/uwp/xbox-apps/frequently-asked-questions
- What's a UWP app? ("not under active development") — https://learn.microsoft.com/en-us/windows/uwp/get-started/universal-application-platform-guide
- Xbox Dev Mode activation, archived — https://learn.microsoft.com/en-us/previous-versions/windows/uwp/xbox-apps/devkit-activation
- Dev Mode activation program terms — https://learn.microsoft.com/en-us/legal/windows/agreements/xbox-one-developer-mode-activation

**Store policy**
- Microsoft Store Policies v7.19, effective 2025-10-14 — https://learn.microsoft.com/en-us/windows/apps/publish/store-policies
- Store policy change history — https://learn.microsoft.com/en-us/windows/apps/publish/store-policies-change-history
- Upload MSIX app packages (Xbox device family) — https://learn.microsoft.com/en-us/windows/apps/publish/publish-your-app/msix/upload-app-packages
- Age ratings in Partner Center — https://learn.microsoft.com/en-us/windows/apps/publish/publish-your-app/msix/age-ratings
- App certification process — https://learn.microsoft.com/en-us/windows/apps/publish/publish-your-app/msix/app-certification-process

**Xbox Requirements — public, version 16.3 dated 2026-07-01**
- Console certification requirements (full XR list) — https://learn.microsoft.com/en-us/gaming/gdk/docs/store/policies/console/certification-requirements
- XR and Store Policies for PC, Mobile, and Creators Program v6.0 — https://learn.microsoft.com/en-us/gaming/gdk/docs/store/policies/pc/live-policies-pc
- XR-017 Title Ratings — https://learn.microsoft.com/en-us/gaming/gdk/docs/store/policies/xr/xr017
- XR-018 User-Generated Content v1.6 — https://learn.microsoft.com/en-us/gaming/gdk/docs/store/policies/xr/xr018
- XR-055 Achievements and Gamerscore — https://learn.microsoft.com/en-us/gaming/gdk/docs/store/policies/fma/xr055-achievements
- Console certification terminology (XR-022 list) — https://learn.microsoft.com/en-us/gaming/gdk/docs/store/policies/console/console-certification-terminology

**Xbox services**
- Xbox services overview — https://learn.microsoft.com/en-us/gaming/gdk/docs/services/fundamentals/live-xbl-overview
- Introduction to Xbox services APIs (XSAPI) — https://learn.microsoft.com/en-us/gaming/gdk/docs/services/fundamentals/xbox-services-api/live-introduction-to-xbox-live-apis
- Using XSAPI — https://learn.microsoft.com/en-us/gaming/gdk/docs/services/fundamentals/xbox-services-api/live-gs-xbl-apis
- Xbox subscription terms (Game Pass Essential rename, F2P multiplayer) — https://www.xbox.com/en-US/legal/subscription-terms
- Game Pass comparison — https://www.xbox.com/en-US/xbox-game-pass/compare

**Accessibility**
- Accessibility in the Store (optional declaration) — https://learn.microsoft.com/en-us/windows/apps/design/accessibility/accessibility-in-the-store
- Xbox Accessibility Guidelines v3.2 — https://learn.microsoft.com/en-us/gaming/accessibility/guidelines

**Not readable without credentials**
- `learn.microsoft.com/en-us/gaming/xbox-nda-docs/` — 404 unauthenticated; contains the Forbidden Terms List
- Xbox Game Store Policy / Publisher Guide — https://www.microsoft.com/en-us/software-download/xboxpublisherguide — gated
- Edge on Xbox support article — https://support.xbox.com/en-US/help/hardware-network/console/use-microsoft-edge-on-xbox — JavaScript-rendered, no body returned
