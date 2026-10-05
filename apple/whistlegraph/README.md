# Whistlegraph

The iPhone app for making AC pieces with voice, sound, and typing. Product domain: `whistlegraph.app`. The practice and archive remain at `whistlegraph.org`.

From this directory, run `./run.sh device` to bundle, build, install, and open the app on a paired iPhone. Use `DEVICE=<identifier>` if more than one phone is paired. `./run.sh simulator` builds for a simulator; select one with `SIMULATOR=<identifier>`. XcodeGen generates `Whistlegraph.xcodeproj` from `project.yml`.

For an unsigned build transferred from poorslice, run `bash sign-device.sh <Whistlegraph.app> <identity> <profile> <entitlements>` before packaging or installing. This signs embedded debug libraries before the app and verifies all nested signatures. An install succeeding does not prove launch succeeds: verify the unlocked phone opens the workspace before marking a build launch-verified.

Typing uses AC's `compkey` sample and QWERTY pitch mapping. Enter sends the prompt, pasted line breaks become spaces, and the limit is 96 characters. Account settings control key and button sounds together.

## Story cards

The stacked-cards button opens the selected version's ancestry as a vertical story. Pause, previous, next, and close leave the editing selection intact. The picture keeps its 4:3 aspect; its version and spoken caption sit directly below it with space reserved around them for social-app overlays. Each version gets a stable colored card background behind both the picture and captions. Version labels use the same bold Comic lettering, dark outline, and cyan/pink shadows as the editing interface; MP4s retain that styling.

Export uses the shared AC `canvas-tape.mjs` hardware encoder, also used by BIOS HD tapes. An internal 1080×1920 canvas composes only program pixels and captions; native AVFoundation adds the original local utterances or file-rendered speech. No screen or microphone recording is used. Export runs through the story in the foreground, with a ten-minute/256 MB limit, cancellation, and a local MP4 preview offering Save video (Photos add-only permission) and Share. Program-generated synth audio is not yet mixed into this story export.

`StoryCardsTests` checks branch navigation, caption placement, selection restoration, and the on-device MP4 flow. The `story` debug fixture uses separate local storage and no cloud thread; it writes `Documents/story-export-test.mp4` for frame/audio inspection.

## Update compatibility

Whistlegraph updates the existing Walkieware installation. Keep these persisted and deployed contracts until an explicit migration replaces them:

- `computer.aesthetic.walkieware`: iOS bundle identifier and Keychain service. Changing the bundle identifier installs a separate app and loses access to the existing container.
- `walkieware://app`: bundled WebView origin. Its local storage contains pieces, version histories, cloud identities, and recovery state.
- `walkieware-*` preferences, local-storage keys, generated source markers, and `Utterances/` recordings.
- `walkie` script-message handler, `walkieware*` JavaScript bridge, `WALKIE_*` fixture controls, and existing backend routes and schemas. These remain compatible with the deployed service and test tools.

The directory, native Swift types, Xcode targets/schemes, installed app name, permission text, and visible web shell use Whistlegraph. This refactor does not configure DNS or publish a website.

## Checks

From the repository root:

```sh
node apple/whistlegraph/bundle.mjs
node --test apple/whistlegraph/Tests/*.test.mjs
node apple/whistlegraph/Tests/native-bridge.test.cjs --native-shell
```

The browser check uses Puppeteer and Chrome with mock inference; it makes no model requests. Native UI tests live in the `WhistlegraphUITests` scheme.

## Edit checks

Every generated edit runs syntax/API checks and waits for matching-source runtime feedback before the visual review. Client errors trigger at most one repair across code and visual checks. A candidate that still fails restores the previous saved version; its failure remains visible after the restored frame paints. Missing or stale evidence cannot trigger a paid repair or save a version. There is no opt-out setting.

The initial generation uses DeepSeek V4.1 Flash with four tool rounds and one output continuation. The single repair uses DeepSeek V4 Pro with a 1,024-token thinking budget, two tool rounds and no continuation. Provider calls retain the 4,096-token output limit. Remote requests and remote edit descriptions use the same 96-grapheme, single-line limit as phone typing; oversized requests are rejected before generation. Diagnostic context and source code are separate from the short user request. Code generation/checking has a 75-second deadline; visual review has a separate 90-second deadline. These bound work, not cost. A painted frame is execution evidence, not visual acceptance.

## Pixel size

Tap the piece name → Pixel size: **1×, 2×, 3×, 4×**. The default is **2×**, matching `bios.mjs`. Bigger values make bigger pixels. Changes resize the live preview without creating a version or restarting the piece; the preference survives app launches.

## Story export

Opening cards starts preparing the MP4 as they play. Each completed card is cached on the device; pause omits paused time and seeking discards only an unfinished card. The selected branch is assembled from those clips without re-encoding their video. Reopening a completed story can share immediately, including after relaunch. Cache keys include piece identity, immutable revision metadata, pixel size, and rendering/voice format. Changed branches reuse unchanged cards. The movie cache evicts older unprotected files above 256 MB; iOS may also reclaim it.

The share button shows progress when preparation is unfinished and opens a compact Save video / Share MP4 popover when ready. Automatic preparation never opens the popover. Save video shows activity through the Photos write. Capture runs at playback speed while cards remain open in the foreground; leaving cards or backgrounding the app stops unfinished work and retains completed clips.

Original voice recordings remain the narration when available. Computer narration uses Jeffrey’s ElevenLabs voice through AC `/api/say`, shared by card playback and export, with a 32 MB local audio cache. The API key stays on the server. If the service is unavailable, device speech can finish the export; fallback clips have separate cache keys so reopening can retry Jeffrey’s voice.

`Tests/StoryCacheCheck.swift` exercises real AVFoundation composition, partial completion/relaunch, warm export, density invalidation, cancellation, and cache eviction with a supplied MP4 fixture; run `sh apple/whistlegraph/Tests/story-cache.test.sh` on macOS to generate that fixture and run the check. `Tests/story-tape.test.mjs` covers pause/resume and stale chunks after cancellation. `StoryCardsTests` covers background preparation and the compact cached-export popover.

## Attempt receipts

The app retains the latest 100 attempt receipts per thread locally and uploads finalized receipts when the backend advertises `attempt-receipts-v1`. Older servers continue working while uploads remain pending. Reconnect retries are idempotent. The server retains the latest 100 receipts per thread; archives retain their separate local journal. Account deletion removes server threads and their receipts.

Receipts contain source hashes, parent version, render IDs, timings, check codes, model identifiers, provider request IDs, and reported usage. Missing usage/cost remains null. They exclude prompt text, generated source, audio, console text, and raw responses. These are client-observed diagnostics, not authoritative billing records; acceptance remains `unreviewed`.

Authenticated `GET /api/walkieware?code=<thread>` includes receipts. `DELETE /api/walkieware?code=<thread>&receipts=1` clears retained server receipts for that owner; pending local receipts may upload afterward.

Additional checks, from the repository root:

```sh
node --test aesel/test/edit-contract.test.mjs aesel/test/attempt-receipt.test.mjs lith/walkieware-socket.test.mjs
node apple/whistlegraph/Tests/native-bridge.test.cjs --native-shell --checked-edits
node apple/whistlegraph/Tests/native-bridge.test.cjs --native-shell --checked-edits --repair-fails
```

The browser checks use mock inference. They verify the contract, repair limit, rollback, render identity, and receipt fields without spending braincells.

## Chalk with words and sound

**Chalk** toggles an ink layer over the preview above the controls. The keyboard and microphone remain available. Hold Talk with one finger and chalk over the preview with another, even when Chalk was off. Releasing Talk normally sends the combined request (up to eight seconds). Swipe left on Talk and release to latch a performance: Type folds away, Talk widens into a chalk-colored Send control, and the microphone stays open while drawing. Tap Send to submit both, up to 45 seconds. The original recording stays local; the model receives the transcript, word timing, measured pitch/energy, and the drawing image plus timed strokes. A `whistlegraph-performance/v1` marker keeps this joint interpretation distinct from an ordinary spoken edit. The Chalk toggle keeps the layer available between recordings and while typing. Typed Enter sends text and the current sketch together; Send beside the drawing can submit it alone. Undo removes the last stroke; Clear discards the sketch. Turning Chalk off returns touch interaction to the piece while retaining visible unsent marks.

Each successful version retains its combined request: transcript, ordered normalized strokes, elapsed stroke timing, optional measured Pencil pressure, and existing sound/word measurements. Sound time zero is aligned to the drawing timeline. Finger pressure is unknown. Failed requests retain the draft and saved request for retry; a successful commit consumes only the matching draft revision. Unsubmitted native sketches are in memory; accepted attempts use the existing durable recovery journal. New/open piece discards the current sketch.

The model receives a PNG of the submitted chalk alongside vector coordinates and measured sound cues in one request. The PNG keeps the preview's aspect ratio and mark positions, with a longest side of 768 pixels. It contains only marks on white; it does not capture the app or the piece. History and recovery retain vectors and regenerate the image, including on a checked-edit repair. Old images leave the model conversation at the next turn; selected-branch text still supplies history.

Chalk follows **understand, reify, evolve**: interpret the idea or intended change in context, give it a concrete working form, and let later input develop it. Marks can propose concepts, relationships, targets or transformations. Tracing, literal fidelity and animation are not defaults. Existing work supplies continuity while allowing coherent transformation; captions describe only implemented behavior. This is a prompting policy, not a trained gesture classifier or a measured recognition guarantee.

Held taps are rendered as dots even when their samples repeat the same position. Stored strokes remain independently normalized to 0..1000 on each axis. Provider evidence converts them to one aspect-correct plane with explicit width and height, then sends each stroke’s full bounds and at most six timed samples. This keeps spatial and temporal cues without inviting a verbatim point-array transcription. The PNG carries the complete sampled form; the compact observations cannot reconstruct every bend or pause. Both describe the same positions, so a single fit scale covers the full drawing without stretching or clipping. The model receives source context, not a captured preview frame, so precise targeting of animated objects remains unresolved. Drawing bypasses text-only local shortcuts. Sampling caps the prompt at 320 points across at most 32 strokes (the native draft retains at most 1,200 points). Timings and endpoints survive sampling; fine detail can be lost.

Deploy the bounded PNG support in `easel-policy.mjs`, `easel-input-images.mjs`, and `easel-paid-credits.mjs` before installing this client for paid inference. The selected model must support image input. The server accepts only static inline PNGs up to 768×768 and 700,000 base64 characters, at most 16 per request. Paid holds conservatively include one token per pixel plus image overhead and serialized bytes; this can require substantially more available credit than the final cost. Actual provider usage settles the charge and refunds the unused hold. Free and paid requests use the same image validation.

```sh
node --test apple/whistlegraph/Tests/drawing-input.test.mjs
node apple/whistlegraph/Tests/native-bridge.test.cjs --native-shell --drawing
node --test system/tests/easel-input-images.test.mjs system/tests/easel-paid-credits.test.mjs
```

## App icon

The app icon depicts **Butterfly Cosplayer (IMAB)** by Jeffrey Alan Scudder. `Artwork/imab-icon-goopy.png` is a generated glossy, thick adaptation of the existing drawing in `pop/hellsine/assets/whistlegraph-butterfly.png`, using the original performance glyph at `system/public/whistlegraph.org/glyphs/imab.jpg` as a color reference. It is not the original score image. The flat adaptation remains in `Artwork/imab-icon.png`; built-in imagegen prompts are retained in `Artwork/imab-icon.prompt.txt` and `Artwork/imab-icon-goopy.prompt.txt`. `./build-icon.sh` packages the adaptation as an opaque 1,024-pixel iOS icon.

Build 98: the brain button below the preview opens pixel size (eye), model selection, provider, daily/purchased braincells, and request usage. New Piece and Your Pieces remain in the piece-name menu, alongside the selected model/provider. Model selection persists per verified account. Jeffrey defaults to hosted Claude Opus 5 with 16,384 output tokens, 4,096 thinking tokens, 12 tool rounds and a 180-second generation/check deadline; other accounts retain their existing budgets. Both still have one shared repair attempt and mandatory source/runtime/picture checks. This is OpenRouter inference, not a remote Claude/Codex subscription connection.

The code ticker highlights JavaScript. The preview compiles completed paint statements during streaming using a temporary closing brace, and complete revision-checked replacements during edits. Provisional errors wait for more source; partial previews never enter the saved version history. The final tool source must still pass checks.


## TV and providers (build 102)

TV discovers AC OS devices on the same LAN. Choose a receiver to forward painted
checkpoints; the current code and generation phase appear in Comic Relief along
the bottom while work is active. Status updates do not restart the piece. A
15-second missing heartbeat clears stale progress using the receiver's own clock.
The receiver executes the piece natively; independent animation state and missing
native APIs can still produce differences from the phone.

The brain menu offers personal Claude Opus 5 and GPT-6 Astra to verified
`@jeffrey`, alongside the existing AC inference choices. Switching applies to
subsequent work and is disabled during a turn. The admin relay separately checks
the verified Auth0 owner subject; provider credentials stay on the server.
Codex usage includes token counts; absent dollar pricing remains an incomplete
cost, never a fabricated zero. The current provider stays selected after upgrade.

Story playback owns a separate WebKit runtime so it can play during generation
without replacing the pixels under visual review. The wood frame belongs to the
phone's chrome and is excluded from the piece, TV output, and story exports.

## Tezos and cost units (build 104)

Brain settings remembers Braincells / USD / Tezos and applies it to balances,
request costs and the thread meter. Balance value uses the existing $5 per
million pack. Inference meters keep provider cost; equivalent braincells are
not wallet deductions. Hosted AC inference charges twice provider cost, drawing
from free allowance first. Tezos conversions use an expiring, timestamped TzKT
rate from `/api/easel-tezos`; an unavailable rate is never replaced with zero.

Buy braincells with tez opens an AC checkout in the browser, where Beacon pairs
with Temple or another Tezos wallet. The user signs and approves in their wallet.
The app stores only a checkout capability bound to the signed-in handle and
reconciles on return; credit is granted by server verification. The production
purchase link is limited to the US App Store storefront; Debug permits device
testing. The API flag `AC_TEZOS_CREDITS_ENABLED` controls new purchases.

`HeaderSheetsTests/testPhoneCostUnitToggle` checks all three units and persistence
on the paired phone without initiating a purchase.
