# Whistlegraph

The iPhone app for making AC pieces with voice, sound, and typing. Product domain: `whistlegraph.app`. The practice and archive remain at `whistlegraph.org`.

From this directory, run `./run.sh device` to bundle, build, install, and open the app on a paired iPhone. Use `DEVICE=<identifier>` if more than one phone is paired. `./run.sh simulator` builds for a simulator; select one with `SIMULATOR=<identifier>`. XcodeGen generates `Whistlegraph.xcodeproj` from `project.yml`.

Typing uses AC's `compkey` sample and QWERTY pitch mapping. Enter sends the prompt, pasted line breaks become spaces, and the limit is 96 characters. Account settings control key and button sounds together.

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

## Checked edits (experimental)

Account settings → **Check edits (experimental)** enables a deterministic edit contract containing the selected branch requests, caption, and known API failures. It defaults off; the existing DeepSeek path with thinking disabled remains the default. Debug launches can set `WALKIE_COMPILED_TASK=1`.

The experiment checks JavaScript syntax, direct unsupported HSL drawing calls, invalid `ink.box`-style calls, and matching-source runtime feedback. Each render carries a SHA-256 source hash and render ID; stale feedback is ignored. An actionable failure after a completed generation permits one repair. A failed or unverified result restores the previous piece without committing a version. A painted frame is execution evidence, not visual or semantic acceptance; animation timing still needs observation.

Limits: four initial tool rounds with one output continuation, two repair rounds without continuations, 4,096 output tokens per provider call, and a 75-second generation/validation deadline. This allows at most seven provider calls per attempt; it is not a monetary cap. A resumed request is a new attempt. Existing deterministic local edits still bypass inference.

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

Both experiment browser checks use mock inference. They verify the contract, repair limit, rollback, render identity, and receipt fields without spending braincells.

## Chalk with words and sound

**Chalk** toggles an ink layer over the preview above the controls. The keyboard and microphone remain available. Hold Talk with one finger and chalk over the preview with another, even when Chalk was off. Releasing Talk sends the combined request (still capped at eight seconds). The Chalk toggle keeps the layer available between recordings and while typing. Typed Enter sends text and the current sketch together; Send beside the drawing can submit it alone. Undo removes the last stroke; Clear discards the sketch. Turning Chalk off returns touch interaction to the piece while retaining visible unsent marks.

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
