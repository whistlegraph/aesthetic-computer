# No Paint model moves

## Native Mac app

`native/build.zsh --install` builds and opens `~/Applications/No Paint.app`.
It uses SwiftUI/AppKit controls and a native image buffer. It attaches to the
existing loopback game, preserving the accepted painting and pending move;
if the server is stopped it starts it offline and resumes its saved session.
This development app uses this checkout's Python backend and cached model
weights. It is not a self-contained distribution. Quitting its window leaves
the shared backend running so browser play can continue.

The bundle uses the original No Paint icon from `nopaint/construct/icons/`.
For a Developer ID build, set `NOPAINT_SIGN_IDENTITY` to your available
certificate name before running `native/build.zsh`; this enables hardened
runtime signing with a timestamp. Without that setting, builds are ad hoc.
`python3 native/add-to-dock.py "$HOME/Applications/No Paint.app"` pins the app,
preserving existing Dock entries and backing up the preferences first.

Fleet installs can run independently of a working checkout. Run
`python3 native/package-runtime.py <new-directory>` to collect only the public
backend sources and shared AC modules. Copy that directory to the destination,
run `npm ci` and prepare the pinned Python dependencies there with
`uv run --no-project --python 3.12 --with-requirements local-requirements.txt python -c 'import torch'`.
Point `NOPAINT_BACKEND` at its `native/start-backend.sh` when building the app,
before signing. Local diffusion also needs the pinned model caches described
below. Each host keeps its own paintings and uses its own existing AC login;
the runtime package contains neither. Native builds target Apple Silicon.

The native interface automatically follows macOS light/dark appearance, including
live changes. The painting and exported pixels keep their original colors.

Classic, Pixels, Color, and Primitives run locally on the CPU with no model,
account, network, or Braincell requirement. Classic chooses among all 13
operations; the other entries limit its palette to spatial changes, color
changes, or paint marks. These include strip shifts, tile turns, pixel sorting,
mosaics, hue, tint, contrast, posterization, strokes, shapes, stipple, hatch,
and diffusion. Small/Medium/Large bounds the amount of change. Fixed seeds
reproduce each move. Blank or otherwise invariant inputs receive a small seed
mark so a turn still changes the image. Each turn records the operation, seed,
library versions, image hashes, and actual intermediate operation states.
Fast moves may complete between UI polls; their final image still fades in.
The normal Paint confirmation, No history, and exact brush-mask preservation
apply, with 0 Braincells and ∞ paints left.

The classic palette uses the existing [Pillow](https://pillow.readthedocs.io/en/stable/reference/index.html)
and NumPy dependencies. [G'MIC](https://gmic.eu/gallery/filtering.html) is a
candidate for a larger filter collection; it is not installed or required.

No / N and U step backward. Hovering No fades to the image that No would restore; moving away fades back without changing history. Paint / P confirms the displayed Braincell price and starts exactly one generation. Each completed result becomes the current painting, then waits. Hold Space for the previous painting. Model switches, Fresh, cropping, and brush edits prepare a new quote without inference.
File → Fresh (⌘N) resets to new 256×256 RGB noise. File → Save Painting (⌘S) writes the accepted PNG with a millisecond timestamp in its filename. New sessions also start from noise. The model menu defaults to Random, drawing an enabled model for each move.
File → Save Upscaled exports a 2× (512×512) or 4× (1024×1024) PNG using
local Real-ESRGAN (`realesr-general-x4v3`), for 0 Braincells. Choose a destination
before inference starts. The app shows actual tile progress and elapsed time,
then reveals the timestamped output in Finder. The working canvas, move history,
and Paint quote are preserved. 2× uses the trained 4× output reduced with Lanczos.
The 4.7 MB checkpoint downloads once from the pinned official release and is
SHA-256 checked; later upscales work offline. Apple Silicon uses MPS; CUDA or CPU
is used on other hosts. The direct 256→1024 test took 2.66 seconds; the first
verified native File-menu export took 6.47 seconds including cold startup.
Outputs and receipts are retained under the session's `upscales/` directory.
Model source and BSD-3 attribution are in `vendor/realesrgan/`.
Choose a specific model to keep it fixed. The selector shows the model actually
chosen; the quick menu lists priced/local models and the current selection.
The full catalog remains in Browse OpenRouter. Unavailable or unaffordable remote models remain selectable but cannot
generate. Random only draws currently available models. Switching local
models can add load time because only one pipeline stays resident. The confirmation button shows estimated seconds and the Braincell quote before
generation, then actual elapsed time while it runs. Estimates use recent moves
from the same model and strength; unmeasured models show — s. Local and fleet
moves cost 0 Braincells. Completed moves retain their settled billing receipt.
An unknown remote billing outcome says “braincells pending.” The model band is one line across the full window, below the centered painting and No/Paint controls. The painting fits the remaining height with letterboxing; long model text truncates instead of shrinking the image. The bar and model picker show paints left from the combined daily and purchased balance divided by the selected AC price. Local models show ∞. Unpriced catalog models say “not enabled on AC.” No and Paint expand vertically to keep the band flush with the window's bottom edge.
Browse OpenRouter in that menu (or Models → Browse OpenRouter) opens a
searchable live catalog filtered to models accepting one image and returning
a raster image. Selecting a row loads current endpoint prices; browsing needs
no API key and never generates an image.

The modeline and OpenRouter picker show **AC cloud unavailable** when the AC
service cannot run image moves. The picker separates provider funding from the
user's Braincells and shows the provider balance and required minimum when
known. The server checks OpenRouter credits read-only, caches the result for
one minute, and reports configuration, funding, or status-check failures
separately. Model selection persists during an outage. Recovery prepares a
quote and still requires Paint; selecting or refreshing never generates or
charges. The server refuses unavailable moves before reserving Braincells.

Connection and action errors live in the bottom modeline; click it for the
details and recovery action. They never add a panel between the painting
controls and the modeline or replace Paint's time/cost with an error message.
Temporary AC connection failures preserve the last verified handle and
balance (marked last known), suspend cloud quotes, and retry automatically.
Local painting remains available. An explicit sign-out stops reconnection.
The local server keeps one AC client process alive over private pipes. Its HTTP
pool retains connections across 30-second refreshes and allows account reads
during generation. Verified identity is cached for one minute and invalidated
when the token changes. Interrupted reads retry once; paid requests are never
automatically replayed. Early No discards the move's late reply without closing
the account connection. Install this bridge's dependency with `npm ci` here.

OpenRouter's advertised partial-image capability is shown separately from the
AC gateway's current final-image delivery. Catalog presence is not an inference
benchmark or proof that a model makes good incremental moves. Output resolution
is provider-specific; the saved No Paint canvas always returns to 256 × 256.
Token and megapixel rates are not flat per-move quotes.

The AC gateway supports configured OpenRouter models alongside fal, sharing the
same handle validation, Braincells reservations, receipts, and refunds. On the
hosted server, set `OPENROUTER_API_KEY`, `NOPAINT_REMOTE_ENABLED=true`, and
`NOPAINT_OPENROUTER_OFFERS=all` for the 50 reviewed raster image-editing models,
or a JSON array of `{model, usd}` objects for a subset. `usd` is an
explicit per-move cost basis for the AC tariff, not an estimate inferred from a
provider's token/MP rate; Braincells use the existing hosted markup. Reviewed
profiles are in `system/backend/nopaint-openrouter-profiles.mjs`. Example model IDs:
`black-forest-labs/flux.2-klein-4b`, `google/gemini-3.1-flash-image`,
`openai/gpt-image-1-mini`, and `openai/gpt-image-2.5-flare`.
Unconfigured or unaffordable models remain unavailable. A completed cloud move
costs Braincells even if rejected; early No only stops waiting on the client.
The gateway was deployed on October 5, 2026 at `726378df28`. Its configured
Klein tariff is 8,000 Braincells per completed move. The first live move failed
because OpenRouter returned HTTP 402 (insufficient provider credits), despite
its balance API reporting $0.265365539 remaining. The chat-completions route
later gave the precise reason: image or video output requires at least $1.00
in account balance. An explicit 256×256 request also failed. The advertised
`:free` Klein variant has no live endpoints. Those failed requests were refunded.
After a $50 provider refill on October 6, the paid route was enabled and a live
256×256 image edit completed in 5.99 seconds including account checks. AC charged
8,000 daily Braincells; OpenRouter reported $0.015 provider cost. The input,
output, and receipt are in `local/cloud-retry-20261006/`. Random also includes
the working Poorslice fleet model described below, at zero Braincells.

GPT Image 2 (`quality: low`, square) and Nano Banana 2.1 (`resolution: 1K`,
square) were tested with the full input painting on October 6. They returned
256×256 edits in 13.66 s and 13.36 s, with reported provider costs of $0.008203
and $0.037740. Their fixed AC prices are 4,000 and 16,000 Braincells per Paint.
These are AC tariffs, not token-price estimates. Benchmarks and images are in
`local/model-verification-20261006/`.

On October 6, revision `d65fb2f523` enabled all 50 reviewed models, including
GPT-5 Image (8,000 Braincells) and GPT-5 Image Mini (6,000). Each profile has
explicit request options and a fixed AC price. The quick selector groups them
by provider. Models added to OpenRouter later still require profile review.
All 50 profiles were checked against the live image-input/output catalog and
endpoint options; selected models were tested with paid calls, not every model.

`nopaint-move-prompt.mjs` selects a seeded concrete operation instead of asking
every provider for an abstract painting move. Its 16 operations cover spatial,
palette, texture, and mark changes. Small/Medium/Large selects the affected
quarter, half, or whole image in the instruction; only the explicit brush mask
enforces exact preservation. GPT Image 2 visibly shifted strips in 14.69 s;
GPT-5 Image returned an edit in 11.19 s. Nano Banana largely ignored that strip
instruction, and Klein mostly softened the input. A shape-addition trial gave
visible circles and rectangles from both (12.03 s / 7.30 s), so those two models
use more visual operations for the precise permutations they ignored. Receipts
record the actual prompt and its version. Evidence is in
`local/openrouter-all-verification-20261006/`; this is a small comparison on one
input, not a guarantee of edit quality across paintings.

File → History (⇧⌘H) groups paintings by every Fresh, including Fresh triggered
by a successful Done. Each painting retains its chronological Paint results,
No decisions, failed moves, crops, model/seed/strength, Braincell receipts, and
actual preview states. Rejected branches remain available after No and Fresh.
The History view browses images and recorded move instructions; Reveal Track
opens that painting's JSONL event log for analysis. PNGs, latent states, and
receipts referenced by the log stay under the session directory. Upscale exports
are recorded without becoming painting steps. These archives are local; Done
publishes accepted steps, not the private decision log. Existing session events
are imported once, so older history has only the details originally captured.
Reopening the app resumes the current track without starting another painting.


File → Done (⌘D) publishes a new painting under the signed-in AC handle, using
AC's existing WIP, recording, upload, and seal services. It publishes the accepted
image and accepted steps, then starts fresh only after the public PNG is verified.
Each explicit Done has a new identity; retries keep their original identity and
receipt, including after restarting the app. Failure preserves the canvas. Fresh
can leave a failed publication behind; its recovery receipt stays on disk.
File → Open Last Painting opens the last verified painting. Done does not charge
Braincells. The publication flow was tested against the real WIP service with
mock storage/HTTP; no test painting was posted to the user's account.

Drag in Zoom mode to crop the accepted bitmap to a rectangle and resize that
crop back to 256×256. The cropped bitmap becomes the next model input and can
be undone with U. Pinch/scroll only inspects the view; double-click fits it. Inpaint marks a brush mask (Option-drag erases).
Options sets brush size or clears the mask. SD 1.5 uses Diffusers' four-channel
inpainting pipeline with the same weights, preserving unmasked latents at each
update. Other engines receive a padded square crop enlarged to 256×256. Every
preview and final result is composited back through the exact brush mask, keeping
all unpainted pixels unchanged. This is not a dedicated inpainting checkpoint.
A Poorslice test measured 7.03 s cold and 1.47 s warm for a Small masked move,
with nine distinct latent states and exact preservation outside the brush.
Evidence: `local/inpaint-verification/1791263500942244000/verification.json`.

Slab includes No Paint in its window census, automatic tiling, and directional
window navigation. The native layout adapts down to a 280×278-point content area;
the stored painting remains 256×256 after cropping. No and Paint
touch, the painting starts at the top, and Undo is available only by keyboard.

## Play locally

Download the pinned SD 1.5 and TAESD checkpoints once, then run locally:

```sh
uv run --no-project --python 3.12 --with-requirements experiments/nopaint-ai-moves-2026-10-05/local-requirements.txt python experiments/nopaint-ai-moves-2026-10-05/evolve_run.py --download
uv run --offline --no-project --python 3.12 --with-requirements experiments/nopaint-ai-moves-2026-10-05/local-requirements.txt python experiments/nopaint-ai-moves-2026-10-05/play.py
```

Open http://127.0.0.1:8767. **No / N** steps backward. **Paint / P** confirms the quoted move and starts
one generation. The result is applied and the app pauses; nothing automatically
generates the following move. Hold **Before / Space** to see the previous canvas. The File menu contains
Fresh, timestamped Save, and Done. Options changes strength. Native U still
returns to the previous accepted image; there is no Undo button.

The default engine is now **Stable Diffusion 1.5 with Euler sampling**. The
32-step schedule and Small/Medium/Large strength produce **8/16/24 real latent
updates** per move. Each move starts from the complete accepted 256×256 RGB
image, with an empty prompt and no classifier-free guidance.

After every update, [TAESD](https://huggingface.co/madebyollin/taesd) decodes the
actual latent into an approximate image preview. The original full VAE decodes
the final proposal. Previews are display-only: Paint can commit only the final
proposal. Each source latent is saved as FP16 NPZ alongside its preview PNG;
receipts record hashes, timings, timesteps, and the exact update count.

Confirming Paint starts a visible elapsed timer and an indeterminate
progress indicator. Actual previews appear automatically as they arrive, with
220 ms display blends; the final image fades in over 400 ms. Interrupted native
blends continue from their visible composite, with fixed timing; new model
sequences reset to the accepted buffer. Buttons and the modeline have pointer
cursors and hover feedback. Interpolation does
not create model updates. Reduced-motion settings skip interpolation. The game
allows **No during generation**; Paint becomes available when the final image
is fully shown. No cancels at the next cooperative boundary, discards late
frames/results, and returns to that move’s input without another generation.
Afterward, Paint shows the next move’s estimate and cost.

`evolve_run.py` without arguments benchmarks eight updates with and without
preview decoding, checks that nine latent states are distinct (initial noise
plus eight updates), and verifies identical final PNG hashes. `play.py
--verify-engine` explicitly runs that local diagnostic before allowing play;
normal startup performs no inference.
The [step-end callback](https://huggingface.co/docs/diffusers/using-diffusers/callback)
returns the original latent tensor. Previewing adds decoding/readback overhead.

The model selector underneath switches between cached local SD 1.5 and
SD-Turbo. Only one local pipeline is loaded at a time, so switching includes
model load time. The selector names the engine that made the displayed proposal.
Each proposal and accepted decision records model provenance. SD-Turbo is also
available through `--engine turbo` for comparison. Its `model_trace.py` records channel-RMS feature maps; those maps
are no longer rendered as evolving pictures in the game.

Verified on this Mac: the first eight-update run took **8.02 s** with nine
decoded previews; its final PNG exactly matched the following unobserved run
(3.57 s). The preview operations themselves totaled 1.50 s. These first/warm
runs are not a controlled estimate of overhead. All nine captured latent
hashes differ. Images, latent arrays, receipts, and `trajectory.gif` are in
`local/evolution-benchmark/20261005-174336/`.

The model stays loaded between moves. A blank input may stay blank with this
model. Proposals, inference receipts, and decisions are saved under
`local/play/<session>/`; a new server process starts a new session.
`local/play-server.json` records the running process ID. Stop a foreground
server with Ctrl-C, or stop that recorded process with `kill <pid>`.
Pass `--resume <session-directory>` to preserve accepted history and a pending
proposal, including when changing engines.

The decision-loop checks use an injected image generator, without model weights:

```sh
cd experiments/nopaint-ai-moves-2026-10-05
uv run --offline --no-project --python 3.12 --with-requirements local-requirements.txt python -m unittest test_play test_model_trace test_remote -v
```

## Mixing local and remote moves

Every engine consumes the accepted 256×256 PNG and returns a 256×256 PNG.
Paint commits that result, so subsequent moves can use a different engine.
Changing the selector discards the pending proposal and uses the accepted image.

**Sign in** connects this game to the shared AC desktop account using Aesel's
`ACSession` PKCE flow. The node bridge reads/refreshes the existing desktop token;
the browser and Python game never receive it. The UI shows the verified @handle
and its existing free + purchased Braincells balance. Signing out disconnects
only No Paint. Local play never reads tokens or contacts AC before sign-in.

Cloud moves use `/api/nopaint-inference` on AC, which owns the provider credential.
The gateway verifies the user and handle, validates the full 256×256 PNG, checks
the displayed price quote, and reserves credits transactionally: daily free
allowance first, then the existing `ac-credit-wallets` balance and spending cap.
Request IDs are account-scoped; retries cannot create a second charge or vendor
call. The image and token are absent from durable billing receipts. Those receipts
participate in AC account export/deletion. Failed/unknown work is refunded;
Lith recovers abandoned holds after five minutes.

To enable the optional fal route, configure `FAL_KEY`,
`NOPAINT_REMOTE_ENABLED=true`, and `NOPAINT_FAL_USD_PER_MOVE`. The last setting is
an explicitly configured flat provider-cost basis per 256×256 move, converted
using the existing Braincells pack value and 2× hosted markup. It is not an
inferred megapixel minimum or a claimed live fal bill. Verify the tariff against
actual provider billing before enabling it. Quote changes require a refresh.
For example, a configured $0.01 cost basis quotes 4,000 Braincells per move.
Fal currently [lists $0.01 per megapixel](https://fal.ai/models/fal-ai/flux-2/klein/4b/edit);
no paid call has verified its minimums for this 256×256 workflow.

The [Klein endpoint](https://fal.ai/models/fal-ai/flux-2/klein/4b/edit/api) returns
a completed image. It uses a fixed edit instruction; Small/Medium/Large change
its wording, not a diffusion strength parameter. The timer runs while waiting.
AC cloud work continues to settlement after an early No or client disconnect:
**a completed generation costs Braincells even when rejected**. Failed work
refunds its reservation. A handle may have one cloud move in progress; rejecting
it can briefly leave the next remote request busy. Local models remain usable.
Only the current generation may publish a result to the canvas.

The older direct-fal adapter (`remote_run.py`, CLI `--engine fal-klein`) is retained
for developer experiments with an explicitly supplied personal `FAL_KEY` and
`NOPAINT_ENABLE_FAL=1`. It is absent from the game selector and does not use AC
credits. Hosted cloud entries require verified AC access: `ac-klein` for fal
or `ac-openrouter:<model>` for configured OpenRouter models.

Automated validation uses fake provider calls and randomly named Mongo test
collections. The live validation and refund are recorded above.

```sh
node --experimental-vm-modules --test system/tests/nopaint-inference.test.mjs system/tests/account-deletion.test.mjs experiments/nopaint-ai-moves-2026-10-05/test_ac_account.mjs
NOPAINT_BILLING_INTEGRATION=1 node --env-file=system/.env --test system/tests/nopaint-billing.test.mjs
```

## Fleet inference

**Poorslice is enabled** as `SD 1.5 · Poorslice · remote`. It runs the same pinned
SD 1.5 + Euler + TAESD model on its M1 Pro GPU, receiving the complete accepted
256×256 RGB PNG each move. A private SSH connection streams real preview images
and the final result. The Mac app displays them through the existing canvas and
No/Paint controls. No provider key, AC token, or Braincells charge is involved.

The worker lives in `~/Developer/ac-inference/nopaint` on Poorslice. Connection
settings are in ignored `local/fleet-worker.json` on the control Mac. It is a
personal fleet connection, not a public AC inference endpoint. One process
holds the GPU lease; a second worker is refused. The model remains warm between
consecutive fleet moves and exits when the game changes engines or shuts down.
Temporary input, latent, and output files are removed after every remote move;
the control Mac keeps its ordinary painting/preview receipts. No cancels at the
next cooperative boundary and discards late previews/results. A failed worker
is excluded from Random for 60 seconds while local models remain available.

Verified October 5: first move **23.26 s** including model startup, next move
**2.07 s** including transfer, each with **nine distinct latent-state previews**.
Early No stopped the proposal after its first preview with no final candidate;
the following move succeeded on the same worker. Evidence is in
`local/fleet-verification/verification.json`. Protocol tests cover full-input
delivery, worker reuse, early rejection, malformed input, and disconnection:

```sh
cd experiments/nopaint-ai-moves-2026-10-05
uv run --offline --no-project --python 3.12 --with-requirements local-requirements.txt python -m unittest test_fleet test_play -v
```

The installed native app also completed a Medium move in **3.68 s**, displaying
17 genuine preview states and zero Braincells while preserving the accepted
painting. Its receipt is `local/fleet-live-verification.json`.

Jastow is a candidate worker, not an enabled engine. The
[July hardware report](../../reports/jas-nzxt-fleet-use-report-2026-07-22.tex)
records an RTX 3070 with 8 GiB VRAM and 16 GiB RAM. The fleet check on October 5
found it offline. Keep one GPU job at a time, with the interactive game and
accepted image remaining independent of worker availability.

For Jastow, start with the currently verified SD models; standard FLUX.2 Klein 4B
[lists approximately 13 GB VRAM](https://huggingface.co/black-forest-labs/FLUX.2-klein-4B),
so using it on Jastow requires a tested quantized/offloaded configuration.

## Local experiment

The revised aim is a “Rubik's cube of novel image transformation”: explore
image-state changes without requiring scene understanding or verbal edit
instructions. `local_run.py` tests `stabilityai/sd-turbo` locally on the Apple
Metal GPU, using an empty prompt. It never calls a paid inference service.
The earlier fal experiment below remains separate.

The local machine has an Apple A18 Pro GPU and 8 GB unified memory. Dependencies
are isolated by `uv`; the FP16 checkpoint is approximately 2.58 GB and lives in
the Hugging Face cache. The checkpoint revision and dependencies are pinned.

```sh
uv run --no-project --python 3.12 --with-requirements experiments/nopaint-ai-moves-2026-10-05/local-requirements.txt python experiments/nopaint-ai-moves-2026-10-05/local_run.py --download
uv run --no-project --python 3.12 --with-requirements experiments/nopaint-ai-moves-2026-10-05/local-requirements.txt python experiments/nopaint-ai-moves-2026-10-05/local_run.py pilot
uv run --no-project --python 3.12 --with-requirements experiments/nopaint-ai-moves-2026-10-05/local-requirements.txt python experiments/nopaint-ai-moves-2026-10-05/local_run.py chain --turns 5 --controls
```

After download, model loading uses local files only. Every move encodes the
complete 256×256 RGB image, performs denoising, and decodes a complete RGB image.
No image latents or other hidden image state persist between moves. The empty
prompt embedding is cached. Seed 20261005 is fixed to explore repeated
application of the same transformation. Pilot strengths 0.25/0.50/0.75 run
1/2/3 denoising steps out of a four-step schedule; the chain uses 0.25.
No masks, blending, or outside-region repair constrain the changes.

The [SD-Turbo model card](https://huggingface.co/stabilityai/sd-turbo) recommends
512×512. Native 256×256 is an experiment, not its advertised quality setting.
An empty prompt does not remove the model's learned semantic biases. Denoising
strength controls corruption of the input latent; it does not guarantee a
particular fraction of changed pixels. Repeated VAE encoding/decoding itself
can cause drift.

### Measured result

Completed nine independent proposals (three inputs × three strengths), a
five-move chain, and three VAE-only reconstruction controls. Open
`local/index.html` to inspect individual moves, `local/comparison.png` for all
starting states, and `local/chain.png` or `local/chain.gif` for the sequence.
The viewer only selects saved outputs; it does not perform inference.

The five-turn chain took 10.40 seconds for its first move, then 2.59, 2.54,
2.35, and 2.00 seconds. Its four warm moves have a median of 2.45 seconds.
Model load and empty-prompt preparation took 35.76 seconds, excluding Python
imports. This chain releases the text encoder after caching the empty prompt.
Its first output exactly matches the first pilot output by SHA-256.

The initial pilot retained the text encoder. Startup took 77.14 seconds;
its first move took 85.60 seconds. Later calls ranged from 4.39 to 138.80
seconds, with the largest outlier at strength 0.75. These runs were on an
active 8 GB laptop, not an isolated benchmark. The faster chain follows the
pilot's GPU warm-up and also releases the text encoder, so the timings do
not isolate either optimization's effect.

Visually, the chain repeatedly sharpens and reinterprets the initial layout,
adding increasingly vivid rectangular details. It does not demonstrate a
diverse vocabulary of independent transformations. At higher strength the
model invents scene-like structure. White input remains nearly white with
this empty prompt and seed, so it cannot yet initiate a blank-canvas session.

VAE-only reconstruction changed 5.79% of the painting's pixels by more than
8/255 in any channel, versus 60.62% for its first low-strength model move.
For random noise, VAE reconstruction alone changed 94.21% of pixels: much of
the noise input's apparent transformation is compression. No hidden image
state persists, and no blending or masking hides these changes.

## Research directions, 2026-10-05

- **SD-Turbo / latent-consistency img2img:** practical existing weights for
  fast, stochastic whole-image changes. Relevant to the image-only loop, with
  limited preservation guarantees. [SD-Turbo](https://huggingface.co/stabilityai/sd-turbo),
  [LCM-LoRA](https://huggingface.co/latent-consistency/lcm-lora-sdv1-5).
- **Neural cellular automata:** learned local update rules are a closer
  conceptual fit to a space of visual moves. The published μNCA family uses
  68–588 learned parameters for texture synthesis, with shader demos. These
  models normally converge toward trained textures and carry additional state
  channels. Their published behavior is not validation of an arbitrary RGB
  painting → RGB painting loop. [μNCA](https://arxiv.org/html/2111.13545v1),
  [Self-Organising Textures](https://distill.pub/selforg/2021/textures/).
- **pix2pix-Turbo / CycleGAN-Turbo:** one-step learned image transformations
  with structure-preserving connections. Released tasks include edges/sketches
  to pictures and day/night/weather translation. A family of abstract No Paint
  moves would need new training. [Authors' implementation](https://github.com/GaParmar/img2img-turbo).
- **MagicBrush / InstructPix2Pix:** relevant when precise instructed edits
  matter. MagicBrush includes multi-turn evaluation and fine-grained editing;
  this does not establish open-ended autonomous move selection.
  [MagicBrush](https://github.com/OSU-NLP-Group/MagicBrush).
- **FLUX.2 Klein 4B:** modern semantic editing and a native MLX implementation.
  A prequantized MFLUX package is 4.62 GB on disk; that is not a peak-memory
  measurement or confirmation of a comfortable 8 GB run. It is lower priority
  for the revised nonsemantic experiment.
  [Weights](https://huggingface.co/mlx-community/flux2-klein-4b-4bit/tree/main),
  [MFLUX](https://github.com/mflux-community/mflux).

No published timing above is treated as a measurement on this Mac. In
particular, StreamDiffusion's throughput on an RTX 4090 does not establish the
latency of a serial feedback loop on the A18 Pro.

## Earlier fal experiment

An isolated image-to-image experiment. Every call sends the complete current
image and receives a complete proposed next state. The canonical canvas is
256×256. No public painting, account, or production code is changed.

The starting states are white, deterministic grayscale noise, and the top
256×256 painting area of the public No Paint record `l4f0ipzy` (the archive's
32px label footer is excluded). See `starts.png` and `plan.json`.

Models:

- `klein`: `fal-ai/flux-2/klein/4b/edit`, 4 steps, explicit output dimensions.
- `kontext`: `fal-ai/flux-kontext/dev`, 28 steps, `match_input` resolution.

The exact fixed prompts are in `plan.json`. “Autonomous” means the model chooses
one small move under that instruction. It does not mean a prompt-free model.
“Specified” asks for a magenta circle centered at (64, 192), diameter 20.48px.

## Run

Credentials use `FAL_KEY`, then the existing local vault dotenv resolution used
by the repository's provider experiment. Keys are never saved in receipts.

```sh
uv run --no-project --with 'httpx[http2]' --with pillow --with numpy python experiments/nopaint-ai-moves-2026-10-05/run.py pilot --live
uv run --no-project --with 'httpx[http2]' --with pillow --with numpy python experiments/nopaint-ai-moves-2026-10-05/run.py specified --live
uv run --no-project --with 'httpx[http2]' --with pillow --with numpy python experiments/nopaint-ai-moves-2026-10-05/run.py chain --model klein --start base --turns 50 --live
```

Use `--model kontext` for the second chain. Use `--size 512` to test 512px
inference separately: inputs are enlarged with nearest-neighbor sampling,
native outputs are retained, and canonical results are reduced with Lanczos.
No output masks, blending, or compositing conceal unwanted changes.

Each run caches successful outputs by input and prompt hashes. A failed or
interrupted request refuses automatic resubmission; inspect its receipt before
moving that failed receipt to an archive and explicitly retrying.

`report` rebuilds the offline `index.html`, contact sheets, and JSON results.
The HTML controls select existing images and never issue paid requests.

## Evidence and limits

Receipts record full API round-trip time, returned dimensions, provider timing
when available, seeds, exact prompts, input/output hashes, and per-pixel change.
The full round trip includes upload, remote processing, response/download, and
canonical image preparation. Failed calls do not count as inference latency.

Changes exceeding 8/255 in any RGB channel are counted. The specified test also
measures change outside a generous 16px-radius target region. This measures
preservation, not visual quality or whether the model made a useful move.

Chain mode automatically adopts every generated result as an experimental
stress test. It does not simulate a person's No/Paint choices.

Initial attempts on 2026-10-05 received HTTP 403 from both model endpoints. A
subsequent read-only diagnostic returned `User is locked. Reason: TOP_UP.`
No model images were returned and no speed/quality findings can be drawn.

The Illy planner currently substitutes an older FLUX model for these unknown
model IDs and adds physical-scene prompt constraints. This experiment therefore
uses exact fal endpoints through the existing `provider_image.py` HTTP helper,
and records its own prompts and provenance without those unrelated constraints.
