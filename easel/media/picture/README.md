# Picture toolkit

`src/media/picture.mjs` creates a portable 512 × 512 picture. It stores accepted
layers and a pending proposal in `picture.json`, renders the current proposal
in `preview.png`, and exports accepted layers to `composite.png`. Accept adds a
layer; discard does not touch accepted pixels. The shared Aesel artifact store
owns revision numbering and rollback of these files.

`ac-line.mjs` vendors the exact `nopaintProposal` contract from
`system/public/aesthetic.computer/disks/line.mjs` and seeded PRNG from
`lib/nopaint-proposals.mjs`. Its generator, bounded geometry, and render score are
AC's Line brush. The portable CPU `ink().line()` implementation rasterizes round
segments without AC runtime dependencies; it does not promise pixel identity
with every AC renderer. A test compares the vendored proposal with its source.
The toolkit is distributed by Aesthetic Computer under the Aesel license.

Use `propose` with a seed and optional RGBA color, thickness, and points, inspect
its preview, then `accept` or `discard`. `import_png` accepts an existing file
inside the artifact directory. Layer opacity and visibility remain editable.
Source assets are content-addressed. Import supports 8-bit noninterlaced gray,
RGB, and RGBA PNG, up to 2048 × 2048 / 32 MB; convert JPEG/indexed PNG first.

Illy's explicit provider/model selection and `.illy.json` provenance contract
are preserved by `illy.mjs`. The installed registry supplies `gpt-image-2` and
fal Flux generate/edit routes. Actual model availability depends on the user's
provider account. Credentials come only from `OPENAI_API_KEY` / `FAL_KEY` in the
process environment, never a studio vault or a project file. No automatic
provider fallback, paid POST retry, or subscription entitlement is assumed.

`image_plan` is free. `generate` requires an explicit provider, model, user
prompt, unique job ID, and paid-action authorization. The caller must obtain
that authorization outside model-authored tool arguments. Job receipts survive
failures; the same job ID cannot be submitted twice. Returned provider bytes
are retained in `jobs/` before PNG validation, including unsupported outputs.
Paid image results become proposals. Generation has a ten-minute bound.

This first adapter does not offer automatic queue reconnect, remote cancellation,
hosted billing, partial provider images, a freehand pointer editor, arbitrary
brush plugins, or JPEG decoding. The shell can show every raster revision now.

## Publish as an AC painting

`/publish` explicitly publishes the current accepted `composite.png` through the
existing AC painting pipeline: a user-bucket upload grant, PNG upload, and
`/api/track-media` registration. AC assigns the painting's `#code`; this is the
existing bitmap short identifier, not a new descriptive hashtag system. Pending
proposals are excluded. Accept the proposal first if it belongs in the painting.

The client verifies the code's owner/slug and the public PNG hash before exposing
its painting URL or `/qr`. It does not upload recordings, source recipes, prompts,
or layers. Public registration retains the backend's existing painting/profile
and ATProto behavior. There is no Picture autopublish.

A receipt under `.easel-media/publications/<artifact>/vN.json` records the exact
accepted output and server result. It stores neither auth tokens nor presigned
query credentials. A retry resolves a lost upload/registration response before
proceeding; an unresolved outcome reports that fact rather than creating another
painting. A new local version has no public QR until explicitly published.
