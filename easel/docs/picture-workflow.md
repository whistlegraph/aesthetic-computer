# Pictures

A Picture gets a public short painting code when its WIP is first saved.
Accepted changes autosave under that code. Pending proposals stay in the local
preview until accepted. AC, No Paint, and Easel use the same saved pixel and step
format.

`/done` (also `/publish`) uploads the accepted PNG and AC playback ZIP, then seals
the existing code. Editing afterward automatically starts a copy with a new code
and a link to its parent. An empty WIP expires after an hour; later creation
traffic reclaims expired empty records.

The assistant can use `artifact_draw` for AC line, Fill, Box, Circle, Wipe,
Invert, vertical Flip, and Blur. These run AC's actual software renderer. Drawing
creates a proposal; `artifact_accept` commits it and `artifact_discard` removes
it.

Ask for OpenAI image generation or editing to use `artifact_generate`, or use
`/render-image PROMPT` and `/edit-image PROMPT`. Edits take `composite.png` as their
reference. The result is a proposal and retains its provider receipt. Requests
use `OPENAI_API_KEY` when configured, otherwise AC sign-in and the hosted image
allowance (five images per day). fal requires `FAL_KEY`. Failed paid requests are
not automatically retried.

`node easel/bin/build-picture-tools.mjs` rebuilds the portable AC renderer and
painting-state bundle from this repository. Focused tests:

```
node --test easel/test/picture.test.mjs easel/test/artifacts.test.mjs easel/test/publish-picture.test.mjs
node --test system/tests/painting-wip.test.mjs system/tests/easel-images.test.mjs
```
