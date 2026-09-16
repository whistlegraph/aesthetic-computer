# Easel 0.7 — Pictures, Sound, Pieces, Papers

Proposed September 15, 2026. Strategy, not implemented functionality.

Easel should make pictures, sounds, pieces of aesthetic.computer software, and papers through one conversation and
one version history. Choose the medium first; choose the tools and providers
within it. An image model, an AC brush, and a hand-drawn stroke can all contribute
to the same picture.

## The experience

```text
New → Picture ─→ brush / draw / generate / edit ─→ canvas ─→ PNG
    → Sound   ─→ compose / synthesize / sample  ─→ player ─→ WAV
    → Piece   ─→ write / run / debug           ─→ runtime → piece URL
    → Paper   ─→ research / write / cite       ─→ pages   → PDF + source
                          ↓
                 v1 → v2 → v3 → restore
```

Keep the conversation scrollable above a fixed composer. The persistent controls
show medium, current artifact/version, and selected engine. Clicking those
controls opens the relevant choices. `/about` maps the actual installed
capabilities and explains unavailable ones.

Medium, runtime, and provider are different choices:

- **Medium:** what the user is making—Picture, Sound, Piece, Paper.
- **Runtime/tool:** how it is made—AC brushes, a Pop instrument, JavaScript.
- **Provider:** who performs a remote operation—AC hosted, OpenAI Images, fal,
  or a user's existing supported account.

The conversation model stays independently selectable. Switching from Claude to
Codex should not switch the image provider or rebuild a sound. `/medium` should
create another artifact in the project, preserving the existing one.

## One project and revision contract

Introduce a project manifest containing a stable project ID, artifact IDs,
medium, editable recipe/source paths, referenced asset hashes, tool/renderer
versions, and the current revision. Keep the project usable without an AC login;
optional server ownership uses a stable account ID, with the handle used for
display and public routes.

| Medium | Editable material | Revision output |
|---|---|---|
| Picture | Brush operations, seeds, layers, generation brief, references | Composite PNG plus immutable input images |
| Sound | Notes/events, instruments, effects, sample/stem references | WAV plus score and required assets |
| Piece | Piece source and asset references | Source revision and running preview |
| Paper | Manuscript, bibliography, figures, evidence references, build recipe | PDF, approved source bundle, and review tied to the PDF hash |

Every accepted update produces `v1`, `v2`, etc. Keep intermediate drafts separate
from accepted versions. Paid render results are saved even if rejected, so the
user can reconsider them without paying again. Rollback appends a new head
pointing at existing material; it never calls a provider to reconstruct history.

Extend [the current snapshot store](../src/revisions.mjs) to recipes and immutable
asset references. Save blobs atomically by content hash. Give exports portable
relative paths. Preserve original paid outputs and `.illy.json` provenance.

Do not make a Git repository per user a prerequisite. Git is useful for optional
recipe/source export; image and audio blobs need an asset store. Shared history
can later sync manifests and blobs under the stable account ID without exposing
unpublished work. The current path-keyed local history remains importable.

## Tools for each medium

**Pictures:** ship a small audited AC brush collection. Reuse the existing
[proposal contract](../../docs/nopaint-brush-proposal-contract.md): draw into a
separate buffer, preview, accept or discard. The same transaction supports a
generated image becoming a layer rather than erasing the canvas. Preserve brush
parameters and seeds so the composition stays editable. Start with brush
proposals, layer composition, generation/editing through Illy, and PNG export.

The [brush adapter](../../system/public/aesthetic.computer/lib/nopaint-brush-piece.mjs)
and [proposal catalog](../../system/public/aesthetic.computer/lib/nopaint-proposals.mjs)
are better foundations than copying the entire `nopaint` application.

**Raster providers:** extract the reusable provider adapters and provenance
contract from [Illy](../../plugins/illy/scripts/illy-mcp.mjs). Its installed
capability registry currently advertises OpenAI `gpt-image-2` generation/editing
and fal Flux routes. Resolve supported models at runtime and show the explicit
selection. Credential availability on the studio machine does not imply access
for an Easel user. Never silently switch providers after a failed paid request.

**Sound:** follow [Pop's compositional direction](../../pop/SCORE.md). Begin with
short phrases and loops built from instruments, notes, rhythm, and effects,
rather than an end-to-end song-generation button. The first complete flow is:
“make a soft bell phrase” → edit notes/timbre → render → play/loop → revise → WAV.
Start from the instrument/effect portion of [Pop's menu](../../pop/lib/menu.mjs),
[rhythm helpers](../../pop/lib/necklace.mjs), and [WAV support](../../pop/lib/wav.mjs).
Add samples and optional vocal performance providers after those primitives
work. Do not ship personal voice assets as defaults.

**Piece — “build a new piece of aesthetic.computer software”:** retain the existing piece workflow and compatibility. Replace the
AC bridge's single hard-coded tool with a capability registry: `write_piece` for
Pieces; brush/layer/image operations for Pictures; score/render/analyze
operations for Sound. Validate operation arguments and confine outputs to the
project. Models receive tool descriptions, never provider credentials.

**Paper:** a writing and research workspace backed by the existing
[papers stack](../../papers/SCORE.md), not merely a PDF export option. Begin with
the question, relevant Platter material, sources, and an outline; draft and revise
sections while keeping citations and evidence attached. The default lane is an
archival LaTeX paper; essays, cards, and other forms are explicit choices.

Use the [Paper MCP](../../slab/bin/paper-mcp.mjs) workflow for discovery, source
reading, builds, figure/table checks, and visual review. Expose bounded tools
such as `find_sources`, `read_source`, `edit_section`, `update_bibliography`,
`attach_figure`, `build_paper`, and `review_pages`. Distinguish sourced claims,
unverified claims, and missing references. Never invent citations.

The preview shows rendered pages alongside the current section, with build errors
pointing back to source. Section edits create source revisions; only a successful
build replaces the PDF preview. A compiling PDF is not a finished paper: require
figure/table QA and visual review tied to the current PDF hash. Restore source,
bibliography, assets, PDF, and the matching review together on rollback.

Pictures and piece measurements can become figures through explicit artifact
references pinned to versions. Updating a source artifact marks dependent paper
figures as potentially stale; it must not silently rewrite a reviewed paper.
Source bundles contain only approved project material, never private evidence or
credentials by implication. The author/byline comes from the user's project
identity, not a packaged hard-coded studio author. Export and public publication
remain separate actions.

## Preview, jobs, and costs

The top-left preview is the artifact being made, in every medium. Keep its
position consistent as the medium changes: Picture shows the canvas, Sound
shows a waveform with playback, Piece shows the running software, and Paper
shows the rendered page being edited. Expand that same preview for closer
inspection and interaction; it must not become a separate, stale copy.

The preview follows the selected artifact and version, including rollback.
While a new version is building, retain the last usable artifact and indicate
the pending update. If the user inspects an older version, label it explicitly
and preserve that selection until they return to the current version. Audio
playback is user-controlled; a refreshed sound never starts playing by itself.

Preview adapters consume artifact revisions rather than assuming every artifact
is a piece URL. Pictures get a canvas; Sound gets transport, waveform, duration,
and a loop region; Pieces keep the running AC view; Paper gets paginated PDF
preview with section navigation and citation/build diagnostics. Slab and a future desktop
shell use the same adapters.

Token streaming remains separate from render jobs. Media work needs durable job
IDs, progress events, cancellation, reconnect, and idempotent submission. Show
real stages—queued, generating, rendering, ready—not invented percentages.
Display partial imagery/audio only when the renderer actually supplies usable
previews. Commit a version after output validation succeeds. Animate the preview
when it applies that revision; a failed render leaves the prior version intact.

Separate text allowances from image/audio charges. Hosted media requires a shown
estimate/ceiling, an atomic reservation before submission, and reconciliation
after completion. A reconnect must attach to the existing job rather than buy
another render. User-supplied API credentials and AC-funded access are distinct
routes. Store credentials outside projects; remove Illy's studio-vault fallback
from the distributable adapter.

Do not promise that a Claude/Codex subscription includes standalone image/audio
API access. Illy cannot itself invoke Codex's built-in image tool; an integration
must advertise that capability only where it is actually callable.

## Packaging and the desktop shell

Bundle versioned schemas, adapters, brush implementations, pure-JS instruments,
effects, rhythm utilities, and WAV support through a reproducible toolkit sync
step. Test the [release tarball](../bin/pack.mjs) outside the monorepo: imports
reaching into `../../pop` or the studio's plugin cache will fail for users.

For Paper, bundle portable templates, bibliography/source-bundle schemas, and
build/QA adapters. Offer a separately installed TeX toolchain or hosted build
worker. A public installation must not require the studio's loopback Paper MCP
daemon, private Platter, fonts without redistribution rights, or hard-coded
author identity. Hosted builds receive only the selected project sources.

Keep native DSP/mastering, ffmpeg-dependent workflows, large sample packs, and
provider credentials out of the core package. Offer separately installed,
versioned capability packs or hosted workers. Audit sample redistribution and
tool dependencies before bundling. Pop's [mastering wrapper](../../pop/lib/master.mjs)
and [DSP status](../../pop/dsp/README.md) describe native dependencies that need
this treatment.

Build the standalone desktop shell against these same project, job, and preview
interfaces. It can package a terminal view plus the canvas/player without Slab,
but must not become a second implementation of editing or history. Start the
shell in parallel only after the contracts stabilize. “Standalone app” and
“offline inference” remain separate claims.

## Release sequence and gates

1. **Foundation:** project/artifact manifest, multi-file revisions, capability
   registry, and preview adapters. Update the stale local-contract document to
   match actual hosted inference and publishing behavior.
2. **Picture slice:** blank canvas → AC brush proposal → accept → Illy generation
   or edit → composite → PNG → restart → rollback.
3. **Sound slice:** scored phrase → synth/effect → render → playback/loop → WAV →
   restart → rollback. Ship a small reliable instrument set.
4. **Paper slice:** consult sources → outline → draft/cite → attach a versioned
   figure → build → inspect/QA → PDF and source bundle → restart → rollback.
5. **Distribution:** extracted-package tests, provider job/billing tests, and a
   desktop-shell preview using the same artifacts. Keep the existing Piece
   workflow working throughout.

Gate 0.7 on all four media completing create → revise → preview → export →
restart → rollback from an installed copy without a repository or studio vault.
Also test provider failures, disconnect/reconnect without duplicate charges,
cancelled jobs, missing optional tools, exact rollback asset hashes, and keyboard
as well as mouse navigation. Paper additionally requires valid references,
figure dependency tracking, source-bundle exclusions, and fresh visual QA after
every PDF change.

Defer a full DAW timeline, video editing, arbitrary plugin execution, automatic
per-user Git hosting, cross-device conflict resolution, and full offline model
packaging. The release should prove four complete making workflows before
expanding the tool catalog.
