# Miniature night

Generated miniature photography was introduced in Xbox package **1.0.0.43** and runs in **1.0.0.44** and the paired AC OS build. Both native displays were visually verified. Sustained 60 FPS is **not yet verified**. The room, puppet heads and limbs, blaster, skateboard, concrete platforms, and rocket share this asset set. AC is purple on the left; Xbox is green on the right. The gun remains in the posed hand.

Run from this directory with `python3 -m http.server 7893 --bind 127.0.0.1`, then open `http://127.0.0.1:7893`. The theme selector switches the same animated pose between flat and miniature materials without restarting it. This is a material/rig demo with synthetic poses, not a running multiplayer game. `preview-miniature.png` is an inspected render; `preview-flat.png` is the comparison.

`theme.mjs` is a presentation-only adapter: the host supplies head position, limb endpoints, hand position, facing, and seat identity. It does not mutate those inputs or touch gameplay random state. `canvasDriver` implements the abstract image/shape drawing contract. All generation happens before play. Two decoded images are retained for the session; animation transforms atlas regions without downloading or regenerating anything.

## Native integration

Xbox package 1.0.0.43 adds the retained texture path and bounded APIs below; its Windows build, packaged asset hashes, installation, and actual display are verified. AC OS implements the same API in its GLES renderer and was visually verified on HDMI. `nativeReady` means available and visually verified on both hosts; `native60FpsVerified` remains false. See `native-verification.json` and `verification-xbox43.png`.

The common native API is `themeReady()`, `themeSprite(asset,sx,sy,sw,sh,cx,cy,w,h,radians,flip,z)`, and `themeQuad(asset,sx,sy,sw,sh,x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4)`. Quad vertices follow TL/TR/BR/BL order. Asset 0 is the background, asset 1 the props. Source rectangles use original master pixels; destination/depth match each host's triangle API. Alpha below 0.5 is discarded before depth writes. `native/prepare.mjs` reproducibly converts the masters into the shared RGBA8 variants, with hashes in `native/manifest.json`.

| Runtime | Existing path | Missing work |
| --- | --- | --- |
| Xbox | D3D11 uploads two packaged RGBA images once, reuses the sprite shader and vertex buffer, and submits at most two theme batches. | Sustained performance measurement. The existing spark and Jeffrey texture paths are preserved. Queue capacity is 1,024 quads. |
| AC OS | Retained atlas textures and UV quads share the offscreen GLES target and its existing final readback. | Continue frame pacing work and measure sustained performance with debug and HDMI enabled. CPU font rasterization is not a GPU glyph atlas. No per-pixel JS rendering. |

Native hosts load the packaged raw RGBA assets once and expose availability through `themeReady()`. The shared game uses photographic materials when available; `__oskiewarGraphicsTheme='flat'` selects vectors, which are also the fallback when the host lacks the texture API. Collision shapes, player state, hash calculation and fixed-step simulation remain unchanged. A future menu selector could switch materials independently at a round boundary without changing the physics protocol.

The game now draws projected heads, limbs, held guns, skateboards and terrain with these assets; the browser adapter remains an isolated reference. Gameplay supplies the detached-head pose, and sprite bounds never determine its collision radius. Version 171 adds photographic grenade pickups, held grenades, launcher/SMG props, muzzle flashes, eight-frame fire/smoke explosions, and bounded WIN/LOSE/TIE accents when the extended native capabilities are available. Individual map materials and some small powerup/debris graphics still use the original artwork or procedural shapes. The browser preview covers one map and two figures.

## Effects extension (native 45 / game 171)

This extension is built and tested but is pending coordinated device installation and visual verification. `themeAssetReady(id)` probes each optional atlas independently: asset 2 is the explosion flipbook (master 1774×887), and asset 3 is the grenade/weapon/flash sheet (master 1254×1254). `themeSprite` accepts an optional thirteenth argument, `depthWrite`, defaulting to true. Older hosts retain the original theme and procedural effects.

Explosions and flashes use straight alpha blending, discard nearly transparent fragments, and never write depth. Solid weapon props retain the 0.5 alpha cutoff before depth writes to prevent faint edge pixels from occluding the scene. Xbox submits original materials, solid props, soft flashes, then explosions; the original 1,024-quad bound remains. Generated assets are retained, and the added graphics never alter gameplay state or random numbers.

`drawPhotoRoundOutcome(result, ageSeconds, x, y, width, height)` draws two bounded photographic accents behind the caller’s result word. It emits no text, refreshes capabilities even on result-only frames, and leaves the caller’s stats and QR panel in front. Exact generation prompts and original output paths are in `effects-provenance.json`; native hashes are in `native/manifest.json`.

Portable native and QuickJS tests pass, as do 28 netplay tests and targeted rendering tests covering frame bounds, pose immutability, depth flags, and older-host fallback. Actual native 45 frame rate and display quality still need verification after installation. The browser material demo intentionally remains the original two-atlas pose reference.

## Cloud sky extension (native 46)

Asset 4 adds an opaque cloud sky for AIR: master `assets/sky-clouds-v1.png` is 1672×940; native `sky-clouds-v1-1024x576.rgba` is 1024×576. Probe `themeAssetReady(4)` before drawing through the existing sprite or quad API. A missing sky never disables the original theme. It uses the original cutout shader and normal depth writes, without blending, and renders before foreground materials. The source image was visually inspected: cloud banks surround open blue-violet sky, with no ground, objects or text. Exact built-in generation prompt and provider are in `sky-provenance.json`.

The fifth atlas adds 2.25 MiB, bringing retained RGBA textures to **12.5 MiB** (13,107,200 bytes). Xbox portable/QuickJS tests verify availability and the exact master bounds. AC native tests cover sky depth and optional-asset fallback. Device deployment and actual display verification are coordinated separately.

## Frame budget

The master images total about 12 MiB decoded RGBA; encoded assets total about 2.8 MiB. The original native pair uses a capped 1024×576 background and 1024×512 atlas, totaling **4.25 MiB RGBA**. The optional effects extension adds a 1024×512 explosion atlas and 1024×1024 weapon atlas, bringing retained texture memory to **10.25 MiB RGBA** (10,747,904 bytes), uploaded outside the frame loop. The runtime retains GPU buffers and batches by atlas. Keep alpha overdraw to each sprite's tight bounds; do not add blur, per-frame image decoding, per-frame uploads, runtime model calls, or JavaScript pixel loops.

The remaining performance acceptance target: run the same deterministic replay on **both actual devices**, debug overlay and HDMI mirroring enabled, with guns, detached heads, grenades, and falling powerups. Measure total frame p95 ≤16.67 ms for sustained 60 FPS, not just this renderer's submission time. Compare dark and miniature variants, check memory and texture-loss recovery, and verify zero new netplay mismatches. Adding assets alone does not establish that target.

## Verification

`node --test theme.test.mjs` checks pose immutability, independent seat/facing identity, atlas bounds, and explicit missing-capability behavior. `node verify.mjs` uses installed Google Chrome through Playwright to render both themes, switch while paused without time changing, resume animation, and record browser errors plus CPU draw-submission samples. See `verification.json`; its timings are **not Xbox/AC OS FPS**.

Generated with the built-in `image_gen` tool. Exact prompts, dimensions, file hashes, and alpha verification are in `provenance.json`. Generated imagery is a first coherent visual direction, not a claim that every effect or every map has unique photographic artwork. The original browser preview remains isolated; native integration is in the shared game and platform renderers.
