# Weekly News From @jeffrey

**status:** draft
**author:** @jeffrey
**for:** news.aesthetic.computer post
**date:** week of april 7–14, 2026
**research:** `node at/news-cli.mjs commits --since "2026-04-06 20:00:00"`

---

259 commits this week, heavy on native os & piece refinement. arena is running natively now, notepat's gun synths got meaner, and hardware support expanded to chromebooks. the native build is starting to feel less like a prototype and more like a self-contained creative environment.

## arena.mjs went native and got real 3D controls

the big one: arena.mjs is now bundled straight into the initramfs ([0173fff9](https://github.com/whistlegraph/aesthetic-computer/commit/0173fff93)). it boots without downloading, runs at solid framerates on the hardware we test with. that meant bringing the CamDoll FPS system to ac-native — the overhead tracking, the motion blur, the whole collision system ([dc1657e0](https://github.com/whistlegraph/aesthetic-computer/commit/dc1657e01)).

controls got a mobile-first overhaul. there's xbox 360 gamepad support now ([17222129](https://github.com/whistlegraph/aesthetic-computer/commit/17222129e)), an on-screen controller minimap ([17222129](https://github.com/whistlegraph/aesthetic-computer/commit/17222129e)), and proper touch button wiring so taps actually hit without event passthrough eating them ([8d7a30ad](https://github.com/whistlegraph/aesthetic-computer/commit/8d7a30ad6)). right-click orbit works without double-rotating ([27614d4f](https://github.com/whistlegraph/aesthetic-computer/commit/27614d4fe)), camera rotation decoupled from player rotation ([96efe097](https://github.com/whistlegraph/aesthetic-computer/commit/96efe097f)).

the 3D geometry got richer: 3D platform blocks with proper striped side faces ([1e5bea99](https://github.com/whistlegraph/aesthetic-computer/commit/1e5bea99f)). the lava animation now syncs to the sim clock instead of drifting with paint time ([8f0f2db4](https://github.com/whistlegraph/aesthetic-computer/commit/8f0f2db4a)). tile highlights stopped flickering by baking deterministic per-tile noise instead of rebuilding every frame ([680f98e8](https://github.com/whistlegraph/aesthetic-computer/commit/680f98e8a)). an NaN fps bug that was turning lava black got fixed ([dc8b8dea](https://github.com/whistlegraph/aesthetic-computer/commit/dc8b8dea9)).

the speed meter lives in the top-right HUD now ([c6524055](https://github.com/whistlegraph/aesthetic-computer/commit/c652405505)) and on iOS specifically, the lava black issue got its own fix pass ([c6524055](https://github.com/whistlegraph/aesthetic-computer/commit/c652405505)).

## notepat keeps getting meaner

notepat war — the gun synthesis side — picked up a full arsenal. there's a ZOO + LASERS kit with classic subtractive synthesis recipes ([6dd62247](https://github.com/whistlegraph/aesthetic-computer/commit/6dd62247)), crisp click layers ([aae7b0e6](https://github.com/whistlegraph/aesthetic-computer/commit/aae7b0e6d)), Friedlander muzzle blasts, full-auto SMG fire, and balanced LMG ([fe86cc88](https://github.com/whistlegraph/aesthetic-computer/commit/fe86cc88a)). the classic 3-layer gun synthesis with leaner physical models came through ([a0cf6e9ca](https://github.com/whistlegraph/aesthetic-computer/commit/a0cf6e9ca)). notepad itself got drag-to-edit gun params and pitched perc noise ([93b9834f](https://github.com/whistlegraph/aesthetic-computer/commit/93b9834f5)). physical sounds are finally audible and there's static perc notation + pad hit flash ([91207d15](https://github.com/whistlegraph/aesthetic-computer/commit/91207d15b)).

## hardware support expanding

chromebooks and budget laptops are now on the roadmap. eMMC install target landed ([ec111dd8](https://github.com/whistlegraph/aesthetic-computer/commit/ec111dd8)), followed by full chromebook hardware support — RTW/MT wifi + SOF audio ([85d31263](https://github.com/whistlegraph/aesthetic-computer/commit/85d3126344))). then the firmware pieces: alsa-sof-firmware now ships in the initramfs so SOF blobs don't need external loading ([bb81b03e](https://github.com/whistlegraph/aesthetic-computer/commit/bb81b03e2)). wifi got unblocked before interface probe ([dddf8eca](https://github.com/whistlegraph/aesthetic-computer/commit/dddf8eca2)). SOF actually got enabled on Jasper Lake with SOF_PCI + LPSS + HDMI codec support ([6471ca64](https://github.com/whistlegraph/aesthetic-computer/commit/6471ca646)).

the os also got an anonymous flash mode — `AC_ANON=1` — with cache-aware OTA pull ([37f6fa49](https://github.com/whistlegraph/aesthetic-computer/commit/37f6fa498)).

## cursor control API and iOS fixes

pieces can now set CSS cursors via a new api.cursor() call ([12eafb70](https://github.com/whistlegraph/aesthetic-computer/commit/12eafb706)). arena uses this to show a custom SVG cursor with click-expansion animation in FPS mode ([0178aa7d](https://github.com/whistlegraph/aesthetic-computer/commit/0178aa7d9)). the cursor switches dynamically based on pen lock state ([1b69b58d](https://github.com/whistlegraph/aesthetic-computer/commit/1b69b58dd)).

iOS got a whole batch of fixes. two-tap keyboard opening was deferring focus and eating input — fixed by deferring programmatic focus ([5f0e2df3](https://github.com/whistlegraph/aesthetic-computer/commit/5f0e2df31)). window.open was getting popup-blocked, so the bios now falls back to same-tab nav ([d309191c](https://github.com/whistlegraph/aesthetic-computer/commit/d309191cc)). ableton downloads were broken because we weren't firing window.open from a DOM hitbox handler — now they work via download-url ([536c7937](https://github.com/whistlegraph/aesthetic-computer/commit/536c7937d), [a5bbe56c](https://github.com/whistlegraph/aesthetic-computer/commit/a5bbe56c4)).

## ui and theme work

the os UI got leaner — reduced density and collapsed secondary sections ([85c78a6b](https://github.com/whistlegraph/aesthetic-computer/commit/85c78a6b)). kidlisp baked first-word color into layer0 to fix a regression ([b8d85446](https://github.com/whistlegraph/aesthetic-computer/commit/b8d85446b)). a service worker cache version bump (v2 → v3) cleared stale bios and ableton instances ([69ccc3e1](https://github.com/whistlegraph/aesthetic-computer/commit/69ccc3e17)).

## new piece: seashells

seashells shipped — initial implementation and documentation ([9137a11b](https://github.com/whistlegraph/aesthetic-computer/commit/9137a11b5)).

---

— @jeffrey

---

## notes to self

- this feels more like "os as pieces" week — arena and notepat running natively is the story
- the hardware expansion (chromebook + budget laptop) signals something shifting
- arena's polish is significant (controls, 3D, cursor api) but less flashy than new features
- iOS fixes are many but small — maybe cluster them better next pass?
- tone still good — concrete, first-person, lowercase
- 259 commits is solid week
