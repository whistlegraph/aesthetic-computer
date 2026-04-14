# AC Weekly — April 7 to April 14

259 commits this week, heavy on native os & piece refinement. arena is running natively now, notepat's gun synths got meaner, and hardware support expanded to chromebooks. the native build is starting to feel less like a prototype and more like a self-contained creative environment.

— @jeffrey

## arena.mjs went native and got real 3D controls

![arena](https://oven.aesthetic.computer/news-screenshot/arena.png)

the big one: arena.mjs now boots instantly without downloading — it's bundled right into the operating system ([0173fff9](https://tangled.sh/aesthetic.computer/core/commit/0173fff93)). the game runs smooth and the collision system, camera tracking, and motion blur all work at solid framerates. that meant bringing the CamDoll FPS system to ac-native ([dc1657e0](https://tangled.sh/aesthetic.computer/core/commit/dc1657e01)).

controls got a mobile-first overhaul. you can use xbox 360 gamepads now ([17222129](https://tangled.sh/aesthetic.computer/core/commit/17222129e)), and there's an on-screen controller that shows you where everything is. touch buttons actually register when you tap them ([8d7a30ad](https://tangled.sh/aesthetic.computer/core/commit/8d7a30ad6)). right-click orbit works without glitching ([27614d4f](https://tangled.sh/aesthetic.computer/core/commit/27614d4fe)), and the camera rotation feels separated from the player rotation so you can look around freely ([96efe097](https://tangled.sh/aesthetic.computer/core/commit/96efe097f)).

the 3D geometry got richer: proper 3D platform blocks with striped sides ([1e5bea99](https://tangled.sh/aesthetic.computer/core/commit/1e5bea99f)). the lava animation stays in sync instead of drifting during gameplay ([8f0f2db4](https://tangled.sh/aesthetic.computer/core/commit/8f0f2db4a)). tile highlights don't flicker anymore ([680f98e8](https://tangled.sh/aesthetic.computer/core/commit/680f98e8a)). there was a bug where the game was showing black lava in certain situations — that's fixed ([dc8b8dea](https://tangled.sh/aesthetic.computer/core/commit/dc8b8dea9)). the speed meter moved to the top-right corner of the screen ([c6524055](https://tangled.sh/aesthetic.computer/core/commit/c652405505)).

## notepat keeps getting meaner

notepat war — the gun synthesis side — picked up a full arsenal. there's a new ZOO + LASERS kit with classic subtractive synthesis recipes ([6dd62247](https://tangled.sh/aesthetic.computer/core/commit/6dd62247)). the guns have crisp click layers ([aae7b0e6](https://tangled.sh/aesthetic.computer/core/commit/aae7b0e6d)), muzzle blast effects, full-auto SMG fire, and balanced machine gun sounds ([fe86cc88a](https://tangled.sh/aesthetic.computer/core/commit/fe86cc88a)). the classic 3-layer gun synthesis with leaner physical models came through ([a0cf6e9ca](https://tangled.sh/aesthetic.computer/core/commit/a0cf6e9ca)). notepat itself lets you drag to edit gun parameters and there's pitched percussion noise ([93b9834f](https://tangled.sh/aesthetic.computer/core/commit/93b9834f5)). physical sounds are finally audible and pad hits flash on screen ([91207d15](https://tangled.sh/aesthetic.computer/core/commit/91207d15b)).

## hardware support expanding

chromebooks and budget laptops are now supported. you can install the os directly on eMMC storage (common in chromebooks) ([ec111dd8](https://tangled.sh/aesthetic.computer/core/commit/ec111dd8)). full chromebook hardware support means WiFi and audio work out of the box ([85d31263](https://tangled.sh/aesthetic.computer/core/commit/85d3126344)). the audio firmware now ships inside the system so you don't need to load separate drivers ([bb81b03e](https://tangled.sh/aesthetic.computer/core/commit/bb81b03e2)). wifi starts up correctly ([dddf8eca](https://tangled.sh/aesthetic.computer/core/commit/dddf8eca2)). audio works on Jasper Lake chips with proper HDMI support ([6471ca64](https://tangled.sh/aesthetic.computer/core/commit/6471ca646)).

the os also got an anonymous flash mode so you can boot without credentials ([37f6fa49](https://tangled.sh/aesthetic.computer/core/commit/37f6fa498)), and the system is smarter about caching during OTA updates.

## cursor control API and iOS fixes

pieces can now control the mouse cursor ([12eafb70](https://tangled.sh/aesthetic.computer/core/commit/12eafb706)) — arena shows a custom SVG cursor with a click animation in first-person mode ([0178aa7d](https://tangled.sh/aesthetic.computer/core/commit/0178aa7d9)). the cursor changes based on whether you're in pen mode or not ([1b69b58d](https://tangled.sh/aesthetic.computer/core/commit/1b69b58dd)).

iOS got a bunch of fixes that were annoying. the keyboard wasn't opening properly on two-tap — fixed ([5f0e2df3](https://tangled.sh/aesthetic.computer/core/commit/5f0e2df31)). pop-up windows were getting blocked — now it falls back to opening in the same tab ([d309191c](https://tangled.sh/aesthetic.computer/core/commit/d309191cc)). ableton downloads were broken because of how the app launches — those work now ([536c7937](https://tangled.sh/aesthetic.computer/core/commit/536c7937d), [a5bbe56c](https://tangled.sh/aesthetic.computer/core/commit/a5bbe56c4)).

## ui and theme work

the os UI got cleaner — less visual density, secondary sections fold away by default ([85c78a6b](https://tangled.sh/aesthetic.computer/core/commit/85c78a6b)). kidlisp's color system works better ([b8d85446](https://tangled.sh/aesthetic.computer/core/commit/b8d85446b)). we bumped the service worker cache so old versions of pieces get cleared out ([69ccc3e1](https://tangled.sh/aesthetic.computer/core/commit/69ccc3e17)).

## new piece: seashells

seashells shipped — initial implementation and documentation ([9137a11b](https://tangled.sh/aesthetic.computer/core/commit/9137a11b5)).
