# Weekly News From @jeffrey

259 commits this week, heavy on native os & piece refinement. arena is running natively now, notepat's gun synths got meaner, and hardware support expanded to chromebooks. the native build is starting to feel less like a prototype and more like a self-contained creative environment.

## arena.mjs went native and got real 3D controls

![arena](https://oven.aesthetic.computer/news-screenshot/arena.png)

the big one: arena.mjs now boots instantly without downloading — it's bundled right into the operating system. the game runs smooth and the collision system, camera tracking, and motion blur all work at solid framerates.

controls got a mobile-first overhaul. you can use xbox 360 gamepads now, and there's an on-screen controller that shows you where everything is. touch buttons actually register when you tap them. right-click orbit works without glitching, and the camera rotation feels separated from the player rotation so you can look around freely.

the 3D geometry got richer: proper 3D platform blocks with striped sides. the lava animation stays in sync instead of drifting during gameplay. tile highlights don't flicker anymore. there was a bug where the game was showing black lava in certain situations — that's fixed. the speed meter moved to the top-right corner of the screen where it's less distracting.

## notepat keeps getting meaner

notepat war — the gun synthesis side — picked up a full arsenal. there's a new ZOO + LASERS kit with classic subtractive synthesis recipes. the guns have crisp click layers, muzzle blast effects, full-auto SMG fire, and balanced machine gun sounds. the synth got leaner and faster. notepat itself lets you drag to edit gun parameters and there's pitched percussion noise. physical sounds are finally audible and pad hits flash on screen.

## hardware support expanding

chromebooks and budget laptops are now supported. you can install the os directly on eMMC storage (common in chromebooks). full chromebook hardware support means WiFi and audio work out of the box. the audio firmware now ships inside the system so you don't need to load separate drivers. wifi starts up correctly. audio works on Jasper Lake chips with proper HDMI support.

the os also got an anonymous flash mode so you can boot without credentials, and the system is smarter about caching during OTA updates.

## cursor control API and iOS fixes

pieces can now control the mouse cursor — arena shows a custom SVG cursor with a click animation in first-person mode. the cursor changes based on whether you're in pen mode or not.

iOS got a bunch of fixes that were annoying. the keyboard wasn't opening properly on two-tap — fixed. pop-up windows were getting blocked — now it falls back to opening in the same tab. ableton downloads were broken because of how the app launches — those work now.

## ui and theme work

the os UI got cleaner — less visual density, secondary sections fold away by default. kidlisp's color system works better. we bumped the service worker cache so old versions of pieces get cleared out.

## new piece: seashells

seashells shipped — a new piece with documentation.

---

— @jeffrey
