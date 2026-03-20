# Weekly News From @jeffrey

**status:** draft
**author:** @jeffrey
**for:** news.aesthetic.computer post
**date:** week of march 13–20, 2026
**research:** reports/2026-03-18-changeloggin-4-all.md

---

this is the first weekly commit changelog for aesthetic computer. i'm going to try posting one of these every week — a digest of what actually shipped, with links to the commits so you can click through to the code. i'll keep experimenting with what kind of information to include and how to present it. thanks for reading and for using ac.

508 commits this week. here's what changed.

## the os boots for real now

ac native os had its biggest week yet. boot time dropped 3.5 seconds from shared library deduplication alone ([bb63330](https://github.com/whistlegraph/aesthetic-computer/commit/bb63330c6), 84MB less in the initramfs — [d3ba789](https://github.com/whistlegraph/aesthetic-computer/commit/d3ba789ec)). there's a proper software volume control now — samples get multiplied in the audio thread, not through flaky ALSA mixer elements ([ede89eb](https://github.com/whistlegraph/aesthetic-computer/commit/ede89eb57)). the boot plays a little arpeggio when you power on without credentials ([005cf4f](https://github.com/whistlegraph/aesthetic-computer/commit/005cf4ffb)). light mode works — notepat's visualizer, recital view, and picture overlay all respect it ([2aed1e9](https://github.com/whistlegraph/aesthetic-computer/commit/2aed1e9dc), [4b75e7e](https://github.com/whistlegraph/aesthetic-computer/commit/4b75e7e3f)).

the layout went responsive ([24a45d7](https://github.com/whistlegraph/aesthetic-computer/commit/24a45d7e9)). buttons center on small screens, text wraps, resize events don't break anything. there's a mobile-first left-aligned layout with colored section headers ([faf36c4](https://github.com/whistlegraph/aesthetic-computer/commit/faf36c4c7)).

oven builds got heavier but more complete: cage, seatd, Mesa, Wayland ([c885381](https://github.com/whistlegraph/aesthetic-computer/commit/c8853811d)), all i915 GPU firmware, Intel SOF audio firmware for every platform we've tested ([1b5c3b7](https://github.com/whistlegraph/aesthetic-computer/commit/1b5c3b746), [e3b544e](https://github.com/whistlegraph/aesthetic-computer/commit/e3b544e95)). claude code ships as a native binary in the image now ([d5b25da](https://github.com/whistlegraph/aesthetic-computer/commit/d5b25dafa)). credentials get injected at flash time through ac-usb ([24d3f5e](https://github.com/whistlegraph/aesthetic-computer/commit/24d3f5eed)).

OTA updates go through cloudflare now — there's an edge CDN at os.aesthetic.computer with named build URLs ([aedd4aa](https://github.com/whistlegraph/aesthetic-computer/commit/aedd4aaa5)) and a mirror selector ([6c538fe](https://github.com/whistlegraph/aesthetic-computer/commit/6c538fe9f)). the machine flushes its full boot-to-shutdown log to MongoDB on poweroff ([6c94411](https://github.com/whistlegraph/aesthetic-computer/commit/6c9441192)) so we can debug remotely.

babypat exists: a bare-metal UEFI musical keyboard in 1,536 bytes ([2740d3c](https://github.com/whistlegraph/aesthetic-computer/commit/2740d3cf6)). colors on keypress, smooth fade. it has its own CLI (bb-os) and UEFI file logging for debug ([3020e36](https://github.com/whistlegraph/aesthetic-computer/commit/3020e36ff)).

## mic capture and recording

spent most of the week fighting ALSA capture on Intel HDA. the problem: DMA transfers were failing with EIO on Kaby Lake because Intel IOMMU was enabled. fixed by disabling IOMMU in the kernel command line ([d2caf3e](https://github.com/whistlegraph/aesthetic-computer/commit/d2caf3e0f)). then the capture thread needed a ring buffer for reliable recording ([a061634](https://github.com/whistlegraph/aesthetic-computer/commit/a061634ef)) — quick key taps were missing data because the read loop was too slow to catch short bursts ([1e5caa5](https://github.com/whistlegraph/aesthetic-computer/commit/1e5caa52c)). there's a spin-wait on mic_stop now so the buffer actually fills before we stop reading ([6aa5f0f](https://github.com/whistlegraph/aesthetic-computer/commit/6aa5f0f84)). period size settled at 1024 frames, buffer at 4 periods ([2f722bd](https://github.com/whistlegraph/aesthetic-computer/commit/2f722bd30)).

recording has a countdown timer ([a061634](https://github.com/whistlegraph/aesthetic-computer/commit/a061634ef)). there's a tap callback for triggering records from JS ([eb23e8f](https://github.com/whistlegraph/aesthetic-computer/commit/eb23e8fec)).

## papers

the papers site got a real build pipeline. the oven auto-builds all papers on push ([3e215b0](https://github.com/whistlegraph/aesthetic-computer/commit/3e215b008)) — xelatex runs three passes, bibtex resolves, and the PDFs deploy to Netlify. incremental builds only rebuild papers whose sources changed ([1a999d4](https://github.com/whistlegraph/aesthetic-computer/commit/1a999d459)). 96 PDFs live on the site now ([e353e4a](https://github.com/whistlegraph/aesthetic-computer/commit/e353e4a00)).

new paper: "CalArts, Callouts, and Papers" — first paper tagged PSYCHO ([fcf828f](https://github.com/whistlegraph/aesthetic-computer/commit/fcf828f66)). the PSYCHO label on the papers index blinks red/yellow/black/white ([4eb6ed0](https://github.com/whistlegraph/aesthetic-computer/commit/4eb6ed03b)) and the title characters color-cycle and wiggle ([cab9481](https://github.com/whistlegraph/aesthetic-computer/commit/cab9481dd)).

tables in cards format no longer overflow the column ([e262808](https://github.com/whistlegraph/aesthetic-computer/commit/e26280854)). every tabular gets wrapped in an adjustbox that clamps to linewidth.

the author line across all papers changed from "Jeffrey Alan Scudder" to "@jeffrey" ([b6ae995](https://github.com/whistlegraph/aesthetic-computer/commit/b6ae99586)). fact-checked the Sucking the Complex paper — TikTok founding date, Ulman, Blas, Saltz citations were wrong ([d5d36b3](https://github.com/whistlegraph/aesthetic-computer/commit/d5d36b343)). added critique-as-content references to the platter ([872766e](https://github.com/whistlegraph/aesthetic-computer/commit/872766e32)). papers now have PDF hit tracking and proper OG meta tags ([76e5f11](https://github.com/whistlegraph/aesthetic-computer/commit/76e5f11cf)).

## kidlisp and keeps

keeps.kidlisp.com → keep.kidlisp.com. singular ([4c349e8](https://github.com/whistlegraph/aesthetic-computer/commit/4c349e80c)). buy.kidlisp.com lost its card carousel ([7d02521](https://github.com/whistlegraph/aesthetic-computer/commit/7d0252127)) and got drop shadows on the Fresh/Sold tabs with auto-cycling cards instead ([e02fd1d](https://github.com/whistlegraph/aesthetic-computer/commit/e02fd1d36)). there's a `buy` command on the CLI now to fulfill objkt marketplace listings ([4469b4f](https://github.com/whistlegraph/aesthetic-computer/commit/4469b4f17)).

kidlisp.com got a dashboard: FPS gauge, interpreter state, memory usage, device info ([531b8cf](https://github.com/whistlegraph/aesthetic-computer/commit/531b8cf34)). GPU rendering toggle in preferences ([351dbb3](https://github.com/whistlegraph/aesthetic-computer/commit/351dbb3ef)). light mode theme ([de43b3f](https://github.com/whistlegraph/aesthetic-computer/commit/de43b3f2f)). settings modal is more compact ([de43b3f](https://github.com/whistlegraph/aesthetic-computer/commit/de43b3f2f)). all KidLisp effects force the CPU path now (FORCE_CPU flag) because Mali and Adreno GPUs were still glitching on some shaders ([f69c760](https://github.com/whistlegraph/aesthetic-computer/commit/f69c76052)).

## the prompt and frontend

the login curtain has a spinning wireframe cube now, over a faster starfield ([f91b7fb](https://github.com/whistlegraph/aesthetic-computer/commit/f91b7fb1a)). the x86 OS button got its margins fixed three separate times ([b4eb33a](https://github.com/whistlegraph/aesthetic-computer/commit/b4eb33a2c), [cfe25b6](https://github.com/whistlegraph/aesthetic-computer/commit/cfe25b6a6), [2ddcb64](https://github.com/whistlegraph/aesthetic-computer/commit/2ddcb6490)) (it's right this time). commit button text is brighter ([ec973bd](https://github.com/whistlegraph/aesthetic-computer/commit/ec973bd32)).

email addresses in chat auto-link correctly instead of getting parsed as @handles ([49c9d92](https://github.com/whistlegraph/aesthetic-computer/commit/49c9d9287)). the profanity filter skips clock-chat (laer-klokken) because timestamps kept getting flagged ([cea3971](https://github.com/whistlegraph/aesthetic-computer/commit/cea397157)). TTS only fires in the prompt piece now, not everywhere ([60b98f5](https://github.com/whistlegraph/aesthetic-computer/commit/60b98f569)).

the Dual Kawase blur replaced the old Gaussian — it's the ARM SIGGRAPH 2015 technique ([3e24959](https://github.com/whistlegraph/aesthetic-computer/commit/3e249595b)). mobileSafeMode blocks all GPU effects on Mali/Adreno except Kawase ([00149953](https://github.com/whistlegraph/aesthetic-computer/commit/00149953a)). the vortex/spin shader got fixed twice ([af23b88](https://github.com/whistlegraph/aesthetic-computer/commit/af23b88e9), [82a8896](https://github.com/whistlegraph/aesthetic-computer/commit/82a8896d0)).

## infrastructure

the oven's papers builder auto-pushes built PDFs to git ([63f4ffa](https://github.com/whistlegraph/aesthetic-computer/commit/63f4ffa77)). the devcontainer cleaned up auto-installed extensions ([a83ba4d](https://github.com/whistlegraph/aesthetic-computer/commit/a83ba4d50)). a Lotus Notes opinion piece got written ([3475e48](https://github.com/whistlegraph/aesthetic-computer/commit/3475e48fa)) (unrelated but it happened this week).

---

— @jeffrey

---

## notes to self

- tone: conversational, first person, lowercase. not a press release.
- focus: what people touch and feel + enough technical detail to be honest
- 508 commits is a lot — the structure helps people scan
- every claim now has a clickable commit hash so readers can verify
- TODO: is the ars electronica framing still relevant? removed for now since it's not this week's news
- TODO: should this link to specific pieces? (aesthetic.computer/squash, etc.)
- TODO: read it out loud — does it sound like me talking?
- TODO: the ALSA capture section might be too deep — is that the right audience for news.aesthetic.computer?
