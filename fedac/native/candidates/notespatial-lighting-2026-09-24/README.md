# Notepat score lighting and raster candidate

Prepared offline; no live device writes, playback, reboot, or USB changes.

The existing held fixture uses `candlelightRgb(t,5) * 4`: a constant orange with shallow noise, independent of notes, harmonies and movements. This candidate replaces that source with a compiled timeline that follows the actual routed score.

- Eleven movement palettes move through amber, mint, violet, coral, blue, pale blue, purple, cyan, gold, lavender and dusk blue. Note pitch contributes 28% of the color; the movement contributes 72%.
- Each fixture follows the notes audible at its own seat, using the same equal-power routing and GM trims as the current audio engine. Held notes affect the held wedge; ring notes remain on the ring.
- Note attack 120 ms, energy rise 160 ms and release 550 ms, plus a 650 ms event tail. Continuous dim color in rests and shallow smooth shimmer avoid binary flashes. Every color channel is bounded to 180 DMX units/second. No strobe channel is used.
- Held RGB peaks at 235/255; room RGB caps at 88/255. These are channel values, not measured photometric brightness. Audio volume does not silently dim the lights.
- Each of the eleven movements has its own raster grammar: flowing lines, pulsing columns, orbiting squares, crossing lines, creeping slits, swaying nested boxes, rising steps, double helix, radial fanfare, returning box ripples, and shrinking dusk lines. Each frame has one dark wipe and at most 24 primitives; one large note label is optional. No framebuffer copies, per-pixel processing, or full-score scans during paint.

`index.html` previews all eleven movements and the four room fixtures plus held fixture. Scrub changes the relative point within each movement. This preview is silent and sends no hardware commands.

## Integration

1. Run `node compile-look.mjs <actual-score.nsscore> <output.nstimeline>` after every score or GM trim change. The checked-in timeline targets SHA-256 `139a77d1758773680fe08038f4a39f8e7a6b48838a687369a4d9e17a28cd2a5c`, 774.4119 seconds. Refuse to pair it with another score. It is not a Sophia lighting score.
2. Load `/pieces/notespatial-look.nstimeline` once using `loadTimeline(system)`. It is larger than native `readFile`'s tail limit, so the loader uses `readFileBytes`. Never parse it each frame.
3. From the existing authoritative `getPerformanceVisualState()`, pass score time only while phase is `playing`; otherwise pass `-1`. `lookAt` uses direct indexing and interpolation, so seeks, dropped frames and different refresh rates do not accumulate drift. Native module caching survives jumps: deploy with versioned module filenames and update imports, rather than overwriting the old module path.
4. Replace the held candle RGB source with `heldSlots(timeline,t)`; preserve current changed-value suppression, 25 Hz limit, failed-write backoff, and leave blackout. d041–043 are slots 40–42; no other slots are written.
5. Replace the room follower's one `address:'all'` color with the four distinct entries from `fixtureFrame(timeline,t).room`: d001 left front, d011 right front, d031 right rear, d021 left rear. These are separate DMX messages; the bridge currently requires care with its queue. Add/verify atomic multi-fixture support or a bounded latest-state sender before live use; do not enqueue four updates at 8 Hz without measuring the bridge. TTL/stop blackout and run-ID guard must remain.
6. Call `paintLook` in place of the old frame renderer, before drawing the existing battery overlay. Do not paint over the already rendered battery. Existing note-only mode can remain selectable; `noteLabels:true` adds the timeline's strongest local pitch on top of the new raster. Pitch labels here use score envelopes rather than native voice lifecycle, so the old live-note renderer remains more exact for voice-steal diagnostics.

No existing wrapper or room follower is modified by this candidate. Preserve composition-volume controls and their single-call fanout; native brightness control remains separate. Hardware color appearance, bridge timing, acoustic synchrony and measured FPS improvement still need an audition. Existing OS DMX writes may block the JS thread; reducing draw calls does not fix that underlying behavior.

## Validation

`node --test look.test.mjs` verifies held/ring isolation, pitch changes, decay, deterministic seeking, blackouts, fixture slots, full-byte loading, raster draw budget and real-score palette/brightness/slew bounds. `preview.png` is a rendered contact sheet from the silent browser preview.
