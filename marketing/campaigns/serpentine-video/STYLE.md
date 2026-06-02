# serpentine-video — illustration set

nine per-segment stills for the Serpentine FAE proposal video. portrait
1024×1536 (matches recap photo layer). candid iphone-snapshot tone, deadpan/
sincere — NOT cinematic, NOT neon-noir. one consistent series: terminal-green /
warm AC palette across all nine.

hard rules (baked into every prompt):
- everything in sharp focus. NO motion blur, NO bokeh, NO out-of-focus.
  convey movement through pose only.
- jeffrey's laptop: citrus-green MacBook Neo (apple's official color) OR plain
  black thinkpad. the Neo lid has a torn white-paper scrap with a hand-penned
  thick whistlegraph BUTTERFLY (never an apple logo, never "pals"). nothing in
  front of the laptop in-camera.
- green-on-black terminals; screens show varied REAL content (terminals / paint
  / code / scopes) — never a thumbnail of the surrounding scene (no recursive
  screens).
- when jeffrey is in frame: head engaged with the screen, eyes on the work,
  angled into it — never the presentational "showing the laptop to camera" pose.
- real outfits only (button-down / hoodie / printed tee / cardigan / sweater /
  flannel). never tank tops, never costumes.
- group scenes: horizontal, peer eye-line, jeffrey one of many — never lone-
  centered / cult-leader.
- diegetic light — light comes from a real source in the scene (screen glow,
  lamp, window).

per-segment generation:
  01 title, 08 plan (NELA room), 09 outro → jeffrey refs (default edits)
  02 problem, 03 os_layer, 04 creative_os, 05 commons, 06 llms, 07 poised
    → --no-jeffrey (conceptual, prompt-only)

run (one at a time / ≤3 parallel):
  node marketing/bin/gen-promo.mjs marketing/campaigns/serpentine-video/01_title --no-mirror
  node marketing/bin/gen-promo.mjs marketing/campaigns/serpentine-video/02_problem --no-jeffrey --no-mirror
  ... etc

then copy each gens/v1.png → recap/out/jeffrey-photos/<segment>.png and
re-run compose (fish bin/compose.fish) — slides.mjs picks them up via photos.txt.
