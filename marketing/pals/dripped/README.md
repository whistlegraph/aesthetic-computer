# Dripped Pals

Original stills and motion recipes for the October 2026 Pals variants.
Public collection: https://pals.aesthetic.computer/dripped

- `stills/psycho-dripped.png`: original opaque 1254px black-background still.
- `stills/psycho-dripped-pink.png`: original opaque 1254px photographic pink-sky still; the AC iOS 1.2 icon uses a 1024px copy.
- `*.prompt.txt`: still-image directions, generated with the built-in image tool (model not exposed).
- `*.motion.json`: FAL Seedance 2.0 motion recipes. Six-second silent square generations requested at 1080p, returned as 1440 × 1440 masters, with the original still as both the first and final frame.

`node marketing/pals/dripped/generate.mjs` generates missing masters through the shared resumable FAL queue. Outputs and generation receipts live in `marketing/podcast/out/pals/turnarounds/`.

`node marketing/pals/dripped/publish-stills.mjs` publishes stills and adds them to the catalogue, refusing to overwrite a different immutable asset.

After inspecting the masters, run `node marketing/pals/dripped/prepare-loops.mjs` to retain the originals as `.source.mp4` and create smooth 5.5-second delivery loops. Encode with `node marketing/podcast/bin/animate-pals.mjs psycho-dripped psycho-dripped-pink --encode-only --duration 6 --webp-size 512 --webp-fps 15 --keep-last-frame`, then publish with `node marketing/podcast/bin/publish-pals-turnaround.mjs psycho-dripped psycho-dripped-pink`. Commit the catalogue and deploy Lith to expose the collection and include the new files in random-Pals placements.

Named files on `pals.aesthetic.computer`: `/pals-psycho-dripped-pink.png`, `.mp4`, `.webp`, `.apng` and the corresponding `/pals-psycho-dripped.*`. Add `?download=1` for an attachment. `/pals.json` contains the complete catalogue.
