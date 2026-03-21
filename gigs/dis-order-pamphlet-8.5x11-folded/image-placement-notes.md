# Dis-Order Pamphlet Image Placement Notes

## v3 Images (from Rebecca/Ari — March 2026)

New section images provided by Ari, stored in `system/public/assets/gigs/disorder-program/images-v3/`:

1. `I. begin with fire` → `system/public/assets/gigs/disorder-program/images-v3/i-begin-with-fire.png` (candles painting)
2. `II. wicked child bacchanal` → `system/public/assets/gigs/disorder-program/images-v3/ii-wicked-child.png` (wine glasses)
3. `III. total darkness` → `system/public/assets/gigs/disorder-program/images-v3/iii-total-darkness.png` (baby/egg + darkness)
4. `IV. lovers' pastoral` → `system/public/assets/gigs/disorder-program/images-v3/iv-lovers-pastoral.png` (heads + flowers)
5. `V. death of the first born` → `system/public/assets/gigs/disorder-program/images-v3/v-death-of-first-born.png` (daughter)

Source files: `system/public/assets/gigs/disorder-program/1.beginwithfire.png` etc.

## Puppet Line Drawings for Cover

Source: `system/public/assets/gigs/disorder-program/Untitled design 3.png`
- Ari's puppet sketches, played with on Canva for cover use
- Auto-split into 13 individual assets in `system/public/assets/gigs/disorder-program/images-v3/puppets/puppet-1.png` through `puppet-13.png`
- Key figures:
  - `puppet-4.png` (800x1412) — tall donkey/horse puppet, full body
  - `puppet-7.png` (1149x1284) — figure with large head + hand/claw
  - `puppet-8.png` (1479x2322) — largest composite: donkey puppet + smaller figure + cat/bear puppet
  - `puppet-13.png` (1102x829) — figure with hands up + small puppet silhouette
  - `puppet-9.png` (1156x836) — two figures with hands
  - `puppet-10.png` (661x816) — single hand puppet figure
- Smaller fragments: puppet-1, 3, 5, 6, 11, 12 (detail sketches, hands, small heads)

Cover concept: white line drawings on dark purple (#2a1438) background, square/rectangular at center

## Source 1 (historical): Parsed PDF notes (`system/public/assets/gigs/disorder-program/photos for fia.pdf`)

Commands used:

```bash
pdftotext -layout 'system/public/assets/gigs/disorder-program/photos for fia.pdf' gigs/dis-order-pamphlet-8.5x11-folded/photos-for-fia.txt
pdfimages -all 'system/public/assets/gigs/disorder-program/photos for fia.pdf' 'system/public/assets/gigs/disorder-program/photos-for-fia-extracted/page'
```

Extracted images directory:

- `system/public/assets/gigs/disorder-program/photos-for-fia-extracted/`
- Files: `page-000.jpg` through `page-018.jpg` (19 extracted images)

## Additional puppet images

4 puppet reference photos in `system/public/assets/gigs/disorder-program/`:
- `puppet image 1.jpeg` through `puppet image 4.jpeg`
- `oracle egg flyer.png`

## Print note

Skirball can print on 11x17 paper if switching to trifold format.
