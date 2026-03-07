# Dis-Order Pamphlet Image Placement Notes

## Source 1: Pamphlet copy placeholders (`pamphlet-copy.txt`)

Left inside flap image prompts:

1. `I. begin with fire`
   - Note: `image of two taper candles with flames--Ari has a great image of these`
2. `II. wicked child bacchanal`
   - Note: `image of donkey--from Ari's work and/or puppet drawings`
3. `III. total darkness`
   - Note: `image of baby/egg--can pull from Ari's work and/or puppet drawings`
4. `IV. lovers' pastoral`
   - Note: `image of two big puppet heads from drawings and/or of flowers (Ari has a flower image)`
5. `V. death of the first born`
   - Note: `image of daughter--either from Ari's paintings or from the puppet drawings: in the puppet drawings, the daughter is the one with the big hands and the small silhouette of a person directly facing her`

## Source 2: Parsed PDF notes (`system/public/assets/gigs/disorder-program/photos for fia.pdf`)

Commands used:

```bash
pdftotext -layout 'system/public/assets/gigs/disorder-program/photos for fia.pdf' gigs/dis-order-pamphlet-8.5x11-folded/photos-for-fia.txt
pdfimages -all 'system/public/assets/gigs/disorder-program/photos for fia.pdf' 'system/public/assets/gigs/disorder-program/photos-for-fia-extracted/page'
```

Extracted images directory:

- `system/public/assets/gigs/disorder-program/photos-for-fia-extracted/`
- Files: `page-000.jpg` through `page-018.jpg` (19 extracted images)

Parsed page notes from the PDF text layer:

- Page 1: `Could work as a person or a candle`
- Page 2: `Could work more as abstract for an area of the program for text overlay` and `Could work as parsley a tree`
- Page 3: `cups`
- Page 4: `Puppet head option`
- Page 5: `Candles`
- Page 6: `Abstract`
- Page 9: `Om thinking this is great for the cover its basically disorder overall (with background deleted as an idea)`
- Page 12: `Egg`
- Page 13: `Daughter`
- Page 14: `Donkey 1`
- Page 15: `Donkey 1`
- Page 16: `Egg`
- Page 17: `Candles`
- Page 18: `Puppet option`

## Provisional mapping: copy slot -> candidate image pages

- `Front cover`: PDF page 9 (`cover` note)
- `I. begin with fire`: PDF pages 5 or 17 (`Candles`), optionally page 1 (`person or a candle`)
- `II. wicked child bacchanal`: PDF pages 14 or 15 (`Donkey 1`)
- `III. total darkness`: PDF pages 12 or 16 (`Egg`)
- `IV. lovers' pastoral`: PDF page 4 or 18 (`Puppet head option` / `Puppet option`), or flower image from Ari's other set
- `V. death of the first born`: PDF page 13 (`Daughter`)

Secondary/background candidates:

- Abstract texture or text-overlay background: PDF pages 2 and 6
- Additional motif options: PDF page 3 (`cups`) and page 2 (`parsley a tree`)
