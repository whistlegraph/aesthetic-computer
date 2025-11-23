# Fundraiser Poster Design

Art Fundraiser Party poster for December 14, 2025 event at Tropico Beauty.

## Current Status

Working on modular layout design in `layout-test.html` with responsive components using container queries.

### Completed
- ✅ Modular layout system with fixed aspect ratios
- ✅ Header row with entry/food blocks and main title
- ✅ Individually colored PARTY letters with shadows
- ✅ Decorative rotated emojis (💵 🍸) on food block
- ✅ Metadata row with artists and raffle information
- ✅ 3×4 artwork grid with numbered cards
- ✅ Footer with Venmo instructions and QR code
- ✅ White space adjustments and padding
- ✅ Responsive typography using container query units (cqh)

## How to Run

From this directory:

```bash
python3 -m http.server 8000
```

Then open: `http://localhost:8000/layout-test.html`

## File Structure

- `layout-test.html` - Main working file with modular components
- `images/` - Artwork images for raffle (12 pieces)
- `venmo_qr.png` - QR code for Venmo payments
- `fundraiser-poster.html` - Earlier iteration
- `fundraiser-poster-fixed.html` - Fixed layout version
- `fundraiserchecklist.html` - Original checklist version
- `PLAN.md` - Design planning document

## Design Details

### Layout Modules
1. **Header Row** (16:3) - Entry info, title, food/drinks
2. **Metadata Row** (4:1) - Artists list + raffle pricing
3. **Artworks Grid** (3:5) - 3×4 grid of raffle items
4. **Spacer** (16:0.3) - White space before footer
5. **Footer** (16:2) - Venmo instructions and pricing

### Key Features
- Container query-based responsive sizing
- All text scales with `cqh` units
- Fixed aspect ratios maintain proportions
- Black borders on right side only (no top/left)
- Red accent color (#cc0000) for RAFFLE text
- Inverted QR codes (white on black)
- Decorative emojis positioned absolutely

## Next Steps

- [ ] Finalize all artwork images
- [ ] Review final spacing and alignment
- [ ] Export to fixed 8×10 print layout
- [ ] Generate print-ready PDF
