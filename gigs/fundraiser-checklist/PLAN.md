# Fundraiser Checklist PDF Plan

## Current State Analysis

### Existing Assets
- **HTML File**: `fundraiserchecklist.html` - Contains artwork entries with images and captions
- **Images Folder**: 12 images (mix of JPG, PNG, GIF formats)
  - image1.png - image12.jpg

### Artwork Inventory (from HTML)
1. **Jonny Negron** - "Free Karma" (2019) - Ink on paper, 11x14 in - `image10.jpg`
2. **Paz G** - "Untitled" (2024) - Stoneware and glaze, 17"H x 17"W x 12"D - `image1.png`
3. **Sisson** - "Blessings Birth Blessings" (2023) - Charcoal and Pastel on Cotton Paper, 20 1/8" x 17 1/8" (with maple wood frame) - `image5.jpg`
4. **Jake Sheiner** - "Wash" (2022) - Acrylic on Canvas, 24x30 in - `image8.png`
5. **Lucy Black** - "Hugging swans" (2025) - Cotton waffle fabric, stuffing and wire on canvas, 8 x 11 in - `image12.jpg`
6. **Alia Shawkat** - "Untitled" (2020) - Oil on paper, 19.5 x 16.5 in Framed - `image4.jpg`
7. **Devendra Banhart** - "Para Paz" (2025) - Oil on Wood in Artist's Frame, 17 x 21 in - `image9.jpg`
8. **Devendra Banhart** - "Blue Thing" (2025) - Oil on Canvas, 20 x 24.5 in - `image3.jpg`
9. **Chloe Pang** - "ABYSS" (2021) - 8.5 x 11 in Print in Frame - `image6.gif`
10. **Max Rippon** - "Don't Seem To Mind The Heavy Metal" (2024) - Oil Paint, Acrylic, Color Pencil, Gesso, and Image transfer on canvas in artist frame, 12 x 15 in - `image2.jpg`
11. **Ari Salka** - "Angel Found Beneath Flower Amidst Season Change" (2025) - Acrylic, Enamel, Raw Pigment, Latex Paint, India Ink, Resin, and Oil on Cradled Wood Panel, 20 x 16 in - `image11.png`
12. **Tuna Bora** - Untitled (2025) - Acrylic on Cradled Panel, 18 x 18 in - `image7.jpg`

### Event Information (New)
- **Event Type**: FUNDRAISER - Art Raffle
- **Purpose**: To help support an undocumented family seeking relocation
- **Participating Artists**: Alina Perkins, Paz G, Devendra Banhart, Ari Salka, Tuna Bora, Sisson, Alia Shawkat, Max Rippon, Mia Weiner
- **DJs**: Jazmin Garcia "Como La Flor" and Ana Calderon
- **Food**: Pine and Crane
- **Bar**: Cash bar
- **Pricing**:
  - Presale: $5 (can go towards a raffle ticket)
  - Day of: $10 / Free Entry with purchase of a raffle ticket
  - Raffle Tickets: $30 each or $150 for 6
- **Venue**: Tropico Beauty, 415 West Palmer Ave, Glendale CA 91204
- **Date/Time**: December 14th, 7-11 pm

## Issues to Address

1. **Missing Raffle Numbers**: Only 3 artworks have raffle numbers assigned (#1, #2, and one incomplete)
2. **Artist Discrepancies**: 
   - Alina Perkins and Mia Weiner are listed as participating artists but don't have artwork entries
   - Jonny Negron, Jake Sheiner, Lucy Black, and Chloe Pang have artwork but aren't in the artist list
3. **Formatting Inconsistencies**: 
   - Some entries have raffle numbers, most don't
   - Inconsistent capitalization and formatting

## Plan for PDF Checklist

### Approach
Build an HTML page (which can be printed/converted to PDF) with:
1. Professional, clean design
2. Event header with all key information
3. Grid/catalog layout of all artworks
4. Each artwork entry includes:
   - Image
   - Artist name
   - Title
   - Year
   - Medium
   - Dimensions
   - Raffle number

### Technical Strategy
1. **Create new HTML file**: `fundraiser-checklist.html`
   - Modern, print-friendly CSS
   - Use CSS Grid or Flexbox for layout
   - Print-optimized styles (@media print)
   - Page breaks where appropriate
2. **Structure**:
   - Header section with event details
   - Artist grid/catalog
   - Footer with pricing/logistics
3. **Convert to PDF**:
   - Option 1: Browser print function (File > Print > Save as PDF)
   - Option 2: Use headless browser tool like Puppeteer
   - Option 3: CSS print media queries for optimal PDF output

### Design Considerations
- **Color scheme**: Professional but warm (matching fundraiser spirit)
- **Typography**: Clean, readable fonts
- **Layout**: 
  - 2-column grid for artworks
  - Full bleed images with captions below
  - Clear hierarchy of information
- **Print optimization**:
  - A4/Letter size pages
  - Proper margins
  - Page breaks to avoid splitting artwork entries
  - High-contrast for readability

### Required Actions
1. ✅ Analyze existing content
2. ⬜ Create new HTML template with event header
3. ⬜ Add CSS for print-friendly layout
4. ⬜ Populate with all 12 artworks and images
5. ⬜ Assign raffle numbers to all entries
6. ⬜ Add footer with pricing and venue information
7. ⬜ Test print preview
8. ⬜ Generate PDF

### File Structure
```
fundraiser checklist/
├── fundraiserchecklist.html (existing, can archive)
├── fundraiser-checklist-2025.html (new, clean version)
├── PLAN.md (this file)
└── images/
    ├── image1.png
    ├── image2.jpg
    ├── ... (all 12 images)
```

## Next Steps
1. Create the new HTML file with proper structure
2. Style it for both screen and print
3. Test PDF conversion
4. Review with organizer for any missing information or corrections
