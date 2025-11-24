# Fundraiser Poster TODO - Client Revisions

## Content Updates

### ✅ Completed
- [x] Remove Mia from artist list
- [x] Add Clara Balzary to artist list
- [x] Update Venmo handle to @FABIENNEGSTOTTENMAYR

### 🔲 To Do

#### 1. Update Artist Names to Match Checklist
**Current:** Need to verify all 12 artist names match the provided list exactly

**Implementation:**
- Replace artist list in metadata section with:
  1. Jonny Negron
  2. Paz G
  3. Sissön
  4. Jake Sheiner
  5. Ari Salka
  6. Lucy Black
  7. Devendra Banhart - Para Paz
  8. Devendra Banhart - Nil Patin Natanava
  9. Max Rippon
  10. Alia Shawkat
  11. Tuna Bora
  12. Clara Balzary

**Files to modify:** `layout-test.html` - `.artists-list` section

---

#### 2. Add Artwork Details to Each Card
**Requirement:** Each artwork card needs specific details from the checklist

**Implementation Plan:**
- Add details below each artwork number:
  - **Artwork 3 (Sissön):** "Charcoal and Pastel on Cotton Paper"
  - **Artwork 5 (Ari Salka):** 
    - Title: "Angel Found Beneath Flower Amidst Season Change"
    - Medium: "Acrylic, enamel, raw pigment, latex paint, india ink, resin, and oil on wood"
  - **Artwork 6 (Lucy Black):**
    - Title: "Capitalize Hugging Swans"
    - Medium: "Cotton waffle fabric, stuffing, and wire on canvas"
  - **Artwork 7 (Devendra - Para Paz):** "Cropped into piece"
  - **Artwork 8 (Devendra - Nil Patin):**
    - Medium: "Oil on wood"
    - Note: "Cropped into piece"
    - Year: "2021"
  - **Artwork 9 (Max Rippon):** "Oil, acrylic, color pencil, gesso, and image transfer on canvas in artist frame"
  - **Artwork 12 (Clara Balzary):**
    - Title: "Texas-Mexico Border - 2025"
    - Medium: "color print"
    - Size: "11 X 14"

**CSS Changes Needed:**
- Reduce `.work-number` font size to make room for details
- Add `.work-details` styling with smaller font (0.7-0.8cqh)
- Adjust card padding and text layout

**Files to modify:** 
- `layout-test.html` - HTML structure for each artwork card
- CSS - `.work-card`, `.work-details` styling

---

#### 3. Typography Updates
**Requirements from style notes:**
- All text in black (verify current implementation)
- No icons/images except emojis and QR codes
- One clean bold font for artist names (no fun warping) - currently using Arial/Helvetica
- Remove any decorative font styling

**Implementation:**
- Verify all text is black (#000)
- Ensure consistent font-weight for artist names
- Remove any text transforms or decorative effects not specified

**Files to modify:** `layout-test.html` - CSS font styling

---

#### 4. Verify Chloe Pang Question
**Note:** Client asked "chloe pang?" - need clarification

**Action Required:**
- Check if Chloe Pang should be added to artist list
- Verify if this relates to a specific artwork
- Awaiting client confirmation

---

## Implementation Order

1. **First Pass - Content Updates (Easy):**
   - Update artist names in HTML
   - Verify all text is black
   - Confirm typography is clean and consistent

2. **Second Pass - Artwork Details (Medium):**
   - Add HTML structure for artwork details
   - Style the detail text appropriately
   - Test sizing and layout with longer text strings

3. **Third Pass - Final Review:**
   - Check all 12 artworks have correct details
   - Verify responsive sizing still works
   - Test PDF generation with new content
   - Get client approval on Chloe Pang question

---

## Notes

- Keep `fundraiser-checklist.html` as original
- Create new file: `fundraiser-checklist-v2.html` with revisions
- Some artworks don't have details listed - use artist name only for these
- Maintain current responsive design and aspect ratios
- Ensure all changes work in print/PDF output
