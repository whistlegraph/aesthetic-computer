# Corner HUD Prompt Label Dragging Issue

## Problem

When a user clicks and drags the corner HUD prompt label, it is supposed to move to the left, revealing a "share" section. However, this functionality is currently broken in several ways:

1.  **Width Calculation:** The buffer size of the label does not expand correctly in width, causing the "share" section to be cut off on the right.
2.  **Inconsistent Behavior:** The drag-to-reveal functionality does not work consistently across all types of prompt corner HUD labels. For example, it fails for KidLisp prompt corner labels and nopaint brushes, while it works for normal pieces like 'starfield'.

## Analysis

The root of the issue lies in the `disk.mjs` file, specifically within the code responsible for drawing the corner HUD prompt label. The current implementation has the following problems:

-   **Incorrect Width Calculation:** The width of the label is not being recalculated correctly when the user scrubs (drags) the label. The `currentHUDScrub` variable, which should represent the amount of drag, is not being properly factored into the width of the label's bounding box.
-   **Inconsistent Application:** The logic for handling the drag-to-reveal feature is not being applied uniformly to all types of HUD labels. This is likely due to different code paths for rendering different types of labels (e.g., normal pieces vs. KidLisp pieces).

## Proposed Solution

To fix this issue, the following changes will be made to `disk.mjs`:

1.  **Fix Width Calculation:**
    -   Expand the HUD label painting buffer with a dedicated left padding region sized to the "share" word so the reveal surface is always present.
    -   Grow the buffer width by the current scrub distance so the main label text never clips on the right as it slides.
    -   Offset the rendered bitmap by the left padding so the prompt text keeps its original on-screen alignment while still exposing the reveal gutter.

2.  **Ensure Consistent Behavior:**
    -   Store the non-scrubbed base width separately from the dynamic buffer width so drag hitboxes, animations, and manual `hud.label` calls all use the same measurements.
    -   Subtract the left padding when exporting the label bitmap so every mode (prompt, KidLisp, nopaint) renders with the same origin.

## Implementation Steps

1.  **Modify `disk.mjs`:**
    -   Expand the label buffer by adding a configurable left pad equal to the measured "share" width, and enlarge it further by the active scrub distance.
    -   Shift the prompt and share text drawing positions by the new padding, and subtract the padding when positioning the bitmap on screen.
    -   Update drag hitbox measurements, animation bookkeeping, and the `hud.label` helper to use the shared base width so every code path stays in sync.

2.  **Test the Changes:**
    -   Verify that the drag-to-reveal functionality works correctly for all types of corner HUD prompt labels, including normal pieces, KidLisp pieces, and nopaint brushes.
    -   Confirm that the "share" section is no longer cut off and that the label's width expands as expected.
