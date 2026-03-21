# 🎄 Merry Segmented Progress Bar - Feature Summary

## Overview
The merry pipeline now displays a **segmented colored progress bar** at the top of the screen, where each segment represents one piece in the pipeline. This provides visual feedback about:
- Which piece is currently playing
- How much time is left in the current piece
- What pieces are coming next
- The overall structure of the pipeline

## Visual Design

### Layout
```
Screen top → [===green===][=>cyan......][...orange...][....pink....]
              ^piece 1    ^piece 2      ^piece 3      ^piece 4
              completed   current       upcoming      upcoming
```

### Segment Sizing
- Each segment width = `(piece duration / total duration) × screen width`
- Example: `merry tone:3 clock:5 wand:2` (total 10s)
  - tone: 30% of bar width (3s / 10s)
  - clock: 50% of bar width (5s / 10s)
  - wand: 20% of bar width (2s / 10s)

### Color Palette (Cycles)
1. **Bright Green** (100, 255, 100)
2. **Cyan** (100, 200, 255)
3. **Orange** (255, 200, 100)
4. **Pink** (255, 100, 200)
5. **Purple** (200, 100, 255)
6. **Yellow** (255, 255, 100)

After 6 pieces, colors repeat.

## State Visualization

### Completed Pieces
- **Brightness**: 100% (full bright)
- **Effect**: Steady (no animation)
- **Meaning**: This piece has finished playing

### Current Piece (Filled Portion)
- **Brightness**: 100% + 20% pulse
- **Effect**: Subtle pulsing animation
- **Meaning**: This is the active part of the current piece

### Current Piece (Unfilled Portion)  
- **Brightness**: 30% (dim preview)
- **Effect**: Static
- **Meaning**: This is how much time is left in the current piece

### Upcoming Pieces
- **Brightness**: 15% (very dim hint)
- **Effect**: Static
- **Meaning**: These pieces haven't started yet

### Leader Pixel
- **Color**: White pulsing (60-100% brightness)
- **Position**: At the exact current progress point
- **Effect**: Fast beacon-like pulse
- **Meaning**: "You are here" marker

## How It Works

### Real-Time Calculation
1. Track when pipeline starts (`startTime`)
2. Track when current piece starts (`currentPieceStart`)
3. Calculate overall progress: `totalElapsed / totalDuration`
4. Calculate piece progress: `pieceElapsed / pieceDuration`
5. Render segments based on these values

### Frame-by-Frame Updates
- Progress bar regenerated every frame
- No caching (always fresh)
- Smooth animation via `Math.sin` functions
- Uses `Date.now()` for accurate timing

### Segment Rendering Algorithm
```javascript
for each piece in pipeline:
  calculate segment width (proportional to duration)
  determine state (completed/current/upcoming)
  if current piece:
    calculate progress within segment
    fill proportionally with pulsing effect
    add white leader pixel at progress point
  if completed:
    fill completely with steady bright color
  if upcoming:
    fill completely with dim preview color
```

## Benefits

### 1. Time Awareness
You can see at a glance:
- How much time has passed
- How much time remains (visually, not numerically)
- Which piece you're watching

### 2. Pipeline Structure
The bar shows the "shape" of your pipeline:
- Short pieces appear as thin segments
- Long pieces appear as wide segments
- Equal durations create even divisions

### 3. Visual Feedback
- No need to look at console logs
- No need to count seconds mentally
- Immediate understanding of progress

### 4. Aesthetic Appeal
- Colorful and vibrant
- Matches the playful nature of aesthetic.computer
- Distinct from tape's red bar (which is at bottom)

## Comparison to Tape Progress Bar

| Feature | Tape (Bottom) | Merry (Top) |
|---------|--------------|-------------|
| **Position** | Bottom of screen | Top of screen |
| **Color** | Red (single) | Multi-color (6 palette) |
| **Style** | Smooth fill | Segmented blocks |
| **Info** | Single piece progress | Multi-piece pipeline |
| **Preview** | None | Shows upcoming pieces |
| **Use Case** | Recording one piece | Chaining many pieces |

## Technical Implementation

### Files Modified
1. **prompt.mjs** - Added `pieceProgress` tracking
2. **disk.mjs** - Segmented rendering logic (100+ lines)
3. **bios.mjs** - Overlay painting and cache exclusions

### Performance
- 1px tall (minimal screen real estate)
- Regenerated every frame for smooth animation
- No caching to ensure real-time updates
- Efficient per-pixel rendering

### Key Variables
```javascript
system.merry = {
  pipeline: [{piece, duration}, ...],
  currentIndex: 0,           // Which piece we're on
  progress: 0.0-1.0,         // Overall progress
  pieceProgress: 0.0-1.0,    // Current piece progress
  startTime: timestamp,       // Pipeline start
  currentPieceStart: timestamp, // Current piece start
  totalDuration: seconds,     // Sum of all durations
  running: true               // Is pipeline active?
}
```

## Example Scenarios

### Even Distribution
```
merry tone clock wand line  (each 5s = 20s total)
[====][====][====][====]
  25%  25%  25%  25%
```

### Uneven Distribution  
```
merry tone:2 clock:10 wand:1  (total 13s)
[==][===========][=]
 15%     77%      8%
```

### Single Long Piece
```
merry starfield:30
[================================]
              100%
```

### Many Short Pieces
```
merry line:1 oval:1 rect:1 (total 3s)
[====][====][====]
 33%  33%  33%
```

## Usage Tips

### For Presentations
Use different piece durations to control pacing:
```
merry title:5 demo:15 conclusion:5
[==][========][==]
```

### For Testing
Use short equal durations to quickly see all pieces:
```
merry piece1:2 piece2:2 piece3:2 piece4:2
[===][===][===][===]
```

### For Performances
Mix durations to create dynamic visual rhythm:
```
merry intro:3 build:10 peak:5 outro:7
[=][======][==][===]
```

## Future Enhancements (Ideas)

### Possible Additions
- Piece names overlay on segments
- Click segments to skip to that piece
- Hover to see piece name + remaining time
- Scrub bar to manually control position
- Pause/resume with visual indicator
- Loop mode with repeating animation

## Summary

The segmented progress bar transforms merry from a simple sequencer into a visual performance tool. You can now see the structure of your pipeline at a glance, understand where you are in the sequence, and anticipate what's coming next - all with a colorful 1px bar at the top of the screen.

**Key Innovation**: Time-proportional segmented visualization that shows past, present, and future simultaneously.
