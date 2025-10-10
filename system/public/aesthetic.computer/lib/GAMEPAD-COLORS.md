# Gamepad Color Coding Reference

Color-coded button and axis mappings for in-game UI and notation.

## 8BitDo Micro Color Scheme

### Face Buttons
- **Button 0 (A)** - Right position
  - Active: `lime` - Bright green action button
  - Inactive: `darkgreen` - Muted green
  
- **Button 1 (B)** - Bottom position
  - Active: `red` - Bright red cancel button
  - Inactive: `darkred` - Muted red
  
- **Button 3 (X)** - Top position
  - Active: `yellow` - Bright yellow
  - Inactive: `darkgoldenrod` - Muted gold
  
- **Button 4 (Y)** - Left position
  - Active: `orangered` - Bright orange-red
  - Inactive: `darkred` - Muted red

### Shoulder Buttons
- **Button 6 (L)** - Left shoulder
  - Active: `orange` - Bright orange
  - Inactive: `darkorange` - Muted orange
  
- **Button 7 (R)** - Right shoulder
  - Active: `orange` - Bright orange
  - Inactive: `darkorange` - Muted orange

### Trigger Buttons
- **Button 8 (L2)** - Left trigger
  - Active: `coral` - Soft coral orange
  - Inactive: `darksalmon` - Muted salmon
  
- **Button 9 (R2)** - Right trigger
  - Active: `coral` - Soft coral orange
  - Inactive: `darksalmon` - Muted salmon

### Center Buttons
- **Button 10 (Select)** - Left center
  - Active: `white` - Bright white
  - Inactive: `gray` - Neutral gray
  
- **Button 11 (Start)** - Right center
  - Active: `white` - Bright white
  - Inactive: `gray` - Neutral gray
  
- **Button 12 (Home)** - Bottom center ❤️
  - Active: `deeppink` - Hot pink heart
  - Inactive: `gray` - Neutral gray

### D-Pad (Axes)
- **Axis 0/1 (D-Pad)** - Directional input
  - Active: `lime` - Bright green
  - Inactive: `gray` - Neutral gray

### Unmapped Buttons
Unique colors for debugging:
- **Button 2**: `cyan` / `darkcyan`
- **Button 5**: `magenta` / `darkmagenta`
- **Button 13**: `violet` / `darkviolet`
- **Button 14**: `springgreen` / `darkseagreen`
- **Button 15**: `gold` / `darkgoldenrod`
- **Button 16**: `hotpink` / `deeppink`

## Standard Gamepad Color Scheme

### Face Buttons
- **Button 0 (A/X)**: `lime` / `darkgreen`
- **Button 1 (B/○)**: `red` / `darkred`
- **Button 2 (X/□)**: `dodgerblue` / `darkblue`
- **Button 3 (Y/△)**: `yellow` / `darkgoldenrod`

### Shoulders & Triggers
- **Buttons 4-5 (LB/RB)**: `orange` / `darkorange`
- **Buttons 6-7 (LT/RT)**: `coral` / `darksalmon`

### Center & Sticks
- **Buttons 8-9 (Select/Start)**: `white` / `gray`
- **Buttons 10-11 (LS/RS)**: `cyan` / `darkcyan`
- **Button 16 (Home)**: `white` / `gray`

### D-Pad & Analog Sticks
- **Buttons 12-15 (D-Pad)**: `lime` / `darkgreen`
- **Axes 0-3 (Sticks)**: `cyan` / `darkcyan`

## Usage Examples

### In Game UI
```javascript
import { getButtonColors } from "../lib/gamepad-mappings.mjs";

const colors = getButtonColors(gamepadId, buttonIndex);
ink(isPressed ? colors.active : colors.inactive).circle(x, y, size);
```

### Button Prompts
Display consistent button colors across all games:
- "Press [A]" in lime for confirm
- "Press [B]" in red for cancel
- "Press [X]" in yellow for alternate action
- "Press [Y]" in orangered for special action

### Notation Standard
Use color names in documentation and chat:
- `lime A` - Confirm/Jump
- `red B` - Cancel/Back
- `yellow X` - Attack/Interact
- `orangered Y` - Special/Menu
- `orange L/R` - Shoulders
- `coral L2/R2` - Triggers
- `white Select/Start` - Menu/Pause
- `deeppink Home` - System menu

## Color Philosophy

**Active colors** are bright and saturated - clearly visible when pressed
**Inactive colors** are darker versions - subtle but distinguishable

This creates clear visual feedback while maintaining aesthetic consistency across all controllers and games.
