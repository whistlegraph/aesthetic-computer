# KidLisp Visualization Tabs

**Status:** Planning  
**Created:** 2025.12.23

## Overview

Add alternative code visualization modes to the KidLisp editor beyond the current Monaco text editor. Each mode provides a different way to understand and interact with KidLisp programs based on their syntax tree structure.

## Tab Modes

### 1. `text` (Current Default)
- **Technology:** Monaco Editor
- **Description:** Traditional syntax-highlighted text editing
- **Features:**
  - Full text editing capabilities
  - Syntax highlighting for KidLisp
  - Error decorations
  - Autocomplete (future)

### 2. `tree` (Hierarchy View)
- **Technology:** Canvas 2D
- **Description:** Visual tree representation of the AST (Abstract Syntax Tree)
- **Features:**
  - Animated node expansion/collapse
  - Color-coded by expression type:
    - Functions → purple (#9370DB)
    - Numbers → orange (#FF9F43)
    - Strings → green (#2ECC71)
    - Symbols → blue (#4ECDC4)
    - Lists → pink (#CD5C9B)
  - Timed playback showing execution flow
  - Hover to see node details
  - Click node to jump to text position
  - Pan/zoom navigation
  - Real-time sync with text editor changes

### 3. `space` (3D Visualization)
- **Technology:** WebGL2
- **Description:** Immersive 3D representation of program structure
- **Features:**
  - Nodes as 3D objects (spheres, boxes based on type)
  - Connections as lines/tubes
  - Depth represents nesting level
  - Animation showing execution timeline
  - Camera orbit controls
  - Time-scrubbing through program execution
  - Particle effects for active expressions
  - Optional VR/XR support (future)

## UI Design

```
┌─────────────────────────────────────────────────┐
│ [text] [tree] [space]          ▶ Play  ⏸ Pause │
├─────────────────────────────────────────────────┤
│                                                 │
│           Active Visualization Area             │
│                                                 │
│                                                 │
└─────────────────────────────────────────────────┘
         ←━━━━━━━━━━━━━━━●━━━━━━━━━━→
              Timeline Scrubber
```

## Technical Architecture

### Shared Components
- **Parser:** Reuse existing KidLisp parser to generate AST
- **Timeline Engine:** Track expression evaluation order and timing
- **Sync Manager:** Keep all views in sync with source code

### File Structure
```
system/public/kidlisp.com/
├── js/
│   ├── visualizations/
│   │   ├── viz-base.js      # Base class for all visualizations
│   │   ├── viz-tree.js      # Canvas 2D tree view
│   │   ├── viz-space.js     # WebGL2 3D view
│   │   └── timeline.js      # Shared timeline/playback
│   └── ast-colors.js        # Consistent color mapping
└── css/
    └── visualizations.css   # Tab styles, timeline UI
```

### AST Node Structure (for visualization)
```javascript
{
  type: 'list' | 'number' | 'string' | 'symbol' | 'function',
  value: any,
  children: Node[],
  position: { line, column, offset },
  timing: { start: ms, end: ms },  // From execution trace
  color: '#RRGGBB',
  depth: number
}
```

## Implementation Phases

### Phase 1: Infrastructure
- [ ] Add tab UI to editor header
- [ ] Create visualization container that swaps views
- [ ] Implement `viz-base.js` with common interface
- [ ] Hook into parser to expose AST

### Phase 2: Tree View (Canvas 2D)
- [ ] Basic tree layout algorithm (Reingold-Tilford or similar)
- [ ] Render nodes and edges
- [ ] Color coding by type
- [ ] Pan/zoom with mouse/touch
- [ ] Click-to-select nodes
- [ ] Sync selection with text editor

### Phase 3: Timeline Playback
- [ ] Instrument interpreter to emit timing events
- [ ] Build timeline data structure
- [ ] Add playback controls (play/pause/scrub)
- [ ] Highlight active nodes during playback
- [ ] Show execution flow animation

### Phase 4: Space View (WebGL2)
- [ ] Basic WebGL2 renderer setup
- [ ] 3D node positioning (force-directed or layered)
- [ ] Camera controls (orbit, pan, zoom)
- [ ] Node meshes and materials
- [ ] Connection lines
- [ ] Integrate with timeline for animation

### Phase 5: Polish
- [ ] Smooth transitions between views
- [ ] Persistent view preference
- [ ] Touch/mobile support
- [ ] Performance optimization for large programs
- [ ] Accessibility considerations

## Color Palette

| Expression Type | Color | Hex |
|----------------|-------|-----|
| Function call | Purple | `#9370DB` |
| Number | Orange | `#FF9F43` |
| String | Green | `#2ECC71` |
| Symbol | Cyan | `#4ECDC4` |
| List | Pink | `#CD5C9B` |
| Special form | Gold | `#F8B500` |
| Comment | Gray | `#888888` |
| Error | Red | `#E74C3C` |

## Example Visualizations

### Tree View Example
For code: `(wipe "red") (ink "yellow") (box 10 10 50)`

```
         ┌─────────┐
         │ program │
         └────┬────┘
    ┌─────────┼─────────┐
    ▼         ▼         ▼
┌──────┐  ┌──────┐  ┌──────┐
│ wipe │  │ ink  │  │ box  │
└──┬───┘  └──┬───┘  └──┬───┘
   ▼         ▼      ┌──┼──┐
┌──────┐  ┌──────┐  ▼  ▼  ▼
│"red" │  │"yel" │  10 10 50
└──────┘  └──────┘
```

### Space View Concept
- Z-axis = nesting depth
- X-axis = sequential position
- Y-axis = expression type grouping
- Animated particles flow through connections during execution

## Open Questions

1. Should tree/space views be read-only or allow direct manipulation (visual programming)?
2. How to handle very large programs without overwhelming the visualization?
3. Should we show all frames of animation or sample key moments?
4. Integration with the existing card deck reference system?

## References

- [Reingold-Tilford Tree Layout](https://en.wikipedia.org/wiki/Reingold%E2%80%93Tilford_algorithm)
- [Three.js](https://threejs.org/) - Potential WebGL helper (or raw WebGL2)
- [D3.js Tree](https://d3js.org/) - Inspiration for tree layouts
- AST Explorer patterns
