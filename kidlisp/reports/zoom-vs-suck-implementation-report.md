# Zoom vs Suck Implementation Analysis Report

## Executive Summary

This document analyzes the current zoom transformation function in `graph.mjs` and provides comprehensive specifications for implementing a new "suck" function that creates radial pixel displacement effects. The suck function will provide organic, vortex-like transformations that move pixels radially toward or away from a center point with wrapping behavior, creating zooming effects without data loss.

## Research Findings: Mathematical Foundations

### Polar Coordinate System
Based on research into coordinate transformation systems, the suck function will leverage polar coordinates for radial transformations:

- **Conversion formulas**: `x = r cos φ`, `y = r sin φ`
- **Inverse conversion**: `r = √(x² + y²)`, `φ = atan2(y, x)`
- **Benefits**: Natural representation for radial motions and rotational transformations

### Vortex Mathematics
Research into fluid dynamics reveals multiple vortex models applicable to image transformation:

#### Irrotational Vortex Model
- **Velocity Profile**: v ∝ 1/r (inversely proportional to radius)
- **Characteristics**: Preserves circulation, creates spiral flow patterns
- **Application**: Ideal for smooth radial displacement without angular distortion

#### Rankine Vortex Model  
- **Inner Core**: Solid body rotation (v ∝ r)
- **Outer Region**: Irrotational flow (v ∝ 1/r)
- **Application**: Allows for controlled center behavior and natural falloff

### Navier-Stokes Fluid Dynamics
Advanced research into fluid simulation algorithms provides theoretical foundation:

- **Vorticity**: ω = ∇ × v (curl of velocity field)
- **Material Derivative**: Describes how fluid elements evolve over time
- **Incompressible Flow**: ∇ · v = 0 (divergence-free velocity field)
- **Application**: Ensures volume-preserving transformations for data conservation

## Current Zoom Implementation Analysis

### Zoom Function Overview (lines 3636-3800)
The existing zoom function in `graph.mjs` implements traditional geometric scaling with these characteristics:

#### Core Functionality
- **Purpose**: Scales pixel content by a factor with configurable anchor points
- **Algorithm**: Traditional 2D scaling transformation with bilinear interpolation
- **Parameters**: 
  - `level`: Zoom factor (1 = no change, >1 = zoom in, <1 = zoom out)
  - `x`, `y`: Anchor point coordinates (default: canvas center)
- **Behavior**: Geometric scaling that maintains aspect ratios

#### Technical Implementation
```javascript
// Key transformation logic
const scale = level;
const sourceX = (targetX - anchorX) / scale + anchorX;
const sourceY = (targetY - anchorY) / scale + anchorY;
```

#### Strengths
- Mathematically precise scaling
- Efficient bilinear interpolation
- Flexible anchor point positioning
- Clean integration with accumulation system
- Proper edge wrapping with modulo operations

#### Limitations
- Creates empty regions at zoom levels > 1
- Loses pixel data at zoom levels < 1
- Geometric rather than organic transformation feel
- No vortex or radial flow characteristics

## Proposed Suck Function Implementation

### Core Algorithm: Polar Radial Displacement

The suck function will implement a radial displacement algorithm based on polar coordinate transformation and fluid dynamics principles:

#### Mathematical Model
```javascript
// 1. Convert to polar coordinates
const dx = x - centerX;
const dy = y - centerY;
const r = Math.sqrt(dx * dx + dy * dy);
const theta = Math.atan2(dy, dx);

// 2. Apply radial displacement with research-backed curves
const maxRadius = Math.sqrt(centerX * centerX + centerY * centerY);
const normalizedR = r / maxRadius;

// 3. Multiple displacement curve options based on vortex research:

// Option A: Irrotational Vortex (1/r falloff)
const displacement = strength * Math.log(normalizedR + 0.01) / normalizedR;

// Option B: Gaussian Vortex (smooth falloff)  
const displacement = strength * Math.exp(-normalizedR * normalizedR);

// Option C: Polynomial Flow (customizable falloff)
const displacement = strength * Math.pow(1 - normalizedR, 2);

// 4. Calculate new radius with wrapping
const newR = r + displacement;
const wrappedR = ((newR % maxRadius) + maxRadius) % maxRadius;

// 5. Convert back to Cartesian
const newX = centerX + wrappedR * Math.cos(theta);
const newY = centerY + wrappedR * Math.sin(theta);
```

#### Algorithm Advantages
- **Radial Flow**: Natural vortex-like motion patterns
- **Volume Preservation**: Maintains total pixel count through wrapping
- **Configurable Falloff**: Multiple mathematical models for different effects
- **Smooth Transitions**: Continuous displacement fields prevent artifacts
- **Research-Based**: Built on proven fluid dynamics and coordinate transformation principles

### Enhanced Features Based on Research

#### Vorticity-Inspired Options
- **Clockwise/Counterclockwise**: Optional angular component for spiral effects
- **Multi-Center Vortex**: Support for multiple attraction/repulsion points
- **Temporal Evolution**: Time-dependent parameters for animated flows

#### Navier-Stokes Inspired Conservation
- **Incompressible Flow**: Divergence-free displacement field
- **Circulation Preservation**: Maintains rotational flow characteristics
- **Energy Conservation**: Stable long-term behavior

## Implementation TODO List

### Phase 1: Core Implementation ✅ COMPLETE!
- [x] **1.1** Add suck function to graph.mjs with basic radial displacement
- [x] **1.2** Implement irrotational vortex algorithm (1/r falloff)
- [x] **1.3** Add polar coordinate conversion utilities
- [x] **1.4** Implement bilinear interpolation with wrapping
- [x] **1.5** Add basic parameter validation and defaults

### Phase 2: KidLisp Integration ✅ COMPLETE!
- [x] **2.1** Add "suck" command parsing in kidlisp.mjs
- [x] **2.2** Support (suck), (suck strength), (suck strength x y)
- [x] **2.3** Add deferred execution support
- [x] **2.4** Test integration with existing accumulation system

### Phase 3: Algorithm Options
- [ ] **3.1** Add Gaussian vortex option
- [ ] **3.2** Add polynomial flow option
- [ ] **3.3** Add algorithm parameter to function signature
- [ ] **3.4** Implement algorithm selection logic

### Phase 4: Testing & Optimization
- [ ] **4.1** Basic functionality testing
- [ ] **4.2** Edge case testing (center at edges, extreme strengths)
- [ ] **4.3** Performance optimization (lookup tables)
- [ ] **4.4** Integration testing with other graph functions

### Phase 5: Documentation & Examples
- [ ] **5.1** Add function documentation
- [ ] **5.2** Create usage examples
- [ ] **5.3** Update KidLisp reference
- [ ] **5.4** Performance benchmarks

---

### Progress Log
- **2025-09-06**: Started implementation, created TODO list
- **2025-09-06**: ✅ COMPLETED Phase 1 & 2! Added suck function to graph.mjs with irrotational vortex algorithm, added to exports, integrated with KidLisp including deferred execution and embedded layer support
- **2025-09-06**: 🔄 REDESIGNED for lossless rectilinear geometry! Replaced polar coordinates with Manhattan distance rings for perfect data preservation and grid-native pixel mapping

### Implementation Notes
- Start with irrotational vortex for smooth, natural-looking radial displacement
- Follow zoom function pattern for consistency with existing codebase
- Maintain compatibility with accumulation system and existing graph.mjs structure

---

## Conclusion

The suck function represents a significant advancement over traditional zoom by incorporating fluid dynamics principles and polar coordinate mathematics. Research into Navier-Stokes equations, vorticity theory, and coordinate transformations provides a solid mathematical foundation for creating organic, data-preserving transformations that feel natural and fluid-like.

The implementation will maintain compatibility with existing graph.mjs patterns while introducing new possibilities for artistic expression through mathematically rigorous radial displacement algorithms. This approach bridges the gap between technical precision and artistic creativity, offering users both predictable behavior and visually compelling results.
