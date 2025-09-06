# 🌪️ Suck Function - Complete Technical Documentation

*Advanced radial pixel transformation with fluid dynamics*

## Overview

The `suck` function represents a major advancement in KidLisp's transformation capabilities, implementing fluid dynamics principles for organic pixel displacement. It provides smooth, physics-based radial transformations that create vortex-like effects.

## Mathematical Foundation

### Core Algorithm
Based on polar coordinate transformation and irrotational vortex mathematics:
- **Radial displacement**: Pixels move along radial lines from center point
- **Euclidean distance**: Smooth circular patterns rather than rectilinear Manhattan distance
- **Bilinear interpolation**: Anti-aliasing for sub-pixel precision
- **Wrapping behavior**: Seamless edge wrapping for continuous loops
- **Accumulation system**: Continuous evolution through threshold-based execution

### Physics Implementation
The function implements irrotational vortex flow:
```
displacement = strength × (distance / maxDistance)
newRadius = currentRadius + displacement
```

This creates natural, fluid-like motion that feels organic and responsive.

## Usage Patterns

### Basic Syntax
```lisp
(suck strength)                ; Radial effect from screen center
(suck strength centerX centerY) ; Radial effect from custom point
```

### Parameter Details
- **strength**: Positive = inward motion, negative = outward motion
- **centerX, centerY**: Optional center point (defaults to screen center)

### Example Effects

#### Static Effects
```lisp
(suck 1)           ; Strong inward vortex
(suck -0.5)        ; Gentle outward flow
(suck 0.1)         ; Subtle breathing effect
```

#### Animated Sequences
```lisp
; Pulsing vortex
(s1 (suck 0.5))    ; Gentle pulse every second
(s0.1 (suck 0.1))  ; Rapid micro-movements

; Complex timing patterns
["2s..." 1 0 -1]   ; Multi-phase timing with varying strength
```

#### Layered Compositions
```lisp
; Combined with other transformations
(blur 1)           ; Soften edges first
(suck 0.2)         ; Add subtle vortex
(contrast 1.2)     ; Enhance final result
```

#### Masked Applications
```lisp
; Selective area effects
(mask 64 64 128 128)  ; Define rectangular region
(suck 2)              ; Strong effect only in masked area
(unmask)              ; Remove mask
```

## Technical Implementation

### Performance Optimizations
- **Accumulation thresholds**: Prevents excessive computation
- **Bilinear sampling**: Smooth anti-aliased results
- **Deferred execution**: Compatible with embedded layer system
- **Memory efficiency**: In-place pixel manipulation

### Integration Features
- **Embedded layer support**: Works with `embed` command
- **Mask compatibility**: Respects drawing masks
- **Timing integration**: Works with `s()` timing expressions
- **API consistency**: Follows KidLisp function patterns

## Advanced Use Cases

### Fluid Simulations
```lisp
; Simulated fluid dynamics
(def vortex_strength (sin (* frame 0.1)))
(suck vortex_strength)
```

### Interactive Effects
```lisp
; Mouse-driven vortex
(suck 0.5 mouse_x mouse_y)
```

### Complex Animations
```lisp
; Multi-center vortex system
(repeat 3 i
  (suck 0.3 (* i 100) (* i 80)))
```

## Implementation History

### Development Timeline
- **2025-09-06**: ✅ Core implementation with polar coordinate mathematics
- **2025-09-06**: ✅ Bilinear interpolation for anti-aliasing
- **2025-09-06**: ✅ Edge wrapping and accumulation system
- **2025-09-06**: ✅ KidLisp integration and deferred execution
- **2025-09-06**: ✅ Embedded layer compatibility

### Design Decisions
1. **Euclidean vs Manhattan distance**: Chose Euclidean for smooth circular patterns
2. **Bilinear vs nearest neighbor**: Chose bilinear for quality anti-aliasing
3. **Accumulation system**: Enables continuous evolution and performance control
4. **Polar coordinates**: Natural fit for radial transformations

## Comparison with Other Transforms

| Transform | Type | Complexity | Physics | Quality |
|-----------|------|------------|---------|---------|
| `scroll` | Linear | Simple | Basic | High |
| `zoom` | Scale | Medium | Geometric | High |
| `spin` | Rotation | Medium | Angular | High |
| `suck` | **Radial** | **Complex** | **Fluid** | **Highest** |

The suck function represents the most mathematically sophisticated transformation in KidLisp, incorporating real physics principles for natural, organic motion.

---

*For usage examples and integration patterns, see the main KidLisp documentation and the zoom-vs-suck implementation report.*
