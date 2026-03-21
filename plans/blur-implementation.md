# Blur Function Implementation Plan

## Overview
Implementing an efficient Gaussian blur function for the aesthetic-computer graphics library, following the pattern established by existing functions like `scroll` and `suck`.

## Research Summary: Efficient Gaussian Blur with Linear Sampling

### Key Optimization Techniques

1. **Separable Gaussian Filter**
   - 2D Gaussian blur can be split into two 1D passes (horizontal + vertical)
   - Reduces complexity from O(N²) to O(2N) where N is the filter radius
   - For a 9-tap filter: 81 samples → 18 samples (9 horizontal + 9 vertical)

2. **Linear Sampling Optimization**
   - Use GPU's bilinear filtering to sample 2 pixels with 1 texture fetch
   - For 9-tap filter: 9 samples → 5 samples per pass
   - Total reduction: 81 samples → 10 samples (5 horizontal + 5 vertical)
   - ~60% performance improvement over discrete sampling

3. **Pascal Triangle Weights**
   - Use binomial coefficients for Gaussian weights
   - Row 6 of Pascal triangle: [1, 6, 15, 20, 15, 6, 1] (sum = 64)
   - Normalized weights: [0.015625, 0.09375, 0.234375, 0.3125, 0.234375, 0.09375, 0.015625]

### Linear Sampling Mathematics

For combining two adjacent samples with weights w1 and w2:
- Combined weight: `w_combined = w1 + w2`
- Offset from center: `offset = w2 / (w1 + w2)`

Example for 9-tap filter with linear sampling:
```
Offsets: [0.0, 1.3846153846, 3.2307692308]
Weights: [0.2270270270, 0.3162162162, 0.0702702703]
```

## Implementation Strategy

### Function Signature
```javascript
function blur(strength = 1, quality = "medium")
```

### Parameters
- `strength`: Blur radius (1-10+), maps to kernel size
- `quality`: "fast" | "medium" | "high" - affects algorithm choice

### Quality Levels
1. **Fast**: Simple box blur approximation
2. **Medium**: Separable Gaussian with linear sampling (5-tap)
3. **High**: Separable Gaussian with linear sampling (9-tap or larger)

### Algorithm Flow

1. **Input Validation**
   - Check strength bounds (0-10)
   - Early return for strength ≤ 0.1

2. **Kernel Generation**
   - Calculate appropriate kernel size based on strength
   - Generate Gaussian weights using Pascal triangle or formula
   - Optimize for linear sampling (combine adjacent weights)

3. **Two-Pass Blur**
   - Pass 1: Horizontal blur into temporary buffer
   - Pass 2: Vertical blur from temp buffer back to main buffer

4. **Boundary Handling**
   - Support for active mask regions
   - Proper edge clamping/wrapping
   - Respect pan translation

### Memory Management
- Use temporary buffer for intermediate results
- Reuse buffers when possible
- Handle detached buffer edge cases (like other functions)

### Performance Considerations
- Early exit for very small blur amounts
- Accumulator pattern for sub-pixel blur values
- Optional SIMD optimization paths
- Progressive blur for large radii

## Code Structure Pattern

Following the existing pattern from `suck` and `scroll`:

```javascript
// Accumulator for sub-pixel blur values
let blurAccumulator = 0.0;

function blur(strength = 1, quality = "medium") {
  if (strength <= 0.1) return; // No blur needed
  
  // Accumulate strength like other effects
  blurAccumulator += strength;
  const threshold = 0.5;
  if (Math.abs(blurAccumulator) < threshold) return;
  
  // Determine working area (mask or full screen)
  let minX = 0, minY = 0, maxX = width, maxY = height;
  if (activeMask) {
    // Apply mask bounds with pan translation
  }
  
  // Safety check for detached buffer
  if (pixels.buffer && pixels.buffer.detached) {
    console.warn('🚨 Pixels buffer detached in blur, recreating');
    pixels = new Uint8ClampedArray(width * height * 4);
    pixels.fill(0);
  }
  
  // Implement separable Gaussian blur with linear sampling
  applyHorizontalBlur(/* parameters */);
  applyVerticalBlur(/* parameters */);
  
  // Reset accumulator
  blurAccumulator = 0.0;
}
```

## Export Pattern
Add to exports section:
```javascript
export {
  // ... existing exports
  blur,
  // ... rest of exports
};
```

## Testing Strategy
1. Visual tests with different strength values (1, 2, 5, 10)
2. Performance benchmarks vs naive implementation
3. Edge case testing (masks, boundaries, zero/negative values)
4. Integration with existing effects (scroll, suck, etc.)

## Implementation Notes
- Use separable approach for CPU implementation (no GPU shaders available)
- Implement linear sampling concept using manual interpolation
- Consider progressive blur for very large radii (multiple passes)
- Maintain consistency with existing function patterns and error handling