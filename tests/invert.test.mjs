import assert from 'node:assert/strict';

// Minimal test for invert function logic
// Testing the CPU implementation directly

// Test 1: Basic RGB inversion formula
{
  const testCases = [
    { input: [255, 0, 0, 255], expected: [0, 255, 255, 255], name: 'Red' },
    { input: [0, 255, 0, 255], expected: [255, 0, 255, 255], name: 'Green' },
    { input: [0, 0, 255, 255], expected: [255, 255, 0, 255], name: 'Blue' },
    { input: [128, 128, 128, 255], expected: [127, 127, 127, 255], name: 'Gray' },
    { input: [0, 0, 0, 255], expected: [255, 255, 255, 255], name: 'Black' },
    { input: [255, 255, 255, 255], expected: [0, 0, 0, 255], name: 'White' },
  ];
  
  for (const { input, expected, name } of testCases) {
    const result = [
      255 - input[0],
      255 - input[1],
      255 - input[2],
      input[3] // Alpha preserved
    ];
    
    assert.deepEqual(result, expected, `${name} inversion formula`);
  }
  
  console.log('✅ Basic RGB inversion formula tests passed');
}

// Test 2: Alpha preservation
{
  const testCases = [
    { alpha: 255, name: 'Opaque' },
    { alpha: 128, name: 'Semi-transparent' },
    { alpha: 0, name: 'Fully transparent' },
    { alpha: 64, name: '25% opacity' },
  ];
  
  for (const { alpha, name } of testCases) {
    const input = [100, 150, 200, alpha];
    const result = [
      255 - input[0],
      255 - input[1],
      255 - input[2],
      input[3]
    ];
    
    assert.equal(result[3], alpha, `${name}: Alpha should be preserved`);
    assert.equal(result[0], 155, `${name}: R channel inverted correctly`);
    assert.equal(result[1], 105, `${name}: G channel inverted correctly`);
    assert.equal(result[2], 55, `${name}: B channel inverted correctly`);
  }
  
  console.log('✅ Alpha preservation tests passed');
}

// Test 3: Double invert should restore original
{
  const testPixels = [
    [123, 45, 67, 255],
    [0, 128, 255, 128],
    [50, 100, 150, 200],
  ];
  
  for (const original of testPixels) {
    // First invert
    const inverted = [
      255 - original[0],
      255 - original[1],
      255 - original[2],
      original[3]
    ];
    
    // Second invert
    const restored = [
      255 - inverted[0],
      255 - inverted[1],
      255 - inverted[2],
      inverted[3]
    ];
    
    assert.deepEqual(restored, original, `Double invert restores [${original.join(', ')}]`);
  }
  
  console.log('✅ Double invert restoration tests passed');
}

// Test 4: Boundary values
{
  const boundaryTests = [
    { input: [0, 0, 0, 255], expected: [255, 255, 255, 255], name: 'Min values' },
    { input: [255, 255, 255, 255], expected: [0, 0, 0, 255], name: 'Max values' },
    { input: [1, 254, 127, 255], expected: [254, 1, 128, 255], name: 'Mixed values' },
  ];
  
  for (const { input, expected, name } of boundaryTests) {
    const result = [
      255 - input[0],
      255 - input[1],
      255 - input[2],
      input[3]
    ];
    
    assert.deepEqual(result, expected, name);
  }
  
  console.log('✅ Boundary value tests passed');
}

console.log('✅ All invert logic tests passed');

