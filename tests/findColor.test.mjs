import assert from 'node:assert/strict';

// Minimal ImageData polyfill so graph.mjs can initialize in Node.
if (typeof globalThis.ImageData === 'undefined') {
  globalThis.ImageData = class ImageData {
    constructor(width, height) {
      this.width = width;
      this.height = height;
      this.data = new Uint8ClampedArray(width * height * 4);
    }
  };
}

const graph = await import('../system/public/aesthetic.computer/lib/graph.mjs');
const { findColor } = graph;

function assertColor(args, expected, message) {
  const actual = findColor(...args);
  assert.deepEqual(actual, expected, message);
}

assertColor(['white'], [255, 255, 255, 255], 'white defaults to opaque');
assertColor(['white', 32], [255, 255, 255, 32], 'white with numeric alpha');
assertColor(["'white"], [255, 255, 255, 255], "KidLisp single-quote prefix");
assertColor([' "gray" '], [128, 128, 128, 255], 'Quoted gray string trims correctly');
assertColor(['`black`'], [0, 0, 0, 255], 'Backtick wrapped color resolves');
assertColor(['|gray|'], [128, 128, 128, 255], 'Pipe wrapped color resolves');
assertColor(['Gray', 96], [128, 128, 128, 96], 'Mixed-case names normalize');
assertColor([[12, 34, 56, 78]], [12, 34, 56, 78], 'Direct RGBA array passes through');
assertColor([[12, 34, 56], 200], [12, 34, 56, 200], 'RGB array with explicit alpha extends correctly');
assertColor([[12, 34, 56, 90], 0.5], [12, 34, 56, 128], 'Fractional alpha overrides array alpha via computeAlpha');

console.log('✅ findColor normalization smoke tests passed');
