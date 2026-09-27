import test from 'node:test';
import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import {pixelGlyph} from '../lib/oskiewar-host.mjs';
test('pixel text advances match native MatrixChunky8, font_1 and Unicode cells', () => {
const header=readFileSync(new URL('../src/font-matrix-chunky8.h', import.meta.url),'utf8');
  const widths=[...header.matchAll(/^    \{\d+, \d+, -?\d+, -?\d+, (\d+),/gm)].map(x=>+x[1]);
  assert.equal(widths.length,95);
  for(let i=0;i<95;i++) assert.equal(pixelGlyph(String.fromCharCode(i+32),48,.5).advance,widths[i]*3);
  assert.deepEqual(pixelGlyph('x',24,.5),{font:'font_1',zoom:1,advance:6,height:10});
  assert.deepEqual(pixelGlyph('é',32,.5),{font:'unifont',zoom:1,advance:8,height:16});
});
