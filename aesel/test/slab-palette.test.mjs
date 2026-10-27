import './own-ink.mjs';
import assert from 'node:assert/strict';
import test from 'node:test';
import {mkdtempSync, writeFileSync, rmSync} from 'node:fs';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import {SlabPalette} from '../src/slab-palette.mjs';
import {renderFrame, cleanText, headerAction} from '../src/render.mjs';

const state = () => ({profile:{name:'pro'}, account:'@jeffrey', input:'draft',
  entries:[{kind:'assistant',text:'Last reply.'}]});

test('one empty line separates the reply and the composer', () => {
  const s = state(), lines = cleanText(renderFrame(s, 80, 16, true)).split('\n');
  const reply = lines.findIndex(line => line.includes('Last reply.'));
  const input = lines.findIndex(line => line.includes('draft'));
  assert.equal(input, reply + 2);
  assert.equal(lines[input - 1].trim(), '');
  assert.equal(s.cursorCell.row, input + 1);
});

test('the handle is always drawn by Aesel, never concealed for an overlay', () => {
  const s = state();
  const frame = renderFrame(s, 80, 16, true);
  assert.doesNotMatch(frame, /\x1b\[8m/);
  assert.match(cleanText(frame), /@jeffrey/);
  assert.equal(headerAction(s,80,16,2,16),'profile');
  assert.equal(headerAction(s,80,16,9,16),'profile');
  assert.equal(headerAction(s,80,16,10,16),'');
});

test('page colors arrive from Slab and ignore other sessions', () => {
  const directory = mkdtempSync(join(tmpdir(), 'aesel-palette-'));
  const palette = new SlabPalette({directory, sessionId:'session'});
  try {
    writeFileSync(palette.file,JSON.stringify({schema:1,sessionId:'session',background:[23,0,27]}));
    palette.poll(); assert.deepEqual(palette.background,[23,0,27]);
    writeFileSync(palette.file,JSON.stringify({schema:1,sessionId:'other',background:[255,255,255]}));
    palette.poll(); assert.deepEqual(palette.background,[23,0,27]);
  } finally { palette.close(); rmSync(directory,{recursive:true,force:true}); }
});
