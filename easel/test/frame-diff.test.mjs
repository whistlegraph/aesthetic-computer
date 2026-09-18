import {test} from 'node:test';
import assert from 'node:assert/strict';
import {FrameDiff} from '../src/frame-diff.mjs';

test('typing changes only the prompt row, without clearing the screen', () => {
  const diff = new FrameDiff();
  diff.update('transcript\n> a\nfooter', 80);
  assert.equal(diff.update('transcript\n> ab\nfooter', 80), '\x1b[?2026h\x1b[2;1H> ab\x1b[?2026l');
  assert.equal(diff.update('transcript\n> ab\nfooter', 80), '');
});
test('resize and external drawings invalidate the previous screen', () => {
  const diff = new FrameDiff();
  diff.update('first\nsecond', 80);
  assert.match(diff.update('first', 40), /\x1b\[2J/);
  diff.reset();
  assert.match(diff.update('first', 40), /\x1b\[2J/);
});
test('desktop resize repaints complete rows without a blank-screen clear',()=>{
 const diff=new FrameDiff({clearOnResize:false});
 assert.match(diff.update('first\nsecond',80),/\x1b\[2J/);
 const resized=diff.update('first\nsecond',40);
 assert.ok(!resized.includes('\x1b[2J'));assert.ok(resized.includes('\x1b[1;1Hfirst'));assert.ok(resized.includes('\x1b[2;1Hsecond'));
 diff.reset();assert.match(diff.update('new',40),/\x1b\[2J/);
});
