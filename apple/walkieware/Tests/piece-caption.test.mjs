import test from 'node:test';
import assert from 'node:assert/strict';
import {pieceCaption} from '../Resources/Web/piece-caption.mjs';
test('caption is explicit, bounded, and never evaluates code',()=>{
 assert.equal(pieceCaption('export const caption = "hold to inflate";'),'hold to inflate');
 assert.equal(pieceCaption('export const caption = (()=>{throw Error("do not run")})()'), '');
 assert.equal(pieceCaption('// export const caption = "fake";'), '');
 assert.equal(pieceCaption('export function paint(){ write("hold to inflate"); }'), '');
 assert.equal(pieceCaption('export const caption = '+JSON.stringify('a'.repeat(200))+';').length,160);
});
