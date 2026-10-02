import test from 'node:test';
import assert from 'node:assert/strict';
import {instantPiece} from '../Resources/Web/instant-piece.mjs';
for(const [text,shape,color] of [['Make a pink circle','circle','pink'],['Please draw a blue square.','box','blue'],['Create a red circle with a bouncing ball','circle','red']])test(text,async()=>{
 const source=instantPiece(text),mod=await import('data:text/javascript,'+encodeURIComponent(source));
 for(const screen of [{width:200,height:100},{width:100,height:200}]){
  const calls=[];const drawing={circle:(...a)=>calls.push(['circle',...a]),box:(...a)=>calls.push(['box',...a])};
  mod.paint({screen,wipe:()=>{},ink:c=>{assert.equal(c,color);return drawing;}});
  assert.equal(calls[0][0],shape);assert.ok(calls[0].slice(1).filter(x=>typeof x==='number').every(Number.isFinite));
 }
});
test('unsupported and negated requests do not invent a starter',()=>{for(const text of ['Do not make a circle','Make a girl jumping rope','Remove the pink circle','Make a pink circle disappear','Make a pink circle; fetch(secret)','A night garden'])assert.equal(instantPiece(text),null,text);});
