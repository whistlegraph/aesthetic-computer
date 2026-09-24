import test from 'node:test';
import assert from 'node:assert/strict';
import {trackInfo,trackLines} from './track-info.mjs';
test('track switches use the new score title and duration, with idle time reset',()=>{
 assert.equal(trackInfo({name:'Wake',dur:52},{phase:'finished',scoreTime:null},999).elapsed,52);
 const next=trackInfo({name:'Femrag++',dur:146.67},{phase:'ready',scoreTime:null},999);
 assert.equal(next.title,'Femrag++');assert.equal(next.elapsed,0);assert.equal(next.duration,146.67);
 assert.equal(trackInfo({name:'Wake',dur:52},{phase:'playing',scoreTime:3},3).elapsed,3);
});
test('long titles keep two readable lines and bound unbroken names',()=>{
 for(const title of ['The MacNeoPolitan Trio - Good morning, Sophia','x'.repeat(200),'a '.repeat(200)]){
 const lines=trackLines(title);assert(lines.length<=2);assert(lines.every(l=>l.length<=52));
 }
});
