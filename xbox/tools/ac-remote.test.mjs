import test from 'node:test';
import assert from 'node:assert/strict';
import {remoteSnapshot} from './ac-remote.mjs';
test('Xbox remote releases all inputs and maps both hands, bumpers and zoom independently',()=>{
 assert.deepEqual(remoteSnapshot(new Set()),[0,0,0,0,0,0,0]);
 const [lx,ly,rx,ry,lt,rt,buttons]=remoteSnapshot(new Set([17,32,29,33,46]),{x:120,y:-120,left:true,right:true});
 assert.deepEqual([lx,ly,rx,ry,lt,rt],[1,1,1,1,1,1]);
 assert.equal(buttons,(1<<8)|(1<<9)|(1<<13));
 assert.deepEqual(remoteSnapshot(new Set([45])),[0,0,0,0,1,0,0]);
 assert.deepEqual(remoteSnapshot(new Set([19])),[0,0,0,0,0,1,0]);
});
