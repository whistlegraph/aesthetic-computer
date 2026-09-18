const {test}=require('node:test');const assert=require('node:assert/strict');const {devStatus}=require('../build-status.cjs');
const running={tree:'a',revision:'123456789',version:'0.7.3'};
const state={online:true,state:'current',checkedAt:new Date().toISOString(),target:{tree:'a'},installed:{tree:'a'}};
test('dev compares source trees rather than app semver',()=>{assert.equal(devStatus(running,state).status,'current');assert.equal(devStatus(running,{...state,target:{tree:'b'},installed:{tree:'b'}}).status,'ready');});
test('offline and stale checks never claim up to date',()=>{assert.equal(devStatus(running,{...state,online:false}).status,'unknown');assert.equal(devStatus(running,{...state,checkedAt:'2000-01-01'}).status,'unknown');});
test('local edits outrank a staged update',()=>{assert.equal(devStatus(running,{...state,modified:['src/tui.mjs'],installed:{tree:'b'}}).status,'modified');});
