const {test}=require('node:test');const assert=require('node:assert/strict');const {devStatus}=require('../build-status.cjs');
const running={tree:'a',revision:'123456789',version:'0.7.3'};
const state={online:true,state:'current',checkedAt:new Date().toISOString(),target:{tree:'a'},installed:{tree:'a'}};
test('dev compares source trees rather than app semver',()=>{assert.equal(devStatus(running,state).status,'current');assert.equal(devStatus(running,{...state,target:{tree:'b'},installed:{tree:'b'}}).status,'ready');});
test('offline and stale checks never claim up to date',()=>{assert.equal(devStatus(running,{...state,online:false}).status,'unknown');assert.equal(devStatus(running,{...state,checkedAt:'2000-01-01'}).status,'unknown');});
test('local edits outrank a staged update',()=>{assert.equal(devStatus(running,{...state,modified:['src/tui.mjs'],installed:{tree:'b'}}).status,'modified');});
test('automatic application waits until the session is initialized',()=>{
 const fs=require('node:fs'),vm=require('node:vm');const source=fs.readFileSync(require.resolve('../main.cjs'),'utf8');
 const start=source.indexOf('function applyPendingDev()'),end=source.indexOf('\nconst buildStatus=',start);
 const calls=[],context={pendingDevAction:'restart',agentReady:false,lastVisibleState:null,pendingRestart:false,requestRestart:action=>calls.push(action)};
 vm.createContext(context);vm.runInContext(source.slice(start,end),context);context.applyPendingDev();assert.deepEqual(calls,[]);assert.equal(context.pendingDevAction,'restart');context.lastVisibleState={status:'working'};context.applyPendingDev();assert.deepEqual(calls,[]);context.agentReady=true;context.applyPendingDev();assert.deepEqual(calls,['restart']);context.applyPendingDev();assert.equal(calls.length,1);
});
