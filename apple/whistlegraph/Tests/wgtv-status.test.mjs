import test from 'node:test';
import assert from 'node:assert/strict';
import vm from 'node:vm';
import {readFileSync} from 'node:fs';
const runtime=readFileSync(new URL('../Resources/Web/wgtv-compat.js',import.meta.url),'utf8');
test('TV progress survives clock skew, expires without heartbeats, and leaves idle art alone',()=>{
 let now=200000,file={busy:true,code:'wgTest',phase:'Checking picture…',updatedAt:1000},boxes=0;
 const api={screen:{width:640,height:360},system:{readFile:()=>JSON.stringify(file)},ink(){return this},box(){boxes++;return this}};
 const context=vm.createContext({Date:{now:()=>now},api});vm.runInContext(runtime,context);
 const paint=()=>{boxes=0;vm.runInContext('wgtvPaintStatus(api)',context);return boxes};
 assert.ok(paint()>20,'Comic Relief glyphs render even when sender clock differs');
 now+=16000;assert.equal(paint(),0,'Disconnected sender does not leave a stuck status');
 file.updatedAt++;now+=300;assert.ok(paint()>20);
 file.busy=false;file.updatedAt++;now+=300;assert.equal(paint(),0,'Idle artwork is unobscured');
});
