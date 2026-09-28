import test from 'node:test';
import assert from 'node:assert/strict';
import {mkdtemp,mkdir,writeFile,rm} from 'node:fs/promises';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import {needsLaunchChooser,chooserKey,savedThreads,NEW_MEDIA} from '../src/launch-chooser.mjs';
import {desktopSnapshot,writeDesktopSession} from '../src/desktop-session.mjs';
test('ordinary launches choose while explicit continuations bypass',()=>{
 assert.equal(needsLaunchChooser([]),true);for(const flag of ['--continue-session','--piece','--prompt','--resume','--medium'])assert.equal(needsLaunchChooser([flag,'value']),false);
 assert.equal(needsLaunchChooser([],false),false);
});
test('Tab switches high level options; arrows stay within actual choices',()=>{
 assert.equal(NEW_MEDIA.length,5);let state={tab:0,index:4};state=chooserKey(state,'\t',2);assert.deepEqual(state,{tab:1,index:0});state=chooserKey(state,'\x1b[B',2);state=chooserKey(state,'\x1b[B',2);assert.equal(state.index,1);assert.deepEqual(chooserKey(state,'\t',2),{tab:0,index:0});
});
test('thread catalog loads exact private snapshots and skips corrupt or foreign workspaces',async t=>{
 const cwd=await mkdtemp(join(tmpdir(),'easel-choose-'));t.after(()=>rm(cwd,{recursive:true,force:true}));
 const dir=join(cwd,'.easel','threads');await mkdir(dir,{recursive:true});
 const snapshot=desktopSnapshot({cwd,backend:'ac',model:'model',live:{file:join(cwd,'saved.mjs'),runtime:'mjs',channel:'channel'},state:{entries:[{id:'u',kind:'user',text:'draw stars'}],input:'draft',cursor:5,history:[],queued:[],medium:'piece'},options:{},engine:{threadId:'exact',messages:[{role:'user',content:'draw stars'}]},handoff:'',archivedConversation:[]});
 const last=join(cwd,'.easel','session.json');await writeDesktopSession(last,snapshot);await writeDesktopSession(join(dir,'duplicate.json'),snapshot);await writeFile(join(dir,'bad.json'),'{');await writeFile(join(dir,'foreign.json'),JSON.stringify({...snapshot,cwd:'/elsewhere'}));
 const threads=await savedThreads(cwd,last);assert.equal(threads.length,1);assert.deepEqual(threads[0].snapshot,snapshot);assert.match(threads[0].label,/draw stars/);
});

test('opening menu fits narrow and wide terminals without leaking thread ANSI',async()=>{
 const {renderChooser}=await import('../src/launch-chooser.mjs');
 const {cleanText,textWidth}=await import('../src/render.mjs');
 for(const columns of [32,80,140]){
  const frame=renderChooser({tab:1,index:0},[{label:'\x1b[31mred\x1b[0m 漢字 thread'}],columns,24);
  const lines=frame.split('\n');assert.equal(lines.length,23);
  for(const line of lines)assert.ok(textWidth(line)<=columns-1);
  const plain=cleanText(frame);assert.ok(plain.includes('red'));assert.ok(!plain.includes('[31m'));
  const title=lines.map(cleanText).find(line=>line.includes('aesel'));
  assert.ok(Math.abs(title.indexOf('aesel')-(columns-1-5)/2)<=1);
 }
});
