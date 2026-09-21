#!/usr/bin/env node
// UI-only transfers. Finder AppleScript arranges/reveals windows; it never
// copies, renames, or moves files. The game's read-only scorer verifies moves.
import assert from 'node:assert/strict';
import { readFile, writeFile, mkdir } from 'node:fs/promises';
import { execFile } from 'node:child_process';
import { setTimeout as delay } from 'node:timers/promises';
import { join } from 'node:path';
import { homedir } from 'node:os';
import { createComputerUseClient } from '../lib/computer-use-client.mjs';
const origin='http://127.0.0.1:7782';
const browser=JSON.parse(await readFile('/tmp/finder-quest-browser.json','utf8'));
const label=process.argv[2]||'baseline';
assert.match(label,/^[a-z0-9-]+$/);
const report={label,at:new Date().toISOString(),calls:[],checks:[]};
const client=createComputerUseClient({servers:{frame:process.env.QUEST_FRAME_URL||'http://127.0.0.1:7767/mcp',puppet:'http://127.0.0.1:17869/mcp'},
  allowedTools:['frame','frame_drag','frame_key','frame_click','puppet_click','puppet_snapshot','puppet_reload']});
const scope={machine:'fixture',target:browser.target};
const digest=r=>r.content.filter(c=>c.type==='text').map(c=>c.text).join('\n');
let observed, game, started;
async function state(){const r=await fetch(origin+'/state');assert.ok(r.ok);const s=await r.json();if(game)assert.equal(s.id,game.id,'Round changed during run');return s;}
async function call(name,args){
  const start=performance.now();const result=await client.call(name,args);
  report.calls.push({tool:name,ms:Math.round(performance.now()-start),...(args.durationMs?{durationMs:args.durationMs}:{})});
  assert.ok(!result.isError,digest(result));
  if(name.startsWith('frame'))observed=digest(result);
  return result;
}
function observation(){assert.match(observed,/capture: ok/);return JSON.parse(observed.match(/^observation: (.+)$/m)[1]);}
function point(name){
  const escaped=name.replace(/[.*+?^${}()|[\]\\]/g,'\\$&');
  const match=observed.match(new RegExp(`AXTextField «${escaped}» @\\((\\d+),(\\d+)\\)`));
  const matches=[...observed.matchAll(new RegExp(`AXTextField «${escaped}» @\\((\\d+),(\\d+)\\)`,'g'))];
  assert.ok(matches.length<=1,'Ambiguous AX controls across Finder windows: '+name);
  assert.ok(match,'Visible native control missing: '+name);return [+match[1],+match[2]];
}
async function frame(){return call('frame',{machine:'local',fast:true,visual:false});}
async function key(key,mod){return call('frame_key',{machine:'local',observationId:observation().id,key,mod,fast:true,visual:false});}
async function clickBrowser(locator,after){const r=JSON.parse(digest(await call('puppet_click',{...scope,locator,...(after?{after:{locator:after}}:{})})));assert.equal(r.performed,true);if(after)assert.equal(r.verification.ok,true);return r;}
async function verify(check,description){
  const start=performance.now();let s;
  while(performance.now()-start<2500){s=await state();if(check(s)){report.checks.push({description,ok:true,ms:Math.round(performance.now()-start)});return s;}await delay(30);}
  report.checks.push({description,ok:false});throw new Error('Postcondition failed: '+description);
}
const literal=s=>JSON.stringify(s);
async function finder(path,bounds,{reveal=false}={}){
  const start=performance.now();
  const script=`tell application "Finder"
    ${reveal?`reveal (POSIX file ${literal(path)})`:`set target of front Finder window to (POSIX file ${literal(path)})`}
    activate
    set bounds of front Finder window to {${bounds.join(',')}}
    set current view of front Finder window to list view
  end tell`;
  await new Promise((resolve,reject)=>{const p=execFile('osascript',['-'],e=>e?reject(e):resolve());p.stdin.end(script);});
  report.calls.push({tool:'finder_navigation',ms:Math.round(performance.now()-start)});
  let last;
  for(let i=0;i<12;i++){
    await frame();assert.match(observed,/frontmost: Finder/);
    const shape=observed.match(/^frame region: .+$/m)?.[0]+observation().windowId;
    if(shape===last)return;
    last=shape;await delay(80);
  }
  throw new Error('Finder window did not settle');
}
async function drag(from,to,description){
  await call('frame_drag',{machine:'local',observationId:observation().id,from,to,durationMs:500,fast:true,visual:false});
  console.log(description);
}
try {
  game=await state();assert.equal(game.sorted,0,'Start with an untouched round');assert.equal(game.startedAt,null);
  report.layout=game.layout;
  await call('puppet_reload',scope);
  await clickBrowser({role:'button',name:'Open quest folder'});
  await finder(game.root,[25,60,710,850]);
  await key('a','cmd');await key('right','opt');
  await call('frame_click',{machine:'local',observationId:observation().id,x:650,y:800,fast:true,visual:false});
  // The root and its two messy subfolders are now visible in one outline.
  for(const f of game.files.filter(f=>!f.download))point(f.name);
  await clickBrowser({role:'button',name:'Start run'});
  started=performance.now();
  await frame();
  for(const file of game.files.filter(f=>!f.download)) {
    const category={svg:'Pictures',txt:'Notes',wav:'Audio'}[file.name.split('.').at(-1)];
    await drag(point(file.name),point(category),'sorted '+file.name);
    await verify(s=>s.files.find(f=>f.id===file.id).sorted,'sort '+file.name);
  }
  for(const file of game.files.filter(f=>f.download)){
    await clickBrowser({role:'link',name:'Download '+file.name});
    await verify(s=>s.files.find(f=>f.id===file.id).inDownloads,'download '+file.name);
  }
  // Keep root folder destinations above the Downloads window.
  await finder(game.root,[25,60,710,450]);
  await key('a','cmd');await key('left','opt');
  const destinations=Object.fromEntries(['Pictures','Notes','Audio'].map(name=>[name,point(name)]));
  // A second Finder window is navigation/setup, never a filesystem transfer.
  await new Promise((resolve,reject)=>{const p=execFile('osascript',['-'],e=>e?reject(e):resolve());p.stdin.end(`tell application "Finder" to make new Finder window to (POSIX file ${literal(join(homedir(),'Downloads'))})`);});
  await finder(join(homedir(),'Downloads'),[25,470,710,850]);
  for(const file of game.files.filter(f=>f.download)){
    await drag(point(file.name),destinations[file.category],'moved download '+file.name);
    await verify(s=>s.files.find(f=>f.id===file.id).sorted,'move download '+file.name);
  }
  // Cross-app return: source coordinates come from the Finder AX tree. The
  // browser drop area's global bounds are observed in the full-screen Frame.
  await call('frame',{machine:'local',screen:true,fast:true,visual:false});
  const shot=await call('puppet_snapshot',scope);
  assert.match(JSON.parse(digest(shot)).tree,/Return sorted quest files/);
  // Screen point supplied after visually inspecting the fixed test layout.
  const drop=JSON.parse(process.env.QUEST_DROP_POINT||'[1210,730]');
  for(const file of game.files.filter(f=>f.download)){
    await finder(join(game.root,file.category,file.name),[25,60,710,450],{reveal:true});
    await drag(point(file.name),drop,'returned '+file.name);
    await verify(s=>s.files.find(f=>f.id===file.id).uploaded,'return '+file.name);
  }
  const final=await verify(s=>s.complete,'complete');
  report.ok=true;report.elapsedMs=Math.round(performance.now()-started);report.sorted=final.sorted;report.returned=final.returned;
}catch(error){report.ok=false;report.error=error.message;if(started)report.elapsedMs=Math.round(performance.now()-started);process.exitCode=1;}
finally {
  const dir=new URL('./runs/',import.meta.url);await mkdir(dir,{recursive:true});
  await writeFile(new URL(label+'.json',dir),JSON.stringify(report,null,2)+'\n');
  console.log(JSON.stringify(report,null,2));
}
