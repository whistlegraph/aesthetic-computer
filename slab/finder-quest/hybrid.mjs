#!/usr/bin/env node
// Fixed, visually inspected Blueberry fixture. Real browser downloads and
// native drags; a visible, owned Terminal handles the batch filesystem subtask.
import assert from 'node:assert/strict';
import { readFile, writeFile, mkdir, mkdtemp } from 'node:fs/promises';
import { execFile } from 'node:child_process';
import { join } from 'node:path';
import { tmpdir } from 'node:os';
import { fileURLToPath } from 'node:url';
import { setTimeout as delay } from 'node:timers/promises';
import { createComputerUseClient } from '../lib/computer-use-client.mjs';
import { openTerminalHelper, closeTerminalHelper } from '../lib/terminal-helper.mjs';

const label=process.argv[2];assert.match(label||'',/^[a-z0-9-]+$/);
const origin='http://127.0.0.1:7782';
const browser=JSON.parse(await readFile('/tmp/finder-quest-browser.json','utf8'));
const scope={machine:'fixture',target:browser.target};
const output=new URL('./runs/'+label+'.json',import.meta.url);
const report={label,at:new Date().toISOString(),strategy:'terminal-batch-native-return',calls:[],checks:[]};
const c=createComputerUseClient({servers:{frame:process.env.QUEST_FRAME_URL||'http://127.0.0.1:7767/mcp',puppet:'http://127.0.0.1:17869/mcp'},allowedTools:['frame','frame_drag','puppet_snapshot','puppet_reload','puppet_click','puppet_type']});
const digest=r=>r.content.filter(c=>c.type==='text').map(c=>c.text).join('\n');
const osa=script=>new Promise((resolve,reject)=>{const p=execFile('osascript',['-'],(e,out)=>e?reject(e):resolve(out.trim()));p.stdin.end(script);});
const quote=s=>"'"+s.replaceAll("'","'\\''")+"'";
let game,started,owned,observed,reserved=false;
async function state(){const s=await fetch(origin+'/state').then(r=>r.json());if(game)assert.equal(s.id,game.id);return s;}
async function call(name,args){const t=performance.now(),r=await c.call(name,args);report.calls.push({tool:name,ms:Math.round(performance.now()-t)});assert.ok(!r.isError,digest(r));if(name.startsWith('frame'))observed=digest(r);return r;}
async function frame(){return call('frame',{machine:'local',ocr:false,visual:false});}
async function click(name,role='button'){await call('puppet_click',{...scope,locator:{role,name}});}
async function verify(predicate,name){const t=performance.now();while(performance.now()-t<3000){const s=await state();if(predicate(s)){report.checks.push({name,ok:true});return s;}await delay(50);}throw new Error('Postcondition failed: '+name);}
async function finder(path,selected){
  const t=performance.now();
  await osa(`tell application "Finder"
    set target of front Finder window to POSIX file ${JSON.stringify(path)}
    set bounds of front Finder window to {25,60,710,450}
    set current view of front Finder window to icon view
    ${selected?`select POSIX file ${JSON.stringify(join(path,selected))}`:''}
    clean up front Finder window by name
    activate
  end tell`);
  // Finder animates clean-up. This fixture's fourth sorted icon settles at
  // (355,270); both endpoints were inspected in a full-display screenshot.
  await delay(400);
  await frame();assert.match(observed,/frontmost: Finder/);
  assert.match(observed,/frame region: 685×390 @\(25,60\)/);
  if(selected){const name=await osa('tell application "Finder" to get name of item 1 of (get selection)');assert.equal(name,selected);}
  report.calls.push({tool:'finder_context_total',ms:Math.round(performance.now()-t)});
}
try{
  await mkdir(new URL('./runs/',import.meta.url),{recursive:true});
  // Reserve the report name before any input; never overwrite prior evidence.
  await writeFile(output,'{}\n',{flag:'wx'});
  reserved=true;
  game=await state();assert.equal(game.sorted,0);assert.equal(game.startedAt,null);
  report.layout=game.layout;report.round=game.id;
  await call('puppet_reload',scope);await call('puppet_snapshot',scope);
  await click('Open quest folder');await finder(game.root);
  const finderPath=await osa('tell application "Finder" to get POSIX path of (target of front Finder window as alias)');
  assert.equal(finderPath.replace(/\/$/,''),game.root);
  report.finderPathVerified=true;
  started=performance.now();await click('Start run');
  for(const f of game.files.filter(f=>f.download)){
    await click('Download '+f.name,'link');await verify(s=>s.files.find(x=>x.id===f.id).inDownloads,'download '+f.id);
  }
  const receipt=join(await mkdtemp(join(tmpdir(),'quest-helper-')),'receipt.json');
  const helperStart=performance.now();owned=await openTerminalHelper();report.ownedTerminal=owned;
  await frame();assert.match(observed,/frontmost: Terminal/);
  const command=[process.execPath,fileURLToPath(new URL('./sort.mjs',import.meta.url)),'--quest',game.id,'--receipt',receipt].map(quote).join(' ');
  await call('puppet_type',{machine:'fixture',tty:owned.tty,text:command,enter:true});
  let result;
  for(let i=0;i<100;i++){try{result=JSON.parse(await readFile(receipt,'utf8'));break;}catch{}await delay(100);}
  assert.ok(result?.ok,JSON.stringify(result||{error:'No receipt; do not repeat command'}));report.helper=result;
  report.terminalClosed=await closeTerminalHelper(owned);assert.equal(report.terminalClosed,true);
  await click('Open quest folder');await frame();assert.match(observed,/frontmost: Finder/);
  report.helperContextMs=Math.round(performance.now()-helperStart);
  await verify(s=>s.sorted===12,'all twelve sorted');
  for(const f of game.files.filter(f=>f.download)){
    await finder(join(game.root,f.category),f.name);
    const shot=await call('frame',{machine:'local',screen:true,ocr:false,visual:false});
    const picture=shot.content.find(c=>c.type==='image');
    if(picture)await writeFile(new URL('./runs/'+label+'-'+f.id+'.jpg',import.meta.url),Buffer.from(picture.data,'base64'));
    // Screen observations cannot authorize window-scoped input.
    await frame();const observation=JSON.parse(observed.match(/^observation: (.+)$/m)[1]);
    await call('frame_drag',{machine:'local',observationId:observation.id,from:[355,270],to:[1210,700],durationMs:500,fast:true,visual:false});
    await verify(s=>s.files.find(x=>x.id===f.id).uploaded,'native return '+f.id);
  }
  const final=await verify(s=>s.complete,'complete');report.ok=true;report.sorted=final.sorted;report.returned=final.returned;
}catch(e){report.ok=false;report.error=e.message;process.exitCode=1;}
finally{
  if(started)report.elapsedMs=Math.round(performance.now()-started);
  if(owned&&!report.terminalClosed){try{report.terminalClosed=await closeTerminalHelper(owned);}catch(e){report.cleanupError=e.message;}}
  // Existence of a previously used label fails before the report is reserved.
  if(reserved)await writeFile(output,JSON.stringify(report,null,2)+'\n');
  console.log(JSON.stringify(report,null,2));
}
