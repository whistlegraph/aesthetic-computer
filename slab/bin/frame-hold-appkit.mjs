#!/usr/bin/env node
// Usage: node slab/bin/frame-hold-appkit.mjs /tmp/frame-hold-fixture
// Binary is compiled from slab/test/fixtures/frame-hold.swift. Owns only that
// app and its temporary Frame MCP; no GUI operation is retried on failure.
import assert from 'node:assert/strict';
import { spawn } from 'node:child_process';
import { createServer } from 'node:net';
import { writeFile } from 'node:fs/promises';
import { setTimeout as delay } from 'node:timers/promises';
import { createComputerUseClient } from '../lib/computer-use-client.mjs';
const binary=process.argv[2];
if(!binary?.startsWith('/')) throw new Error('Pass the absolute fixture binary path');
const report={at:new Date().toISOString(),fixture:'AppKit NSButton',samples:[]};
const text=r=>(r.content||[]).filter(c=>c.type==='text').map(c=>c.text).join('\n');
const events=[];
let app, daemon, pending='';
try {
  const reservation=createServer();
  await new Promise(r=>reservation.listen(0,'127.0.0.1',r));const port=reservation.address().port;
  await new Promise(r=>reservation.close(r));
  daemon=spawn(process.execPath,[new URL('./frame-mcp.mjs',import.meta.url).pathname,'--http',String(port)],{stdio:'ignore'});
  const client=createComputerUseClient({servers:{frame:`http://127.0.0.1:${port}/mcp`},allowedTools:['frame','frame_click']});
  for(let i=0;;i++){
    try {await client.discover();break;}
    catch(error){if(i===50)throw error;await delay(50);}
  }
  app=spawn(binary,[],{stdio:['ignore','pipe','pipe']});
  app.stdout.on('data',chunk=>{
    pending+=chunk;
    const lines=pending.split('\n');pending=lines.pop();
    for(const line of lines)if(line)events.push(JSON.parse(line));
  });
  for(let i=0;!events.some(e=>e.ready);i++){assert.ok(i<100,'AppKit startup timed out');await delay(20);}
  await delay(300); // window setup, excluded from samples
  const call=async(name,args)=>{const r=await client.call(name,args);assert.ok(!r.isError,text(r));return r;};
  const first=text(await call('frame',{machine:'local',fast:false,visual:false}));
  const button=first.match(/AXButton «Add one» @\((-?\d+),(-?\d+)\)/);
  const output=first.match(/«Count: [0O]» @\((-?\d+),(-?\d+)\)/);
  assert.ok(button&&output,'Frame must observe fixture button and counter: '+first);
  let observation=JSON.parse(first.match(/^observation: (.+)$/m)[1]), expected=0;
  const holds=(process.env.SLAB_HOLD_SWEEP || '40,10,5,2.5').split(',').map(Number);
  assert.ok(holds.length && holds.every(n=>Number.isFinite(n)&&n>=0&&n<=1000));
  const singleSamples=holds.length*100;
  for(let i=0;i<singleSamples+2;i++){
    const count=i<singleSamples?1:i-singleSamples+2;
    const holdMs=i<singleSamples?holds[i%holds.length]:Math.min(...holds);expected+=count;
    const start=performance.now();
    const result=await call('frame_click',{machine:'local',observationId:observation.id,x:+button[1],y:+button[2],
      count,holdMs,settleMs:0,ocr:false,visual:false,
      verify:{x:+output[1],y:+output[2],role:'AXStaticText',attribute:'AXValue',equals:`Count: ${expected}`}});
    const body=text(result),receipt=JSON.parse(body.match(/^native input: (.+)$/m)[1]);
    assert.match(body,/capture: verified/);assert.equal(receipt.holdMs,holdMs);
    for(let attempt=0;!events.some(e=>e.count===expected)&&attempt<50;attempt++)await delay(1);
    const actions=events.filter(e=>e.count!==undefined);
    assert.equal(actions.length,expected,'Exactly one AppKit action per click');
    assert.equal(actions.at(-1).count,expected);
    report.samples.push({count,holdMs,ms:+(performance.now()-start).toFixed(2),receipt,event:actions.at(-1)});
    observation=JSON.parse(body.match(/^observation: (.+)$/m)[1]);
  }
  // Observe a short quiet interval to detect any delayed extra actions.
  await delay(100);
  assert.equal(events.filter(e=>e.count!==undefined).length,expected);
  report.ok=true;
} catch(error){report.ok=false;report.error=error.message;process.exitCode=1;}
finally {
  app?.kill();daemon?.kill();
  await writeFile('/tmp/frame-hold-appkit.json',JSON.stringify(report,null,2)+'\n');
  console.log(JSON.stringify(report,null,2));
}
