import test from 'node:test';
import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import vm from 'node:vm';

// Execute the shipped WKUserScript with a ready runtime; URL-only tests miss
// the dropped-source handoff which previously restored the corner HUD.
const swift=readFileSync(new URL('../Sources/AeselPieceView.swift',import.meta.url),'utf8');
const script=swift.match(/addUserScript\(WKUserScript\(source: """\n([\s\S]*?)\n        """, injectionTime/)[1];
test('native source injection carries view flags without forwarding auth parameters',()=>{
  const sent=[],ticks=[];
  const window={preloaded:true,__aeselSource:'export function paint({wipe}) { wipe("orange"); }',acSEND:message=>sent.push(message),addEventListener(){},webkit:{messageHandlers:{previewFailure:{postMessage(){}}}}};
  vm.runInNewContext(script,{window,location:{search:'?noauth=true&nolabel=true&preview&code=private&state=private'},URLSearchParams,setInterval:fn=>(ticks.push(fn),1),clearInterval(){},setTimeout(){}});
  ticks[1]();ticks[1]();
  assert.equal(sent.length,1);
  assert.equal(sent[0].type,'dropped:piece');
  const flags=new URLSearchParams(sent[0].content.search);
  for(const key of ['nogap','nolabel','autoreload'])assert.equal(flags.get(key),'true');
  assert(flags.has('preview'));
  for(const key of ['code','state','noauth'])assert(!flags.has(key));
  window.__aeselSource+='\n// revised';window.__aeselRender();
  assert.equal(sent.length,2);assert.equal(sent[1].content.source,window.__aeselSource);
});

test('a late ready event recovers the startup timeout and injects the saved piece',()=>{
  const events={},messages=[],sent=[],timeouts=[];
  const window={__aeselSource:'export function paint({wipe}) { wipe("purple"); }',acSEND:m=>sent.push(m),
    addEventListener:(name,fn)=>{events[name]=fn},webkit:{messageHandlers:{previewFailure:{postMessage:m=>messages.push(m)}}}};
  vm.runInNewContext(script,{window,location:{search:''},URLSearchParams,setInterval:()=>1,clearInterval(){},setTimeout:fn=>timeouts.push(fn)});
  timeouts[0]();
  assert.match(messages[0],/did not become ready/);
  events.message({data:{type:'ready'}});
  assert.equal(messages[1].ready,true);
  assert.equal(sent.length,1);
  events.error({message:'A later runtime error'});
  assert.equal(messages[2],'A later runtime error');
});

test('speaker indicator follows audio, holds musical rests and remains reachable while muted',()=>{
  let now=0,samples=[];const ticks=[],reported=[];
  const window={__aeselSource:'piece',__aeselVolume:1,AC:{readOutputWaveform:()=>samples},addEventListener(){},webkit:{messageHandlers:{previewAudio:{postMessage:m=>reported.push(m)}}}};
  vm.runInNewContext(script,{window,location:{search:''},URLSearchParams,performance:{now:()=>now},setInterval:fn=>ticks.push(fn),clearInterval(){},setTimeout(){}});
  const tick=ticks[0];tick();assert.equal(reported.length,0);
  samples=[0.1];tick();assert.equal(reported.at(-1).active,true);
  samples=[];now=1000;tick();assert.equal(reported.length,1);
  window.__aeselVolume=0;now=4000;tick();assert.equal(reported.length,1);
  window.__aeselVolume=1;tick();assert.equal(reported.at(-1).active,false);
  window.__aeselSource='new piece';tick();assert.equal(reported.length,2);
  samples=[0.2];tick();assert.equal(reported.at(-1).source,'new piece');
});
