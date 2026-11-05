import test from 'node:test';
import assert from 'node:assert/strict';
import { createGmBridge } from '../public/aesthetic.computer/lib/notepat-gm-bridge.mjs';
import { setGMAssetBase } from '../public/aesthetic.computer/lib/gm.mjs';
const tick=()=>new Promise(resolve=>setImmediate(resolve));
const deferred=()=>{let resolve;const promise=new Promise(r=>resolve=r);return {resolve,promise};};

test('preload all sample files before audio unlock; first tap responds immediately and never retriggers after release',async()=>{
  const originalFetch=globalThis.fetch, downloads=new Map(), statuses=[];
  let currentContext=null;
  const param=()=>({value:0,setValueAtTime(v){this.value=v},linearRampToValueAtTime(v){this.value=v},setTargetAtTime(v){this.value=v},cancelAndHoldAtTime(){}});
  const ctx={sampleRate:48000,currentTime:1,destination:{},started:[],stopped:[],
    createBuffer:(_,n)=>({kind:'loading',getChannelData:()=>new Float32Array(n)}),
    decodeAudioData:async()=>({kind:'sample'}),
    createGain:()=>({gain:param(),connect(){},disconnect(){}}),
    createStereoPanner:()=>({pan:param(),connect(){},disconnect(){}}),
    createBufferSource(){return {playbackRate:param(),connect(){},disconnect(){},start(){ctx.started.push(this)},stop(){ctx.stopped.push(this)}}},
  };
  globalThis.fetch=async url=>String(url).endsWith('manifest.json')
    ?{ok:true,json:async()=>({patches:{0:{notes:[60,63,66]}}})}
    :{ok:true,arrayBuffer:()=>{const d=deferred();downloads.set(String(url),d);return d.promise}};
  try {
    setGMAssetBase('https://example.invalid/gm');
    const bridge=createGmBridge({context:()=>currentContext,destination:()=>ctx.destination,status:s=>statuses.push(s)});
    const preloading=bridge.program(0); await tick();
    assert.equal(downloads.size,3,'all samples download before the first gesture');
    assert.equal(ctx.started.length,0,'preloading is silent');
    currentContext=ctx;
    const warming=bridge.program(0); await tick();
    const first=bridge.note({id:1,program:0,note:65});
    assert.equal(ctx.started.length,1,'first note starts synchronously while downloads are pending');
    assert.equal(ctx.started[0].buffer.kind,'loading');
    bridge.stop(1); await first;
    assert.equal(ctx.stopped.length,1);
    for(const d of downloads.values()) d.resolve(new ArrayBuffer(8));
    await Promise.all([preloading,warming]);await tick();
    assert.equal(ctx.started.length,1,'download completion cannot play a released note');
    assert.equal(statuses.at(-1).ready,true);
    const next=bridge.note({id:2,program:0,note:65});
    assert.equal(ctx.started.length,2,'a warm sample starts without awaiting a download');
    assert.equal(ctx.started[1].buffer.kind,'sample');
    await next;bridge.stop(2);bridge.panic();assert.equal(ctx.stopped.length,2);
  } finally {globalThis.fetch=originalFetch;setGMAssetBase('https://assets.aesthetic.computer/gm');}
});
