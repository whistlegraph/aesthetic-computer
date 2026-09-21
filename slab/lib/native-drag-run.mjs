import assert from 'node:assert/strict';
import { setTimeout as delay } from 'node:timers/promises';
const text=r=>(r.content||[]).filter(c=>c.type==='text').map(c=>c.text).join('\n');
export async function playNativeDrag(call,page,context,report){
  const geometry=await context.newCDPSession(page);
  try {
    const {windowId}=await geometry.send('Browser.getWindowForTarget');
    await geometry.send('Browser.setWindowBounds',{windowId,bounds:{left:40,top:40,width:1000,height:800}});
    await page.waitForFunction(()=>window.outerWidth===1000&&window.screenX===40,null,{timeout:3000});
    await delay(200);
  } finally {await geometry.detach();}
  const useDefaults=process.env.SLAB_DRAG_DEFAULTS==='1';
  const durations=(process.env.SLAB_DRAG_SWEEP||'128,64,32,16,8,0').split(',').map(Number);
  const repeats=Number(process.env.SLAB_DRAG_REPEATS||25),holdMs=Number(process.env.SLAB_DRAG_HOLD||0),releaseMs=Number(process.env.SLAB_DRAG_RELEASE||0);
  const batches=Number(process.env.SLAB_DRAG_BATCHES||1);
  assert.ok(durations.length&&durations.every(n=>Number.isFinite(n)&&n>=0&&n<=2000));
  assert.ok(Number.isInteger(repeats)&&repeats>0&&repeats<=1000);
  assert.ok(Number.isInteger(batches)&&batches>0&&batches<=20);
  assert.ok(durations.length*repeats*batches<=1000,'At most 1000 drag attempts per run');
  report.dragSamples=[];
  report.dragBatches=batches;
  for(let batch=1;batch<=batches;batch++){
  if(batch>1)await page.reload();
  await page.bringToFront();
  const first=text(await call('frame',{machine:'local',fast:false,visual:false},'native'));
  assert.match(first,/Frame drag check/);
  const source=first.match(/AXButton «Drag token» @\((-?\d+),(-?\d+)\)/);
  const target=first.match(/AXButton «Drop here» @\((-?\d+),(-?\d+)\)/);
  const release=first.match(/«Released: [0O]» @\((-?\d+),(-?\d+)\)/);
  assert.ok(source&&target&&release,'Frame must observe both drag endpoints and release counter: '+first);
  let observation=JSON.parse(first.match(/^observation: (.+)$/m)[1]),expected=0;
  for(const durationMs of durations)for(let i=0;i<repeats;i++){
    const start=performance.now();expected++;
    const result=await call('frame_drag',{machine:'local',observationId:observation.id,
      from:[+source[1],+source[2]],to:[+target[1],+target[2]],...(useDefaults?{}:{durationMs,holdMs,releaseMs}),settleMs:0,ocr:false,visual:false,
      verify:{x:+release[1],y:+release[2],role:'AXStaticText',attribute:'AXValue',equals:`Released: ${expected}`,timeoutMs:500}},'native');
    const body=text(result),receipt=JSON.parse(body.match(/^native input: (.+)$/m)[1]);
    const sample={batch,attempt:expected,verified:false,holdMs:receipt.holdMs,durationMs:receipt.durationMs,releaseMs:receipt.releaseMs,ms:+(performance.now()-start).toFixed(2),receipt,
      drops:await page.locator('#drops').textContent(),released:await page.locator('#releases').textContent(),rejected:await page.locator('#rejects').textContent(),
      events:await page.evaluate(()=>window.dragTrace)};
    report.dragSamples.push(sample);
    assert.match(body,/capture: verified/,'Drag/release not verified; input not repeated');
    assert.equal(receipt.kind,'drag');assert.equal(receipt.releasePosted,true);
    if(useDefaults){assert.equal(receipt.holdMs,0);assert.equal(receipt.durationMs,32);assert.equal(receipt.releaseMs,32);}
    assert.equal(sample.drops,`Drops: ${expected}`);assert.equal(sample.released,`Released: ${expected}`);
    assert.equal(sample.rejected,'Rejected: 0');
    assert.ok(sample.events.every(event=>event.trusted),'Fixture receipts must come from real input');
    for(const [kind,target] of [['dragstart','source'],['drop','target'],['dragend','source']])
      assert.equal(sample.events.filter(event=>event.kind===kind&&event.target===target).length,1,`One ${kind} per drag`);
    sample.verified=true;
    observation=JSON.parse(body.match(/^observation: (.+)$/m)[1]);
  }
  await delay(100);
  assert.equal(await page.locator('#drops').textContent(),`Drops: ${expected}`);
  assert.equal(await page.locator('#releases').textContent(),`Released: ${expected}`);
  }
}
