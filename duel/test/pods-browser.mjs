import assert from 'node:assert/strict';
import { once } from 'node:events';
import { mkdir } from 'node:fs/promises';
import { chromium } from 'playwright';
import { createDuelServer } from '../server.mjs';
import { lot } from '../geography.mjs';

const app = createDuelServer({ resolveHandleColors: async () => null });
await app.ready;
await app.geography.change(g => { g.settle('jeffrey'); g.settle('oskie'); });
app.server.listen(0, '127.0.0.1'); await once(app.server, 'listening');
const base = `http://127.0.0.1:${app.server.address().port}/`;
const shots = new URL('../../tmp/duel/pods/', import.meta.url);
await mkdir(shots, { recursive: true });
const browser = await chromium.launch({ channel: 'chrome', headless: true });
const errors = [];
try {
  const page = await browser.newPage({ viewport: { width: 1280, height: 800 } });
  page.on('pageerror', e => errors.push(e.message));
  await page.route('**/favicon.ico', r => r.fulfill({ status: 204 }));
  await page.route('**/pod-render', r => r.fulfill({ contentType: 'text/html', body: `<!doctype html><style>html,body{margin:0;width:100%;height:100%;overflow:hidden}canvas{width:100%;height:100%;display:block;image-rendering:pixelated}</style><canvas id="game"></canvas><script type="module">
    import {createView} from './view.mjs';
    window.view=await createView(document.querySelector('canvas'));
    window.state={bare:true,camera:{x:5,y:2,z:-4,yaw:0,pitch:0},players:[],park:{neighbours:{west:'lobby'}},lots:${JSON.stringify({rects:Array.from({length:8},(_,k)=>lot(k)),homes:Object.values(app.geography.geography.pods).map(p=>({handle:p.handle,pod:p.id,lot:app.geography.geography.placements[p.id].lot}))})}};
    function frame(){view.render(state);requestAnimationFrame(frame);}frame();window.ready=true;
  </script>` }));
  await page.goto(base + 'pod-render');
  await page.waitForFunction(() => window.ready);
  for (const [name, width, height] of [['tags',1280,800],['tags-mobile',390,844]]) {
    await page.setViewportSize({width,height});
    await page.evaluate(()=>new Promise(resolve=>requestAnimationFrame(()=>{view.render(state);requestAnimationFrame(resolve);})));
    await page.screenshot({path:new URL(name+'.png',shots).pathname});
  }
  await page.setViewportSize({width:1280,height:800});
  await page.evaluate(()=>new Promise(resolve=>requestAnimationFrame(()=>{state.camera={x:13,y:15,z:-18,yaw:-35,pitch:-35};view.render(state);requestAnimationFrame(resolve);})));
  await page.waitForFunction(()=>view.aimOrigin()?.y>14);
  await page.screenshot({path:new URL('park.png',shots).pathname});
  // Actual client input: walk from the plaza along the east path, then back.
  await page.goto(base+'?room=lobby&debug');
  await page.waitForFunction(()=>window.doolDebug?.().seat>=0);
  const session=[...app.rooms.get('lobby').sessions.values()][0];
  Object.assign(app.rooms.get('lobby').match.seats[session.seat].player,{x:6.9,z:0,y:1.7,yaw:0});
  await page.waitForFunction(()=>window.doolDebug().player?.x>6.5);
  await page.keyboard.down('a');
  await page.waitForFunction(()=>window.doolDebug().room==='e1n0'&&window.doolDebug().player?.x<0,{},{timeout:10000});
  await page.keyboard.up('a');
  await page.waitForFunction(()=>window.doolDebug().lots?.homes.length===2);
  await page.keyboard.down('d');
  await page.waitForFunction(()=>window.doolDebug().room==='lobby'&&window.doolDebug().player?.x>0,{},{timeout:10000});
  await page.keyboard.up('d');
  // Walk under the exterior tag to enter the pod.
  await page.goto(base+'?room=e1n0&debug');
  await page.waitForFunction(()=>window.doolDebug?.().seat>=0 && window.doolDebug().lots?.homes.length===2);
  const visitor=[...app.rooms.get('e1n0').sessions.values()].at(-1);
  Object.assign(app.rooms.get('e1n0').match.seats[visitor.seat].player,{x:6.25,z:0,y:1.7,yaw:0});
  await page.waitForFunction(()=>window.doolDebug().player?.x>6);
  await page.keyboard.down('w');
  await page.waitForFunction(()=>window.doolDebug().park?.pod?.handle==='oskie',{},{timeout:10000});
  await page.keyboard.up('w');
  // Stable links open interiors; the visitor can return to the current park location.
  const pod=app.geography.geography.forHandle('oskie').id;
  await page.goto(base+'?pod='+pod+'&debug');
  await page.waitForFunction(()=>window.doolDebug?.().park?.pod?.handle==='oskie');
  assert.equal(await page.evaluate(()=>window.doolDebug().room),pod);
  assert.deepEqual(errors,[]);
  console.log(JSON.stringify({screenshots:shots.pathname,walk:'plaza → residential block → plaza',podLink:true,errors}));
} catch(e) { console.log({errors,rooms:[...app.rooms.keys()]}); throw e; } finally {await browser.close();await app.close();}
