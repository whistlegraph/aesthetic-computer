import test from 'node:test';
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import { chromium } from 'playwright';
import { inputROM } from './fixtures/nes-input.mjs';

const pub = new URL('../public/', import.meta.url);
async function fixture({ login = false } = {}) {
  const browser = await chromium.launch({channel:process.env.PLAYWRIGHT_CHANNEL,headless:true});
  const page = await browser.newPage({viewport:{width:390,height:844},hasTouch:true});
  const errors=[], writes=[];
  page.on('pageerror',error=>{errors.push(error.message);console.error('Browser error:',error.message);});
  const worklet = await readFile(new URL('nes-player/audio-worklet.mjs', pub), 'utf8');
  await page.addInitScript(({login,worklet})=>{
    if(login) localStorage.setItem('session-aesthetic',encodeURIComponent(btoa(JSON.stringify({accessToken:'fixture',account:{sub:'fixture'}}))));
    let library;
    Object.defineProperty(window,'jsnes',{configurable:true,get:()=>library,set(value){
      const Core=value.NES;
      const Wrapped=class extends Core {constructor(options){
        const onFrame=options.onFrame;
        super({...options,onFrame(buffer){window.fixtureFrames=(window.fixtureFrames||0)+1;onFrame(buffer);}});
        window.fixtureNES=this;
      }};
      library={...value,NES:Wrapped};
    }});
    const Worklet=window.AudioWorkletNode;
    window.AudioWorkletNode=class extends Worklet {constructor(...args){
      super(...args);window.fixtureWorklet=true;const send=this.port.postMessage.bind(this.port);
      this.port.postMessage=(data,...rest)=>{if(data instanceof Float32Array&&data.some(sample=>Math.abs(sample)>.001))window.fixtureSound=true;send(data,...rest);};
    }};
    const Audio=window.AudioContext;
    window.AudioContext=class extends Audio {constructor(options){super(options);window.fixtureAudio=this;
      const add=this.audioWorklet.addModule.bind(this.audioWorklet);
      // Chromium's worklet requests bypass Playwright routing. Execute the actual
      // local module from a blob, so this still tests real audio processing.
      this.audioWorklet.addModule=()=>{
        const url=URL.createObjectURL(new Blob([worklet],{type:'text/javascript'}));
        return add(url).then(value=>{window.fixtureAudioLoad='ready';return value;},error=>{window.fixtureAudioLoad=String(error);throw error;}).finally(()=>URL.revokeObjectURL(url));
      };}};
  },{login,worklet});
  const posts=[{code:'nes-fixture',name:'@fixture',board:'application/x-nes-rom',replies:0,
    file:{name:'MIME.nes',type:'application/x-nes-rom',url:'/api/mime?file=nes-fixture'}},
    {code:'next',name:'@fixture',board:'text/plain',text:'Next card',media:{kind:'mood',code:'next'}}];
  await page.route('**/*',async route=>{
    const request=route.request(),url=new URL(request.url());
    if(url.pathname==='/api/mime'){
      if(url.searchParams.has('file')) return route.fulfill({body:Buffer.from(inputROM()),contentType:'application/x-nes-rom'});
      if(url.searchParams.has('me')) return route.fulfill({json:{handle:login?'@fixture':null}});
      if(url.searchParams.has('engagement')) return route.fulfill({json:{ok:true}});
      if(request.method()==='POST'){
        writes.push(request.postDataJSON());
        return route.fulfill(writes.length===1?{status:503,json:{error:'Try again'}}:{json:{code:'nes-fixture',parent:null,board:'application/x-nes-rom'}});
      }
      if(url.searchParams.has('thread')) return route.fulfill({json:{op:posts[0],replies:[]}});
      return route.fulfill({json:{recent:posts,hasMore:false}});
    }
    let path=url.pathname;
    if(url.hostname==='mime.ac'&&(path==='/'||path.startsWith('/p/')||/^\/nes~[A-Za-z0-9_-]+$/.test(path))) path='/mime/index.html';
    if(path.endsWith('/'))path+='index.html';
    try{
      const body=await readFile(new URL('.'+path,pub));
      const ext=path.split('.').at(-1),contentType={html:'text/html',mjs:'text/javascript',js:'text/javascript',ttf:'font/ttf',json:'application/json'}[ext]||'application/octet-stream';
      return route.fulfill({body,contentType,headers:{'Access-Control-Allow-Origin':'*'}});
    }catch{return route.fulfill({body:''});}
  });
  return {browser,page,errors,writes};
}

test('a real NES ROM renders, accepts keyboard/touch/gamepad, pauses and releases held controls',async()=>{
  const {browser,page,errors}=await fixture();
  try{
    await page.goto('https://mime.ac/nes~nes-fixture?ac-automation=1');
    await page.locator('#nes-fixture .piece-preview iframe').waitFor({timeout:10000});
    const player=await (await page.locator('#nes-fixture .piece-preview iframe').elementHandle()).contentFrame();
    await player.waitForFunction(()=>window.fixtureFrames>10);
    assert.equal(await page.locator('#nes-fixture .media-label').first().getAttribute('aria-label'),'nes~nes-fixture');
    assert.equal(await page.locator('#nes-fixture .media-label').first().getAttribute('href'),'https://mime.ac/nes~nes-fixture');
    assert.equal(await page.locator('#nes-fixture .feed-media').getAttribute('data-media-state'),'ready');
    assert.ok(await player.locator('canvas').evaluate(canvas=>new Set(canvas.getContext('2d').getImageData(0,0,256,240).data).size>3));
    await player.locator('canvas').click();
    await player.locator('#ab [data-key="0"]').focus();
    await page.keyboard.down('x');
    await player.waitForFunction(()=>window.fixtureNES.cpu.mem[1]&1);
    await page.waitForFunction(()=>document.querySelector('#nes-fixture').hasAttribute('data-full'));
    await player.waitForFunction(()=>window.fixtureAudio?.state==='running');
    try { await player.waitForFunction(()=>window.fixtureSound===true,{},{timeout:5000}); }
    catch { throw new Error(JSON.stringify(await player.evaluate(()=>({load:window.fixtureAudioLoad,worklet:window.fixtureWorklet,audio:window.fixtureAudio?.state,frames:window.fixtureFrames,keys:window.fixtureNES.cpu.mem[1],sound:document.querySelector('#sound').textContent})))); }
    await page.keyboard.up('x');
    await player.waitForFunction(()=>!(window.fixtureNES.cpu.mem[1]&1));
    const a=await player.locator('#ab [data-key="0"]').boundingBox();
    await page.mouse.move(a.x+a.width/2,a.y+a.height/2);await page.mouse.down();
    await player.waitForFunction(()=>window.fixtureNES.cpu.mem[1]&1);
    await player.evaluate(()=>window.__mimeSettle(false));
    const stopped=await player.evaluate(()=>window.fixtureFrames);
    await page.mouse.up();await page.waitForTimeout(150);
    assert.equal(await player.evaluate(()=>window.fixtureFrames),stopped);
    assert.equal(await player.evaluate(()=>window.fixtureNES.controllers[1].state[0]&1),0);
    assert.equal(await player.evaluate(()=>window.fixtureAudio.state),'suspended');
    await player.evaluate(()=>window.__mimeSettle(true));
    await player.waitForFunction(count=>window.fixtureFrames>count,stopped);
    await player.evaluate(()=>{
      window.fixturePad={connected:true,mapping:'standard',axes:[1,0],buttons:Array.from({length:16},()=>({pressed:false}))};
      Object.defineProperty(navigator,'getGamepads',{value:()=>[window.fixturePad]});
    });
    await player.waitForFunction(()=>window.fixtureNES.cpu.mem[1]&128);
    await player.evaluate(()=>window.fixturePad=null);
    await player.waitForFunction(()=>!(window.fixtureNES.cpu.mem[1]&128));
    await page.waitForTimeout(1300);
    await page.screenshot({path:'/tmp/mime-nes-phone.png'});
    // Leaving via the always-available rail stops the same iframe, preserving its game.
    await page.mouse.move(2,700);await page.mouse.down();await page.mouse.move(2,100,{steps:8});await page.mouse.up();
    await page.waitForTimeout(500);
    assert.equal(await player.evaluate(()=>window.fixtureAudio.state),'suspended');
    const offscreen=await player.evaluate(()=>window.fixtureFrames);await page.waitForTimeout(150);
    assert.equal(await player.evaluate(()=>window.fixtureFrames),offscreen);
    await page.setViewportSize({width:844,height:390});
    await page.goto('https://aesthetic.computer/nes-player/');
    await page.locator('#rom').setInputFiles({name:'MIME.nes',mimeType:'application/octet-stream',buffer:Buffer.from(inputROM())});
    await page.waitForFunction(()=>window.fixtureFrames>10);
    for(const button of await page.locator('#controls button, #tools button').all()) {
      const box=await button.boundingBox();assert.ok(box.x>=8&&box.y>=48&&box.x+box.width<=836&&box.y+box.height<=342);
    }
    await page.screenshot({path:'/tmp/mime-nes-landscape.png'});
    assert.deepEqual(errors,[]);
  }finally{await browser.close();}
});

test('NES file picker validates locally, keeps a failed upload and opens the published post',async()=>{
  const {browser,page,errors,writes}=await fixture({login:true});
  try{
    await page.goto('https://mime.ac/?ac-automation=1');
    await page.locator('.mime-startup').waitFor({state:'detached'});
    await page.getByRole('button',{name:'Add NES ROM',exact:true}).click();
    const dialog=page.getByRole('dialog',{name:'Add NES ROM'}),input=dialog.getByLabel('NES ROM',{exact:true});
    await input.setInputFiles({name:'bad.nes',mimeType:'application/octet-stream',buffer:Buffer.alloc(16)});
    await dialog.getByRole('status').filter({hasText:'Choose an iNES'}).waitFor();
    assert.equal(await dialog.getByRole('button',{name:'Post ROM',exact:true}).isEnabled(),false);
    await input.setInputFiles({name:'MIME.nes',mimeType:'application/octet-stream',buffer:Buffer.from(inputROM())});
    const post=dialog.getByRole('button',{name:'Post ROM',exact:true});
    await page.waitForFunction(()=>!document.querySelector('#nes-dialog input').disabled);
    assert.equal(await dialog.getByRole('button',{name:'Log in to post',includeHidden:true}).isVisible(),false);
    await page.screenshot({path:'/tmp/mime-nes-import.png'});
    await post.click();await dialog.getByRole('status').filter({hasText:'Try again'}).waitFor();
    assert.equal(writes.length,1);assert.equal(writes[0].file.type,'application/x-nes-rom');
    assert.deepEqual(Buffer.from(writes[0].file.data,'base64'),Buffer.from(inputROM()));
    const uploaded=page.waitForResponse(response=>response.url().endsWith('/api/mime')&&response.request().method()==='POST'&&response.status()===200);
    await post.click();await uploaded;await dialog.waitFor({state:'detached'});
    await page.waitForURL('**/nes~nes-fixture?ac-automation=1');
    await page.locator('#nes-fixture .piece-preview iframe[data-ready="true"]').waitFor();
    assert.equal(writes.length,2);assert.deepEqual(errors,[]);
  }finally{await browser.close();}
});

test('an unavailable ROM releases the loading cover and can be retried; unrelated URLs are refused',async()=>{
  const {browser,page,errors}=await fixture();
  try{
    let reads=0;
    await page.route('**/api/mime?file=retry',route=>route.fulfill(++reads===1
      ? {status:503,body:'Unavailable'} : {body:Buffer.from(inputROM()),contentType:'application/x-nes-rom'}));
    await page.goto('https://aesthetic.computer/nes-player/?file='+encodeURIComponent('https://mime.ac/api/mime?file=retry'));
    await page.getByRole('status').filter({hasText:'Could not load'}).waitFor();
    assert.equal(await page.evaluate(()=>window.__mimeReady),true);
    await page.getByRole('button',{name:'Retry',exact:true}).click();
    await page.waitForFunction(()=>window.fixtureFrames>10);
    assert.equal(reads,2);
    await page.goto('https://aesthetic.computer/nes-player/?file='+encodeURIComponent('https://example.invalid/private'));
    await page.getByRole('status').filter({hasText:'Open a ROM uploaded to MIME'}).waitFor();
    assert.equal(await page.evaluate(()=>window.__mimeReady),true);
    assert.deepEqual(errors,[]);
  }finally{await browser.close();}
});
