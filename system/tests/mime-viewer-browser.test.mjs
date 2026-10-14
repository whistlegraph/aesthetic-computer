import test from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {extname} from 'node:path';
import {chromium} from 'playwright';
const pub=new URL('../public/',import.meta.url);
test('MIME viewer handle follows login and logout, and real AC keeps navigation keys inside the piece', {timeout:120000}, async()=>{
 const browser=await chromium.launch({channel:process.env.PLAYWRIGHT_CHANNEL,headless:true});
 const page=await browser.newPage({viewport:{width:390,height:844}}), errors=[], logs=[];
 page.on("console",m=>{if(logs.length>40)logs.shift();logs.push(m.text().slice(0,300));});
 page.on('pageerror',e=>errors.push(e.message));
 const source=await readFile(new URL('fixtures/mime-viewer.mjs',import.meta.url),'utf8');
 const probe="let lastKey = '';\n" + source.replace('const viewer = handle();','const viewer = handle(); globalThis.viewerProbe = {handle:viewer,lastKey};') + '\nexport function act({event}) { if(event.is("keyboard:down")) lastKey = event.key; }';
 const shell=(await readFile(new URL('../offline-index.html',import.meta.url),'utf8')).replace('src="aesthetic','src="/aesthetic').replace('href="aesthetic','href="/aesthetic');
 let handle='@fixture';
 await page.addInitScript(()=>{
   // Run the actual disk module in-process so its piece output can be asserted.
   window.Worker=undefined;
   if(location.hostname==='mime.ac') localStorage.setItem('session-aesthetic',encodeURIComponent(btoa(JSON.stringify({accessToken:'host-only-token',account:{sub:'host-only-id'}}))));
 });
 await page.route('**/*',async route=>{
   const u=new URL(route.request().url());
   if(u.hostname==='mime.ac'&&u.pathname==='/')return route.fulfill({contentType:'text/html',body:await readFile(new URL('mime/index.html',pub))});
   if(u.pathname==='/api/mime')return route.fulfill({json:u.searchParams.has('me')?{handle}:{recent:[{code:'wgpiece_wgViewer',when:new Date().toISOString(),name:'@author',media:{kind:'wgpiece',code:'wgViewer'},file:{type:'text/javascript',name:'viewer',url:'/fixture'}}],hasMore:false}});
   if(u.pathname==='/api/mime-feed')return route.fulfill({json:{source:probe}});
   if(u.pathname==='/wipe')return route.fulfill({contentType:'text/html',body:shell});
   if(u.pathname.startsWith('/api/')||!['GET','HEAD'].includes(route.request().method()))return route.fulfill({json:{}});
   const file=new URL('.'+u.pathname+(u.pathname.endsWith('/')?'index.html':''),pub);
   if(!file.href.startsWith(pub.href))return route.abort();
   try{return route.fulfill({body:await readFile(file).catch(()=>readFile(new URL("aesthetic.computer"+u.pathname,pub))),contentType:({'.mjs':'text/javascript','.js':'text/javascript','.html':'text/html','.json':'application/json','.ttf':'font/ttf','.woff2':'font/woff2','.css':'text/css','.png':'image/png'})[extname(file.pathname)]||'application/octet-stream'});}catch{return route.fulfill({status:404,body:''});}
 });
 try{
   await page.goto('https://mime.ac/?ac-automation=1');
   await page.waitForFunction(()=>document.querySelector('.piece-preview iframe')?.dataset.ready==='true',null,{timeout:45000});
   await page.locator('.mime-startup').waitFor({state:'detached'});
   const runtime=page.frames().find(f=>new URL(f.url()).pathname==='/mime');
   await runtime.waitForFunction(()=>globalThis.viewerProbe?.handle==='@fixture');
   assert.equal(await runtime.evaluate(()=>window.acUSER??null),null);
   assert.equal(await runtime.evaluate(()=>localStorage.getItem('session-aesthetic')),null);
   await page.mouse.click(195,422);
   for(const key of ['Escape','Enter','Backspace','`']){
     await page.keyboard.press(key);
     await runtime.waitForFunction(key=>globalThis.viewerProbe?.lastKey===key,key);
     assert.equal(new URL(runtime.url()).pathname,'/mime');
     assert.equal(await runtime.evaluate(()=>viewerProbe.handle),'@fixture');
   }
   await page.screenshot({path:'/tmp/mime-viewer-signed-in.png'});
   // Unknown origins and sibling frames cannot replace the parent identity.
   const player=page.frames().find(f=>new URL(f.url()).pathname==='/mime-play/');
   await player.evaluate(()=>dispatchEvent(new MessageEvent('message',{source:parent,origin:'https://untrusted.invalid',data:{type:'mime:viewer',handle:'@intruder'}})));
   assert.equal(await runtime.evaluate(()=>viewerProbe.handle),'@fixture');
   handle='@second';
   await page.evaluate(()=>dispatchEvent(new Event('focus')));
   await runtime.waitForFunction(()=>viewerProbe?.handle==='@second');
   await page.evaluate(()=>{localStorage.removeItem('session-aesthetic');dispatchEvent(new Event('focus'));});
   await runtime.waitForFunction(()=>viewerProbe?.handle===null);
   await page.screenshot({path:'/tmp/mime-viewer-signed-out.png'});
   assert.deepEqual(errors,[]);
 }catch(e){console.log({errors,logs,frames:await Promise.all(page.frames().map(async f=>({url:f.url(),state:await f.evaluate(()=>({preloaded:window.preloaded,send:!!window.acSEND,noauth:window.acNOAUTH,ready:window.__mimeReady,body:document.body.innerText.slice(0,150)})).catch(()=>null)})))});await page.screenshot({path:'/tmp/mime-viewer-failure.png'});throw e;}finally{await browser.close();}
});
