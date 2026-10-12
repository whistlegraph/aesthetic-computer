const assert=require('node:assert/strict'),{createServer}=require('node:http'),{readFile,stat}=require('node:fs/promises'),{resolve,extname}=require('node:path'),{createHash}=require('node:crypto'),puppeteer=require('puppeteer');
const root=resolve(__dirname,'../../system/public');
const server=createServer(async(req,res)=>{try{let file=resolve(root,'.'+new URL(req.url,'http://local').pathname);if(!file.startsWith(root+'/'))throw Error('path');if((await stat(file)).isDirectory())file+='/index.html';res.setHeader('Content-Type',({'.mjs':'text/javascript','.js':'text/javascript','.wasm':'application/wasm','.html':'text/html'})[extname(file)]||'application/octet-stream');res.end(await readFile(file));}catch{res.writeHead(404).end();}});
(async()=>{
 await new Promise(r=>server.listen(0,'127.0.0.1',r));const base='http://127.0.0.1:'+server.address().port;
 const browser=await puppeteer.launch({headless:true,executablePath:process.env.CHROME_PATH||'/Applications/Google Chrome.app/Contents/MacOS/Google Chrome',args:['--autoplay-policy=no-user-gesture-required']});
 try{
  const page=await browser.newPage(),errors=[],logs=[];page.on('pageerror',e=>errors.push(e.message));page.on('console',m=>logs.push(m.text()));
  await page.setViewport({width:430,height:800});
  await page.goto(base+'/doom-player/?file='+encodeURIComponent(base+'/doom-player/examples/'+(process.env.DOOM_TEST_WAD||'first-arena.wad')));
  await page.waitForFunction(()=>document.body.dataset.state==='ready',{timeout:60000});
  assert.ok(await page.evaluate(()=>window.__mimeReady));await page.click('#start');
  await page.waitForFunction(()=>document.body.dataset.state==='playing');
  await new Promise(r=>setTimeout(r,1500));assert.ok(logs.some(l=>l.includes('adding level.wad')));assert.ok(logs.some(l=>l.includes('game started')));
  const canvas=await page.$('canvas'),hash=async()=>createHash('sha256').update(await canvas.screenshot()).digest('hex');
  await page.evaluate(()=>__mimeSettle(false));await new Promise(r=>setTimeout(r,300));const paused=await hash();await new Promise(r=>setTimeout(r,200));assert.equal(await hash(),paused,'paused engine holds the frame');
  await page.evaluate(()=>__mimeSettle(true));await page.keyboard.down('ArrowUp');await new Promise(r=>setTimeout(r,550));await page.keyboard.up('ArrowUp');await page.keyboard.down('Control');await new Promise(r=>setTimeout(r,250));await page.keyboard.up('Control');
  await page.evaluate(()=>__mimeSettle(false));assert.notEqual(await hash(),paused,'running engine advances after movement and fire');
  await page.screenshot({path:'/tmp/ac-doom-playable.png'});assert.deepEqual(errors,[]);
  await page.goto(base+'/doom-player/?preview=1');
  await page.waitForFunction(()=>document.body.dataset.state==='playing'&&window.__mimeReady,{timeout:60000});
  assert.equal(await page.$eval('#boot',el=>getComputedStyle(el).display),'none');
  assert.equal(await page.$eval('#controls',el=>getComputedStyle(el).display),'none');
  assert.equal(await page.$eval('canvas',el=>el.getAttribute('aria-label')),'Empty Doom room');
  await page.screenshot({path:'/tmp/ac-doom-empty-room.png'});
  await page.evaluate(()=>{
    window.__touchKeys=[];window.addEventListener('keydown',e=>__touchKeys.push(['down',e.code]));window.addEventListener('keyup',e=>__touchKeys.push(['up',e.code]));
    window.__touch=(type,id,x,y)=>canvas.dispatchEvent(new PointerEvent(type,{pointerType:'touch',pointerId:id,clientX:x,clientY:y,bubbles:true,cancelable:true}));
    // Synthetic pointers cannot acquire native capture; gesture events still exercise the runtime.
    canvas.setPointerCapture=()=>{};
    __touch('pointerdown',1,100,100);__touch('pointerup',1,100,100);
  });
  await new Promise(r=>setTimeout(r,160));
  assert.deepEqual(await page.evaluate(()=>__touchKeys),[['down','ControlLeft'],['up','ControlLeft']],'tap fires and releases');
  await page.evaluate(()=>{
    __touchKeys=[];__touch('pointerdown',1,100,100);__touch('pointermove',1,145,100);
    __touch('pointerdown',2,250,100);__touch('pointermove',2,285,140);
  });
  const keys=await page.evaluate(()=>__touchKeys);
  assert.ok(keys.some(k=>k[0]==='down'&&k[1]==='ArrowRight'),'drag looks right');
  assert.ok(keys.some(k=>k[0]==='down'&&k[1]==='ArrowUp'),'second finger moves forward');
  assert.ok(keys.some(k=>k[0]==='down'&&k[1]==='ArrowDown'),'second finger drags backward');
  assert.ok(keys.some(k=>k[0]==='down'&&k[1]==='Period'),'second finger strafes');
  await page.evaluate(()=>{__touch('pointerup',1,145,100);__touch('pointercancel',2,285,140);});
  assert.equal(await page.evaluate(()=>__touchKeys.some(k=>k[0]==='down'&&k[1]==='ControlLeft')),false,'drag and multi-touch never fire');
  assert.ok(await page.evaluate(()=>__touchKeys.some(k=>k[0]==='up'&&k[1]==='ArrowDown')),'cancel releases movement');
  assert.deepEqual(errors,[]);

  console.log('PASS Doom: real exported PWAD loaded in WASM, rendered, movement/fire, pause/resume');
 }finally{await browser.close();server.close();}
})().catch(error=>{console.error(error);process.exitCode=1;server.close();});
