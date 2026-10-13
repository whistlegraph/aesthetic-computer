const assert=require('node:assert/strict'),{readFile}=require('node:fs/promises'),{resolve,extname}=require('node:path'),puppeteer=require('puppeteer');
(async()=>{const browser=await puppeteer.launch({headless:true,executablePath:process.env.CHROME_PATH||'/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'});try{
 const page=await browser.newPage(),html=await readFile('system/public/mime/index.html','utf8');await page.setViewport({width:430,height:800});await page.setRequestInterception(true);
 const errors=[];page.on('pageerror',error=>errors.push(error.message));
 page.on('request',async request=>{const u=new URL(request.url());
  if(u.hostname==='mime.ac'&&u.pathname==='/')return request.respond({contentType:'text/html',body:html});
  if(u.pathname==='/api/mime'&&u.searchParams.has('file'))return request.respond({contentType:'application/x-doom-wad',headers:{'Access-Control-Allow-Origin':'*'},body:await readFile('system/public/doom-player/examples/first-arena.wad')});
  if(u.pathname==='/api/mime')return request.respond({contentType:'application/json',body:JSON.stringify({recent:[{code:'wadfixture',parent:null,name:'@maker',when:new Date().toISOString(),file:{name:'arena.wad',type:'application/x-doom-wad',url:'/api/mime?file=wadfixture'}}],hasMore:false})});
  if(u.pathname.startsWith('/doom-player/')){
   const path=resolve('system/public','.'+u.pathname+(u.pathname.endsWith('/')?'index.html':''));
   try{return request.respond({contentType:({'.html':'text/html','.mjs':'text/javascript','.js':'text/javascript','.wasm':'application/wasm'})[extname(path)]||'application/octet-stream',body:await readFile(path)});}catch{return request.respond({status:404,body:''});}
  }
  return request.respond({status:200,body:''});
 });
 await page.goto('https://mime.ac/?ac-automation=1');await page.waitForSelector('.doom-player');await page.waitForFunction(()=>!document.querySelector('.mime-startup'));
 const label=await page.$eval('.reel-command .media-label',el=>el.getAttribute('aria-label'));assert.equal(label,'doom~wadfixture');
 const url=await page.$eval('.doom-player',el=>el.src);assert.equal(new URL(url).pathname,'/doom-player/');assert.equal(new URL(url).searchParams.get('file'),'https://mime.ac/api/mime?file=wadfixture');assert.equal(new URL(url).searchParams.get('preview'),'1');
 const frame=await page.$('.doom-player'),box=await frame.boundingBox();assert.ok(box.width>400&&box.height>700,'WAD player fills the reel');
 const player=await frame.contentFrame();await player.waitForFunction(()=>document.body.dataset.state==='playing'&&window.__mimeReady,{timeout:60000});
 assert.equal(await player.$eval('#boot',el=>getComputedStyle(el).display),'none','no file picker or Play gate');
 assert.equal(await player.$eval('#controls',el=>getComputedStyle(el).display),'none','no control panel shrinking playback');
 const dimensions=await player.$eval('canvas',el=>({width:el.clientWidth,height:el.clientHeight,viewportWidth:innerWidth,viewportHeight:innerHeight}));
 assert.equal(dimensions.width,dimensions.viewportWidth);assert.equal(dimensions.height,dimensions.viewportHeight);
 const loaded=await player.evaluate(()=>Array.from(Module.FS.readFile('/level.wad')));assert.deepEqual(Buffer.from(loaded),await readFile('system/public/doom-player/examples/first-arena.wad'),'plays the published WAD');
 await page.click('.reel-command .media-label');
 assert.equal(await page.$eval('.doom-player',el=>el.inert),false,'entering interaction enables the game');
 assert.equal(await page.evaluate(()=>document.activeElement?.classList.contains('doom-player')),true,'entering interaction focuses the game');
 await player.evaluate(()=>{window.movementEvents=[];for(const type of ['keydown','keyup'])window.addEventListener(type,e=>{if(['ArrowUp','ArrowDown','Comma','Period'].includes(e.code))movementEvents.push([type,e.code]);});});
 for(const code of ['KeyW','KeyS','KeyA','KeyD']){await page.keyboard.down(code);await page.keyboard.up(code);}
 assert.deepEqual(await player.evaluate(()=>movementEvents),['ArrowUp','ArrowDown','Comma','Period'].flatMap(code=>[['keydown',code],['keyup',code]]),'WASD reaches the embedded player');
 await page.keyboard.down('KeyW');await page.keyboard.down('KeyD');
 await player.evaluate(()=>window.dispatchEvent(new Event('blur')));
 assert.deepEqual(await player.evaluate(()=>movementEvents.slice(-2)),[['keyup','ArrowUp'],['keyup','Period']],'losing focus releases diagonal movement');
 await page.keyboard.up('KeyW');await page.keyboard.up('KeyD');
 const originalURL=page.url(),playerURL=player.url();assert.equal(new URL(playerURL).searchParams.get('mime'),'1');
 for(const stopped of [false,true]){
  if(stopped)await player.evaluate(()=>Module.onExit(0));
  if(stopped)assert.equal(await player.$eval('#status',el=>el.textContent),'Doom stopped (0)');
  const navigation=player.waitForNavigation({waitUntil:'domcontentloaded'});
  await page.keyboard.press('Escape');await navigation;
  await player.waitForFunction(()=>document.body.dataset.state==='playing'&&window.__mimeReady,{timeout:60000});
  assert.equal(player.url(),playerURL,'Escape reloads the same WAD');
  assert.equal(page.url(),originalURL,'Escape keeps the same MIME post');
  assert.equal(await player.$eval('#boot',el=>getComputedStyle(el).display),'none','restart clears the stopped screen');
  assert.equal(await page.$eval('.doom-player',el=>el.inert),false,'restart keeps interaction enabled');
 }
 for(const viewport of [{width:430,height:800},{width:700,height:334},{width:1200,height:800}]){
  await page.setViewport(viewport);
  const corner=await page.$eval('.reel-type',el=>{const label=el.getBoundingClientRect(),card=el.closest('.feed-post').getBoundingClientRect();return {right:card.right-label.right,bottom:card.bottom-label.bottom};});
  assert.ok(Math.abs(corner.right-4)<1&&Math.abs(corner.bottom-4)<1,'MIME label stays at the bottom-right card edge: '+JSON.stringify(corner));
 }
 await page.setViewport({width:700,height:334});
 await page.screenshot({path:'/tmp/ac-mime-doom-autoplay.png'});assert.deepEqual(errors,[]);
 console.log('PASS MIME WAD: global identifier, published WAD auto-starts in WASM, full-frame canvas, no file picker or controls');
 }finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
