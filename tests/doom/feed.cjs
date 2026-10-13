const assert=require('node:assert/strict'),{readFile}=require('node:fs/promises'),{resolve,extname}=require('node:path'),puppeteer=require('puppeteer');
(async()=>{const browser=await puppeteer.launch({headless:true,executablePath:process.env.CHROME_PATH||'/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'});try{
 const page=await browser.newPage(),html=await readFile('system/public/mime/index.html','utf8');await page.setViewport({width:430,height:800});await page.setRequestInterception(true);
 const errors=[];page.on('pageerror',error=>errors.push(error.message));
 page.on('request',async request=>{const u=new URL(request.url());
  if(u.hostname==='mime.ac'&&(u.pathname==='/'||u.pathname.startsWith('/p/')))return request.respond({contentType:'text/html',body:html});
  if(u.pathname==='/api/mime'&&u.searchParams.has('file'))return request.respond({contentType:'application/x-doom-wad',headers:{'Access-Control-Allow-Origin':'*'},body:await readFile('system/public/doom-player/examples/first-arena.wad')});
  if(u.pathname==='/api/mime')return request.respond({contentType:'application/json',body:JSON.stringify({recent:[{code:'wadfixture',parent:null,name:'@maker',when:new Date().toISOString(),file:{name:'arena.wad',type:'application/x-doom-wad',url:'/api/mime?file=wadfixture'}},{code:'nextfixture',parent:null,name:'@maker',board:'text/plain',text:'Next post',when:new Date().toISOString()}],hasMore:false})});
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
 await player.evaluate(()=>{window.movementEvents=[];for(const type of ['keydown','keyup'])window.addEventListener(type,e=>{if(['ArrowUp','ArrowDown','Comma','Period'].includes(e.code))movementEvents.push([type,e.code]);});});
 await page.evaluate(()=>{document.body.tabIndex=-1;document.body.focus();});
 await page.keyboard.press('KeyW');
 await page.waitForFunction(()=>document.activeElement?.classList.contains('doom-player'));
 assert.deepEqual(await player.evaluate(()=>movementEvents),[['keydown','ArrowUp'],['keyup','ArrowUp']],'first W activates and releases movement without a title click');
 assert.equal(await page.$eval('.doom-player',el=>el.inert),false,'entering interaction enables the game');
 assert.equal(await page.evaluate(()=>document.activeElement?.classList.contains('doom-player')),true,'entering interaction focuses the game');
 await player.evaluate(()=>movementEvents=[]);
 for(const code of ['KeyW','KeyS','KeyA','KeyD']){await page.keyboard.down(code);await page.keyboard.up(code);}
 assert.deepEqual(await player.evaluate(()=>movementEvents),['ArrowUp','ArrowDown','Comma','Period'].flatMap(code=>[['keydown',code],['keyup',code]]),'WASD reaches the embedded player');
 await page.keyboard.down('KeyW');await page.keyboard.down('KeyD');
 await player.evaluate(()=>window.dispatchEvent(new Event('blur')));
 assert.deepEqual(await player.evaluate(()=>movementEvents.slice(-2)),[['keyup','ArrowUp'],['keyup','Period']],'losing focus releases diagonal movement');
 await page.keyboard.up('KeyW');await page.keyboard.up('KeyD');
 // The global identifier opens Share; the player control exits interaction.
 await page.$eval('.feed-media[data-interacting] .media-toggle',button=>button.click());
 await page.click('.media-shield');
 assert.equal(await page.$eval('.doom-player',el=>el.inert),false,'clicking the game itself enables play');
 assert.equal(await page.evaluate(()=>document.activeElement?.classList.contains('doom-player')),true,'clicking game focuses it');
 const playerURL=player.url();assert.equal(new URL(playerURL).searchParams.get('mime'),'1');
 await player.evaluate(()=>{window.actionKeys=[];window.originalModule=Module;for(const type of ['keydown','keyup'])window.addEventListener(type,e=>{if(['ControlLeft','Space'].includes(e.code))actionKeys.push([type,e.code]);});});
 for(const code of ['Space','Enter','KeyE'])await page.keyboard.press(code);
 assert.deepEqual(await player.evaluate(()=>actionKeys),['ControlLeft','ControlLeft','Space'].flatMap(code=>[['keydown',code],['keyup',code]]),'Space/Enter fire and E uses doors');
 for(const method of ['escape','parent','wheel','stopped']){
  if(method==='parent')await page.evaluate(()=>document.body.focus());
  if(method==='stopped')await player.evaluate(()=>Module.onExit(0));
  if(method==='wheel'){
   const bounds=await frame.boundingBox();await page.mouse.move(bounds.x+bounds.width/2,bounds.y+bounds.height/2);
   await page.mouse.wheel({deltaY:180});
  }else await page.keyboard.press('Escape');
  await page.waitForFunction(()=>location.pathname==='/p/nextfixture');
  assert.equal(player.url(),playerURL,'leaving Doom keeps the player URL');
  assert.equal(await player.evaluate(()=>Module===originalModule),true,'Escape/wheel never reload Doom');
  assert.equal(await page.$eval('.doom-player',el=>el.inert),true,'leaving returns control to the feed');
  if(method==='stopped')break;
  await page.evaluate(()=>document.querySelector('#wadfixture').scrollIntoView({block:'start'}));
  await page.waitForFunction(()=>location.pathname==='/p/wadfixture');
  await page.evaluate(()=>document.body.focus());await page.keyboard.press('KeyW');
  await page.waitForFunction(()=>document.activeElement?.classList.contains('doom-player'));
 }
 await page.evaluate(()=>document.querySelector('#wadfixture').scrollIntoView({block:'start'}));
 await page.waitForFunction(()=>location.pathname==='/p/wadfixture');
 for(const viewport of [{width:430,height:800},{width:700,height:334},{width:1200,height:800}]){
  await page.setViewport(viewport);
  const corner=await page.$eval('.reel-type',el=>{const label=el.getBoundingClientRect(),card=el.closest('.feed-post').getBoundingClientRect();return {right:card.right-label.right,bottom:card.bottom-label.bottom};});
  assert.ok(Math.abs(corner.right-14)<1&&Math.abs(corner.bottom-18)<1,'MIME label matches the corner margins: '+JSON.stringify(corner));
 }
 await page.setViewport({width:700,height:334});
 await page.screenshot({path:'/tmp/ac-mime-doom-autoplay.png'});assert.deepEqual(errors,[]);
 console.log('PASS MIME Doom: autoplay, WASD, Space/Enter fire, E use, Escape advances without reload, wheel scrolling, return to play, corner labels');
 }finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
