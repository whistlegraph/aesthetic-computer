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
 await page.screenshot({path:'/tmp/ac-mime-doom-autoplay.png'});assert.deepEqual(errors,[]);
 console.log('PASS MIME WAD: global identifier, published WAD auto-starts in WASM, full-frame canvas, no file picker or controls');
 }finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
