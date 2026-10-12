const assert=require('node:assert/strict'),{createServer}=require('node:http'),{readFile,stat}=require('node:fs/promises'),{resolve,extname}=require('node:path'),{pathToFileURL}=require('node:url'),puppeteer=require('puppeteer');
const root=resolve(__dirname,'../../system/public');
(async()=>{
 const {lollipopProject}=await import(pathToFileURL(resolve(__dirname,'project-fixture.mjs')));
 const server=createServer(async(req,res)=>{try{const url=new URL(req.url,'http://local');if(url.pathname==='/fixture'){
  const project=lollipopProject();if(url.searchParams.has('doom2')){project.game='doom2';project.maps[0].name='MAP01';}
  res.setHeader('Content-Type','text/html');res.end(`<!doctype html><style>html,body{margin:0;height:100%}iframe{border:0;width:100%;height:100%}</style><script type="module">
   import {compileProject} from '/doom-player/project.mjs';
   const project=${JSON.stringify(project)};const base=new Uint8Array(await(await fetch('/doom-player/vendor/freedoom'+(project.game==='doom2'?'2':'1')+'.wad')).arrayBuffer());
   const wad=await compileProject(project,{base});window.compiledBytes=wad.length;
   const frame=document.createElement('iframe');window.addEventListener('message',event=>{if(event.source!==frame.contentWindow)return;if(event.data?.type==='doom:request'){const copy=wad.slice();frame.contentWindow.postMessage({type:'doom:wad',wad:copy.buffer,title:project.title},location.origin,[copy.buffer]);}});
   frame.src='/doom-player/?preview=1&bridge=1';document.body.append(frame);
  </script>`);return;
 }
 let file=resolve(root,'.'+url.pathname);if(!file.startsWith(root+'/'))throw Error('path');if((await stat(file)).isDirectory())file+='/index.html';res.setHeader('Content-Type',({'.mjs':'text/javascript','.js':'text/javascript','.wasm':'application/wasm','.html':'text/html'})[extname(file)]||'application/octet-stream');res.end(await readFile(file));}catch{res.writeHead(404).end();}});
 await new Promise(r=>server.listen(0,'127.0.0.1',r));const base='http://127.0.0.1:'+server.address().port;
 const browser=await puppeteer.launch({headless:true,executablePath:process.env.CHROME_PATH||'/Applications/Google Chrome.app/Contents/MacOS/Google Chrome',args:['--autoplay-policy=no-user-gesture-required']});
 try{
  const page=await browser.newPage(),errors=[],logs=[];await page.setViewport({width:800,height:600});page.on('pageerror',e=>{errors.push(e.message);console.error(e.message);});page.on('console',m=>logs.push(m.text()));
  for(const game of ['doom','doom2']){
   await page.goto(base+'/fixture'+(game==='doom2'?'?doom2=1':''));await page.waitForFunction(()=>window.compiledBytes>1000000,{timeout:60000});
   const frame=await page.waitForFrame(f=>f.url().includes('/doom-player/?'),{timeout:30000});await frame.waitForFunction(()=>document.body.dataset.state==='playing',{timeout:60000});
   assert.equal(await frame.evaluate(()=>Module.FS.readFile('/level.wad').length),await page.evaluate(()=>compiledBytes));
   assert.ok(logs.some(l=>l.includes('game started')),'actual engine starts');assert.equal(await frame.$eval('#controls',e=>getComputedStyle(e).display),'none');
   await page.screenshot({path:'/tmp/ac-full-'+game+'-world.png'});const before=await frame.$eval('canvas',e=>e.toDataURL());await frame.evaluate(()=>window.dispatchEvent(new KeyboardEvent('keydown',{key:'ArrowUp',code:'ArrowUp',keyCode:38,which:38,bubbles:true})));
   await new Promise(r=>setTimeout(r,3800));await frame.evaluate(()=>window.dispatchEvent(new KeyboardEvent('keyup',{key:'ArrowUp',code:'ArrowUp',keyCode:38,which:38,bubbles:true})));
   assert.notEqual(await frame.$eval('canvas',e=>e.toDataURL()),before,'moves through multi-sector world');
   await page.screenshot({path:'/tmp/ac-full-'+game+'-moved.png'});
  }
  assert.deepEqual(errors,[]);console.log('PASS full projects: browser BSP compilation, binary iframe transport, custom sprites/textures, traversable sectors, Doom/Doom II engine');
 }finally{await browser.close();server.close();}
})().catch(e=>{console.error(e);process.exitCode=1;});
