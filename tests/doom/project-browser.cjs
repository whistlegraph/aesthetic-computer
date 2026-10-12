const assert=require('node:assert/strict'),{createServer}=require('node:http'),{readFile,stat}=require('node:fs/promises'),{resolve,extname}=require('node:path'),{pathToFileURL}=require('node:url'),puppeteer=require('puppeteer'),{createHash}=require('node:crypto');
const root=resolve(__dirname,'../../system/public');
(async()=>{
 const {lollipopProject}=await import(pathToFileURL(resolve(__dirname,'project-fixture.mjs')));
 const server=createServer(async(req,res)=>{try{const url=new URL(req.url,'http://local');if(url.pathname==='/fixture'){
  const project=lollipopProject();if(url.searchParams.has('doom2')){project.game='doom2';project.maps[0].name='MAP01';}
  res.setHeader('Content-Type','text/html');res.end(`<!doctype html><style>html,body{margin:0;height:100%}iframe{border:0;width:100%;height:100%}</style><script type="module">
   const project=${JSON.stringify(project)};
   const frame=document.createElement('iframe');window.addEventListener('message',event=>{if(event.source!==frame.contentWindow)return;
    if(event.data?.type==='doom:request'){frame.contentWindow.postMessage({type:'doom:compile',id:1,operation:'project',project},location.origin);}
    if(event.data?.type==='doom:compiled'){if(event.data.error){window.fixtureError=event.data.error;return;}window.compiledBytes=event.data.wad.byteLength;frame.contentWindow.postMessage({type:'doom:wad',wad:event.data.wad,title:project.title},location.origin,[event.data.wad]);}
   });
   frame.src='/doom-player/?preview=1&bridge=1';document.body.append(frame);
  </script>`);return;
 }
 let file=resolve(root,'.'+url.pathname);if(!file.startsWith(root+'/'))throw Error('path');if((await stat(file)).isDirectory())file+='/index.html';res.setHeader('Content-Type',({'.mjs':'text/javascript','.js':'text/javascript','.wasm':'application/wasm','.html':'text/html'})[extname(file)]||'application/octet-stream');res.end(await readFile(file));}catch{res.writeHead(404).end();}});
 await new Promise(r=>server.listen(0,'127.0.0.1',r));const base='http://127.0.0.1:'+server.address().port;
 const browser=await puppeteer.launch({headless:true,executablePath:process.env.CHROME_PATH||'/Applications/Google Chrome.app/Contents/MacOS/Google Chrome',args:['--autoplay-policy=no-user-gesture-required']});
 try{
  const page=await browser.newPage(),errors=[],logs=[];await page.setViewport({width:800,height:600});page.on('pageerror',e=>{errors.push(e.message);console.error(e.message);});page.on('console',m=>logs.push(m.text()));
  for(const game of ['doom','doom2']){
   await page.goto(base+'/fixture'+(game==='doom2'?'?doom2=1':''));await page.waitForFunction(()=>window.compiledBytes>1000000||window.fixtureError,{timeout:60000});
   assert.equal(await page.evaluate(()=>window.fixtureError),undefined);const frame=await page.waitForFrame(f=>f.url().includes('/doom-player/?'),{timeout:30000});await frame.waitForFunction(()=>document.body.dataset.state==='playing',{timeout:60000});
   assert.equal(await frame.evaluate(()=>Module.FS.readFile('/level.wad').length),await page.evaluate(()=>compiledBytes));
   assert.ok(logs.some(l=>l.includes('game started')),'actual engine starts');assert.equal(await frame.$eval('#controls',e=>getComputedStyle(e).display),'none');
   await page.screenshot({path:'/tmp/ac-full-'+game+'-world.png'});
   const canvas=await frame.$('canvas'),hash=async()=>createHash('sha256').update(await canvas.screenshot()).digest('hex'),before=await hash();
   await frame.click('#canvas');await page.keyboard.down('ArrowUp');
   await new Promise(r=>setTimeout(r,3800));await page.keyboard.up('ArrowUp');
   assert.notEqual(await hash(),before,'moves through multi-sector world');
   await page.screenshot({path:'/tmp/ac-full-'+game+'-moved.png'});
  }
  assert.deepEqual(errors,[]);console.log('PASS full projects: browser BSP compilation, binary iframe transport, custom sprites/textures, traversable sectors, Doom/Doom II engine');
 }finally{await browser.close();server.close();}
})().catch(e=>{console.error(e);process.exitCode=1;});
