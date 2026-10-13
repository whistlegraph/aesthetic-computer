const assert=require('node:assert/strict'),{resolve,extname}=require('node:path'),{readFile}=require('node:fs/promises'),{createServer}=require('node:http'),{createHash}=require('node:crypto'),puppeteer=require('puppeteer');
const root=resolve(__dirname,'../Resources/Web');
const server=createServer(async(req,res)=>{
 try{const path=resolve(root,'.'+new URL(req.url,'http://local').pathname);if(!path.startsWith(root+'/'))throw Error('path');res.setHeader('Content-Type',({'.html':'text/html','.mjs':'text/javascript','.js':'text/javascript','.json':'application/json'})[extname(path)]||'application/octet-stream');res.end(await readFile(path));}catch{res.writeHead(404).end();}
});
(async()=>{
 await new Promise(r=>server.listen(0,'127.0.0.1',r));
 const browser=await puppeteer.launch({headless:true,executablePath:'/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'});
 try{
  for(const ware of ['piece','painting','doom']){
   const context=await browser.createBrowserContext(),page=await context.newPage();page.setDefaultTimeout(60000);
   await page.evaluateOnNewDocument(ware=>{
    localStorage.setItem('whistlegraph-ware',ware);window.__whistlegraphNativeShell=true;window.__whistlegraphDisableThread=true;
    window.__whistlegraphRemixFixtureIdentity={id:'00000000-0000-4000-8000-000000000001',code:'fixture-old'};
    window.__nativeMessages=[];window.webkit={messageHandlers:{whistlegraph:{postMessage:m=>__nativeMessages.push(m)}}};
    const original=window.fetch.bind(window);
    window.fetch=function(url,init){
     if(this!==window)throw TypeError('Can only call Window.fetch on instances of Window');
     if(String(url).includes('/userinfo'))return Promise.resolve(Response.json({sub:'fixture-maker'}));
     if(String(url).includes('/handle?for='))return Promise.resolve(Response.json({handle:'fixture'}));
     if(String(url).includes('/api/handle-colors'))return Promise.resolve(Response.json({colors:[]}));
     if(String(url).includes('/api/easel-inference'))throw Error('Remixing must not request inference');
     return original(url,init);
    };
   },ware);
   await page.goto('http://127.0.0.1:'+server.address().port+'/index.html?whistlegraph=1');
   await page.waitForFunction(()=>typeof window.whistlegraphRemix==='function');
   await page.evaluate(()=>whistlegraphEngineEvent({kind:'account',token:'fixture'}));
   await page.waitForFunction(()=>__nativeMessages.some(m=>m.action==='snapshot'&&m.snapshot.handle?.includes('fixture')));
   const bytes=ware==='piece'?Buffer.from('export function paint({wipe}){wipe("#76d17b");}'):await readFile(ware==='painting'?'apple/whistlegraph/Resources/Fixtures/remix.png':'system/public/doom-player/examples/empty-room.wad');
   const reference={code:'original',handle:'@artist',version:7,url:'https://mime.ac/p/original',hash:createHash('sha256').update(bytes).digest('hex')};
   const payload={ware,title:'Remix',reference,...(ware==='piece'?{source:bytes.toString()}:{data:bytes.toString('base64')})};
   const reload=page.waitForNavigation();
   assert.equal((await page.evaluate(p=>whistlegraphRemix(p),payload)).accepted,true);await reload;
   await page.waitForFunction(()=>__nativeMessages.some(m=>m.action==='snapshot'&&m.snapshot.versions?.[0]?.remixURL));
   const snapshot=await page.evaluate(()=>__nativeMessages.filter(m=>m.action==='snapshot').at(-1).snapshot);
   assert.equal(snapshot.head,0);assert.equal(snapshot.versions.length,1);assert.equal(snapshot.versions[0].remixURL,reference.url);assert.match(snapshot.versions[0].utterance,/Stole from @artist\/original · v7/);
   if(ware==='painting'){
    assert.deepEqual(snapshot.painting,{width:32,height:48});
    await page.waitForFunction(()=>document.getElementById('painting-preview')?.width===32);
    assert.deepEqual(await page.evaluate(()=>Array.from(document.getElementById('painting-preview').getContext('2d').getImageData(16,24,1,1).data)),[30,90,210,255]);
   }
   if(ware==='doom'){
    const runtime=await page.waitForFrame(frame=>frame.url().includes('/doom-player/'));
    await runtime.waitForFunction(()=>document.body.dataset.state==='playing');
   }
   console.log('PASS '+ware+' native engine remix saves and reloads v0 with original attribution');
   await context.close();
  }
 }finally{await browser.close();server.close();}
})().catch(e=>{console.error(e);process.exitCode=1;});
