const assert=require('node:assert/strict');
const {resolve,extname}=require('node:path');
const {readFile}=require('node:fs/promises');
const http=require('node:http');
const puppeteer=require('puppeteer');
const root=resolve(__dirname,'../Resources/Web');
const server=http.createServer(async(req,res)=>{
 try{const path=resolve(root,'.'+new URL(req.url,'http://local').pathname);if(!path.startsWith(root+'/'))throw Error('path');res.setHeader('Content-Type',({'.html':'text/html','.mjs':'text/javascript','.js':'text/javascript','.json':'application/json'})[extname(path)]||'application/octet-stream');res.end(await readFile(path));}catch{res.writeHead(404);res.end();}
});
(async()=>{
 await new Promise(r=>server.listen(0,'127.0.0.1',r));
 const browser=await puppeteer.launch({headless:true,executablePath:'/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'});
 try{
  const page=await browser.newPage(),errors=[];page.on('pageerror',e=>errors.push(e.message));await page.setViewport({width:430,height:430,deviceScaleFactor:2});
  await page.evaluateOnNewDocument(()=>{
   localStorage.setItem('whistlegraph-ware','painting');window.__whistlegraphNativeShell=true;window.__whistlegraphDisableThread=true;
   window.__nativeMessages=[];window.webkit={messageHandlers:{whistlegraph:{postMessage:m=>__nativeMessages.push(m)}}};
   const original=window.fetch.bind(window);
   window.fetch=function(url,init){
    if(this!==window)throw TypeError('Can only call Window.fetch on instances of Window');
    if(String(url).includes('/userinfo'))return Promise.resolve(Response.json({sub:'fixture-maker'}));
    if(String(url).includes('/handle?for='))return Promise.resolve(Response.json({handle:'fixture'}));
    if(String(url).includes('/api/handle-colors'))return Promise.resolve(Response.json({colors:Array.from({length:8},(_,i)=>({r:20*i,g:180,b:220}))}));
    if(String(url).includes('/api/easel-image')){(window.__imageRequests ||= []).push(JSON.parse(init.body));if(window.__imageFailure)return Promise.resolve(Response.json({error:'Provider unavailable; request not retried'},{status:502}));return Promise.resolve(Response.json({png:window.__imageResult,provenance:{provider:'fixture',model:'fixture',mode:'generate'}}));}
    if(String(url).includes('/api/mime')){window.__published=JSON.parse(init.body);return Promise.resolve(Response.json({code:'pngfixture'}));}
    if(String(url).includes('/api/easel-inference')){
     const req=JSON.parse(init.body);window.__requests=(window.__requests||0)+1;
     const last=req.messages.at(-1),isResult=Array.isArray(last.content)&&last.content.some(c=>c.type==='tool_result');
     let events=[{type:'message_delta',delta:{stop_reason:'end_turn'}}];
     if(!isResult){
      const p={format:1,title:'Pink Moon',width:64,height:64,background:'#102030',layers:[{id:'moon',kind:'ellipse',color:'#ff6699',x:16,y:16,width:32,height:32}]};
      if(window.__bad)p.width=9000;
      events=[{type:'content_block_start',index:0,content_block:{type:'tool_use',id:'painting-fixture',name:window.__remote?'artifact_generate':'artifact_painting'}},{type:'content_block_delta',index:0,delta:{type:'input_json_delta',partial_json:JSON.stringify(window.__remote?{prompt:'A pink moon photograph',title:'Generated Moon',edit:!!window.__edit}:{painting:p})}},{type:'content_block_stop',index:0},{type:'message_delta',delta:{stop_reason:'tool_use'}}];
     }
     return Promise.resolve(new Response(events.map(e=>'data: '+JSON.stringify(e)+'\n\n').join(''),{headers:{'Content-Type':'text/event-stream'}}));
    }
    return original(url,init);
   };
  });
  const url='http://127.0.0.1:'+server.address().port+'/index.html?whistlegraph=1';
  const ready=()=>page.waitForFunction(()=>__nativeMessages.some(m=>m.action==='snapshot'&&m.snapshot.ware==='painting'));
  const last=()=>page.evaluate(()=>__nativeMessages.filter(m=>m.action==='snapshot').at(-1).snapshot);
  await page.goto(url);await ready();assert.equal((await last()).head,0);
  await page.evaluate(()=>whistlegraphEngineEvent({kind:'account',token:'fixture'}));await page.waitForFunction(()=>__nativeMessages.filter(m=>m.action==='snapshot').at(-1).snapshot.colors.length===8);
  await page.evaluate(()=>whistlegraphNativeCommand({action:'ask',text:'Paint a pink moon'}));await page.waitForFunction(()=>__nativeMessages.filter(m=>m.action==='snapshot').at(-1).snapshot.head===1&&!whistlegraphIsBusy());
  assert.equal((await last()).caption,'Pink Moon');
  assert.deepEqual(await page.evaluate(()=>Array.from(document.getElementById('painting-preview').getContext('2d').getImageData(32,32,1,1).data)),[255,102,153,255]);
  await page.screenshot({path:'/tmp/whistlegraph-painting-preview.png'});
  await page.evaluate(()=>whistlegraphNativeCommand({action:'exportPng'}));
  const exported=await page.evaluate(()=>__nativeMessages.find(m=>m.action==='exportPng'));
  assert.equal(exported.filename,'pink-moon.png');assert.equal(Buffer.from(exported.data,'base64').subarray(0,8).toString('hex'),'89504e470d0a1a0a');
  await page.evaluate(()=>whistlegraphNativeCommand({action:'publishPainting'}));await page.waitForFunction(()=>__nativeMessages.filter(m=>m.action==='snapshot').at(-1).snapshot.code==='pngfixture'&&!whistlegraphIsBusy());
  const published=await page.evaluate(()=>__published);assert.equal(published.file.type,'image/png');assert.equal(published.file.name,'pink-moon.png');assert.equal(published.file.data,exported.data);
  const saved=await page.evaluate(()=>localStorage.getItem('whistlegraph-painting-versions'));
  await page.evaluate(()=>{window.__bad=true;whistlegraphNativeCommand({action:'ask',text:'Make the moon larger'});});await page.waitForFunction(()=>!whistlegraphIsBusy());
  assert.equal(await page.evaluate(()=>localStorage.getItem('whistlegraph-painting-versions')),saved,'invalid staging never saves a version');
  await page.evaluate(()=>whistlegraphNativeCommand({action:'undoPainting'}));await page.waitForFunction(()=>__nativeMessages.filter(m=>m.action==='snapshot').at(-1).snapshot.head===0&&!whistlegraphIsBusy());
  await page.evaluate(()=>whistlegraphNativeCommand({action:'checkout',version:1}));await page.waitForFunction(()=>__nativeMessages.filter(m=>m.action==='snapshot').at(-1).snapshot.head===1&&!whistlegraphIsBusy());
  await page.evaluate(()=>{whistlegraphNativeCommand({action:'newPiece'});});await page.waitForNavigation();await ready();assert.equal((await last()).head,0);assert.equal((await last()).code,'');
  await page.evaluate(()=>whistlegraphNativeCommand({action:'openPiece',piece:'legacy'}));await page.waitForNavigation();await ready();assert.equal((await last()).head,1);assert.equal((await last()).code,'pngfixture');
  // Imported pixels start their own v0, stay immutable through an AI revision,
  // and disappear only when opening a different project.
  await page.evaluate(data=>whistlegraphEngineEvent({kind:'importPng',name:'My Moon.png',data}),exported.data);await page.waitForNavigation();await ready();assert.equal((await last()).head,0);assert.equal((await last()).caption,'My Moon');
  await page.evaluate(()=>{window.__bad=false;whistlegraphEngineEvent({kind:'account',token:'fixture'});});
  await page.waitForFunction(()=>__nativeMessages.filter(m=>m.action==='snapshot').at(-1).snapshot.handle==='fixture');
  await page.evaluate(()=>whistlegraphNativeCommand({action:'ask',text:'Add a moon to the image'}));await page.waitForFunction(()=>__nativeMessages.filter(m=>m.action==='snapshot').at(-1).snapshot.head===1&&!whistlegraphIsBusy());
  const layers=await page.evaluate(()=>{const index=JSON.parse(localStorage.getItem('whistlegraph-painting-projects'));const ledger=JSON.parse(localStorage.getItem('whistlegraph-painting-project-'+index.current+'-versions'));return JSON.parse(ledger.versions.at(-1).source).layers;});
  assert.equal(layers[0].kind,'raster');assert.equal(await page.evaluate(async asset=>(await import('./painting-assets.mjs')).loadPaintingAsset(asset),layers[0].asset),exported.data);
  assert.equal(layers[0].data,undefined,'history references pixels without duplicating them');assert.equal(layers[1].kind,'ellipse');
  await page.evaluate(data=>{window.__remote=true;window.__imageResult=data;whistlegraphNativeCommand({action:'ask',text:'Generate a moon photograph'});},exported.data);
  await page.waitForFunction(()=>__nativeMessages.filter(m=>m.action==='snapshot').at(-1).snapshot.head===2&&!whistlegraphIsBusy());
  assert.equal((await last()).caption,'Generated Moon');
  assert.equal((await page.evaluate(()=>__imageRequests)).length,1);
  await page.evaluate(()=>{window.__edit=true;whistlegraphNativeCommand({action:'ask',text:'Edit the photograph with a cloud'});});
  await page.waitForFunction(()=>__nativeMessages.filter(m=>m.action==='snapshot').at(-1).snapshot.head===3&&!whistlegraphIsBusy());
  assert.equal(await page.evaluate(()=>__imageRequests.at(-1).reference),exported.data,'image edit sends the current rendered composite');
  const preserved=await page.evaluate(()=>{const index=JSON.parse(localStorage.getItem('whistlegraph-painting-projects'));return localStorage.getItem('whistlegraph-painting-project-'+index.current+'-versions');});
  await page.evaluate(()=>{window.__imageFailure=true;whistlegraphNativeCommand({action:'ask',text:'Edit the image again'});});await page.waitForFunction(()=>!whistlegraphIsBusy());
  assert.equal((await last()).head,3);assert.equal((await last()).attempt.status,'failed');
  assert.equal((await page.evaluate(()=>__imageRequests)).length,3,'one provider request per turn, including failure');
  assert.equal(await page.evaluate(()=>{const index=JSON.parse(localStorage.getItem('whistlegraph-painting-projects'));return localStorage.getItem('whistlegraph-painting-project-'+index.current+'-versions');}),preserved);
  assert.ok(preserved.length<4000,'raster revisions hold small references');
  await page.evaluate(()=>whistlegraphEngineEvent({kind:'account',token:''}));assert.equal((await last()).handle,'');assert.deepEqual((await last()).colors,[]);
  assert.deepEqual(errors,[]);console.log('Painting bridge: prompt, pixels, export, MIME, invalid rollback, undo, projects, PNG import, hosted generation/edit/failure and saved palette PASS');
 }finally{await browser.close();server.close();}
})().catch(error=>{console.error(error);process.exitCode=1;server.close();});
