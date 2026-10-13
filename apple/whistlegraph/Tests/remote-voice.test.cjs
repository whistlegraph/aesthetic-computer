const assert=require('node:assert/strict');
const {resolve,extname}=require('node:path');
const {readFile}=require('node:fs/promises');
const {createServer}=require('node:http');
const puppeteer=require('puppeteer');
const root=resolve(__dirname,'../Resources/Web');
const server=createServer(async(req,res)=>{
 try {
  const path=resolve(root,'.'+new URL(req.url,'http://local').pathname);
  if(!path.startsWith(root+'/'))throw Error('path');
  res.setHeader('Content-Type',({'.html':'text/html','.mjs':'text/javascript','.js':'text/javascript','.json':'application/json'})[extname(path)]||'application/octet-stream');
  res.end(await readFile(path.endsWith('/engine.mjs')&&process.env.REMOTE_VOICE_ENGINE?process.env.REMOTE_VOICE_ENGINE:path));
 }catch{res.writeHead(404).end();}
});
(async()=>{
 await new Promise(r=>server.listen(0,'127.0.0.1',r));
 const browser=await puppeteer.launch({headless:true,executablePath:'/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'});
 try {
  const page=await browser.newPage();page.setDefaultTimeout(12000);
  const errors=[];page.on('pageerror',e=>errors.push(e.message));
  await page.evaluateOnNewDocument(()=>{
   window.__whistlegraphNativeShell=true;window.__nativeMessages=[];
   window.webkit={messageHandlers:{whistlegraph:{postMessage:m=>__nativeMessages.push(m)}}};
  });
  await page.evaluateOnNewDocument(await readFile(resolve(__dirname,'../Resources/Fixtures/remote-voice.js'),'utf8'));
  await page.goto('http://127.0.0.1:'+server.address().port+'/index.html?whistlegraph=1');
  await page.waitForFunction(()=>typeof whistlegraphAsk==='function');
  await page.evaluate(()=>whistlegraphEngineEvent({kind:'account',token:'fixture-only'}));
  await page.waitForFunction(()=>__nativeMessages.some(m=>m.action==='snapshot'&&m.snapshot.handle==='fixture'));
  for(const [index,outcome] of ['done','done','failed','rejected','waiting','stalled'].entries()) {
   const id=await page.evaluate(({outcome,index})=>{
    window.__remoteVoiceOutcome=outcome;__nativeMessages.length=0;
    voiceStart();const id=__nativeMessages.find(m=>m.action==='start')?.id;
    whistlegraphNativeEvent({id,kind:'listening'});voiceEnd();
    whistlegraphNativeEvent(index===1?{id,kind:'mixedFinal',text:JSON.stringify({transcript:'Add three people',words:[{text:'people',atMs:600,durationMs:200}],sound:{schema:'whistlegraph-sound/v1',durationMs:1200,audibleMs:1000,frames:[{atMs:600,rms:.2,pitchHz:220}],onsetsMs:[400]}})}:{id,kind:'final',text:'Add three people'});return id;
   },{outcome,index});
   assert.ok(id,'A second voice request can start without relaunching');
   if(outcome==='waiting'||outcome==='stalled') {
    await page.waitForFunction(()=>whistlegraphIsBusy());
    if(outcome==='stalled')await page.waitForFunction(()=>typeof __releaseRemotePoll==='function');
    await page.evaluate(()=>whistlegraphNativeCommand({action:'stop'}));
   }
   await page.waitForFunction(()=>__nativeMessages.some(m=>m.action==='voiceIdle'));
   assert.equal(await page.evaluate(()=>whistlegraphRecording()),false,'Voice processing finishes');
   assert.equal(await page.evaluate(()=>whistlegraphIsBusy()),false,'Generation finishes');
   assert.equal(await page.$eval('#speak',e=>e.disabled),false,'Talk unlocks');
   if(outcome==='done')assert.equal(await page.evaluate(()=>JSON.parse(localStorage.getItem('whistlegraph-source-versions')).head),index+1,'Saved version is adopted before unlocking');
   console.log('PASS remote voice '+outcome+' restores input');
  }
  await page.evaluate(()=>{
   window.__remoteVoiceOutcome='waiting';__nativeMessages.length=0;voiceStart();
   const id=__nativeMessages.find(m=>m.action==='start').id;
   whistlegraphNativeEvent({id,kind:'listening'});voiceEnd();whistlegraphNativeEvent({id,kind:'final',text:'Another request'});
  });
  await page.waitForFunction(()=>whistlegraphIsBusy());
  await page.evaluate(async()=>{__releaseRemotePoll();await new Promise(r=>setTimeout(r,100));});
  assert.equal(await page.evaluate(()=>whistlegraphIsBusy()&&whistlegraphRecording()),true,'A stale poll cannot release a newer voice request');
  await page.evaluate(()=>whistlegraphNativeCommand({action:'stop'}));
  await page.waitForFunction(()=>!whistlegraphRecording());
  console.log('PASS stale result leaves the next voice request running');
  assert.deepEqual(errors,[]);
 }finally{await browser.close();server.close();}
})().catch(e=>{console.error(e);process.exitCode=1;});
