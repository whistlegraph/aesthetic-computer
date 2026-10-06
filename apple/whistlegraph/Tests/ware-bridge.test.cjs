const assert=require('node:assert/strict');
const {resolve,extname}=require('node:path');
const {readFile,mkdir}=require('node:fs/promises');
const http=require('node:http');
const puppeteer=require('puppeteer');
const root=resolve(__dirname,'../Resources/Web');
const server=http.createServer(async(req,res)=>{
  try{
    const path=resolve(root,'.'+new URL(req.url,'http://local').pathname);
    if(!path.startsWith(root+'/'))throw Error('bad path');
    const data=await readFile(path);
    res.setHeader('Content-Type',({'.html':'text/html','.mjs':'text/javascript','.js':'text/javascript','.json':'application/json'})[extname(path)]||'application/octet-stream');res.end(data);
  }catch{res.writeHead(404);res.end();}
});
(async()=>{
  await new Promise(r=>server.listen(0,'127.0.0.1',r));
  const browser=await puppeteer.launch({headless:true,executablePath:process.env.CHROME_PATH||'/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'});
  try{
    const page=await browser.newPage(),errors=[];page.on('pageerror',e=>errors.push(e.message));
    await page.setViewport({width:430,height:322,deviceScaleFactor:2});
    await page.setRequestInterception(true);page.on('request',r=>r.url().startsWith('http://127.0.0.1:')?r.continue():r.respond({status:200,contentType:'text/html',body:'Preview fixture'}));
    const ledger={format:1,head:1,versions:[{id:0,parent:null,source:'// Walkieware v0 — base color.',request:null,createdAt:'2026-10-05T00:00:00Z'},{id:1,parent:0,source:'export function paint({wipe}) {wipe("pink");}',request:'Pink piece',createdAt:'2026-10-05T00:01:00Z'}]};
    const guides=Object.fromEntries(await Promise.all(['pieces.md','screen.md','hand.md','kidlisp.md','api.json'].map(async n=>['/easel/context/'+n,await readFile(resolve(root,'easel/context',n),'utf8')])));
    await page.evaluateOnNewDocument((ledger,guides)=>{
      if(!localStorage.getItem('fixture-seeded')){localStorage.setItem('walkieware-source-versions',JSON.stringify(ledger));localStorage.setItem('fixture-seeded','1');}
      window.__whistlegraphNativeShell=true;window.__whistlegraphDisableThread=true;window.__aeselGuides=guides;
      window.__nativeMessages=[];window.__roomRequests=[];window.__roomConfigured=false;window.__roomRevision=0;
      window.webkit={messageHandlers:{whistlegraph:{postMessage:m=>{__nativeMessages.push(m);if(m.action==='render')setTimeout(()=>window.whistlegraphEngineEvent({kind:'previewEvent',event:{kind:'painted'}}),10);}}}};
      const original=fetch.bind(window);
      window.fetch=(url,init)=>{
        if(String(url).includes('/userinfo'))return Promise.resolve(Response.json({sub:'fixture-maker'}));
        if(String(url).includes('/handle?for='))return Promise.resolve(Response.json({handle:'fixture'}));
        if(String(url).includes('/api/handle-colors'))return Promise.resolve(Response.json({colors:[]}));
        if(String(url).includes('/api/whistlegraph-roblox')){
          __roomRequests.push({method:init.method,...(init.body?{body:JSON.parse(init.body)}:{})});
          return Promise.resolve(Response.json(init.method==='GET'?{available:__roomConfigured}:{revision:++__roomRevision,launchURL:'https://www.roblox.com/share?code=fixture&type=ExperienceDetails'}));
        }
        if(String(url).includes('/api/easel-inference'))throw Error('Local edits must not use paid inference');
        return original(url,init);
      };
    },ledger,guides);
    const ready=async ware=>{
      await page.waitForFunction(ware=>typeof whistlegraphNativeCommand==='function'&&__nativeMessages.some(m=>m.action==='snapshot'&&m.snapshot.ware===ware),{},ware);
      await page.evaluate(()=>{whistlegraphEngineEvent({kind:'previewReady'});whistlegraphEngineEvent({kind:'account',token:'fixture'});});
    };
    const last=()=>page.evaluate(()=>__nativeMessages.filter(m=>m.action==='snapshot').at(-1).snapshot);
    const change=async ware=>{await Promise.all([page.waitForNavigation({waitUntil:'load'}),page.evaluate(ware=>whistlegraphNativeCommand({action:'setWare',ware}),ware)]);await ready(ware);};
    await page.goto('http://127.0.0.1:'+server.address().port+'/index.html?whistlegraph=1');await ready('piece');
    assert.equal((await last()).head,1);
    const pieceBefore=await page.evaluate(()=>localStorage.getItem('whistlegraph-source-versions'));
    assert.deepEqual(JSON.parse(pieceBefore),ledger,'legacy history migrated without source changes');
    await change('roblox');assert.equal((await last()).head,0);
    await page.evaluate(()=>whistlegraphAsk('make it bounce'));assert.equal((await last()).head,1);
    const bouncing=await page.evaluate(()=>JSON.parse(localStorage.getItem('whistlegraph-roblox-room-versions')));
    assert.ok(JSON.parse(bouncing.versions[1].source).objects.find(o=>o.id==='bridge').bounce>0);
    await page.click('#room-draw');const canvas=await page.$('#room-plan'),box=await canvas.boundingBox();
    await page.mouse.move(box.x+box.width*.25,box.y+box.height*.65);await page.mouse.down();
    await page.mouse.move(box.x+box.width*.7,box.y+box.height*.25,{steps:10});await page.mouse.up();
    assert.equal((await last()).head,2);
    await page.evaluate(()=>whistlegraphNativeCommand({action:'exportRoom'}));
    const exported=await page.evaluate(()=>__nativeMessages.find(m=>m.action==='exportRoom').source);
    assert.match(exported,/<roblox/);assert.match(exported,/Whistlegraph/);assert.match(exported,/path-/);
    await mkdir(resolve(__dirname,'screenshots'),{recursive:true});
    await page.screenshot({path:resolve(__dirname,'screenshots/roblox-room.png')});
    await page.evaluate(()=>whistlegraphNativeCommand({action:'playRoblox'}));
    await page.waitForFunction(()=>__nativeMessages.filter(m=>m.action==='snapshot').at(-1).snapshot.roblox.notice.includes('not set up'));
    assert.equal(await page.evaluate(()=>__nativeMessages.some(m=>m.action==='openRoblox')),false);
    await page.evaluate(()=>{__roomConfigured=true;whistlegraphNativeCommand({action:'playRoblox'});});
    await page.waitForFunction(()=>__nativeMessages.some(m=>m.action==='openRoblox'));
    const requests=await page.evaluate(()=>__roomRequests);
    assert.equal(requests.at(-1).method,'POST');assert.equal(requests.at(-1).body.expectedRevision,0);
    assert.equal(requests.at(-1).body.playerId,undefined,'the app cannot claim a Roblox identity');
    await change('piece');assert.equal(await page.evaluate(()=>localStorage.getItem('whistlegraph-source-versions')),pieceBefore);
    await Promise.all([page.waitForNavigation({waitUntil:'load'}),page.evaluate(()=>whistlegraphAsk('switch to Roblox'))]);await ready('roblox');
    assert.equal((await last()).head,2,'room work survives switching away');
    assert.deepEqual(errors,[]);console.log('PASS ware bridge: migration, switching, drawing, versions, export, durable save and launch gating');
  }finally{await browser.close();server.close();}
})().catch(e=>{console.error(e);process.exitCode=1;server.close();});
