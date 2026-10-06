// Real browser runtime + authenticated thread service. No mocked paint acknowledgements.
const {readFile,writeFile}=require('node:fs/promises');
const {resolve,extname}=require('node:path');
const {createServer}=require('node:http');
const {randomUUID}=require('node:crypto');
const puppeteer=require('puppeteer');
(async()=>{
 const {ACSession}=await import('../../../aesel/src/ac-session.mjs');
 const token=await new ACSession().token();if(!token)throw Error('Sign in first');
 const root=resolve(__dirname,'../Resources/Web');
 const source=await readFile(resolve(__dirname,'spaces/CF7BBDFF-0C91-4754-B1C3-1C7878099F31/spaceship.mjs'),'utf8');
 let ledger={format:1,head:0,versions:[{id:0,parent:null,source,request:null,createdAt:new Date().toISOString(),layers:0}]};
 const restored=process.env.WHISTLEGRAPH_THREAD_FILE?JSON.parse(await readFile(process.env.WHISTLEGRAPH_THREAD_FILE,'utf8')):null;
 if(restored)ledger=restored.ledger;
 const guides=Object.fromEntries(await Promise.all(['pieces.md','screen.md','hand.md','kidlisp.md','api.json'].map(async name=>['/easel/context/'+name,await readFile(resolve(root,'easel/context',name),'utf8')])));
 const swift=await readFile(resolve(__dirname,'../Sources/WhistlegraphAccount.swift'),'utf8');
 const preview=swift.match(/static let script = """\n([\s\S]*?)\n    """/)[1];
 const server=createServer(async(req,res)=>{try{const path=resolve(root,'.'+new URL(req.url,'http://local').pathname);if(!path.startsWith(root+'/'))throw Error();const bytes=await readFile(path);res.setHeader('Content-Type',({'.html':'text/html','.js':'text/javascript','.mjs':'text/javascript','.json':'application/json','.md':'text/plain'})[extname(path)]||'application/octet-stream');res.end(bytes);}catch{res.writeHead(404);res.end();}});
 await new Promise(r=>server.listen(0,'127.0.0.1',r));
 const browser=await puppeteer.launch({headless:true,args:['--window-size=430,1000'],executablePath:'/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'});
 await writeFile('/tmp/whistlegraph-browser-cdp.json',JSON.stringify({endpoint:browser.wsEndpoint()}));
 const page=await browser.newPage();await page.setViewport({width:430,height:900});
 let pendingSource='',threadID=randomUUID(),code='',renders=0;
 const render=async()=>{const frame=page.frames().find(f=>f.url().startsWith('https://aesthetic.computer/'));if(frame&&pendingSource)await frame.evaluate(({source,id})=>window.whistlegraphRender?.(source,id),{source:pendingSource,id:threadID});};
 await page.exposeFunction('__nativeBridge',async(body)=>{
  if(body.action==='account')await page.evaluate(token=>window.whistlegraphEngineEvent?.({kind:'account',token}),token);
  if(body.action==='render'){pendingSource=body.source;threadID=body.threadID||threadID;await render();}
  if(body.action==='previewReady'){await render();await page.evaluate(()=>window.whistlegraphEngineEvent?.({kind:'previewReady'}));}
  if(body.action==='previewEvent'){if(body.event.kind==='painted')renders++;await page.evaluate(event=>window.whistlegraphEngineEvent?.({kind:'previewEvent',event}),body.event);}
  if(body.action==='threadStatus'&&body.code){code=body.code;await writeFile('/tmp/whistlegraph-browser-code.json',JSON.stringify({code,threadID,url:'http://127.0.0.1:'+server.address().port,scope:'Desktop browser running the real Whistlegraph engine and AC preview; not physical iPhone'}));console.log(code,body.status);}
 });
 await page.evaluateOnNewDocument(({guides,ledger,restored})=>{
  window.webkit={messageHandlers:{whistlegraph:{postMessage:body=>{void window.__nativeBridge(body);}}}};
  if(window===window.top){window.__aeselGuides=guides;if(restored){localStorage.setItem('whistlegraph-source-thread',JSON.stringify({id:restored.id,code:restored.code}));localStorage.setItem('whistlegraph-source-cloud-revision',String(restored.revision));}localStorage.setItem('whistlegraph-source',ledger.versions[0].source);localStorage.setItem('whistlegraph-source-versions',JSON.stringify(ledger));}
 },{guides,ledger,restored});
 await page.evaluateOnNewDocument(preview);
 page.on('pageerror',e=>console.error(e.message));
 await page.goto('http://127.0.0.1:'+server.address().port+'/index.html?whistlegraph=1');
 const started=Date.now();while((!code||!renders)&&Date.now()-started<60000)await new Promise(r=>setTimeout(r,250));
 if(!code||!renders)throw Error('Live thread or real preview did not become ready');
 await page.screenshot({path:resolve(__dirname,'screenshots/thread-live.png')});
 console.log('READY',code,'real painted frames',renders);
 // Keep the process available while the separate agent CLI inspects/edits it.
 const stop=async()=>{await browser.close();server.close();process.exit();};process.on('SIGTERM',stop);process.on('SIGINT',stop);
 // Captures are requested separately to avoid competing screenshot viewport overrides.
})().catch(e=>{console.error(e.message);process.exit(1);});
