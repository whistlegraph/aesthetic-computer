// Explicit live check: real browser runtime, personal relay and four real frames.
import {createServer} from 'node:http';
import {readFile,writeFile,mkdir} from 'node:fs/promises';
import {resolve,extname} from 'node:path';
import puppeteer from 'puppeteer';
import {ACSession} from '../../../aesel/src/ac-session.mjs';
const root=resolve('apple/whistlegraph/Resources/Web'),out='/tmp/whistlegraph-relay-check';
await mkdir(out,{recursive:true});
const token=await new ACSession().token();
const swift=await readFile('apple/whistlegraph/Sources/WhistlegraphAccount.swift','utf8');
const preview=swift.split('static let script = """')[1].split('"""')[0];
const seed=Object.fromEntries(await Promise.all(['pieces.md','screen.md','hand.md','kidlisp.md','api.json'].map(async n=>['/easel/context/'+n,await readFile(root+'/easel/context/'+n,'utf8')])));
const server=createServer(async(req,res)=>{try{const file=resolve(root,'.'+new URL(req.url,'http://local').pathname);if(!file.startsWith(root+'/'))throw Error();const data=await readFile(file);res.setHeader('content-type',({'.html':'text/html','.js':'text/javascript','.mjs':'text/javascript','.json':'application/json'})[extname(file)]||'application/octet-stream');res.end(data);}catch{res.writeHead(404);res.end();}});
await new Promise(r=>server.listen(0,'127.0.0.1',r));
const browser=await puppeteer.launch({headless:true,executablePath:'/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'});
try{
 const page=await browser.newPage();await page.setViewport({width:430,height:800});
 let ready=false,lastRender,snapshot,reviewFrames=0;const errors=[],inference=[];
 page.on('pageerror',e=>errors.push(e.message));page.on('request',r=>{if(r.url().includes('/api/easel-inference')||r.url().includes('/api/aesel/'))inference.push(new URL(r.url()).pathname);});
 const runtime=()=>page.frames().find(f=>f.url().startsWith('https://aesthetic.computer/'));
 const render=async m=>{lastRender=m;if(ready)await runtime().evaluate(m=>window.walkiewareRender(m.source,'relay-live-test',m.renderID),m);};
 await page.exposeFunction('__nativeBridge',async m=>{
  try{
   if(m.action==='snapshot')snapshot=m.snapshot;
   if(m.action==='render')await render(m);
   if(m.action==='previewReady'){ready=true;if(lastRender)await render(lastRender);await page.evaluate(()=>window.walkiewareEngineEvent({kind:'previewReady'}));}
   if(m.action==='previewEvent')await page.evaluate(e=>window.walkiewareEngineEvent({kind:'previewEvent',event:e}),m.event);
   if(m.action==='visualCapture'){
    const frames=[],started=Date.now();
    for(let i=0;i<4;i++){
     if(i)await new Promise(r=>setTimeout(r,800));
     const element=await page.$('#live-piece');const bytes=await element.screenshot({encoding:'base64'});
     const frame=await page.evaluate(async png=>{const img=new Image();img.src='data:image/png;base64,'+png;await img.decode();const k=Math.min(384/img.width,384/img.height,1);const canvas=document.createElement('canvas');canvas.width=Math.round(img.width*k);canvas.height=Math.round(img.height*k);canvas.getContext('2d').drawImage(img,0,0,canvas.width,canvas.height);return {width:canvas.width,height:canvas.height,png:canvas.toDataURL('image/png').split(',')[1]};},bytes);
     frames.push({...frame,atMs:Date.now()-started});await writeFile(out+'/frame-'+i+'.png',Buffer.from(frame.png,'base64'));
    }
    reviewFrames+=frames.length;await page.evaluate(e=>window.walkiewareEngineEvent(e),{kind:'visualCapture',captureID:m.captureID,sourceHash:m.sourceHash,renderID:m.renderID,frames});
   }
  }catch(e){errors.push('bridge: '+e.message);}
 });
 await page.evaluateOnNewDocument((guides,preview)=>{
  window.webkit={messageHandlers:{walkie:{postMessage:m=>void window.__nativeBridge(m)}}};
  if(window===top){window.__walkiewareNativeShell=true;window.__aeselGuides=guides;window.__walkiewareDisableThread=true;}
  (0,eval)(preview);
 },seed,preview);
 await page.goto('http://127.0.0.1:'+server.address().port+'/index.html?walkie=1');
 await page.waitForFunction(()=>typeof window.walkiewareAsk==='function');
 await page.addStyleTag({content:'#live-piece{visibility:visible!important}'});
 await page.evaluate(token=>window.walkiewareEngineEvent({kind:'account',token}),token);
 for(let i=0;i<60 && (!ready||snapshot?.handle!=='jeffrey');i++)await new Promise(r=>setTimeout(r,500));
 if(!ready||snapshot?.handle!=='jeffrey'){console.log(JSON.stringify({ready,handle:snapshot?.handle,errors,frames:await Promise.all(page.frames().map(async f=>({url:f.url(),state:await f.evaluate(()=>({title:document.title,preloaded:!!window.preloaded,send:typeof window.acSEND,render:typeof window.walkiewareRender,text:document.body.innerText.slice(0,100)})).catch(()=>null)})))}));await page.screenshot({path:out+'/startup.png'});}
 if(!ready||snapshot?.handle!=='jeffrey')throw Error('Runtime or signed-in account not ready');
 console.log('Real browser ready; generating through personal relay');
 await page.evaluate(()=>window.walkiewareAsk('Make a pink circle centered on black. Keep it static.'));
 await new Promise(r=>setTimeout(r,500));
 const stored=await page.evaluate(()=>({attempt:JSON.parse(localStorage.getItem('walkieware-source-attempt')),source:localStorage.getItem('walkieware-source'),receipts:JSON.parse(localStorage.getItem('walkieware-source-receipts'))}));
 const report={provider:snapshot?.inference?.provider,attempt:stored.attempt,source:stored.source,reviewFrames,apiCalls:inference,errors,receipt:stored.receipts?.at(-1)?.receipt};
 await writeFile(out+'/live-report.json',JSON.stringify(report,null,2));console.log(JSON.stringify({provider:report.provider,attempt:report.attempt,reviewFrames,errors}));
 if(inference.some(path=>path==='/api/easel-inference')||reviewFrames<4||stored.attempt?.status!=='completed')throw Error('Personal generation/review did not pass');
}finally{await browser.close();await new Promise(r=>server.close(r));}
