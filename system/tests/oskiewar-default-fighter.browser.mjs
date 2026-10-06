// Poorslice regression: defaults restore live account authority without granting
// or accepting anything. Delayed requests cannot outlive a sign-out.
import assert from 'node:assert/strict';
import {createServer} from 'node:http';
import {readFile} from 'node:fs/promises';
import puppeteer from 'puppeteer';
let mode='saved',requests=[],pending;
const fighter={version:1,recipe:'oskiewar-capsule-fighter-v1',hash:'a'.repeat(64),appearance:{skin:'#f1d9c9',hair:'#4a3c31',shirt:'#f0f0f0',pants:'#1c4ea6',shoes:'#000000',hairStyle:'long',beard:false,glasses:false,sleeves:'long'}};
const result=()=>({status:'accepted',handle:mode==='other'?'@other':'@jeffrey',validUntil:mode==='expired'?1:Date.now()+600000,fighter});
const server=createServer(async(req,res)=>{
  if(req.url==='/') {res.setHeader('Content-Type','text/html');res.end(`<script type="module">
    import mount from '/oskiewar-wizard.mjs'; window.signedIn=true; window.defaults=0;
    window.wizard=mount({bearer:async()=>window.signedIn?'test':null,defaultHandle:'@jeffrey',onDefault:()=>window.defaults++});
    await wizard.restoreSaved(); window.ready=true;
  </script>`);return;}
  if(req.url.endsWith('.mjs')){res.setHeader('Content-Type','text/javascript');res.end(await readFile(new URL('../../xbox/live'+req.url,import.meta.url)));return;}
  if(req.url==='/api/oskiewar-generation'){
    let body='';for await(const c of req)body+=c;requests.push(JSON.parse(body).action);
    const send=()=>{res.setHeader('Content-Type','application/json');res.end(JSON.stringify(result()));};
    if(mode==='delayed'){pending=send;return;}
    if(mode==='error'){res.statusCode=403;res.end(JSON.stringify({code:'handle_required'}));return;}
    if(mode==='empty'){res.end(JSON.stringify({status:'empty',handle:'@jeffrey'}));return;}
    send();return;
  }
  res.statusCode=404;res.end();
});
await new Promise(r=>server.listen(0,'127.0.0.1',r));
const browser=await puppeteer.launch({headless:true,executablePath:process.env.CHROME_BIN||'/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'});
const page=await browser.newPage(),errors=[];page.on('pageerror',e=>errors.push(e.message));
const selected=()=>page.evaluate(()=>globalThis.__oskiewarFighterAppearance?.handle||null);
const change=async signedIn=>page.evaluate(signedIn=>{window.signedIn=signedIn;dispatchEvent(new CustomEvent('oskiewar:account-change',{detail:{signedIn}}));},signedIn);
try{
 await page.goto('http://127.0.0.1:'+server.address().port);await page.waitForFunction(()=>window.ready);
 assert.equal(await selected(),'@jeffrey');assert.equal(await page.$eval('#wizard-panel',e=>e.hidden),true);
 await page.reload();await page.waitForFunction(()=>window.ready);assert.equal(await selected(),'@jeffrey');
 await change(false);assert.equal(await selected(),null);
 await change(true);await page.waitForFunction(()=>globalThis.__oskiewarFighterAppearance?.handle==='@jeffrey');
 for(const state of ['other','expired','empty','error']){
   mode=state;await page.evaluate(()=>window.wizard.restoreSaved());assert.equal(await selected(),null);
   assert.equal(await page.$eval('#wizard-panel',e=>e.hidden),true);
   assert.equal(await page.evaluate(()=>globalThis.__oskiewarAccountDoor),undefined,'background restoration does not open a handle dialog');
 }
 mode='delayed';await change(true);
 for(let i=0;i<100&&!pending;i++)await new Promise(r=>setTimeout(r,10));assert.ok(pending);
 await change(false);assert.equal(await selected(),null);pending();pending=null;
 await new Promise(r=>setTimeout(r,100));assert.equal(await selected(),null);
 assert.ok(requests.length>=8);assert.ok(requests.every(action=>action==='account'));assert.deepEqual(errors,[]);
 console.log('PASS: default saved fighter on entry/reload/login; immediate sign-out; foreign handle, expired, missing and unavailable results stay unequipped; late response cannot re-equip; no acceptance or grant.');
}finally{await browser.close();await new Promise(r=>server.close(r));}
