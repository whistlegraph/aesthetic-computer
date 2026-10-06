// Browser regression on Poorslice. Synthetic API fixtures; the live journey
// uses oskiewar-turnstile-live.mjs separately, without API interception.
import assert from 'node:assert/strict';
import { createServer } from 'node:http';
import { readFile, mkdir } from 'node:fs/promises';
import puppeteer from 'puppeteer';

const shotDir = process.env.OSKIEWAR_SHOTS || '/tmp/oskiewar-mosono-browser';
await mkdir(shotDir, { recursive: true });
const appearance = {skin:'#d7a079',hair:'#392819',shirt:'#446688',pants:'#223344',shoes:'#eeeeee',hairStyle:'short',beard:true,glasses:true,sleeves:'short'};
const fighter = {version:1,recipe:'oskiewar-capsule-fighter-v1',hash:'a'.repeat(64),appearance};
let saved = false, denied = false, missingHandle = false, modelCalls = 0, acceptedCalls = 0;
const result = status => ({status,handle:'@fixture',fighter,validUntil:Date.now()+600000});
const server = createServer(async (req,res) => {
  const send = (body,status=200,type='application/json') => {res.writeHead(status,{'content-type':type});res.end(type==='application/json'?JSON.stringify(body):body);};
  if(req.url === '/') return send(`<style>canvas{position:fixed;inset:0;width:100vw;height:100vh}</style><button id="open">Add yourself</button><script type="module">
    import mount from '/oskiewar-wizard.mjs'; window.signedIn=true;
    window.wizard=mount({bearer:async()=>window.signedIn?'fixture-token':null});
    document.querySelector('#open').onclick=()=>wizard.open();</script>`,200,'text/html');
  if(req.url.endsWith('.mjs')) return send(await readFile(new URL('../../xbox/live'+req.url,import.meta.url),'utf8'),200,'text/javascript');
  if(!req.url.startsWith('/api/')) return send({},404);
  let raw='';for await(const c of req)raw+=c;
  const body=JSON.parse(raw);
  if(req.url==='/api/oskiewar-consent') return send(denied?{outcome:'refuse',capability:null}:{outcome:'allow',capability:{jws:'fixture',sources:['appearance']}});
  if(req.url==='/api/oskiewar-submission') return body.action==='status'?send({},404):send({manifest:[{source:'appearance',hash:'sha256:'+'b'.repeat(64)}]},201);
  if(missingHandle) return send({code:'handle_required',message:'Choose your AC handle before making a fighter.'},409);
  if(body.action==='account') return send(saved?result('accepted'):{status:'empty',handle:'@fixture'});
  if(body.action==='generate'){modelCalls++;return send(result('complete'));}
  if(body.action==='accept'){acceptedCalls++;saved=true;return send(result('accepted'));}
  if(body.action==='withdraw'){saved=false;return send({status:'withdrawn'});}
  send({},400);
});
await new Promise(r=>server.listen(0,'127.0.0.1',r));
const browser=await puppeteer.launch({headless:true,executablePath:process.env.CHROME_BIN||'/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'});
const page=await browser.newPage();
const errors=[];page.on('pageerror',e=>errors.push(e.message));
const wait=predicate=>page.waitForFunction(predicate,{timeout:20000});
const note=()=>page.$eval('#wizard-note',el=>el.textContent);
const open=async()=>{await page.click('#open');await wait(()=>document.querySelector('#wizard-go')&&!document.querySelector('#wizard-go').disabled);};
try {
  await page.setViewport({width:1100,height:850});
  await page.goto(`http://127.0.0.1:${server.address().port}`);
  await open();
  assert.equal(await page.$eval('#wizard-title',el=>el.textContent),'Add @fixture');
  assert.equal(await page.$('input[value="voice"]'),null);
  await page.click('#wizard-go');await page.waitForSelector('input[type=file]');
  const upload=await page.$('input[type=file]');await upload.uploadFile(new URL('./oskiewar-generation.test.mjs',import.meta.url).pathname);
  await page.click('#wizard-go');await wait(()=>document.querySelector('#wizard-go').textContent==='Accept & use in practice');
  assert.equal(saved,false);assert.equal(await page.evaluate(()=>!!globalThis.__oskiewarFighterAppearance),false);
  assert.equal(await page.$eval('#wizard-card canvas',el=>getComputedStyle(el).position),'static');
  await page.screenshot({path:shotDir+'/01-review.png'});
  await page.click('#wizard-go');await wait(()=>!!globalThis.__oskiewarFighterAppearance);
  assert.equal(acceptedCalls,1);assert.equal(saved,true);assert.match(await note(),/@fixture/);
  await page.click('#wizard-back');
  assert.equal(await page.evaluate(()=>document.activeElement?.id==='wizard-back'),false,'closing returns keyboard input to the game');
  await page.reload();await open();
  await wait(()=>document.querySelector('#wizard-go').textContent==='Use in practice');
  await page.click('#wizard-go');await wait(()=>!!globalThis.__oskiewarFighterAppearance);
  assert.equal(modelCalls,1);assert.equal(acceptedCalls,1);
  await new Promise(r=>setTimeout(r,16000));
  assert.ok(await page.evaluate(()=>Array.isArray(globalThis.__oskiewarFighterAppearance.appearance.skin)));
  await page.setViewport({width:390,height:844});await page.screenshot({path:shotDir+'/02-mobile-saved.png'});
  await page.evaluate(()=>{window.signedIn=false;});
  await wait(()=>!globalThis.__oskiewarFighterAppearance);
  await page.evaluate(()=>{window.signedIn=true;});
  await page.click('button::-p-text(Withdraw my material)');
  await wait(()=>document.querySelector('#wizard-note').textContent.startsWith('Withdrawn.'));
  assert.equal(saved,false);
  await page.reload();denied=true;await open();await page.click('#wizard-go');
  await wait(()=>document.querySelector('#wizard-note').textContent.includes('Not granted'));
  assert.equal(await page.$('input[type=file]'),null);assert.equal(modelCalls,1);
  await page.screenshot({path:shotDir+'/03-refusal.png'});
  await page.reload();missingHandle=true;await open();
  assert.equal(await page.evaluate(()=>globalThis.__oskiewarAccountDoor),'handle');
  assert.deepEqual(errors,[]);
  console.log('PASS: review, explicit acceptance, handle binding, reload recovery, renewal colors, sign-out, withdrawal, refusal, missing handle; desktop + mobile.');
}finally{await browser.close();await new Promise(r=>server.close(r));}
