// Full game renderer regression on Poorslice. Local changed assets over the
// deployed shell; synthetic appearance, no model call or account mutation.
import assert from 'node:assert/strict';
import { readFile, mkdir } from 'node:fs/promises';
import puppeteer from 'puppeteer';
const dir = process.env.OSKIEWAR_SHOTS || '/tmp/oskiewar-practice-browser';
await mkdir(dir, {recursive:true});
const browser = await puppeteer.launch({headless:true, executablePath:process.env.CHROME_BIN || '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'});
const page = await browser.newPage(), errors = [], sockets = [], replays = [];
page.on('pageerror', e => errors.push(e.message));
const cdp = await page.createCDPSession(); await cdp.send('Network.enable');
cdp.on('Network.webSocketCreated', e => sockets.push(e.url));
await page.setRequestInterception(true);
page.on('request', async request => {
  const url = new URL(request.url());
  if (url.pathname.includes('oskiewar-replays') && request.method() === 'POST') replays.push(url.pathname);
  let file;
  if (url.origin === 'https://oskiewar.com') {
    if (url.pathname === '/') file = 'mac-test.html';
    else if (['/oskiewar.js','/oskiewar-fighter.mjs','/oskiewar-wizard.mjs'].includes(url.pathname)) file = url.pathname.slice(1);
  }
  if (file) await request.respond({status:200,contentType:file.endsWith('html')?'text/html':'text/javascript',body:await readFile(new URL('../../xbox/live/'+file,import.meta.url))});
  else await request.continue();
});
try {
  await page.setViewport({width:1200,height:900});
  await page.goto('https://oskiewar.com/?practice',{waitUntil:'domcontentloaded'});
  await page.waitForFunction(()=>globalThis.__oskiewarTouch?.screen,{timeout:60000});
  await page.evaluate(()=>{globalThis.__oskiewarFighterAppearance={handle:'@fixture',validUntil:Date.now()+600000,appearance:{skin:[217,164,126],hair:[54,42,34],shirt:[54,95,144],pants:[24,55,104],shoes:[235,235,235],hairStyle:'short',beard:false,glasses:false,sleeves:'long'}};});
  await page.keyboard.down('Enter'); await new Promise(r=>setTimeout(r,300)); await page.keyboard.up('Enter');
  await page.waitForFunction(()=>globalThis.__oskiewarTouch?.practiceFighter==='@fixture',{timeout:30000});
  await page.screenshot({path:dir+'/practice.png'});
  await page.evaluate(()=>{globalThis.__oskiewarFighterAppearance=null;});
  await page.waitForFunction(()=>!globalThis.__oskiewarTouch?.practiceFighter);
  assert.deepEqual(errors,[]); assert.deepEqual(sockets,[]); assert.deepEqual(replays,[]);
  console.log('PASS: actual game renders generated appearance, clears it on withdrawal, and opens no game sockets or replay uploads.');
} catch(error) { await page.screenshot({path:dir+'/failure.png'}); throw error; }
finally { await browser.close(); }
