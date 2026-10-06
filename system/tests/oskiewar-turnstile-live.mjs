// Run on Poorslice with a designated/approved AC account. No API mocks.
// This creates a grant, spends one generation, and withdraws ALL Oskiewar
// material for that account. Never run without explicit test-account consent.
import assert from 'node:assert/strict';
import { readFile, mkdir } from 'node:fs/promises';
import puppeteer from 'puppeteer';
import sharp from 'sharp';

if (process.env.OSKIEWAR_LIVE_WITHDRAW !== '1') throw Error('Requires OSKIEWAR_LIVE_WITHDRAW=1 and an approved test account.');
const base = process.env.OSKIEWAR_URL || 'https://oskiewar.com';
const dir = process.env.OSKIEWAR_SHOTS || '/tmp/oskiewar-mosono-live';
await mkdir(dir,{recursive:true});
const auth = JSON.parse(await readFile(process.env.AC_TOKEN_FILE || `${process.env.HOME}/.ac-token`,'utf8'));
const access = auth.access_token || auth.accessToken || auth.token;
const info = await fetch('https://hi.aesthetic.computer/userinfo',{headers:{Authorization:`Bearer ${access}`}});
assert.equal(info.status,200,'test account must have a live Auth0 session');
const {sub} = await info.json();
const fixture = dir+'/synthetic-fighter.png';
await sharp(Buffer.from(`<svg width="256" height="256" xmlns="http://www.w3.org/2000/svg"><rect width="256" height="256" fill="#eee"/><rect x="45" y="160" width="166" height="96" rx="28" fill="#365f90"/><ellipse cx="128" cy="104" rx="55" ry="66" fill="#d9a47e"/><path d="M75 90Q65 20 130 28Q194 24 182 91L164 60L94 64Z" fill="#362a22"/><circle cx="108" cy="100" r="6"/><circle cx="148" cy="100" r="6"/><path d="M107 134Q128 150 149 134" stroke="#633" fill="none" stroke-width="4"/><text x="8" y="248" font-size="10">SYNTHETIC TEST · ${Date.now()}</text></svg>`)).png().toFile(fixture);
const browser = await puppeteer.launch({headless:true,executablePath:process.env.CHROME_BIN||'/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'});
const page = await browser.newPage();
const evidence=[]; let source, grant, acceptedHash;
page.on('response',async response=>{
  const url=new URL(response.url());
  if(!url.pathname.startsWith('/api/oskiewar-'))return;
  evidence.push({path:url.pathname,status:response.status()});
  try {
    const data=await response.json();
    if(url.pathname==='/api/oskiewar-consent'&&data.capability?.jws)grant=data.capability.jws;
    if(url.pathname==='/api/oskiewar-submission')source=data.manifest?.find(x=>x.source==='appearance')?.hash?.replace(/^sha256:/,'')||source;
  }catch{}
});
const wait=fn=>page.waitForFunction(fn,{timeout:120000});
const post=body=>page.evaluate(async body=>{
  const token=await globalThis.__oskiewarAccount.bearer();
  const r=await fetch('/api/oskiewar-generation',{method:'POST',headers:{'Content-Type':'application/json',authorization:'Bearer '+token},body:JSON.stringify(body)});
  return {status:r.status,body:await r.json()};
},body);
const open=async()=>{
  await page.evaluate(()=>globalThis.__oskiewarWizard.open());
  await wait(()=>document.querySelector('#wizard-go')&&!document.querySelector('#wizard-go').disabled);
};
try {
  await page.setViewport({width:1200,height:900});
  await page.goto(base,{waitUntil:'domcontentloaded'});
  assert.equal(await page.evaluate(async()=>{
    const r=await fetch('/api/oskiewar-generation',{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify({action:'account'})});return r.status;
  }),401,'anonymous requests must be refused');
  // Restore a real, existing authenticated session; this does not simulate OTP delivery.
  await page.evaluateOnNewDocument(({sub,access})=>{
    if(location.origin==='https://oskiewar.com'&&!sessionStorage.getItem('turnstile-session-loaded')){
      localStorage.setItem('ac-otp-session',JSON.stringify({sub,access,expires:Date.now()+3600000}));
      sessionStorage.setItem('turnstile-session-loaded','1');
    }
  },{sub,access});
  await page.reload({waitUntil:'domcontentloaded'});
  await wait(()=>globalThis.__oskiewarAccount?.ready&&globalThis.__oskiewarAccount?.signedIn&&globalThis.__oskiewarWizard);
  const handle=await page.evaluate(()=>globalThis.__oskiewarAccount.handle);
  assert.ok(handle.startsWith('@'));
  await open();
  // The approved test cleanup makes repeated live runs deterministic.
  await page.click('button::-p-text(Withdraw my material)');
  await wait(()=>document.querySelector('#wizard-note').textContent.startsWith('Withdrawn.'));
  await page.click('#wizard-back');await open();
  await page.click('#wizard-go');
  await page.waitForSelector('input[name="appearance"][type="file"]',{timeout:30000});
  await page.screenshot({path:dir+'/01-granted.png'});
  await (await page.$('input[name="appearance"][type="file"]')).uploadFile(fixture);
  await page.click('#wizard-go');
  await wait(()=>document.querySelector('#wizard-go').textContent==='Accept & use in practice'||document.querySelector('#wizard-note').className==='trouble');
  assert.equal(await page.$eval('#wizard-go',el=>el.textContent),'Accept & use in practice',await page.$eval('#wizard-note',el=>el.textContent));
  assert.equal(await page.evaluate(()=>!!globalThis.__oskiewarFighterAppearance),false,'review cannot equip before acceptance');
  await page.screenshot({path:dir+'/02-generated.png'});
  assert.equal(await page.$eval('#wizard-card canvas',el=>getComputedStyle(el).position),'static');
  await Promise.all([page.waitForNavigation({waitUntil:'domcontentloaded'}),page.click('#wizard-go')]);
  await wait(()=>!!globalThis.__oskiewarFighterAppearance);
  assert.ok(new URL(page.url()).searchParams.has('practice'));
  const account=await post({action:'account'});assert.equal(account.status,200);assert.equal(account.body.status,'accepted');
  assert.equal(account.body.handle.toLowerCase(),handle.toLowerCase());acceptedHash=account.body.fighter.hash;
  await page.click('#wizard-back');
  await page.click('#screen', {offset:{x:20,y:20}});
  await page.keyboard.down('Enter');await new Promise(r=>setTimeout(r,300));await page.keyboard.up('Enter');
  await wait(()=>globalThis.__oskiewarTouch?.screen==='game');
  await wait(()=>!!globalThis.__oskiewarTouch?.practiceFighter);
  assert.equal(await page.evaluate(()=>globalThis.__oskiewarTouch.practiceFighter.toLowerCase()),handle.toLowerCase());
  await page.keyboard.down('ArrowRight');await new Promise(r=>setTimeout(r,1000));await page.keyboard.up('ArrowRight');
  assert.ok(await page.evaluate(()=>Array.isArray(globalThis.__oskiewarFighterAppearance?.appearance.skin)));
  await page.screenshot({path:dir+'/03-local-practice.png'});
  await page.reload({waitUntil:'domcontentloaded'});
  await wait(()=>globalThis.__oskiewarAccount?.ready&&globalThis.__oskiewarWizard);
  await open();await wait(()=>document.querySelector('#wizard-go').textContent==='Use in practice');
  assert.equal((await post({action:'account'})).body.fighter.hash,acceptedHash);
  await page.setViewport({width:390,height:844});await page.screenshot({path:dir+'/04-mobile-restored.png'});
  assert.equal((await post({action:'status',hash:source,capability:grant.slice(0,-4)+'AAAA'})).status,403,'tampered capability refused');
  await page.click('button::-p-text(Withdraw my material)');await wait(()=>document.querySelector('#wizard-note').textContent.startsWith('Withdrawn.'));
  assert.equal(await page.evaluate(()=>!!globalThis.__oskiewarFighterAppearance),false);
  await wait(()=>!globalThis.__oskiewarTouch?.practiceFighter);
  assert.equal((await post({action:'account'})).body.status,'empty');
  assert.equal((await post({action:'status',hash:source,capability:grant})).status,403,'withdrawn grant refuses old preview');
  await page.screenshot({path:dir+'/05-withdrawn.png'});
  console.log(JSON.stringify({result:'PASS',handle,checks:['anonymous refusal','real authenticated session','REGARDE grant','protected upload','real model generation','review','server acceptance','local practice','reload recovery','mobile','tamper refusal','withdrawal','revoked preview refusal'],requests:evidence},null,2));
}catch(error){await page.screenshot({path:dir+'/failure.png'}).catch(()=>{});throw error;}
finally{await browser.close();}
