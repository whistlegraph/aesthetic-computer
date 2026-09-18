// Focused integration: real browser bundle/agent loop, mocked account and network.
// Nothing is sent to inference, storage or publishing services.
import { chromium } from 'playwright';
import assert from 'node:assert/strict';
import { mkdir } from 'node:fs/promises';
const browser=await chromium.launch({channel:'chrome',headless:true});
const context=await browser.newContext();
const page=await context.newPage();
const errors=[];page.on('pageerror',error=>errors.push(error.message));
let rounds=0,uploaded='',requests=[];
const source='export function paint({ wipe, ink, circle }) { wipe("purple"); ink("pink").circle(100, 100, 40).draw(); }\n';
const json=(route,data)=>route.fulfill({contentType:'application/json',body:JSON.stringify(data)});
await context.route('**/*',async route=>{
  const url=new URL(route.request().url());
  if(url.pathname==='/try/auth0.js') return route.fulfill({contentType:'text/javascript',body:`window.auth0={Auth0Client:class {
    async checkSession(){} async isAuthenticated(){return !localStorage.getItem('signedOut');}
    async getUser(){return {sub:localStorage.getItem('testAccount') || 'auth0|alice'};}
    async getTokenSilently(){return 'test-only-token';}
    async logout(){localStorage.setItem('signedOut','yes');location.reload();}
    async loginWithRedirect(){localStorage.removeItem('signedOut');location.reload();}
  }};`});
  if(url.hostname==='localhost') return route.continue();
  requests.push({url:url.href,method:route.request().method()});
  if(url.pathname==='/userinfo')return json(route,{sub:'auth0|alice'});
  if(url.pathname==='/handle')return json(route,{handle:'alice'});
  if(url.pathname==='/api/easel-credits')return json(route,{remaining:500,purchased:0});
  if(url.pathname==='/api/easel-inference'){
    rounds++;
    const body=route.request().postDataJSON();
    assert(body.system && body.tools.some(t=>t.name==='write_piece'));
    const events=rounds===1 ? [
      {type:'content_block_start',index:0,content_block:{type:'tool_use',id:'write-1',name:'write_piece'}},
      {type:'content_block_delta',index:0,delta:{type:'input_json_delta',partial_json:JSON.stringify({source})}},
      {type:'content_block_stop',index:0},
      {type:'message_delta',delta:{stop_reason:'tool_use'}},
    ] : [
      {type:'content_block_delta',index:0,delta:{type:'text_delta',text:'A pink circle on purple. <img src=x onerror=alert(1)>'}},
      {type:'message_delta',delta:{stop_reason:'end_turn'}},
    ];
    return route.fulfill({contentType:'text/event-stream',body:events.map(e=>'data: '+JSON.stringify(e)+'\n\n').join('')});
  }
  if(url.pathname.includes('/presigned-upload-url/'))return json(route,{uploadURL:'https://upload.test/piece'});
  if(url.hostname==='upload.test'){uploaded=route.request().postData();return route.fulfill({body:''});}
  if(url.pathname.endsWith('.mjs'))return route.fulfill({body:uploaded});
  if(url.pathname.startsWith('/@alice/'))return route.fulfill({contentType:'text/html',body:'<body style="background:purple"><p style="color:pink">Preview fixture</p></body>'});
  return json(route,{});
});
try {
  await page.goto('http://localhost:8771/try/');
  await page.locator('#workspace').waitFor({state:'visible'});
  await page.locator('#input').fill('Make a pink circle.');
  await page.locator('#send').click();
  await page.locator('#log .answer').waitFor();
  await page.locator('#preview').waitFor({state:'visible'});
  await page.waitForFunction(()=>!document.getElementById('send').disabled);
  assert.equal(uploaded,source);assert.equal(rounds,2);
  assert.equal(await page.locator('#log img').count(),0);
  const firstId=await page.locator('#history').inputValue();
  await page.reload();
  await page.locator('#workspace').waitFor({state:'visible'});
  assert.equal(await page.locator('#history').inputValue(),firstId);
  assert.match(await page.locator('#log').innerText(),/Make a pink circle/);
  await page.locator('#new').click();
  await page.waitForFunction(id=>document.getElementById('history').value!==id,firstId);
  assert.equal(await page.locator('#log li').count(),0);
  await page.locator('#history').selectOption(firstId);
  await page.locator('#log .answer').waitFor();
  await mkdir(new URL('./test-results/',import.meta.url),{recursive:true});
  await page.screenshot({path:new URL('./test-results/desktop.png',import.meta.url).pathname});
  await page.setViewportSize({width:390,height:844});
  assert(await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth));
  await page.screenshot({path:new URL('./test-results/mobile.png',import.meta.url).pathname,fullPage:true});
  const storage=await page.evaluate(()=>JSON.stringify(localStorage));
  assert(!storage.includes('test-only-token'));
  await page.locator('#logout').click();
  await page.locator('#gate').waitFor({state:'visible'});
  await page.evaluate(()=>{localStorage.setItem('testAccount','auth0|bob');localStorage.removeItem('signedOut');});
  await page.reload();
  await page.locator('#workspace').waitFor({state:'visible'});
  assert(!((await page.locator('#log').innerText()).includes('pink circle')));
  assert.equal(await page.locator('#history option').count(),1);
  assert.deepEqual(errors,[]);
  console.log('PASS: generation, upload, preview, reload, thread switching, text safety, account isolation, sign-out and mobile layout. Network mocked.');
}finally {await browser.close();}
