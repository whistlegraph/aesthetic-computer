import test from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {chromium} from 'playwright';

test('mood descenders have room, letters move in sync, and chat permalinks survive reload',async()=>{
 const browser=await chromium.launch({channel:process.env.PLAYWRIGHT_CHANNEL,headless:true});
 try{
 const context=await browser.newContext({viewport:{width:704,height:334}}),page=await context.newPage();
 const html=await readFile(new URL('../public/mime/index.html',import.meta.url),'utf8');
 const font=await readFile(new URL('../public/whistlegraph.app/ComicRelief-Bold.ttf',import.meta.url));
 const icon=await readFile(new URL('../public/mime/favicon.svg',import.meta.url),'utf8');
 const posts=[{code:'mood_fixture',parent:null,name:'@staywoke_zine',text:'even ____ got feelings inside',media:{kind:'mood',code:'h7p'}},
 {code:'chat_clock_fixture',parent:null,name:'@tester',text:'hello from laer-klokken',media:{kind:'chat',code:'xyz'}},
 {code:'mood_lines',parent:null,name:'@test',text:('gyp'.repeat(28)+'\n').repeat(15),media:{kind:'mood',code:'lines'}},
 {code:'mood_long',parent:null,name:'@test',text:'long gyp '.repeat(150),media:{kind:'mood',code:'long'}}].map(p=>({...p,when:new Date().toISOString(),board:'text/plain',file:null}));
 await context.route('**/*',async route=>{
  const u=new URL(route.request().url());
  if(u.hostname==='mime.ac'&&(u.pathname==='/'||u.pathname.startsWith('/p/')||/^\/[A-Za-z0-9_~!-]+$/.test(u.pathname)))return route.fulfill({contentType:'text/html',body:html});
  if(u.pathname==='/api/mime')return route.fulfill({contentType:'application/json',body:JSON.stringify({recent:posts,hasMore:false})});
  if(u.pathname==='/favicon.svg')return route.fulfill({contentType:'image/svg+xml',body:icon});
  if(u.pathname.endsWith('ComicRelief-Bold.ttf'))return route.fulfill({contentType:'font/ttf',body:font});
  return route.fulfill({body:''});
 });
 await page.goto('https://mime.ac/p/mood~h7p?ac-automation=1');
 await page.locator('.mime-startup').waitFor({state:'detached'});await page.evaluate(()=>document.fonts.ready);
 const mood=page.locator('#mood_fixture');
 await page.waitForFunction(()=>document.querySelector('#mood_fixture').hasAttribute('data-onstage'));
 assert.equal(await mood.locator('.reel-type').getAttribute('aria-label'), 'mood');
 assert.equal(await page.locator('#chat_clock_fixture .reel-type').getAttribute('aria-label'), 'chat');
 const checks=await mood.evaluate(e=>{
  const body=e.querySelector('.body'),box=body.getBoundingClientRect();
  const letters=[...body.querySelectorAll('.mood-letter')].map(l=>l.getBoundingClientRect());
  return {bottom:box.bottom-Math.max(...letters.map(r=>r.bottom)),top:Math.min(...letters.map(r=>r.top))-box.top,
   overflow:body.scrollWidth>body.clientWidth,font:parseFloat(getComputedStyle(body).fontSize),
   corners:[...e.querySelectorAll('.reel-command .pal-over > span, .meta .pal-over > span')].map(l=>getComputedStyle(l).animationPlayState)};
 });
 assert.ok(checks.bottom>5,JSON.stringify(checks));assert.ok(checks.top>2);assert.equal(checks.overflow,false);assert.ok(checks.font>25);
 assert.ok(checks.corners.length>10&&checks.corners.every(s=>s==='running'));
 const letter=mood.locator('.pal-over > span').first();const before=await letter.evaluate(e=>getComputedStyle(e).transform);
 await page.waitForTimeout(250);assert.notEqual(await letter.evaluate(e=>getComputedStyle(e).transform),before);
 const pair=await mood.locator('.reel-command .pal-text').evaluate(e=>[...e.children].map(layer=>getComputedStyle(layer.firstElementChild).transform));assert.equal(pair[0],pair[1]);
 await page.screenshot({path:'/tmp/mime-mood-desktop.png'});
 await page.setViewportSize({width:390,height:844});await page.screenshot({path:'/tmp/mime-mood-phone.png'});
 await page.locator('#chat_clock_fixture').evaluate(e=>e.scrollIntoView());await page.waitForURL('**/chat~xyz?*');
 assert.ok(await page.locator('#chat_clock_fixture .mood-letter').count()>0);
 await page.reload();await page.locator('.mime-startup').waitFor({state:'detached'});assert.equal(new URL(page.url()).pathname,'/chat~xyz');
 await page.emulateMedia({reducedMotion:'reduce'});
 assert.equal(await page.locator('#chat_clock_fixture .mood-letter').first().evaluate(e=>getComputedStyle(e).animationName),'none');
 await page.locator('#mood_long').evaluate(e=>e.scrollIntoView());
 for(const viewport of [{width:390,height:844},{width:320,height:480},{width:844,height:390},{width:704,height:334}]) {
  await page.setViewportSize(viewport);
  await page.waitForTimeout(100);
  const fit=await page.locator('#mood_long').evaluate(e=>{
   const body=e.querySelector('.body'),b=body.getBoundingClientRect(),card=e.getBoundingClientRect();
   const corners=e.querySelector('.meta').getBoundingClientRect();
   return {overflow:getComputedStyle(body).overflow,scroll:body.scrollHeight>body.clientHeight+1||body.scrollWidth>body.clientWidth+1,
    top:b.top-card.top,bottom:corners.top-b.bottom,text:body.querySelector('.mood-accessible').textContent};
  });
  assert.equal(fit.overflow,'visible');assert.equal(fit.scroll,false,JSON.stringify({viewport,fit}));
  assert.ok(fit.top>=48&&fit.bottom>=10,JSON.stringify({viewport,fit}));assert.equal(fit.text,posts.find(p=>p.code==='mood_long').text);
 }
 await page.screenshot({path:'/tmp/mime-long-mood.png'});
 await page.locator('#mood_lines').evaluate(e=>e.scrollIntoView());
 await page.setViewportSize({width:320,height:480});await page.waitForTimeout(100);
 const linesFit=await page.locator('#mood_lines').evaluate(card=>{const e=card.querySelector('.body'),b=e.getBoundingClientRect();return {top:b.top-card.getBoundingClientRect().top,bottom:card.querySelector('.meta').getBoundingClientRect().top-b.bottom,width:e.scrollWidth,available:e.clientWidth};});
 assert.ok(linesFit.top>=48&&linesFit.bottom>=10&&linesFit.width<=linesFit.available,JSON.stringify(linesFit));
 await page.emulateMedia({reducedMotion:'no-preference'});
 await page.waitForFunction(()=>document.querySelector('#mime-favicon').href.startsWith('data:image/png'));
 const iconBefore=await page.locator('#mime-favicon').getAttribute('href');await page.waitForTimeout(260);
 assert.notEqual(await page.locator('#mime-favicon').getAttribute('href'),iconBefore);
 await page.emulateMedia({reducedMotion:'reduce'});
 await page.waitForFunction(()=>document.querySelector('#mime-favicon').href.includes('/favicon-32.png?v=3'));
 }finally{await browser.close();}
});

test('a mood draft survives sign-in and failure, then the confirmed post appears in the feed', async () => {
 const browser=await chromium.launch({channel:process.env.PLAYWRIGHT_CHANNEL,headless:true});
 try {
  const page=await browser.newPage({viewport:{width:390,height:844},colorScheme:'dark'}), errors=[], requests=[];
  page.on('pageerror',error=>errors.push(error.message));
  const html=await readFile(new URL('../public/mime/index.html',import.meta.url),'utf8');
  const font=await readFile(new URL('../public/whistlegraph.app/ComicRelief-Bold.ttf',import.meta.url));
  const original={code:'mood_fixture',name:'@fixture',board:'text/plain',text:'A little room for a mood.',media:{kind:'mood',code:'abc'},when:new Date().toISOString()};
  const saved={...original,code:'mood_saved',text:'Still curious.',media:{kind:'mood',code:'new'}};
  await page.addInitScript(()=>{
   window.auth0={Auth0Client:class {
    async loginWithRedirect() { location.href='/?code=fixture&state=fixture&ac-automation=1'; }
    async handleRedirectCallback() {
     localStorage.setItem('session-aesthetic',encodeURIComponent(btoa(JSON.stringify({accessToken:'fixture',account:{sub:'fixture'}}))));
     return {appState:{hash:'#/',mood:true}};
    }
   }};
  });
  await page.route('**/*',route=>{
   const u=new URL(route.request().url());
   if(u.hostname==='mime.ac'&&(u.pathname==='/'||u.pathname.startsWith('/p/')||/^\/[A-Za-z0-9_~!-]+$/.test(u.pathname)))return route.fulfill({contentType:'text/html',body:html});
   if(u.pathname==='/api/mime')return route.fulfill({contentType:'application/json',body:JSON.stringify(u.searchParams.has('me')?{handle:'@fixture'}:{recent:[original],hasMore:false})});
   if(u.pathname==='/api/mood') {
    requests.push({body:route.request().postDataJSON(),auth:route.request().headers().authorization});
    return route.fulfill({status:requests.length===1?503:200,contentType:'application/json',body:JSON.stringify(requests.length===1?{error:'offline'}:{mood:saved.text,post:saved})});
   }
   if(u.pathname.endsWith('ComicRelief-Bold.ttf'))return route.fulfill({contentType:'font/ttf',body:font});
   return route.fulfill({body:''});
  });
  await page.goto('https://mime.ac/?ac-automation=1');
  await page.locator('.mime-startup').waitFor({state:'detached'});
  await page.locator('#mood_fixture .reel-type').click();
  const dialog=page.getByRole('dialog',{name:'Add a mood'}), text=dialog.getByRole('textbox',{name:'Mood'});
  await text.fill('Still curious.');
  await dialog.getByRole('button',{name:'Back',exact:true}).click();
  await page.locator('#mood_fixture .reel-type').click();
  assert.equal(await text.inputValue(),'Still curious.');
  await dialog.getByRole('button',{name:'Log in to post',exact:true}).click();
  await dialog.getByRole('button',{name:'Post mood',exact:true}).waitFor();
  assert.equal(await text.inputValue(),'Still curious.');
  await text.fill('   ');
  assert.equal(await dialog.getByRole('button',{name:'Post mood',exact:true}).isDisabled(),true);
  await text.fill('Still curious.');
  await dialog.getByRole('button',{name:'Post mood',exact:true}).click();
  await dialog.getByRole('status').filter({hasText:'Could not post'}).waitFor();
  assert.equal(await text.inputValue(),'Still curious.');
  await page.screenshot({path:'/tmp/mime-mood-composer.png'});
  await dialog.getByRole('button',{name:'Post mood',exact:true}).click();
  await page.waitForURL('**/mood~new*');
  await page.waitForFunction(()=>document.querySelector('#mood_saved')?.hasAttribute('data-onstage'));
  assert.equal(await page.locator('#mood_saved .mood-accessible').textContent(),'Still curious.');
  assert.equal(await page.locator('#mood_saved .reel-type').getAttribute('aria-label'),'mood');
  assert.deepEqual(requests,[{body:{mood:'Still curious.'},auth:'Bearer fixture'},{body:{mood:'Still curious.'},auth:'Bearer fixture'}]);
  assert.deepEqual(errors,[]);
  await page.screenshot({path:'/tmp/mime-mood-posted.png'});
 } finally { await browser.close(); }
});
