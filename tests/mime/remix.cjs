const assert = require('node:assert/strict');
const {readFile} = require('node:fs/promises');
const puppeteer = require('puppeteer');
(async () => {
 const browser = await puppeteer.launch({headless:true,executablePath:process.env.CHROME_PATH || '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'});
 try {
  const page = await browser.newPage(), errors = [];
  const html = await readFile('system/public/mime/index.html','utf8');
  const png = await readFile('apple/whistlegraph/Resources/Fixtures/remix.png');
  const font = await readFile('apple/mime/Resources/Fonts/ComicRelief-Bold.ttf');
  const post = {code:'fixture-remix-png',parent:null,name:'@painter',nameColors:[{r:255,g:145,b:197},{r:101,g:221,b:214}],version:3,when:new Date().toISOString(),file:{name:'Blue.png',type:'image/png',url:'https://mime.ac/fixture.png'}};
  await page.setViewport({width:393,height:852});
  await page.emulateMediaFeatures([{name:'prefers-color-scheme',value:'dark'}]);
  page.on('pageerror',e=>errors.push(e.message));await page.setRequestInterception(true);
  page.on('request',async request=>{
   const u=new URL(request.url());
   if(u.hostname==='mime.ac'&&(u.pathname==='/'||u.pathname.startsWith('/p/')))return request.respond({contentType:'text/html',body:html});
   if(u.pathname==='/api/mime')return request.respond({contentType:'application/json',body:JSON.stringify({recent:[post,{...post,code:'second',version:9}],hasMore:false})});
   if(u.pathname==='/fixture.png')return request.respond({contentType:'image/png',body:png});
   if(u.pathname.endsWith('ComicRelief-Bold.ttf'))return request.respond({contentType:'font/ttf',headers:{'Access-Control-Allow-Origin':'*'},body:font});
   if(u.pathname.endsWith('mime-layout.json'))return request.respond({contentType:'application/json',headers:{'Access-Control-Allow-Origin':'*'},body:await readFile('system/public/mime-layout.json')});
   return request.respond({status:200,body:''});
  });
  await page.goto('https://mime.ac/?ac-automation=1');
  await page.waitForSelector('.reel-version');await page.waitForFunction(()=>!document.querySelector('.mime-startup'));await page.evaluate(()=>document.fonts.ready);
  assert.equal(await page.$eval('.reel-version',e=>e.getAttribute('aria-label')),'v3');
  const rects=await page.$eval('.feed-post',e=>Object.fromEntries(['.reel-command .media-label','.reel-version','.name','.reel-type'].map(s=>{const r=e.querySelector(s).getBoundingClientRect();return [s,{x:r.x,y:r.y,right:r.right,bottom:r.bottom,width:r.width,height:r.height}];})));
  assert.ok(rects['.reel-version'].x>rects['.reel-command .media-label'].right,'version follows the global code');
  assert.ok(rects['.reel-version'].width>=44&&rects['.reel-version'].height>=44,'version is a usable tap target');
  assert.ok(Math.abs(rects['.name'].bottom-rects['.reel-type'].bottom)<3,'handle and type share a baseline');
  assert.equal(await page.$eval('.name .pal-over > span',e=>getComputedStyle(e).color),'rgb(255, 145, 197)');
  const glyph='.reel-version .pal-over > span';
  const motion=await page.$eval(glyph,e=>getComputedStyle(e).transform);
  await page.waitForFunction((s,old)=>getComputedStyle(document.querySelector(s)).transform!==old,{},glyph,motion);
  assert.equal(await page.$eval('#second .reel-version .pal-over > span',e=>getComputedStyle(e).animationPlayState),'paused');
  await page.screenshot({path:'/tmp/mime11-web-feed.png'});
  await page.click('.reel-version');await page.waitForSelector('#remix-dialog[open]');
  assert.equal(await page.$eval('#remix-dialog a',e=>e.href),'mime://remix?post=fixture-remix-png&version=3');
  assert.equal(await page.$eval('#remix-dialog',e=>getComputedStyle(e).backgroundColor),'rgb(23, 21, 38)');
  await page.screenshot({path:'/tmp/mime11-web-remix.png'});
  await page.keyboard.press('Escape');await page.waitForSelector('#remix-dialog',{hidden:true});
  assert.equal(await page.evaluate(()=>document.activeElement.classList.contains('reel-version')),true);
  await page.emulateMediaFeatures([{name:'prefers-reduced-motion',value:'reduce'}]);
  assert.equal(await page.$eval(glyph,e=>getComputedStyle(e).animationName),'none');
  await page.setViewport({width:1200,height:800});await page.screenshot({path:'/tmp/mime11-web-desktop.png'});
  assert.deepEqual(errors,[]);
  console.log('PASS web version placement, saved handle colors, visible letter motion, offstage/reduced-motion pause, native remix handoff and keyboard dismissal');
 } finally {await browser.close();}
})().catch(e=>{console.error(e);process.exitCode=1;});
