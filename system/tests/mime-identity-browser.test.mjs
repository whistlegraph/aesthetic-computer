import {serveMimeInterface} from './fixtures/mime-interface.mjs';
import test from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {chromium} from 'playwright';

test('MIME shares Comic glyphs and metadata alignment, and WADs use the embedded player',async()=>{
 const browser=await chromium.launch({channel:process.env.PLAYWRIGHT_CHANNEL,headless:true});
 try{
  const page=await browser.newPage({viewport:{width:430,height:932}}),errors=[];page.on('pageerror',e=>errors.push(e.message));
  const html=await readFile(new URL('../public/mime/index.html',import.meta.url),'utf8');
  const font=await readFile(new URL('../../apple/mime/Resources/Fonts/ComicRelief-Bold.ttf',import.meta.url));
  const layout=await readFile(new URL('../public/mime-layout.json',import.meta.url));
  const posts=[{code:'wadfixture',name:'@jeffrey',when:new Date().toISOString(),file:{name:'empty-room.wad',type:'application/x-doom-wad',url:'/fixture/room.wad'}}];
  await page.route('**/*',route=>{
    const shared = serveMimeInterface(route); if (shared) return shared;
   const url=new URL(route.request().url());
   if(url.pathname==='/')return route.fulfill({contentType:'text/html',body:html});
   if(url.pathname==='/api/mime')return route.fulfill({contentType:'application/json',body:JSON.stringify({recent:posts,hasMore:false})});
   if(url.pathname==='/mime-layout.json')return route.fulfill({contentType:'application/json',body:layout});
   if(url.pathname.endsWith('/ComicRelief-Bold.ttf'))return route.fulfill({contentType:'font/ttf',body:font});
   if(url.pathname==='/doom-player/')return route.fulfill({contentType:'text/html',body:'<body style="margin:0;background:#324"><canvas aria-label="Empty Doom room"></canvas><script>parent.postMessage({type:"doom:ready"},"*")</script>'});
   return route.fulfill({body:''});
  });
  await page.goto('https://mime.ac/');await page.waitForSelector('.reel-type');
  await page.waitForFunction(()=>!document.querySelector('.mime-startup'));
  await page.evaluate(()=>document.fonts.ready);
  const presentation=await page.evaluate(()=>{
   const type=document.querySelector('.reel-type'),author=document.querySelector('.byline > .name'),svg=document.querySelector('.wordmark svg');
   return {label:type.getAttribute("aria-label")||type.querySelector(".pal-over").textContent,authorY:author.getBoundingClientRect().bottom,typeY:type.getBoundingClientRect().bottom,font:getComputedStyle(svg).fontFamily,letters:[...svg.querySelectorAll('.mime-letter text:last-child')].map(n=>n.textContent).join(''),loaded:document.fonts.check('700 44px "MIME Comic"'),player:document.querySelector('.doom-player').src};
  });
  await page.screenshot({path:'/tmp/mime-comic-web.png'});
  assert.equal(presentation.label,'application/x-doom-wad');assert.equal(presentation.letters,'MIME');assert.ok(presentation.font.includes('MIME Comic'));assert.equal(presentation.loaded,true);
  assert.ok(Math.abs(presentation.authorY-presentation.typeY)<2,'handle and MIME type share a text baseline');
  const player=new URL(presentation.player);assert.equal(player.searchParams.get('preview'),'1');assert.equal(player.searchParams.get('mime'),'1');assert.equal(player.searchParams.get('file'),'https://mime.ac/fixture/room.wad');
  await page.screenshot({path:'/tmp/mime-comic-web.png'});assert.deepEqual(errors,[]);
 }finally{await browser.close();}
});
