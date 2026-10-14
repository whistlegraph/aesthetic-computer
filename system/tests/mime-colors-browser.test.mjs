import {serveMimeInterface} from './fixtures/mime-interface.mjs';
import test from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {chromium} from 'playwright';

test('saved handle colors survive animated lettering and MIME family/subtype fills stay distinct',async()=>{
 const browser=await chromium.launch({channel:process.env.PLAYWRIGHT_CHANNEL,headless:true});
 try {
  const page=await browser.newPage({viewport:{width:704,height:500}});
  const html=await readFile(new URL('../public/mime/index.html',import.meta.url),'utf8');
  const font=await readFile(new URL('../../apple/mime/Resources/Fonts/ComicRelief-Bold.ttf',import.meta.url));
  const colors=[{r:200,g:90,b:230},{r:40,g:210,b:150},{r:50,g:160,b:255}];
  const posts=['text/plain','text/html','application/x-doom-wad'].map((type,i)=>({code:'color'+i,name:'@ab',nameColors:colors,when:new Date().toISOString(),board:type,text:'Color study',media:{kind:'mood',code:'color'+i}}));
  await page.route('**/*',route=>{
    const shared = serveMimeInterface(route); if (shared) return shared;
    const u=new URL(route.request().url());
    if(u.pathname==='/')return route.fulfill({contentType:'text/html',body:html});
    if(u.pathname==='/api/mime')return route.fulfill({contentType:'application/json',body:JSON.stringify({recent:posts,hasMore:false})});
    if(u.pathname.endsWith('/ComicRelief-Bold.ttf'))return route.fulfill({contentType:'font/ttf',body:font});
    return route.fulfill({body:''});
  });
  await page.goto('https://mime.ac/?ac-automation=1');
  await page.waitForSelector('.reel-type');
  await page.waitForFunction(()=>!document.querySelector('.mime-startup'));
  const fills=await page.evaluate(()=>[...document.querySelectorAll('.feed-post')].map(post=>({
    author:[...post.querySelectorAll('.byline > .name .pal-over > span')].map(n=>getComputedStyle(n).color),
    type:[...post.querySelectorAll('.reel-type .pal-over > span')].map(n=>getComputedStyle(n).color),
    label:post.querySelector('.reel-type').getAttribute('aria-label'),
    handle:post.querySelector('.byline > .name').getAttribute('aria-label'),
  })));
  assert.deepEqual(fills[0].author,['rgb(200, 90, 230)','rgb(40, 210, 150)','rgb(50, 160, 255)']);
  assert.equal(fills[0].handle,'@ab');
  assert.equal(fills[0].type[0],fills[1].type[0]);
  assert.notEqual(fills[0].type[5],fills[1].type[5]);
  assert.notEqual(fills[2].type[0],fills[2].type[12]);
  assert.equal(fills[0].label,'text/plain');
  await page.screenshot({path:'/tmp/mime-semantic-colors.png'});
 } finally {await browser.close();}
});
