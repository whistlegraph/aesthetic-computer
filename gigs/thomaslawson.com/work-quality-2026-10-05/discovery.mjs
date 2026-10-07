import fs from 'node:fs/promises';
import {createRequire} from 'node:module';
import assert from 'node:assert/strict';
const require=createRequire('/Users/aesthetic/aesthetic-computer/package.json');
const root='/Users/aesthetic/Developer/ac-builds/tom-closeout-20261005';
const origin='https://www.thomaslawson.com';
const out=root+'/discovery-live';await fs.mkdir(out,{recursive:true});
const results=[];
const check=(name,value)=>{assert.ok(value,name);results.push({name,pass:true});console.log('PASS '+name);};
const browser=await require('puppeteer-core').connect({browserWSEndpoint:JSON.parse(await fs.readFile(root+'/browser.json')).ws,defaultViewport:null});
const page=await browser.newPage();
try{
 const index=await fetch(origin+'/wp-json/tl/v1/search-index').then(r=>r.json());
 const works=index.items.filter(r=>r.type==='Artwork');
 const candle=works.find(r=>r.title==='Candlelight');
 check('Every public artwork has a unique readable URL',new Set(works.map(r=>r.url)).size===works.length&&works.every(r=>r.url.startsWith(origin+'/artwork/')));
 const sitemap=await fetch(origin+'/tl-artwork-sitemap.xml').then(r=>r.text());
 check('Sitemap lists every public artwork and the archive',works.every(r=>sitemap.includes('<loc>'+r.url+'</loc>'))&&sitemap.includes(origin+'/art-archive/'));
 const robots=await fetch(origin+'/robots.txt').then(r=>r.text());
 check('Robots advertises both WordPress and artwork sitemaps',robots.includes('/wp-sitemap.xml')&&robots.includes('/tl-artwork-sitemap.xml'));
 await page.setViewport({width:390,height:844,isMobile:true,hasTouch:true,deviceScaleFactor:1});
 await page.setJavaScriptEnabled(false);
 await page.goto(origin+'/art-archive/',{waitUntil:'networkidle2'});
 check('Archive contains artwork links without JavaScript',await page.$$eval('.tl-quality-records a',links=>links.length===30&&links.every(a=>a.pathname.startsWith('/artwork/'))));
 await page.goto(candle.url,{waitUntil:'networkidle2'});
 const data=await page.evaluate(()=>({h1:document.querySelector('main h1')?.textContent,text:document.querySelector('main')?.innerText,canonical:[...document.querySelectorAll('link[rel=canonical]')].map(e=>e.href),schema:JSON.parse(document.querySelector('#tl-quality-schema')?.textContent||'{}'),robots:[...document.querySelectorAll('meta[name=robots]')].map(e=>e.content),visible:document.querySelector('main')?.getBoundingClientRect().height>100}));
 check('Artwork title and metadata are visible without JavaScript',data.h1==='Candlelight'&&data.text.includes('Thomas Lawson')&&data.visible);
 check('Artwork has one canonical and indexable robots metadata',data.canonical.length===1&&data.canonical[0]===candle.url&&!data.robots.some(r=>r.includes('noindex')));
 const entity=data.schema['@graph']?.find(e=>e['@type']==='VisualArtwork');
 check('Structured artwork data matches the visible record',entity?.name==='Candlelight'&&entity.url===candle.url&&entity.creator['@id']===origin+'/#artist');
 await fs.writeFile(out+'/server-rendered-record.json',JSON.stringify(data,null,2));
 await page.screenshot({path:out+'/artwork-no-js.png'});
 for(const type of ['Artwork','Writing','Exhibition','Project','Page']){
  const count=index.items.filter(r=>r.type===type).length;
  const max=Math.max(1,Math.ceil(count/30));
  for(let n=1;n<=max;n++){
   const url=new URL('/art-archive/',origin);url.searchParams.set('type',type);url.searchParams.set('pg',n);
   const response=await fetch(url);const text=await response.text();
   assert.equal(response.status,200,url.href);assert.ok(text.includes('tl-quality-records'),url.href);
  }
  check(type+' archive pagination responds with server-rendered records',true);
 }
 for(const path of ['/artwork/unknown-test-record/','/art-archive/?pg=999'])check('Unknown route retains 404: '+path,(await fetch(origin+path)).status===404);
 await page.setJavaScriptEnabled(true);
 await page.goto(origin+'/inthestudio_2022-present/',{waitUntil:'networkidle2'});
 await page.tap('.tl-header-search');await page.type('#tl-search-input','Candlelight');await page.waitForSelector('.tl-search-card');
 await page.tap('.tl-search-image');
 await page.waitForFunction(url=>location.href===url&&document.querySelector('main h1')?.textContent==='Candlelight',{timeout:30000},candle.url);
 await page.waitForSelector('#tl-search-panel');
 check('Artwork search opens the requested record on mobile',page.url()===candle.url&&await page.$eval('main h1',e=>e.textContent==='Candlelight'));
 check('Search overlay is closed after navigation',await page.$eval('#tl-search-panel',e=>!e.open));
 await page.click('nav[aria-label="Artwork context"] a');
 await page.waitForSelector('.tl-search-target',{timeout:10000});
 check('Artwork context link locates the work in its studio period',await page.$eval('.tl-search-target',e=>e.innerText.includes('Candlelight')));
 await page.goto(origin+'/about/',{waitUntil:'networkidle2'});
 const media=await page.$$eval('video,iframe',items=>items.map(e=>({tag:e.tagName,src:e.src,sources:[...e.querySelectorAll('source')].map(s=>s.src),tracks:[...e.querySelectorAll('track')].map(t=>({kind:t.kind,src:t.src}))})));
 results.push({name:'About media inventory',media});console.log(JSON.stringify({media}));
}catch(error){results.push({failure:error.message});console.error(error.stack);process.exitCode=1;}
await fs.writeFile(out+'/results.json',JSON.stringify(results,null,2));
await page.close();await browser.disconnect();process.exit(process.exitCode||0);
