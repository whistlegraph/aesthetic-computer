import fs from 'node:fs/promises';
import {createRequire} from 'node:module';
const require=createRequire('/Users/aesthetic/aesthetic-computer/package.json');
const root='/Users/aesthetic/Developer/ac-builds/tom-closeout-20261005';
const origin='https://www.thomaslawson.com';
const out=root+'/'+(process.argv[2]||'live');await fs.mkdir(out,{recursive:true});
const browser=await require('puppeteer-core').connect({browserWSEndpoint:JSON.parse(await fs.readFile(root+'/browser.json')).ws,defaultViewport:null});
const pages=await fetch(origin+'/wp-json/wp/v2/pages?per_page=100&_fields=id,link,slug').then(r=>r.json());
const index=await fetch(origin+'/wp-json/tl/v1/search-index').then(r=>r.json());
const art=index.items.filter(row=>row.type==='Artwork');
const samples=[art.find(r=>r.title==='Candlelight'),art.find(r=>r.title.toLowerCase()==='greed'),art.at(-1)].filter(Boolean);
const urls=[...new Set(['/',...pages.map(p=>new URL(p.link).pathname),'/inthestudio_2022-present/','/inthestudio_2020-2022/','/exhibitions/','/art-archive/',...samples.map(r=>new URL(r.url).pathname)])];
await fs.writeFile(out+'/inventory.json',JSON.stringify({pages,indexCount:index.items.length,artworkCount:art.length,urls},null,2));
const records=[];let cursor=0;
async function worker(){
 const page=await browser.newPage();const errors=[];page.on('pageerror',e=>errors.push(e.message));
 await page.emulateMediaFeatures([{name:'prefers-reduced-motion',value:'reduce'}]);
 while(cursor<urls.length){
  const path=urls[cursor++];
  try{
   await page.setViewport({width:390,height:844,isMobile:true,hasTouch:true,deviceScaleFactor:1});
   const response=await page.goto(origin+path,{waitUntil:'networkidle2',timeout:60000});
   await page.waitForFunction(()=>document.documentElement.dataset.tlQuality,{timeout:15000});
   await page.addScriptTag({path:require.resolve('axe-core/axe.min.js')});
   const data=await page.evaluate(async()=>({title:document.title,canonical:document.querySelector('link[rel=canonical]')?.href,
    violations:(await axe.run({runOnly:{type:'tag',values:['wcag2a','wcag2aa','wcag21aa','best-practice']}})).violations.map(v=>({id:v.id,impact:v.impact,nodes:v.nodes.map(n=>({html:n.html,target:n.target,summary:n.failureSummary}))})),
    brokenImages:[...document.querySelectorAll('main img')].filter(e=>e.complete&&!e.naturalWidth&&e.getClientRects().length).map(e=>({src:e.src,alt:e.alt})),
    overflow390:document.documentElement.scrollWidth>innerWidth
   }));
   const overflow={390:data.overflow390};delete data.overflow390;
   if(['/','/about/','/art-archive/','/beyond-the-studio-portraits-of-new-york/'].includes(path)||path===new URL(samples[0].url).pathname){
    await page.evaluate(async()=>{const img=document.querySelector('.tl-feature img,main img');if(img&&!img.complete)await Promise.race([img.decode().catch(()=>{}),new Promise(r=>setTimeout(r,10000))]);});
    await page.screenshot({path:out+'/'+(path.replaceAll('/','')||'home')+'-390.png'});
   }
   for(const width of [320,768,1440]){
    await page.setViewport({width,height:900,isMobile:true,hasTouch:true,deviceScaleFactor:1});
    await new Promise(resolve=>setTimeout(resolve,150));
    overflow[width]=await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth);
   }
   if(path==='/art-archive/'||path===new URL(samples[0].url).pathname)await page.screenshot({path:out+'/'+path.replaceAll('/','')+'-1440.png'});
   records.push({path,status:response.status(),...data,overflow,errors:errors.splice(0)});
   console.log(JSON.stringify({path,status:response.status(),violations:data.violations.map(v=>[v.id,v.nodes.length]),overflow,broken:data.brokenImages.length}));
  }catch(error){records.push({path,error:error.message});console.log(JSON.stringify({path,error:error.message}));}
 }
 await page.close();
}
await Promise.all([worker(),worker()]);
await fs.writeFile(out+'/audit.json',JSON.stringify(records,null,2));
console.log(JSON.stringify({total:records.length,issues:records.filter(r=>r.error||r.status!==200||r.violations?.length||r.brokenImages?.length||r.errors?.length||Object.values(r.overflow||{}).some(Boolean)).map(r=>r.path)}));
await browser.disconnect();process.exit(0);
