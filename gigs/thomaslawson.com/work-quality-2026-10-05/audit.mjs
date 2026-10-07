// Run on poorslice; public-site evidence stays in the private build directory.
import fs from 'node:fs/promises';
import {createRequire} from 'node:module';
const require = createRequire('/Users/aesthetic/aesthetic-computer/package.json');
const puppeteer = require('puppeteer-core');
const root = '/Users/aesthetic/Developer/ac-builds/tom-closeout-20261005';
const browser = await puppeteer.connect({browserWSEndpoint:JSON.parse(await fs.readFile(root+'/browser.json')).ws, defaultViewport:null});
const out = root + '/' + (process.argv[2] || 'baseline');
await fs.mkdir(out, {recursive:true});
const page = await browser.newPage();
const errors=[];page.on('pageerror', e=>errors.push(e.message));
const urls = ['/', '/about/', '/notes/', '/in-the-studio/', '/inthestudio_2022-present/', '/inthestudio_1980-1982/', '/beyond-the-studio/', '/beyond-the-studio-portraits-of-new-york/', '/bookshelf/', '/bookshelf-reallife/', '/art-in-a-broader-context/', '/exhibitions/', '/contact/'];
const records=[];
for (const path of urls) {
  await page.setViewport({width:390,height:844,isMobile:true,hasTouch:true,deviceScaleFactor:1});
  const response=await page.goto('https://www.thomaslawson.com'+path,{waitUntil:'networkidle2',timeout:60000});
  await page.waitForSelector('#tl-site-header',{timeout:15000});
  if(process.argv.includes('--preview')) {
    await page.addStyleTag({path:root+'/quality.css'});
    await page.addScriptTag({path:root+'/quality.js'});
  }
  await page.addScriptTag({path:require.resolve('axe-core/axe.min.js')});
  const result=await page.evaluate(async()=>({
    title:document.title,overflow:document.documentElement.scrollWidth>innerWidth,
    heading:document.querySelector('main h1')?.textContent,
    violations:(await axe.run({runOnly:{type:'tag',values:['wcag2a','wcag2aa','wcag21aa','best-practice']}})).violations.map(v=>({id:v.id,impact:v.impact,nodes:v.nodes.map(n=>({html:n.html,target:n.target,summary:n.failureSummary}))})),
    brokenImages:[...document.querySelectorAll('main img')].filter(e=>e.complete&&!e.naturalWidth&&e.getClientRects().length).map(e=>({src:e.src,alt:e.alt})),
    headings:[...document.querySelectorAll('main h1, main h2, main h3')].filter(e=>e.getClientRects().length).slice(0,12).map(e=>[e.tagName,e.textContent.trim()])
  }));
  records.push({path,status:response.status(),...result,errors:errors.splice(0)});
  await fs.writeFile(out+'/audit.json',JSON.stringify(records,null,2));
  if(['/', '/about/', '/inthestudio_2022-present/', '/bookshelf/'].includes(path)) {
    await page.evaluate(async()=>{const img=document.querySelector('.tl-feature img,main img');if(img&&!img.complete)await Promise.race([img.decode().catch(()=>{}),new Promise(r=>setTimeout(r,10000))]);});
    await page.evaluate(()=>scrollTo(0,0));
    await page.screenshot({path:out+'/'+(path.replaceAll('/','')||'home')+'-390.png'});
  }
  console.log(JSON.stringify({path,status:response.status(),overflow:result.overflow,violations:result.violations.map(v=>[v.id,v.nodes.length]),broken:result.brokenImages.length}));
}
await page.goto('https://www.thomaslawson.com/inthestudio_2022-present/',{waitUntil:'networkidle2'});
await page.waitForSelector('.tl-header-search');
if(process.argv.includes('--preview')) {
  await page.addStyleTag({path:root+'/quality.css'});
  await page.addScriptTag({path:root+'/quality.js'});
}
await page.click('.tl-header-search');
await page.type('#tl-search-input','Candlelight');
await page.waitForSelector('.tl-search-card');
const searchBefore=await page.evaluate(()=>[...document.querySelectorAll('.tl-search-card')].map(e=>({text:e.innerText,url:e.href})));
await page.click('.tl-search-card');
if(previewOrLive()) await page.waitForFunction(()=>!document.querySelector('#tl-search-panel')?.open,{timeout:10000});
const searchAfter=await page.evaluate(()=>({url:location.href,dialog:document.querySelector('#tl-search-panel').open,target:document.querySelector('.tl-search-target')?.innerText,active:document.activeElement?.outerHTML.slice(0,250)}));
await fs.writeFile(out+'/same-page-search.json',JSON.stringify({searchBefore,searchAfter},null,2));
console.log(JSON.stringify({samePageSearch:searchAfter}));
await page.close();await browser.disconnect();process.exit(0);
function previewOrLive(){return process.argv.includes('--preview')||process.argv[2]==='live';}
