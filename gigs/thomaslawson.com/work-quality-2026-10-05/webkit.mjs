import fs from 'node:fs/promises';
import {createRequire} from 'node:module';
import assert from 'node:assert/strict';
const root='/Users/aesthetic/Developer/ac-builds/tom-closeout-20261005';
const require=createRequire(root+'/package.json');
const {webkit,devices}=require('playwright');
const browser=await webkit.launch({headless:true});
const context=await browser.newContext({...devices['iPhone 13'],reducedMotion:'reduce'});
const page=await context.newPage();const results=[];const preview=process.argv.includes('--preview');
const out=root+'/'+(preview?'webkit-preview':'webkit-live');await fs.mkdir(out,{recursive:true});
async function open(path){await page.goto('https://www.thomaslawson.com'+path,{waitUntil:'networkidle'});await page.locator('#tl-site-header').waitFor();if(preview){await page.addStyleTag({path:root+'/quality.css'});await page.addScriptTag({path:root+'/quality.js'});}}
async function check(name,fn){assert.ok(await fn(),name);results.push({name,pass:true});console.log('PASS '+name);}
try{
  await open('/');
  await page.waitForFunction(()=>document.querySelector('.tl-feature img')?.naturalWidth>0);
  await check('WebKit portrait artwork fits mobile viewport',()=>page.evaluate(()=>{const r=document.querySelector('.tl-feature-art').getBoundingClientRect();return r.left>=0&&r.right<=innerWidth&&document.documentElement.scrollWidth===innerWidth;}));
  const title=await page.locator('.tl-feature-carousel').getAttribute('data-title');
  await page.getByRole('button',{name:'Next work',exact:true}).tap();
  await check('WebKit touch carousel works',async()=>await page.locator('.tl-feature-carousel').getAttribute('data-title')!==title);
  await page.locator('.tl-header-menu').tap();await check('WebKit menu opens',()=>page.locator('#tl-menu-panel').evaluate(e=>e.open));
  await page.getByRole('button',{name:'Close navigation',exact:true}).tap();
  await page.locator('.tl-header-search').tap();await page.locator('#tl-search-input').fill('Greed');
  await page.locator('.tl-search-card').first().waitFor();await check('WebKit search returns matching artwork',()=>page.locator('.tl-search-title').first().innerText().then(t=>t.toLowerCase()==='greed'));
  await page.getByRole('button',{name:'Close search the archive',exact:true}).tap();
  await page.evaluate(()=>scrollTo(0,0));await page.screenshot({path:out+'/home.png'});
  await open('/about/');
  await page.locator('.tl-quality-image-button').first().tap();await page.locator('.tl-quality-viewer').waitFor();
  await check('WebKit image dialog is modal',()=>page.locator('.tl-quality-viewer').evaluate(e=>e.open));
  await page.getByRole('button',{name:'Close image',exact:true}).tap();await page.locator('.tl-quality-viewer').waitFor({state:'detached'});
  await check('WebKit image dialog restores focus',()=>page.evaluate(()=>document.activeElement.matches('.tl-quality-image-button')));
}catch(error){results.push({failure:error.message});console.error(error.stack);process.exitCode=1;}
await fs.writeFile(out+'/results.json',JSON.stringify(results,null,2));await browser.close();process.exit(process.exitCode||0);
