import fs from 'node:fs/promises';
import {createRequire} from 'node:module';
import assert from 'node:assert/strict';
const require=createRequire('/Users/aesthetic/aesthetic-computer/package.json');
const root='/Users/aesthetic/Developer/ac-builds/tom-closeout-20261005';
const browser=await require('puppeteer-core').connect({browserWSEndpoint:JSON.parse(await fs.readFile(root+'/browser.json')).ws,defaultViewport:null});
const page=await browser.newPage();const results=[];
await page.setViewport({width:390,height:844,isMobile:true,hasTouch:true,deviceScaleFactor:1});
try{
 for(const path of ['/inthestudio_1980-1982-snapshot-2022-10-07/','/','/about/']){
  await page.goto('https://www.thomaslawson.com'+path,{waitUntil:'networkidle2'});
  await page.addScriptTag({path:require.resolve('axe-core/axe.min.js')});
  const violations=await page.evaluate(async()=>(await axe.run({runOnly:{type:'tag',values:['wcag2a','wcag2aa','wcag21aa','best-practice']}})).violations.map(v=>({id:v.id,nodes:v.nodes.map(n=>n.html)})));
  results.push({path,violations});console.log(JSON.stringify({path,violations}));assert.equal(violations.length,0);
 }
 await page.goto('https://www.thomaslawson.com/beyond-the-studio-theatre-dance-fashion/',{waitUntil:'networkidle2'});
 await page.bringToFront();
 await page.$eval('video',v=>{v.muted=true;v.scrollIntoView({block:'center'});});
 await page.focus('video');await page.keyboard.press('Space');
 await page.waitForFunction(()=>document.querySelector('video').currentTime>0,{timeout:30000});
 const video=await page.$eval('video',v=>({time:v.currentTime,duration:v.duration,readyState:v.readyState,error:v.error?.message,paused:v.paused,muted:v.muted,tracks:[...v.textTracks].map(t=>({label:t.label,kind:t.kind})),src:v.currentSrc,viewport:innerWidth,width:v.getBoundingClientRect().width,overflow:document.documentElement.scrollWidth>innerWidth}));
 assert.ok(video.time>0&&!video.paused&&!video.error&&!video.overflow&&video.width<=390);
 results.push({name:'Deirdre plays inline from keyboard at mobile width (muted)',video});console.log(JSON.stringify({video}));
 await page.screenshot({path:root+'/live-final/deirdre-playing.png'});await page.$eval('video',v=>v.pause());
}catch(error){results.push({failure:error.message});console.error(error.stack);process.exitCode=1;}
await fs.writeFile(root+'/live-final/targeted-final.json',JSON.stringify(results,null,2));
await page.close();await browser.disconnect();process.exit(process.exitCode||0);
