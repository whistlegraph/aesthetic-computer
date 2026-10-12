const assert=require('node:assert/strict'),{readFile}=require('node:fs/promises'),puppeteer=require('puppeteer');
(async()=>{const browser=await puppeteer.launch({headless:true,executablePath:process.env.CHROME_PATH||'/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'});try{
 const page=await browser.newPage(),html=await readFile('system/public/mime/index.html','utf8');await page.setViewport({width:430,height:800});await page.setRequestInterception(true);
 page.on('request',request=>{const u=new URL(request.url());if(u.hostname==='mime.ac'&&u.pathname==='/')return request.respond({contentType:'text/html',body:html});if(u.pathname==='/api/mime')return request.respond({contentType:'application/json',body:JSON.stringify({recent:[{code:'wadfixture',parent:null,name:'@maker',when:new Date().toISOString(),file:{name:'arena.wad',type:'application/x-doom-wad',url:'/api/mime?file=wadfixture'}}],hasMore:false})});if(u.pathname==='/doom-player/')return request.respond({contentType:'text/html',body:'<button onclick="this.textContent=\'playing\'">Play</button>'});return request.respond({status:200,body:''});});
 await page.goto('https://mime.ac/?ac-automation=1');await page.waitForSelector('.doom-player');await page.waitForFunction(()=>!document.querySelector('.mime-startup'));
 const label=await page.$eval('.reel-command .media-label',el=>el.getAttribute('aria-label'));assert.equal(label,'doom~wadfixture');
 const url=await page.$eval('.doom-player',el=>el.src);assert.equal(new URL(url).pathname,'/doom-player/');assert.equal(new URL(url).searchParams.get('file'),'https://mime.ac/api/mime?file=wadfixture');
 const frame=await page.$('.doom-player'),box=await frame.boundingBox();assert.ok(box.width>400&&box.height>700,'WAD player fills the reel');
 console.log('PASS MIME WAD: global AC identifier, shared player URL, full card layout');
 }finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
