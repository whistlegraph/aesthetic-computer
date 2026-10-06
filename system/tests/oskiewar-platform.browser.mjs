// Run on Poorslice: desktop/touch boot and a module-only release update.
import assert from 'node:assert/strict';
import {createServer} from 'node:http';
import {readFile} from 'node:fs/promises';
import {extname} from 'node:path';
import puppeteer from 'puppeteer';
import {runtimeManifest,assetFile} from '../../xbox/tools/oskiewar-manifest.mjs';
const original=runtimeManifest();let manifest=original;
const types={'.html':'text/html','.mjs':'text/javascript','.js':'text/javascript','.json':'application/json','.svg':'image/svg+xml','.png':'image/png','.ttf':'font/ttf','.woff2':'font/woff2'};
const server=createServer(async(req,res)=>{
 const url=new URL(req.url,'http://localhost');
 if(url.pathname==='/oskiewar-release.json'){res.setHeader('Content-Type','application/json');res.end(JSON.stringify(manifest));return;}
 if(url.pathname.startsWith('/api/')){res.setHeader('Content-Type','application/json');res.end('{}');return;}
 const path=url.pathname==='/'?'/mac-test.html':url.pathname;
 if(!original.files[path]){res.statusCode=404;res.end();return;}
 res.setHeader('Content-Type',types[extname(path)]||'application/octet-stream');
 res.setHeader('ETag',original.files[path].sha256);res.setHeader('Cache-Control','no-store');
 res.end(await readFile(assetFile(path)));
});
await new Promise(r=>server.listen(0,'127.0.0.1',r));
const browser=await puppeteer.launch({headless:true,executablePath:process.env.CHROME_BIN||'/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'});
try{
 for(const mobile of [false,true]){
  manifest=original;
  const page=await browser.newPage(),errors=[];page.on('pageerror',e=>errors.push(e.message));
  await page.setViewport(mobile?{width:390,height:844,isMobile:true,hasTouch:true}:{width:1280,height:900});
  await page.goto(`http://127.0.0.1:${server.address().port}/?practice${mobile?'&touch':''}`);
  await page.waitForFunction(()=>globalThis.__oskiewarTouch?.screen==='title',{timeout:30000});
  assert.equal(await page.evaluate(()=>globalThis.__oskiewarRelease?.release),original.release);
  // Game ETag stays identical: the complete release must trigger this reload.
  manifest={...original,release:'f'.repeat(64)};
  const navigation=page.waitForNavigation({waitUntil:'domcontentloaded'});
  await page.evaluate(()=>dispatchEvent(new Event('pageshow')));await navigation;
  await page.waitForFunction(()=>globalThis.__oskiewarRelease?.release==='f'.repeat(64)&&globalThis.__oskiewarTouch?.screen==='title');
  {const b=await (await page.$('#screen')).boundingBox();if(mobile)await page.touchscreen.tap(b.x+b.width*.5,b.y+b.height*.65);else await page.mouse.click(b.x+b.width*.5,b.y+b.height*.65);}
  await page.waitForFunction(()=>globalThis.__oskiewarTouch?.screen!=='title');
  manifest={...original,release:'e'.repeat(64)};
  await page.evaluate(()=>dispatchEvent(new Event('pageshow')));
  await page.waitForFunction(()=>document.querySelector('#status').textContent==='update is ready');
  assert.deepEqual(errors,[]);await page.close();
 }
 console.log('PASS: desktop/touch boot; module-only release reloads title and preserves active play with update notice.');
}finally{await browser.close();await new Promise(r=>server.close(r));}
