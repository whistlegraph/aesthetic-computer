import {createServer} from 'node:http';
import {readFile,writeFile} from 'node:fs/promises';
import {fileURLToPath} from 'node:url';
import path from 'node:path';
import assert from 'node:assert/strict';
import {chromium} from 'playwright';
const root=path.dirname(fileURLToPath(import.meta.url));
const server=createServer(async(req,res)=>{
 try{const pathname=new URL(req.url,'http://local').pathname;
 const file=path.resolve(root,'.'+(pathname==='/'?'/index.html':pathname));
 if(!file.startsWith(root+path.sep))throw Error('outside');
 const bytes=await readFile(file);res.setHeader('Content-Type',file.endsWith('.mjs')?'text/javascript':file.endsWith('.html')?'text/html':'image/png');res.end(bytes);
 }catch{res.statusCode=404;res.end();}
});
await new Promise(r=>server.listen(0,'127.0.0.1',r));
const browser=await chromium.launch({headless:true,channel:process.env.CHROME_CHANNEL||'chrome'});
try{
 const page=await browser.newPage({viewport:{width:1280,height:784},deviceScaleFactor:1});
 const errors=[];page.on('pageerror',e=>errors.push(e.message));
 await page.goto(`http://127.0.0.1:${server.address().port}`);
 await page.waitForFunction(()=>globalThis.themePreview?.ready);
 await page.evaluate(()=>themePreview.setTime(2));
 await page.locator('canvas').screenshot({path:path.join(root,'preview-miniature.png')});
 const before=await page.evaluate(()=>themePreview.state());
 await page.selectOption('select','flat');
 await page.evaluate(()=>themePreview.render(themePreview.state().time));
 await page.locator('canvas').screenshot({path:path.join(root,'preview-flat.png')});
 const after=await page.evaluate(()=>themePreview.state());assert.equal(after.time,before.time);assert.equal(after.theme,'flat');
 await page.selectOption('select','photorealistic');
 await page.click('#motion');
 await page.waitForTimeout(2200);
 const stats=await page.evaluate(()=>themePreview.state());
 assert.ok(stats.time>before.time);assert.deepEqual(errors,[]);
 const samples=stats.samples.sort((a,b)=>a-b);
 const result={status:'browser prototype verified',switchablePrototype:true,nativeVerification:'see native-verification.json',frames:stats.frames,canvas:[1280,720],
  drawSubmissionMs:{median:samples[Math.floor(samples.length*.5)],p95:samples[Math.floor(samples.length*.95)]},
  measurement:'Local Chromium CPU draw submission only; not native GPU completion or FPS. No device test.',
  pausedThemeSwitchPreservesTime:true,errors};
 await writeFile(path.join(root,'verification.json'),JSON.stringify(result,null,2)+'\n');console.log(JSON.stringify(result,null,2));
}finally{await browser.close();server.close();}
