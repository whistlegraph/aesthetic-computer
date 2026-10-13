import test from 'node:test';
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import { chromium } from 'playwright';

test('MIME new posts preserve runtimes, preloading is bounded and silent, and wg links are public', async () => {
  const browser = await chromium.launch({channel: process.env.PLAYWRIGHT_CHANNEL, headless:true});
  const context = await browser.newContext({viewport:{width:700,height:700}});
  const page = await context.newPage();
  const html = await readFile(new URL('../public/mime/index.html', import.meta.url),'utf8');
  const host = await readFile(new URL('../public/mime-play/index.html', import.meta.url),'utf8');
  const script = await readFile(new URL('../public/mime-play/player.mjs', import.meta.url),'utf8');
  const post = (code, kind='wgpiece') => ({code:kind+'_'+code,parent:null,name:'@jeffrey',when:new Date().toISOString(),media:{kind,code},file:{name:code,type:'text/javascript',url:'/fixture'}});
  const older = [post('wgFirst'),post('second','piece'),post('third','piece'),post('fourth','piece'),post('fifth','piece')];
  const fresh=post('wgNewest'), archived=post('wgNirin');let recent=older, feedReads=0;
  const runtimeLoads=[];
  await context.route('**/*',async route=>{
    const u=new URL(route.request().url());
    if(u.hostname==='mime.ac' && (u.pathname==='/'||u.pathname.startsWith('/p/')||/^\/wg/.test(u.pathname)))return route.fulfill({contentType:'text/html',body:html});
    if(u.pathname==='/api/mime'){
      if(u.searchParams.has('feed'))feedReads++;
      const code=u.searchParams.get('thread');
      const op=[...older,fresh,archived].find(p=>p.code===code||p.media.code===code);
      return route.fulfill({contentType:'application/json',body:JSON.stringify(code?{op,replies:[]}:{recent,hasMore:false})});
    }
    if(u.pathname==='/api/mime-feed')return route.fulfill({contentType:'application/json',body:JSON.stringify({source:'export function paint() {}'})});
    if(u.pathname==='/mime-play/')return route.fulfill({contentType:'text/html',body:host});
    if(u.pathname==='/mime-play/player.mjs')return route.fulfill({contentType:'text/javascript',body:script});
    if(['/wipe','/second','/third','/fourth','/fifth'].includes(u.pathname)){
      runtimeLoads.push(u.pathname);assert.equal(u.searchParams.get('noauth'),'true');assert.equal(u.searchParams.get('mime'),'1');
      return route.fulfill({contentType:'text/html',body:`<body style="background:#365;color:white"><canvas></canvas><script>
        window.volume=0;window.running=true;window.preloaded=true;window.dropped=0;
        window.AC={setMasterVolume:n=>volume=n,startAudio:()=>{}};
        window.acPAUSE=()=>running=false;window.acRESUME=()=>running=true;
        window.acSEND=()=>{dropped++;preloaded=true;setTimeout(()=>parent.postMessage({type:'ready'},'*'),20)};
        setTimeout(()=>parent.postMessage({type:'ready'},'*'),20);
      </script>`});
    }
    return route.fulfill({body:''});
  });
  const errors=[];page.on('pageerror',e=>errors.push(e.message));
  const engine=async id=>{const wrapper=await page.locator('#'+id+' .piece-preview > iframe').elementHandle();const host=await wrapper.contentFrame();const inner=await host.$('iframe');return inner.contentFrame();};
  try{
    await page.goto('https://mime.ac/?ac-automation=1');
    await page.waitForFunction(()=>document.querySelector('#piece_third iframe')?.dataset.ready==='true');
    assert.equal(await page.locator('.piece-preview > iframe').count(),3,'current plus two ahead');
    assert.ok(!runtimeLoads.includes('/fourth'),'distant program is not booted');
    const second=await engine('piece_second');
    assert.deepEqual(await second.evaluate(()=>({volume,running})),{volume:0,running:false},'warm program holds silently');
    await page.locator('.mime-startup').waitFor({state:'detached'});
    await page.evaluate(()=>{window.savedCard=document.getElementById('piece_second');window.savedFrame=savedCard.querySelector('iframe');savedCard.scrollIntoView();});
    await page.waitForURL('**/p/piece_second?*');
    await second.waitForFunction(()=>running&&volume===1);
    recent=[fresh,...older];
    await page.evaluate(()=>document.dispatchEvent(new Event('visibilitychange')));
    await page.locator('.new-pill').waitFor();
    const reads=feedReads;
    await page.locator('.new-pill').click();
    await page.waitForURL('**/wgNewest?*');
    await page.waitForFunction(()=>scrollY===0);
    assert.equal(feedReads,reads,'new button uses posts already fetched');
    assert.equal(await page.locator('.new-pill').count(),0);
    assert.equal(await page.locator('.mime-startup').count(),0,'no restart/loading overlay');
    assert.equal(await page.evaluate(()=>savedCard===document.getElementById('piece_second')&&savedFrame===savedCard.querySelector('iframe')),true,'existing cards and engines are retained');
    assert.equal(await page.locator('#wgpiece_wgNewest').count(),1);
    assert.equal(await page.locator('header h1').evaluate(e=>getComputedStyle(e).opacity),'1');
    await page.evaluate(()=>document.dispatchEvent(new Event('visibilitychange')));
    await page.waitForTimeout(100);assert.equal(await page.locator('.new-pill').count(),0,'no duplicate notification');
    await page.goto('https://mime.ac/p/wgpiece_wgNirin?ac-automation=1');
    await page.waitForURL('**/wgNirin?*');
    assert.equal(await page.locator('.feed-post').first().getAttribute('id'),'wgpiece_wgNirin');
    await page.reload();await page.waitForSelector('#wgpiece_wgNirin');
    assert.equal(new URL(page.url()).pathname,'/wgNirin');
    assert.deepEqual(errors,[]);
  }catch(error){console.log('browser errors',errors);console.log('frames',page.frames().map(f=>f.url()));throw error;}finally{await browser.close();}
});
