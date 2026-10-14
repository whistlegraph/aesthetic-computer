import test from 'node:test';
import assert from 'node:assert/strict';
import { readFile, mkdir } from 'node:fs/promises';
import { chromium } from 'playwright';
import { chatPost } from '../backend/mime-feed-engine.mjs';

const root = new URL('../public/', import.meta.url);
const piece = code => ({code, parent:null, name:'@fixture', when:new Date().toISOString(), media:{kind:'wgpiece',code},file:{name:code,type:'text/javascript',url:'/fixture'}});
const youtube = chatPost({_id:'6acfadf5fcbe3ccb3a32290d',text:'https://www.youtube.com/watch?v=8pT-d9juqCo&list=RD8pT-d9juqCo&start_radio=1',when:new Date().toISOString()},'@prutti','clock','qjj');

test('live center, held input, corner cooldown, edge glow, emergency escape and YouTube', async () => {
  const browser = await chromium.launch({headless:true, channel:process.env.PLAYWRIGHT_CHANNEL});
  const context = await browser.newContext({viewport:{width:390,height:844},hasTouch:true});
  const errors=[];
  const screenshot = async name => {
    if (!process.env.MIME_SCREENSHOT_DIR) return;
    await mkdir(process.env.MIME_SCREENSHOT_DIR,{recursive:true});
    await page.screenshot({path:process.env.MIME_SCREENSHOT_DIR+'/'+name+'.png'});
  };
  await context.route('**/*', async route => {
    const url=new URL(route.request().url());
    const path=url.pathname;
    if (url.hostname==='mime.ac' && (path==='/' || path.startsWith('/p/') || path.startsWith('/wg'))) return route.fulfill({contentType:'text/html',body:await readFile(new URL('mime/index.html',root),'utf8')});
    if(path==='/api/mime')return route.fulfill({contentType:'application/json',body:JSON.stringify({recent:[piece('wgFirst'),piece('wgSecond'),youtube],hasMore:false})});
    if(path==='/api/mime-feed')return route.fulfill({contentType:'application/json',body:JSON.stringify({source:'export function paint(){}'})});
    if(path.startsWith('/mime-play/')||path.startsWith('/mime-youtube/'))return route.fulfill({contentType:path.endsWith('.mjs')?'text/javascript':'text/html',body:await readFile(new URL(path.slice(1)+(path.endsWith('/')?'index.html':''),root),'utf8')});
    if(path==='/wipe')return route.fulfill({contentType:'text/html',body:`<style>body{margin:0;height:100vh;display:grid;place-items:center;background:#264;font:30px system-ui}button{font:inherit;padding:35px;background:#ffbada;border:0;border-radius:50%}</style><button onclick="this.textContent='Touched '+(++window.taps)">Touch me</button><script>
      window.taps=0;window.preloaded=true;window.AC={setMasterVolume(){},startAudio(){}};window.acPAUSE=()=>{};window.acRESUME=()=>{};
      window.acSEND=message=>{if(message.type!=="dropped:piece")return;window.preloaded=true;parent.postMessage({type:'ready'},'*')};
      parent.postMessage({type:'ready'},'*');
    </script>`});
    if(path==='/iframe_api')return route.fulfill({contentType:'text/javascript',body:`window.YT={Player:class{constructor(slot,options){window.ytOptions=options;this.options=options;let f=document.createElement('iframe');f.title='YouTube video player';f.src='https://www.youtube-nocookie.com/embed/'+options.videoId;slot.replaceWith(f);setTimeout(()=>options.events.onReady(),0)}pauseVideo(){window.ytPaused=true}}};window.onYouTubeIframeAPIReady();`});
    if(url.hostname==='www.youtube-nocookie.com')return route.fulfill({contentType:'text/html',body:'<body style="background:black;color:white">YouTube video player</body>'});
    if(path.endsWith('/hqdefault.jpg'))return route.fulfill({contentType:'image/svg+xml',body:'<svg xmlns="http://www.w3.org/2000/svg" width="640" height="360"><rect width="640" height="360" fill="#753"/></svg>'});
    return route.fulfill({body:''});
  });
  const page=await context.newPage();page.on('pageerror',e=>errors.push(e.message));
  const swipe=async(x,y,dy)=>{await page.mouse.move(x,y);await page.mouse.down();await page.mouse.move(x,y+dy,{steps:8});await page.mouse.up();};
  try {
    await page.goto('https://mime.ac/?ac-automation=1');
    await page.locator('.mime-startup').waitFor({state:'detached'});
    const first=page.locator('#wgFirst');
    const button=page.frameLocator('#wgFirst .piece-preview > iframe').frameLocator('iframe').getByRole('button');
    await button.click();
    assert.equal(await button.innerText(),'Touched 1','first touch reaches the program');
    assert.equal(await first.getAttribute('data-full'),'');
    const box=await button.boundingBox();
    await page.mouse.move(box.x+box.width/2,box.y+box.height/2);await page.mouse.down();
    await page.waitForTimeout(1400);
    assert.equal(await first.getAttribute('data-full'),'','held finger does not time out');
    await screenshot('center-active');
    await page.mouse.up();
    await swipe(195,810,-180);
    assert.equal(await page.evaluate(()=>scrollY),0,'wider strip cannot escape during cooldown');
    await page.waitForFunction(()=>!document.querySelector('#wgFirst').hasAttribute('data-cooling'));
    assert.equal(await first.getAttribute('data-full'),null);
    await page.mouse.move(195,810);await page.mouse.down();
    assert.equal(await page.locator('#wgFirst .reel-edge.bottom:not(.escape)').getAttribute('data-glow'),'');
    await screenshot('edge-ready');
    assert.equal(await first.getAttribute('data-full'),null,'returning focus to an edge cannot hide the labels again');
    await page.mouse.move(195,610,{steps:8});await page.mouse.up();
    await page.waitForFunction(()=>Math.abs(document.querySelector('#wgSecond').getBoundingClientRect().top)<1);
    const secondButton=page.frameLocator('#wgSecond .piece-preview > iframe').frameLocator('iframe').getByRole('button');
    await secondButton.click();
    assert.equal(await page.locator('#wgSecond').getAttribute('data-cooling'),'');
    await swipe(2,420,-180);
    await page.waitForFunction(()=>Math.abs(document.querySelector('#chat_clock_6acfadf5fcbe3ccb3a32290d').getBoundingClientRect().top)<1);
    const youtubeFrame=page.frameLocator('#'+youtube.code+' .piece-preview > iframe');
    await youtubeFrame.getByRole('button',{name:'Play YouTube video'}).click();
    await youtubeFrame.locator('iframe[title="YouTube video player"]').waitFor();
    assert.match(await youtubeFrame.getByRole('link',{name:'Watch on YouTube'}).getAttribute('href'),/8pT-d9juqCo/);
    assert.equal(await page.locator('#'+youtube.code+' .reel-command .media-label').getAttribute('aria-label'),'chat~qjj');
    await page.waitForTimeout(1300);
    await screenshot('youtube');
    await swipe(2,420,180);
    await page.waitForFunction(()=>scrollY<1000);
    const ytHandle=await page.locator('#'+youtube.code+' .piece-preview > iframe').elementHandle();
    const yt=await ytHandle.contentFrame();
    assert.equal(await yt.evaluate(()=>window.ytPaused),true,'offscreen YouTube pauses');
    assert.deepEqual(errors,[]);
  } finally {await context.close();await browser.close();}
});
