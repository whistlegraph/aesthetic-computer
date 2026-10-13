import test from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {chromium} from 'playwright';
import {qrcode,ErrorCorrectLevel} from '../public/aesthetic.computer/dep/@akamfoad/qr/qr.mjs';

const publicRoot = new URL('../public/', import.meta.url);
test('top-left identifiers share only their MIME post, with a scannable QR, visible URL and copy action', async () => {
  const browser = await chromium.launch({channel:process.env.PLAYWRIGHT_CHANNEL,headless:true});
  try {
    const page = await browser.newPage({viewport:{width:704,height:334}});
    const posts = [
      {code:'whistlegraph_fixture',media:{kind:'whistlegraph',code:'wgHafar',url:'https://aesthetic.computer/wgHafar'}},
      {code:'mood_fixture',media:{kind:'mood',code:'abc'}},
      {code:'upload_fixture'},
    ].map(p=>({...p,parent:null,name:'@jeffrey',when:new Date().toISOString(),board:'text/plain',text:'A post to share'}));
    const errors=[]; page.on('pageerror',e=>errors.push(e.message));
    await page.addInitScript(()=>{
      Object.defineProperty(navigator,'clipboard',{configurable:true,value:{writeText:async text=>{window.copied=text;}}});
    });
    await page.route('**/*',async route=>{
      const u=new URL(route.request().url());
      if(u.pathname==='/')return route.fulfill({contentType:'text/html',body:await readFile(new URL('mime/index.html',publicRoot),'utf8')});
      if(u.pathname==='/api/mime')return route.fulfill({contentType:'application/json',body:JSON.stringify({recent:posts,hasMore:false})});
      if(u.pathname.endsWith('/mime-share.mjs') || u.pathname.endsWith('/qr/qr.mjs'))return route.fulfill({contentType:'text/javascript',body:await readFile(new URL('.'+u.pathname,publicRoot),'utf8')});
      return route.fulfill({body:''});
    });
    await page.goto('https://mime.ac/?ac-automation=1');
    await page.waitForFunction(()=>!document.querySelector('.mime-startup') && document.querySelector('.reel-command'));
    const expected=['https://mime.ac/wgHafar','https://mime.ac/p/mood~abc','https://mime.ac/p/upload_fixture'];
    for(let i=0;i<posts.length;i++) {
      const code=page.locator('#'+posts[i].code+' .reel-command .media-label');
      await code.scrollIntoViewIfNeeded();
      await page.waitForTimeout(400);
      const before=await page.evaluate(()=>({url:location.href,y:scrollY}));
      assert.equal(await code.getAttribute('href'),expected[i]);
      await code.click();
      const dialog=page.getByRole('dialog',{name:'Share this post'});
      await dialog.waitFor();
      const link=dialog.locator('.share-url');
      assert.equal(await link.getAttribute('href'),expected[i]);
      assert.equal(await link.textContent(),expected[i].replace('https://',''));
      const qr=dialog.locator('svg');
      const cells=qrcode(expected[i],{errorCorrectLevel:ErrorCorrectLevel.M}).modules;
      const path=[];cells.forEach((row,y)=>row.forEach((dark,x)=>{if(dark)path.push(`M${x+4} ${y+4}h1v1h-1z`);}));
      assert.equal(await qr.getAttribute('viewBox'),`0 0 ${cells.length+8} ${cells.length+8}`,'four-module quiet zone');
      assert.equal(await qr.locator('path').getAttribute('d'),path.join(''),'QR encodes exactly the visible URL');
      await dialog.getByRole('button',{name:'Copy link'}).click();
      assert.equal(await page.evaluate(()=>window.copied),expected[i]);
      await page.keyboard.press('ArrowDown');
      for(let n=0;n<5;n++) {
        await page.keyboard.press('Tab');
        assert.equal(await page.evaluate(()=>!!document.activeElement.closest('dialog')),true);
      }
      const bounds=await dialog.boundingBox();
      assert.ok(bounds.y>=0 && bounds.y+bounds.height<=334,JSON.stringify(bounds));
      if(i===0) {
        await page.screenshot({path:'/tmp/mime-share-desktop.png'});
        await page.setViewportSize({width:320,height:640});
        assert.equal(await dialog.evaluate(n=>n.scrollWidth<=n.clientWidth),true);
        await page.screenshot({path:'/tmp/mime-share-phone.png'});
        await page.setViewportSize({width:704,height:334});
      }
      if(i===1) await page.mouse.click(2,2); else await page.keyboard.press('Escape');
      await dialog.waitFor({state:'detached'});
      assert.equal(await code.evaluate(n=>document.activeElement===n),true);
      assert.deepEqual(await page.evaluate(()=>({url:location.href,y:scrollY})),before);
    }
    assert.deepEqual(errors,[]);
  } finally {await browser.close();}
});
