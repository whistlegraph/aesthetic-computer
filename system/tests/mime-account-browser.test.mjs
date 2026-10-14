import test from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {chromium} from 'playwright';
import {serveMimeInterface} from './fixtures/mime-interface.mjs';
const pub = new URL('../public/', import.meta.url);
for (const signup of [false, true]) test(`logo ${signup ? 'sign-up' : 'login'} returns to the same MIME post and updates its viewer`, {timeout:45000}, async()=>{
  const browser=await chromium.launch({channel:process.env.PLAYWRIGHT_CHANNEL,headless:true});
  try {
    const page=await browser.newPage({viewport:{width:390,height:844}}),errors=[];
    page.on('pageerror',e=>errors.push(e.message));
    await page.addInitScript(()=>{
      window.auth0={Auth0Client:class {
        async loginWithRedirect(options){
          sessionStorage.setItem('auth-fixture',JSON.stringify(options));
          location.href='/?code=fixture&state=fixture&ac-automation=1';
        }
        async handleRedirectCallback(){
          localStorage.setItem('session-aesthetic',encodeURIComponent(btoa(JSON.stringify({accessToken:'expired-host-token',account:{sub:'fixture'}}))));
          localStorage.setItem('@@auth0spajs@@::LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt::fixture','1');
          return {appState:JSON.parse(sessionStorage.getItem('auth-fixture')).appState};
        }
        async getTokenSilently(){return 'fixture';}
        async logout(){localStorage.clear();location.href='/?ac-automation=1';}
      }};
    });
    const post={code:'piece_fixture',name:'@author',when:new Date().toISOString(),board:'text/plain',text:'Viewer test',media:{kind:'mood',code:'fixture'}};
    await page.route('**/*',async route=>{
      const shared=serveMimeInterface(route);if(shared)return shared;
      const u=new URL(route.request().url());
      if(u.pathname==='/api/mime'&&u.searchParams.has('me'))assert.equal(route.request().headers().authorization,'Bearer fixture','Fresh OAuth credentials win over a stale host session');
      if(u.pathname==='/api/mime')return route.fulfill({json:u.searchParams.has('me')?{handle:'@fixture'}:u.searchParams.has('thread')?{op:post,replies:[]}:{recent:[post],hasMore:false}});
      if(u.hostname==='mime.ac'&&(u.pathname==='/'||u.pathname==='/mood~fixture'))return route.fulfill({contentType:'text/html',body:await readFile(new URL('mime/index.html',pub))});
      if(u.pathname.endsWith('ComicRelief-Bold.ttf'))return route.fulfill({contentType:'font/ttf',body:await readFile(new URL('whistlegraph.app/ComicRelief-Bold.ttf',pub))});
      return route.fulfill({body:''});
    });
    await page.goto('https://mime.ac/mood~fixture?ac-automation=1');
    await page.locator('.mime-startup').waitFor({state:'detached'});
    assert.equal(await page.locator('#create-work').count(),0);
    const logo=page.getByRole('link',{name:'MIME account'});
    await logo.click();
    const panel=page.getByRole('dialog',{name:'MIME',exact:true});
    await panel.getByRole('button',{name:'Log in',exact:true}).waitFor();
    assert.equal(await panel.getByRole('button',{name:"I'm new",exact:true}).isVisible(),true);
    assert.equal(await page.title(),'MIME — every file has a type');
    assert.equal(await page.evaluate(()=>interfaceView().media.active),false);
    await page.screenshot({path:'/tmp/mime-account-signed-out.png'});
    await page.keyboard.press('Escape');
    assert.equal(await panel.isVisible(),false);
    assert.equal(await logo.evaluate(el=>el===document.activeElement),true);
    await logo.click();
    await panel.getByRole('button',{name:signup?"I'm new":'Log in',exact:true}).click();
    await page.waitForFunction(()=>interfaceView().media.viewerHandle==='@fixture');
    assert.equal(new URL(page.url()).pathname,'/mood~fixture');
    const request=await page.evaluate(()=>JSON.parse(sessionStorage.getItem('auth-fixture')));
    assert.equal(request.appState.path,'/mood~fixture');
    assert.equal(request.authorizationParams.screen_hint,signup?'signup':undefined);
    assert.equal(request.authorizationParams.redirect_uri,'https://mime.ac/');
    await logo.click();
    await page.getByRole('dialog',{name:'@fixture',exact:true}).waitFor();
    await page.screenshot({path:'/tmp/mime-account-signed-in.png'});
    await page.getByRole('button',{name:'Open editor',exact:true}).click();
    assert.equal(await page.getByRole('link',{name:'Open editor in the app'}).getAttribute('href'),'mime://create');
    await page.keyboard.press('Escape');
    await logo.click();await page.getByRole('button',{name:'Sign out',exact:true}).click();
    await page.waitForFunction(()=>interfaceView().account.status==='signedOut');
    assert.equal(await page.evaluate(()=>interfaceView().media.viewerHandle),null);
    assert.deepEqual(errors,[]);
  }finally{await browser.close();}
});
