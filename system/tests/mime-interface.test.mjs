import test from 'node:test';
import assert from 'node:assert/strict';
import {readFile, mkdtemp, rm} from 'node:fs/promises';
import {createContext, runInContext} from 'node:vm';
import {spawnSync} from 'node:child_process';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import {fileURLToPath} from 'node:url';
const source = await readFile(new URL('../public/mime/interface.js', import.meta.url), 'utf8');
const context = createContext({}); runInContext(source, context);
const policy = context.MimeInterface;
const plain = value => JSON.parse(JSON.stringify(value));
const events = [
  {}, {type:'account.open'}, {type:'account.login'}, {type:'account.signup'},
  {type:'account.changed',account:{status:'checking',handle:'@stale',token:'private'}},
  {type:'account.login'},
  {type:'account.changed',account:{status:'ready',handle:'jeffrey'}},
  {type:'editor.open'}, {type:'account.open'}, {type:'account.signOut'},
  {type:'account.changed',account:{status:'ready',handle:''}}, {type:'account.handle'}, {type:'account.retry'},
  {type:'account.changed',account:{status:'failed',notice:'Try signing in again'}},
  {type:'account.signup'}, {type:'feed.open'},
  {type:'work.create',mimeType:'text/javascript'}, {type:'work.remix',post:'piece_abc',version:2},
  {type:'work.remix',post:'../../bad',version:2}, {type:'work.create',mimeType:'javascript:alert(1)'},
  {type:'work.remix',post:'piece_abc',version:-1}, {type:'execute',source:'unsafe'},
];
const run = () => {
  let state = {};
  return events.map(event => { const result = plain(policy.dispatch(state,event)); state=result.state; return {view:result.view,effects:result.effects}; });
};
test('MIME policy owns account states, public identity, navigation and bounded editor intents',()=>{
  const results=run();
  assert.deepEqual(results[1].view.account.actions.map(a=>a.label),['Log in',"I'm new",'Back to feed']);
  assert.equal(results[1].view.media.active,false);
  assert.deepEqual(results[2].effects,[{type:'authenticate',signUp:false}]);
  assert.deepEqual(results[3].effects,[{type:'authenticate',signUp:true}]);
  assert.equal(results[4].view.media.viewerHandle,null);
  assert.deepEqual(results[5].effects,[]);
  assert.equal(results[6].view.media.viewerHandle,'@jeffrey');
  assert.equal(results[7].view.media.active,true);
  assert.deepEqual(results[7].effects,[{type:'openEditor'}]);
  assert.equal(results[9].view.media.viewerHandle,null);
  assert.equal(results[10].view.account.status,'needsHandle');
  assert.deepEqual(results[16].effects,[{type:'createWork',mimeType:'text/javascript'}]);
  assert.deepEqual(results[17].effects,[{type:'remixWork',post:'piece_abc',version:2}]);
  assert.ok(results.slice(18).every(r=>r.effects.length===0));
  assert.doesNotMatch(JSON.stringify(results),/private|token|stale/);
  assert.deepEqual(plain(policy.dispatch({}, {type:'editor.open'})).effects,[{type:'openEditor'}], 'App deep links can reach the editor account gate');
  const saved=plain(policy.dispatch({capabilities:{saved:true}}, {type:'saved.open'}));
  assert.deepEqual(saved.effects,[{type:'openSaved'}]);
  assert.deepEqual(plain(policy.dispatch({}, {type:'saved.open'})).effects,[]);
  const state={screen:'feed',account:{status:'ready',handle:'@jeffrey'}};
  const before=JSON.stringify(state); policy.dispatch(state,{type:'account.signOut'}); assert.equal(JSON.stringify(state),before);
});
test('indexed slogan and App Store subtitle agree with the shared brand', async()=>{
  const html=await readFile(new URL('../public/mime/index.html',import.meta.url),'utf8');
  assert.ok(html.includes('<title>'+policy.brand.title+'</title>'));
  assert.ok(html.includes('property="og:title" content="'+policy.brand.title+'"'));
  assert.ok(html.includes('name="twitter:title" content="'+policy.brand.title+'"'));
  assert.equal((await readFile(new URL('../../apple/mime/fastlane/metadata/ios/en-US/subtitle.txt',import.meta.url),'utf8')).trim(),policy.brand.slogan);
});
test('Swift JavaScriptCore and the web execute the same interface transitions', {skip:process.platform!=='darwin',timeout:60000},async()=>{
  const temp=await mkdtemp(join(tmpdir(),'mime-interface-'));
  try{
    const root=fileURLToPath(new URL('../../',import.meta.url)),binary=join(temp,'check');
    const build=spawnSync('swiftc',['-framework','JavaScriptCore','apple/mime/Sources/MimeInterface.swift','apple/mime/Tests/MimeInterfaceCheck.swift','-o',binary],{cwd:root,encoding:'utf8',timeout:45000});
    assert.equal(build.status,0,build.stderr);
    const native=spawnSync(binary,['system/public/mime/interface.js'],{cwd:root,input:JSON.stringify(events),encoding:'utf8',timeout:10000});
    assert.equal(native.status,0,native.stderr);
    assert.deepEqual(JSON.parse(native.stdout),run());
  }finally{await rm(temp,{recursive:true,force:true});}
});
