import test from 'node:test';
import assert from 'node:assert/strict';
import {createOskiewarAccount} from '../lib/oskiewar-host.mjs';
const secret='s'.repeat(43),token='test-private-access-token';
const saved={handle:'alice',session:{accessToken:token,account:{label:'@alice'}}};
const result={matchId:'lan:abc-1',seat:0,handles:['alice','bob'],roundWins:[5,2],winner:0};
function fixture(signed=false) {
  let now=1000000;
  const files=new Map(signed?[['/mnt/.oskiewar-account.json',JSON.stringify(saved)]]:[]);
  files.set('/mnt/config.json','OS-DEVELOPER-CREDENTIALS');
  const calls=[];
  const system={readFile:p=>files.get(p)??null,writeFile:(p,v)=>{files.set(p,v);return true;},pty2:{active:false,
    spawn:(cmd,args)=>{calls.push({cmd,args,base:args[1].match(/curl --config (\S+)\.cfg/)[1]});return true;},kill:()=>{}}};
  const account=createOskiewarAccount(()=>system,()=>now);
  return {account,system,files,calls,reload:()=>createOskiewarAccount(()=>system,()=>now),advance:ms=>now+=ms,
    complete(body,status=200,rc=0) {const {base}=calls.at(-1);files.set(base+'.json',JSON.stringify(body));files.set(base+'.http',String(status));files.set(base+'.rc',String(rc));now+=251;return account.state();},
    config:()=>files.get(calls.at(-1).base+'.cfg'),body:()=>JSON.parse(files.get(calls.at(-1).base+'.body'))};
}
test('browser pairing stores account privately and never returns or puts credentials in process args',()=>{
  const f=fixture();assert.equal(f.account.action('login'),true);
  assert.deepEqual(f.body(),{action:'create',kind:'browser'});
  let state=f.complete({code:'ABC234',pollSecret:secret});
  assert.equal(state.status,'waiting');assert.equal(state.pairUrl,'https://aesthetic.computer/api/device-pair-login?code=ABC234');
  assert.ok(!JSON.stringify(state).includes(secret));
  f.advance(1500);f.account.state();assert.ok(f.config().includes(secret));
  assert.ok(!JSON.stringify(f.calls).includes(secret));assert.ok(!JSON.stringify(f.calls).includes(token));
  state=f.complete({status:'claimed',...saved});assert.equal(state.status,'signed-in');assert.equal(state.handle,'alice');
  assert.ok(!JSON.stringify(state).includes(token));assert.deepEqual(JSON.parse(f.files.get('/mnt/.oskiewar-account.json')),saved);
  assert.equal(f.account.action('logout'),true);assert.equal(f.account.state().status,'signed-out');
  assert.equal(f.files.get('/mnt/config.json'),'OS-DEVELOPER-CREDENTIALS');assert.equal(f.files.get('/mnt/.oskiewar-account.json'),'{}');
});
test('invalid codes, incomplete claims and expired codes cannot sign in',()=>{
  for(const code of ['ABC000','ABC234?','<tag>']) {const f=fixture();f.account.action('login');assert.equal(f.complete({code,pollSecret:secret}).status,'error');}
  const f=fixture();f.account.action('login');f.complete({code:'ABC234',pollSecret:secret});f.advance(600001);assert.equal(f.account.state().status,'error');
  const g=fixture();g.account.action('login');g.complete({code:'ABC234',pollSecret:secret});g.advance(1500);g.account.state();assert.equal(g.complete({status:'claimed',handle:'alice',session:{}}).status,'error');
});
test('cancel ignores a late consumed claim and preserves developer credentials',()=>{
  const f=fixture();f.account.action('login');f.complete({code:'ABC234',pollSecret:secret});f.advance(1500);f.account.state();f.account.action('cancel');
  assert.equal(f.complete({status:'claimed',...saved}).status,'signed-out');assert.equal(f.files.get('/mnt/.oskiewar-account.json'),'{}');
});
test('does not replace another running secondary PTY',()=>{
  const f=fixture();f.system.pty2.active=true;f.account.action('login');assert.equal(f.calls.length,0);assert.equal(f.account.state().status,'error');
});
test('authenticated report keeps token in private config and retries exact body after transient failure',()=>{
  const f=fixture(true);assert.equal(f.account.report(JSON.stringify(result)),true);const original=f.body();
  assert.ok(f.config().includes('Authorization: Bearer '+token));assert.ok(!JSON.stringify(f.calls).includes(token));
  assert.equal(f.complete({error:'busy'},503).reportStatus,'Retrying result submission');assert.equal(f.calls.length,1);
  f.advance(3000);f.account.state();assert.equal(f.calls.length,2);assert.deepEqual(f.body(),original);
  assert.equal(f.complete({status:'pending',recorded:false},202).reportStatus,'Waiting for opponent confirmation');
  f.advance(60000);f.account.state();assert.equal(f.calls.length,2);
});
test('permanent report rejections stop retries and surface auth or consensus failure',()=>{
  for(const [code,message] of [[401,'Sign in again to submit results'],[409,'Result reports disagreed'],[400,'Result rejected']]) {
    const f=fixture(true);f.account.report(JSON.stringify(result));assert.equal(f.complete({error:'rejected'},code).reportStatus,message);
    f.advance(60000);f.account.state();assert.equal(f.calls.length,1);
  }
});
test('invalid result cannot target arbitrary endpoints or other player seats',()=>{
  const f=fixture(true);
  for(const patch of [{seat:1},{matchId:'https://bad.example'},{handles:['bob','alice']},{roundWins:[Infinity,2]}])
    assert.equal(f.account.report(JSON.stringify({...result,...patch})),false);
  assert.equal(f.calls.length,0);
});
test('leaderboard filters blank and duplicate handles, caches public response and throttles 30 seconds',()=>{
  const f=fixture();assert.equal(f.account.action('leaderboard','["alice",""]'),true);assert.ok(f.config().includes('?handles=alice"'));
  const board={top:[{handle:'alice',matchesWon:1}],players:[],pair:null};assert.deepEqual(f.complete(board).leaderboard,board);
  assert.equal(f.account.action('leaderboard','["alice","alice"]'),false);f.advance(30000);
  assert.equal(f.account.action('leaderboard','["alice","alice"]'),true);assert.ok(f.config().includes('?handles=alice"'));
  assert.equal(f.complete({error:'offline'},503).leaderboardError,'Leaderboard unavailable');
});

test('pending browser QR survives reload unchanged, stays private and resumes polling',()=>{
  const f=fixture();f.account.action('login');const before=f.complete({code:'ABC234',pollSecret:secret});
  const persisted=JSON.parse(f.files.get('/mnt/.oskiewar-account.json'));
  assert.deepEqual(persisted,{pending:{code:'ABC234',pollSecret:secret,expiresAt:before.expiresAt}});
  const restored=f.reload();const after=restored.state();assert.equal(after.status,'waiting');
  assert.equal(after.code,before.code);assert.equal(after.pairUrl,before.pairUrl);assert.equal(after.expiresAt,before.expiresAt);
  assert.ok(!JSON.stringify(after).includes(secret));
  assert.equal(restored.action('login'),true);assert.equal(f.calls.length,1);
  f.advance(1500);restored.state();assert.equal(f.calls.length,2);
  assert.ok(f.config().includes('code=ABC234&secret='+secret));assert.ok(!JSON.stringify(f.calls).includes(secret));
  assert.equal(restored.action('cancel'),true);assert.equal(f.reload().state().status,'signed-out');
  assert.equal(f.files.get('/mnt/config.json'),'OS-DEVELOPER-CREDENTIALS');
});
test('expired or malformed saved pairing is discarded without restoring or exposing it',()=>{
  for(const pending of [
    {code:'ABC234',pollSecret:secret,expiresAt:999999},
    {code:'ABC000',pollSecret:secret,expiresAt:1005000},
    {code:'ABC234',pollSecret:'invalid',expiresAt:1005000},
    {code:'ABC234',pollSecret:secret,expiresAt:999999999},
  ]) {
    const f=fixture();f.files.set('/mnt/.oskiewar-account.json',JSON.stringify({pending}));
    const state=f.reload().state();assert.equal(state.status,'signed-out');assert.equal(state.code,'');
    assert.equal(f.files.get('/mnt/.oskiewar-account.json'),'{}');assert.equal(f.calls.length,0);
    assert.equal(f.files.get('/mnt/config.json'),'OS-DEVELOPER-CREDENTIALS');
  }
});
test('pending pairing is not shown as saved when private USB write fails',()=>{
  const f=fixture();f.account.action('login');const write=f.system.writeFile;
  f.system.writeFile=(p,v,...args)=>p==='/mnt/.oskiewar-account.json'?false:write(p,v,...args);
  const state=f.complete({code:'ABC234',pollSecret:secret});assert.equal(state.status,'error');assert.equal(state.code,'');
  assert.ok(!JSON.stringify(state).includes(secret));assert.equal(f.files.get('/mnt/config.json'),'OS-DEVELOPER-CREDENTIALS');
});
test('logout discards persisted pending QR while cancel on a signed-in account retains session',()=>{
  const f=fixture();f.account.action('login');f.complete({code:'ABC234',pollSecret:secret});f.account.action('logout');
  assert.equal(f.reload().state().status,'signed-out');assert.equal(f.files.get('/mnt/.oskiewar-account.json'),'{}');
  const g=fixture(true);g.account.action('cancel');assert.equal(g.reload().state().status,'signed-in');
  assert.deepEqual(JSON.parse(g.files.get('/mnt/.oskiewar-account.json')),saved);
});
