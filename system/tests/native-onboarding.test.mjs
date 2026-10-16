import test from 'node:test';
import assert from 'node:assert/strict';
import { signupLayout } from '../public/aesthetic.computer/lib/onboarding-ui.mjs';
import * as support from '../public/aesthetic.computer/disks/support.mjs';
import * as handle from '../public/aesthetic.computer/disks/handle.mjs';
import { createOnboardingAudio } from '../public/aesthetic.computer/lib/onboarding-audio.mjs';

const view = { title: 'Enter the code', description: 'Sent to temporary@example.test. Check spam too.', field: { value: '123', label: 'Code' }, status: '', check: '', buttons: [{ id: 'submit', primary: true, label: 'Continue' }, { id: 'resend', label: 'Resend code' }, { id: 'back', label: 'Wrong email?' }] };
test('phone keyboard layouts keep the field and primary action visible and leave other controls scrollable', () => {
  for (const [w,h] of [[152,150],[179,180],[187,212],[207,380],[472,442]]) {
    const l = signupLayout(view,w,h);
    for (const id of ['field','submit']) {
      const b = l.items.find(b=>b.id===id);
      assert.ok(b.x>=0 && b.x+b.w<=w,`${id} width ${w}`);
      assert.ok(b.y>=34 && b.y+b.h<=h,`${id} height ${h}`);
      assert.ok(b.h>=24);
    }
    const end = signupLayout(view,w,h,10000);
    const back = end.items.find(b=>b.id==='back');
    assert.ok(back.y+back.h<=h);
  }
});
function fixture() {
  const writes=[], jumps=[], posts=[], audio=[];
  class Button { box={}; act(e,{push}) { if(e.click && !this.disabled && e.x>=this.box.x && e.x<this.box.x+this.box.w && e.y>=this.box.y && e.y<this.box.y+this.box.h)push(); } }
  const pen={box(){return pen;},write(s,p){writes.push({s,p});return pen;}};
  const store={handle:'x',retrieve:async()=>store['handle:color-preview'],persist(){},delete:async key=>{delete store[key]}};
  let result={status:200,url:'https://pay.aesthetic.computer/test'}, paid=false;
  const api={screen:{width:187,height:414},ui:{Button},store,query:{source:'signup'},ink:()=>pen,wipe(){},needsPaint(){},help:{repeat(){}},sound:{synth(){}},send:e=>audio.push(e),jump:u=>jumps.push(u),net:{userRequest:async(method,path,body)=>{if(method==='GET')return{status:200,monthlySupporter:paid};posts.push({path,body});return result;}}};
  const draw=piece=>{writes.length=0;piece.paint(api)};
  const click=(piece,label)=>{const item=writes.find(w=>w.s===label);assert.ok(item,`Missing ${label}`);piece.act({...api,event:{click:true,x:item.p.x,y:item.p.y,is:()=>false}})};
  return {api,writes,jumps,posts,audio,draw,click,setResult:v=>result=v,setPaid:v=>paid=v};
}
const settle=()=>new Promise(r=>setImmediate(r));
test('native amount entry authenticates perk checkout, rejects bad amounts, and avoids duplicate support',async()=>{
  const f=fixture(); support.boot(f.api);f.draw(support);
  f.click(support,'Another amount'); f.draw(support);
  for(const key of ['0','.','5','0']){f.click(support,key);f.draw(support)}
  f.click(support,'Done');f.draw(support);f.click(support,'Continue to checkout');await settle();f.draw(support);
  assert.equal(f.posts.length,0);assert.ok(f.writes.some(w=>w.s==='Choose $1 to $2,500.'));
  f.click(support,'$4');f.draw(support);f.click(support,'Continue to checkout');await settle();
  assert.equal(f.posts[0].body.amount,400);assert.equal(f.posts[0].body.source,'signup');assert.equal(f.posts[0].body.recurring,true);
  assert.equal(f.jumps.at(-1),'https://pay.aesthetic.computer/test');
  f.setResult({status:409,code:'already_supporting'});f.draw(support);f.click(support,'Continue to checkout');await settle();assert.equal(f.jumps.at(-1),'handle');
  f.setResult({status:200,url:'https://evil.example/test'});f.draw(support);f.click(support,'Continue to checkout');await settle();assert.equal(f.jumps.length,2);
  support.leave(f.api);
});
test('native handle preview persists through support and saving uses server entitlement',async t=>{
  t.mock.method(globalThis,'fetch',async()=>({json:async()=>({colors:[{r:1,g:2,b:3},{r:4,g:5,b:6}]})}));
  const f=fixture();await handle.boot(f.api);await settle();f.draw(handle);f.click(handle,'Support monthly');
  assert.equal(f.posts.length,0);assert.equal(f.jumps.at(-1),'support?source=signup');assert.equal(f.api.store['handle:color-preview'].colors[0].r,1);
  f.setPaid(true);f.setResult({status:200});handle.leave(f.api);await handle.boot(f.api);await settle();f.draw(handle);f.click(handle,'Save colors');await settle();
  assert.equal(f.posts[0].path,'/.netlify/functions/handle-colors');assert.equal(f.api.store['handle:color-preview'],undefined);
  f.setResult({status:403,code:'monthly_support_required'});handle.leave(f.api);await handle.boot(f.api);await settle();f.draw(handle);f.click(handle,'Save colors');await settle();f.draw(handle);assert.ok(f.writes.some(w=>w.s==='Support monthly'));handle.leave(f.api);
});
test('music needs a gesture, respects mute, and stops on leaving or hiding',async()=>{
 const listeners={},timers=new Map();let id=0,contexts=0;
 class Context {state='running';currentTime=0;destination={};constructor(){contexts++}createOscillator(){return{frequency:{},connect(){},start(){},stop(){},disconnect(){}}}createGain(){return{gain:{setValueAtTime(){},linearRampToValueAtTime(){},exponentialRampToValueAtTime(){}},connect(){},disconnect(){}}}}
 const win={AudioContext:Context,document:{hidden:false,addEventListener:(n,fn)=>listeners[n]=fn},sessionStorage:{getItem:()=>null,setItem(){}},setInterval:fn=>{timers.set(++id,fn);return id},clearInterval:id=>timers.delete(id)};
 const music=createOnboardingAudio(win);music.enter();assert.equal(contexts,0);listeners.pointerdown();assert.equal(timers.size,1);listeners.keydown();assert.equal(timers.size,1);
 music.toggle();assert.equal(timers.size,0);music.toggle();assert.equal(timers.size,1);win.document.hidden=true;listeners.visibilitychange();assert.equal(timers.size,0);music.leave();win.document.hidden=false;listeners.visibilitychange();assert.equal(timers.size,0);
});

test('reopening support without a source still authenticates the handle perk', async()=>{
 const f=fixture();f.api.query={};support.boot(f.api);f.draw(support);f.click(support,'Continue to checkout');await settle();
 assert.equal(f.posts[0].body.source,'handle-colors');support.leave(f.api);
});
test('the handle editor scrolls in a short landscape viewport',async t=>{
 t.mock.method(globalThis,'fetch',async()=>({json:async()=>({colors:null})}));
 const f=fixture();f.api.screen={width:320,height:152};f.api.store.handle='sixteenletters42';await handle.boot(f.api);await settle();f.draw(handle);
 handle.act({...f.api,event:{is:name=>name==='scroll',y:-1000}});f.draw(handle);
 const button=f.writes.find(w=>w.s==='Support monthly');assert.ok(button.p.y>=34 && button.p.y+17<=152);f.click(handle,'Support monthly');assert.equal(f.jumps.at(-1),'support?source=signup');handle.leave(f.api);
});
