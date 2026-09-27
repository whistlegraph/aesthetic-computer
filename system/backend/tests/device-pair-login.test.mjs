import test from 'node:test';
import assert from 'node:assert/strict';
import vm from 'node:vm';
import {handler} from '../../netlify/functions/device-pair-login.mjs';
const settle=async()=>{for(let i=0;i<6;i++)await new Promise(r=>setImmediate(r));};
async function browser(options={}) {
  const {body}=await handler(options.event||{queryStringParameters:{code:'ABC234'}});
  const nodes=new Map(),calls=[];
  function node() {return {value:'',children:[],style:{},hidden:false,disabled:false,className:'',listeners:{},
    get textContent(){return this.children.map(c=>typeof c==='string'?c:c.textContent).join('');},
    set textContent(value){this.children=[String(value)];},
    replaceChildren(...children){this.children=children;},
    addEventListener(type,fn){this.listeners[type]=fn;},focus(){this.focused=true;}};}

  for(const match of body.matchAll(/<[^>]+\bid="([^"]+)"[^>]*>/g)) {
    const id=match[1];nodes.set(id,Object.assign(node(),{hidden:/\bhidden\b/.test(match[0])}));
  }
  const codeNode={textContent:''};
  const otp={token:async()=>options.otpToken||null,sendCode:async email=>email,verify:async()=>({access:'otp-token'}),...options.otp};
  for(const method of ['token','sendCode','verify']) {const fn=otp[method];otp[method]=async(...args)=>{calls.push([method,...args]);return fn(...args);};}
  const auth={isAuthenticated:async()=>!!options.signedIn,getTokenSilently:async()=>'sdk-token',handleRedirectCallback:async()=>{},loginWithPopup:async()=>{},...options.auth};
  for(const method of Object.keys(auth)){const fn=auth[method];auth[method]=async(...args)=>{calls.push([method,...args]);return fn(...args);};}
  const values=new Map(options.storage||[]);
  const location={origin:'https://aesthetic.computer',search:options.search||'?code=ABC234'};
  const context={URLSearchParams,document:{createElement:()=>node(),getElementById:id=>nodes.get(id),querySelector:()=>codeNode,body:{classList:{toggle(){}}}},
    location,window:{location},sessionStorage:{getItem:key=>values.get(key),setItem:(key,value)=>values.set(key,value)},
    history:{replaceState(...args){calls.push(['replaceState',...args]);}},
    otpSignIn:config=>{calls.push(['otpConfig',config]);return otp;},
    auth0:{Auth0Client:function(config){calls.push(['sdkConfig',config]);return auth;}},
    fetch:async(url,request)=>{calls.push(['fetch',url,request]);return options.fetch?options.fetch(url,request):{ok:true,status:200,json:async()=>({handle:'alice',kind:'browser'})};}};
  vm.createContext(context);
  const script=body.match(/<script type="module">([\s\S]*?)<\/script>/)[1].replace(/^\s*import[^\n]+\n/,'');
  new vm.Script(script).runInContext(context);await settle();
  return {body,nodes,calls,codeNode,context,async fire(id,type='click'){await nodes.get(id).listeners[type]({preventDefault(){}});await settle();}};
}
test('unsigned phone shows native email and six-digit inputs, with no mail or redirect on load',async()=>{
  const f=await browser();assert.equal(f.nodes.get('email-form').hidden,false);assert.equal(f.nodes.get('code-form').hidden,true);
  assert.match(f.body,/id="email"[^>]*type="email"[^>]*autocomplete="email"/);
  assert.match(f.body,/id="otp"[^>]*inputmode="numeric"[^>]*autocomplete="one-time-code"/);
  assert.equal(f.calls.filter(c=>['sendCode','verify','fetch','loginWithPopup'].includes(c[0])).length,0);
  assert.equal(f.codeNode.textContent,'ABC234');assert.ok(!f.body.includes('loginWithRedirect'));
  assert.equal(f.calls.find(c=>c[0]==='otpConfig')[1].redirectUri,'https://aesthetic.computer');
});
test('cached OTP identity claims automatically before SDK without exposing token in text',async()=>{
  const f=await browser({otpToken:'private-otp'});assert.equal(f.calls.some(c=>c[0]==='sdkConfig'),false);
  const request=f.calls.find(c=>c[0]==='fetch');assert.equal(request[1],'/api/device-pair');
  assert.equal(request[2].headers.Authorization,'Bearer private-otp');assert.deepEqual(JSON.parse(request[2].body),{action:'claim',code:'ABC234'});
  assert.match(f.nodes.get('status').textContent,/Paired as @alice/);assert.equal(f.nodes.get('email-form').hidden,true);
  assert.ok(!f.nodes.get('status').textContent.includes('private-otp'));
});
test('cached SDK identity also claims and configures only the registered origin callback',async()=>{
  const f=await browser({signedIn:true});assert.equal(f.calls.find(c=>c[0]==='sdkConfig')[1].authorizationParams.redirect_uri,'https://aesthetic.computer');
  assert.equal(f.calls.find(c=>c[0]==='fetch')[2].headers.Authorization,'Bearer sdk-token');
});
test('typed email is trimmed on submit, then six digits verify and claim the scanned code',async()=>{
  const f=await browser();f.nodes.get('email').value=' person@example.com ';
  await f.fire('email-form','submit');assert.deepEqual(f.calls.find(c=>c[0]==='sendCode'),['sendCode','person@example.com']);
  assert.equal(f.nodes.get('email-form').hidden,true);assert.equal(f.nodes.get('code-form').hidden,false);assert.equal(f.nodes.get('otp').focused,true);
  f.nodes.get('otp').value='123456';await f.fire('code-form','submit');
  assert.deepEqual(f.calls.find(c=>c[0]==='verify'),['verify','person@example.com','123456']);
  assert.equal(f.nodes.get('status').className,'ok');
});
test('invalid email and short code stay editable and never call auth transport',async()=>{
  const f=await browser();f.nodes.get('email').value='nope';await f.fire('email-form','submit');
  assert.equal(f.calls.some(c=>c[0]==='sendCode'),false);assert.equal(f.nodes.get('email').value,'nope');
  f.nodes.get('otp').value='12';await f.fire('code-form','submit');assert.equal(f.calls.some(c=>c[0]==='verify'),false);
});
test('send and verification errors preserve input and permit retries without automatic resend',async()=>{
  let failSend=true;
  const f=await browser({otp:{sendCode:async email=>{if(failSend)throw {hint:'could not reach sign-in service'};return email;},verify:async()=>{throw {hint:'that code does not match'};}}});
  f.nodes.get('email').value='person@example.com';await f.fire('email-form','submit');
  assert.equal(f.nodes.get('email-form').hidden,false);assert.equal(f.nodes.get('login-btn').disabled,false);assert.match(f.nodes.get('status').textContent,/could not reach/);
  failSend=false;await f.fire('email-form','submit');f.nodes.get('otp').value='654321';await f.fire('code-form','submit');
  assert.equal(f.nodes.get('otp').value,'654321');assert.equal(f.nodes.get('verify-btn').disabled,false);assert.match(f.nodes.get('status').textContent,/does not match/);
  assert.equal(f.calls.filter(c=>c[0]==='sendCode').length,2);assert.equal(f.calls.some(c=>c[0]==='fetch'),false);
});
test('tenant or captcha fault offers popup only after user click, using registered origin',async()=>{
  const f=await browser({otp:{sendCode:async()=>{throw {tenant:true,hint:'sign-in needs the web page'};}}});
  f.nodes.get('email').value='person@example.com';await f.fire('email-form','submit');
  assert.equal(f.nodes.get('popup-btn').hidden,false);assert.equal(f.calls.some(c=>c[0]==='loginWithPopup'),false);
  await f.fire('popup-btn');const popup=f.calls.find(c=>c[0]==='loginWithPopup');
  assert.equal(popup[1].authorizationParams.redirect_uri,'https://aesthetic.computer');assert.equal(f.nodes.get('status').className,'ok');
});
test('old redirect callback restores pairing code separately from OAuth code',async()=>{
  const f=await browser({signedIn:true,search:'?code=oauth-code&state=old-state',storage:[['ac-device-pair-code','XYZ234']],event:{rawQuery:'code=oauth-code&state=old-state'}});
  assert.equal(f.codeNode.textContent,'XYZ234');assert.equal(f.calls.some(c=>c[0]==='handleRedirectCallback'),true);
  assert.equal(JSON.parse(f.calls.find(c=>c[0]==='fetch')[2].body).code,'XYZ234');
});
test('bad scanned code never authenticates and dynamic handle is plain text',async()=>{
  const bad=await browser({event:{rawQuery:'code='+encodeURIComponent('</script><script>alert(1)</script>')}});
  assert.equal(bad.nodes.get('email-form').hidden,true);assert.equal(bad.calls.some(c=>['token','sdkConfig','fetch'].includes(c[0])),false);
  assert.ok(!bad.body.includes('<script>alert(1)</script>'));
  const f=await browser({otpToken:'private',fetch:async()=>({ok:true,status:200,json:async()=>({handle:'<img src=x>',kind:'browser'})})});
  assert.match(f.nodes.get('status').textContent,/<img src=x>/);assert.equal(f.nodes.get('status').innerHTML,undefined);
});
test('failed device claim retries with same private token without sending another email',async()=>{
  let fails=true;
  const f=await browser({otpToken:'private',fetch:async()=>{if(fails)throw Error('offline');return {ok:true,status:200,json:async()=>({handle:'alice',kind:'browser'})};}});
  assert.equal(f.nodes.get('retry-pair').hidden,false);fails=false;await f.fire('retry-pair');
  assert.equal(f.calls.filter(c=>c[0]==='fetch' && c[1]==='/api/device-pair').length,2);assert.equal(f.calls.some(c=>c[0]==='sendCode'),false);assert.equal(f.nodes.get('status').className,'ok');
});

 test('paired handle uses saved colors for every character including @, without sending auth to palette lookup',async()=>{
  const colors=Array.from({length:6},(_,i)=>({r:20+i,g:70+i,b:100+i}));
  const f=await browser({otpToken:'secret',fetch:async url=>({ok:true,status:200,json:async()=>url.startsWith('/api/oskiewar-leaderboard')?{players:[{handle:'alice',colors}]}:{handle:'alice',kind:'browser'}})});
  const handle=f.nodes.get('status').children[1];
  assert.equal(handle.textContent,'@alice');
  assert.deepEqual(handle.children.map(c=>c.style.color),colors.map(c=>'rgb('+[c.r,c.g,c.b].join(',')+')'));
  assert.equal(f.calls.find(c=>c[0]==='fetch' && c[1].startsWith('/api/oskiewar-leaderboard'))[2],undefined);
});
test('palette failures retain successful login and readable plain handle',async()=>{
  const f=await browser({otpToken:'secret',fetch:async url=>{if(url.startsWith('/api/oskiewar-leaderboard'))throw Error('offline');return {ok:true,status:200,json:async()=>({handle:'alice',kind:'browser'})};}});
  assert.equal(f.nodes.get('status').className,'ok');assert.match(f.nodes.get('status').textContent,/Paired as @alice/);
  assert.equal(f.nodes.get('retry-pair').hidden,true);
});
