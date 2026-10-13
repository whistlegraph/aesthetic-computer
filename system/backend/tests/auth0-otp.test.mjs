import test from 'node:test';
import assert from 'node:assert/strict';
import vm from 'node:vm';
import {readFileSync} from 'node:fs';
import {webcrypto} from 'node:crypto';
const source=readFileSync(new URL('../../public/aesthetic.computer/lib/auth0-otp.mjs',import.meta.url),'utf8').replaceAll('export ','');
function fixture({verificationError,authorizationError}={}) {
 const listeners=new Map();let mailed=0,removed=0;
 const context=vm.createContext({URL,TextEncoder,TextDecoder,crypto:webcrypto,btoa,atob,setTimeout,clearTimeout,
  addEventListener:(type,fn)=>listeners.set(type,fn),removeEventListener:type=>listeners.delete(type),
  document:{createElement:()=>({style:{},setAttribute(){},remove(){removed++;}}),body:{appendChild(frame){
    const at=new URL(frame.src);queueMicrotask(()=>listeners.get('message')?.({origin:at.origin,data:{type:'authorization_response',response:{...authorizationError,state:at.searchParams.get('state')}}}));
  }}},
  fetch:async(url)=>{if(url.endsWith('/passwordless/start'))mailed++;
    const error=url.endsWith('/co/authenticate')&&verificationError;
    return {ok:!error,status:error?400:200,text:async()=>JSON.stringify(error||{login_ticket:'test-ticket'})};}
 });
 vm.runInContext(source+';globalThis.otp=otpSignIn({clientId:"test",redirectUri:"https://aesthetic.computer",store:null,silentTimeout:1000});',context);
 return {otp:context.otp,mailed:()=>mailed,removed:()=>removed};
}
test('a rejected browser login ticket offers visible sign-in instead of resending the accepted email code',async()=>{
 const f=fixture({authorizationError:{error:'invalid_request',error_description:'Unknown login ticket.'}});
 await assert.rejects(f.otp.verify('person@example.test','123456'),error=>error.code==='silent-blocked'&&error.tenant===true&&!/expired/.test(error.hint));
 assert.equal(f.mailed(),0);assert.equal(f.removed(),1);
});
test('an expired email code remains distinguishable from a browser sign-in failure',async()=>{
 const f=fixture({verificationError:{error:'invalid_grant',error_description:'The verification code has expired.'}});
 await assert.rejects(f.otp.verify('person@example.test','123456'),error=>error.code==='expired'&&error.tenant===false);
 assert.equal(f.removed(),0);assert.equal(f.mailed(),0);
});
