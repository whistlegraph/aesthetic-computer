import '../phone/shim/globals.js';
import { createSession } from '../phone/session.mjs';
import { accountStorage } from './storage.mjs';

// A few shared engine helpers postdate the oldest browser build we target.
if (!Array.prototype.at) Object.defineProperty(Array.prototype, 'at', {value(index) { return this[index < 0 ? this.length + index : index]; }});
if (!Array.prototype.findLast) Object.defineProperty(Array.prototype, 'findLast', {value(fn) { for(let i=this.length-1;i>=0;i--) if(fn(this[i],i,this)) return this[i]; }});
if (!AbortSignal.timeout) AbortSignal.timeout = ms => { const c=new AbortController(); setTimeout(()=>c.abort(),ms); return c.signal; };
if (!AbortSignal.prototype.throwIfAborted) AbortSignal.prototype.throwIfAborted = function() { if(this.aborted) throw new DOMException('Stopped','AbortError'); };

const $ = id => document.getElementById(id);
const callback = `${location.origin}/try/`;
let auth, session, streaming, subject = '', source = '', submitting = false;

function gate(message, error = false) {
  $('gate').hidden = false;
  $('workspace').hidden = true;
  $('gate-status').textContent = message;
  $('gate-status').className = error ? 'error' : '';
}
function line(kind, text) {
  const node = document.createElement('li');
  node.className = kind;
  node.textContent = text;
  $('log').append(node);
  return node;
}
function follow(fn) {
  const log = $('log'), near = log.scrollHeight - log.scrollTop - log.clientHeight < 100;
  fn();
  if (near) log.scrollTop = log.scrollHeight;
}
function busy(value) {
  $('send').disabled = value;
  $('stop').hidden = !value;
  $('new').disabled = value;
  $('history').disabled = value;
  $('logout').disabled = value;
}
function event(e) {
  if(e.type === 'you' || e.type === 'note' || e.type === 'bad') follow(()=>line(e.type,e.text));
  if(e.type === 'busy') busy(e.busy);
  if(e.type === 'status') $('status').textContent = e.text;
  if(e.type === 'signedIn') { $('handle').textContent = e.handle ? `@${e.handle}` : ''; $('logout').hidden = false; }
  if(e.type === 'credits') $('credits').textContent = Number.isFinite(e.total) ? `${e.total.toLocaleString()} braincells` : '';
  if(e.type === 'piece' || e.type === 'source') { source = e.source || ''; $('download').disabled = !source; }
  if(e.type === 'thread') {
    $('log').replaceChildren(); streaming = null;
    $('preview').hidden = true; $('preview').removeAttribute('src'); $('empty').hidden = false; $('piece-link').hidden = true;
    for(const item of e.events || []) event(item);
  }
  if(e.type === 'preview') {
    const url = new URL(e.url);
    if(url.origin !== 'https://aesthetic.computer') return;
    $('preview').src = url.href; $('preview').hidden = false; $('empty').hidden = true;
    $('piece-link').href = url.href; $('piece-link').hidden = false;
  }
  if(e.type === 'history') {
    const selected = session?.state.id;
    $('history').replaceChildren(...e.items.map(item => { const o=document.createElement('option');o.value=item.id;o.textContent=item.title;o.selected=item.id===selected;return o; }));
  }
  if(e.type === 'bridge') {
    const p=e.params || {};
    if(e.method === 'turn/started') streaming = null;
    if(e.method === 'turn/progress') $('status').textContent = p.phase || 'Working…';
    if(e.method === 'item/agentMessage/delta') follow(()=>{ if(!streaming) streaming=line('answer',''); streaming.append(document.createTextNode(p.delta || '')); });
    if(e.method === 'item/completed' && p.item?.type === 'agentMessage') {
      if(!streaming) follow(()=>line('answer',p.item.text || ''));
      streaming=null;
    }
    if(e.method === 'turn/completed') {
      streaming=null;
      if(p.turn?.error?.message) line('bad',p.turn.error.message);
      $('status').textContent=p.turn?.error ? 'Could not finish' : p.turn?.status === 'interrupted' ? 'Stopped' : 'Ready';
    }
  }
}

async function enter() {
  const user=await auth.getUser();
  if(!user?.sub) throw new Error('Sign in again to open your pieces.');
  subject=user.sub;
  const token=await auth.getTokenSilently();
  session=createSession({storage:accountStorage(localStorage,user.sub),emit:event});
  await session.adoptToken(token);
  if(!session.state.handle) {
    gate('Claim an @handle at aesthetic.computer/handle, then return here.');
    const link=document.createElement('a');link.href='https://aesthetic.computer/handle';link.textContent='Choose your handle';
    $('gate-status').append(document.createElement('br'),link);
    return;
  }
  await session.begin();
  await session.open();
  $('gate').hidden=true;$('workspace').hidden=false;
  $('input').focus({preventScroll:true});
}

async function boot() {
  if(!window.auth0 || !crypto.subtle) throw new Error('This browser could not load secure sign-in. Try a current Firefox or Chrome.');
  auth=new window.auth0.Auth0Client({
    domain:'hi.aesthetic.computer',clientId:'LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt',
    cacheLocation:'localstorage',useRefreshTokens:true,useRefreshTokensFallback:true,
    httpTimeoutInSeconds:30,authorizationParams:{redirect_uri:callback},
  });
  const query=new URLSearchParams(location.search);
  if(query.has('state') && (query.has('code') || query.has('error'))) {
    try { await auth.handleRedirectCallback(); }
    finally { history.replaceState({},'',callback); }
  } else await auth.checkSession();
  $('login').disabled=false;
  if(await auth.isAuthenticated()) await enter();
  else gate('');
}
$('login').onclick=()=>{
  $('login').disabled=true;
  auth.loginWithRedirect({authorizationParams:{redirect_uri:callback}}).catch(error=>{gate(error.message,true);$('login').disabled=false;});
};
$('logout').onclick=async()=>{
  if(session?.state.busy || session?.state.publishing) { line('note','Wait for this turn and upload to finish before signing out.');return; }
  session?.signOut();
  await auth.logout({logoutParams:{returnTo:callback}});
};
$('composer').onsubmit=async e=>{
  e.preventDefault();
  const text=$('input').value.trim();
  if(!text || submitting || session?.state.busy) return;
  submitting=true;busy(true);
  try {
    // Renew via the SDK before each turn instead of persisting engine tokens.
    const token=await auth.getTokenSilently();
    if((await auth.getUser())?.sub !== subject) throw new Error('Your signed-in account changed. Reload to open its pieces.');
    session.state.token=token;
    $('input').value='';
    await session.ask(text);
    if(session.state.publishing) await session.state.publishing;
  } catch(error) {line('bad',error.message);}
  finally {submitting=false;busy(false);}
};
$('input').onkeydown=e=>{if(e.key==='Enter' && !e.shiftKey && !e.isComposing && matchMedia('(hover: hover)').matches){e.preventDefault();$('send').click();}};
$('stop').onclick=()=>session?.stop();
$('new').onclick=async()=>{try{await session.newPiece();$('input').focus();}catch(error){line('bad',error.message);}};
$('history').onchange=async()=>{try{await session.resumeSession($('history').value);}catch(error){line('bad',error.message);}};
$('download').onclick=()=>{
  const url=URL.createObjectURL(new Blob([source],{type:'text/javascript'}));
  const a=document.createElement('a');a.href=url;a.download=`${session.state.slug}.mjs`;a.click();setTimeout(()=>URL.revokeObjectURL(url),1000);
};
window.addEventListener('pagehide',()=>session?.saveCurrent());
void boot().catch(error=>{gate(error.message,true);$('login').disabled=!auth;});
