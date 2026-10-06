// Reuse AC's PKCE login and shared desktop token. Tokens never leave this process.
import {createHash} from 'node:crypto';
import {setDefaultResultOrder} from 'node:dns';
import {readFile} from 'node:fs/promises';
import {pathToFileURL} from 'node:url';
import {createInterface} from 'node:readline';
import {ACSession, SITE, USER_AGENT} from '../../aesel/src/ac-session.mjs';
import {verifyAccount, offlineError} from '../../aesel/src/account-access.mjs';
import {finishPainting} from './done.mjs';

export function createAccountBridge({session=new ACSession(), site=SITE, fetch=globalThis.fetch}={}) {
  let verified;
  async function accountFetch(url, options={}) {
    try { return await fetch(url, options); }
    catch (error) {
      // Retry one interrupted read within its original timeout. Writes retain
      // their existing request IDs and are never replayed by this transport.
      if (!['GET','HEAD'].includes(options.method || 'GET') || options.signal?.aborted ||
          !['ECONNRESET','EPIPE','UND_ERR_SOCKET','UND_ERR_CONNECT_TIMEOUT'].includes(error.cause?.code)) throw error;
      return fetch(url, options);
    }
  }
  async function authorized() {
    const token = await session.token();
    if (verified?.token === token && verified.until > Date.now()) return verified.identity;
    const account = await verifyAccount(token, {fetch:accountFetch, site, userAgent:USER_AGENT, timeoutMs:20_000});
    if (!account.handle) throw Error('Choose an AC handle before using remote models.');
    const id = createHash('sha256').update(account.sub).digest('hex');
    const identity = {token, account, id};
    verified = {token, identity, until:Date.now()+60_000};
    return identity;
  }
  async function request(path, token, body) {
    let response;
    try { response = await accountFetch(site+path, {method:body ? 'POST' : 'GET',
      headers:{Authorization:`Bearer ${token}`, 'User-Agent':USER_AGENT, 'Content-Type':'application/json'},
      ...(body ? {body:JSON.stringify(body)} : {}), signal:AbortSignal.timeout(body ? 200_000 : 20_000)}); }
    catch (error) { throw offlineError(error); }
    if (!response.ok) {
      // Never print an upstream body that might contain echoed credentials or pixels.
      const messages = {401:'Sign in again with AC.', 402:'Not enough Braincells. Choose a local model.',
        403:'Verify your AC email and claim a handle first.', 409:'Move already submitted or price changed. Refresh and try again.',
        429:'A cloud move is still processing or your daily limit is reached.', 503:'AC remote images are unavailable.'};
      throw Object.assign(Error(messages[response.status] || `AC request failed (HTTP ${response.status}).`),
        {status:response.status, code:response.status>=500?'offline':undefined});
    }
    return response.json();
  }
  async function status() {
    const {token,account,id} = await authorized();
    const credits = await request('/api/easel-credits', token);
    if (credits.handle !== '@'+account.handle) throw Error('AC account changed. Sign in again.');
    let remote = null, models = [], remote_status = null, remote_service = null;
    try {
      const catalog = await request('/api/nopaint-inference', token);
      models = catalog.models || [];
      remote = models.find(model=>model.id === 'ac-klein') || null;
      remote_service = catalog.service?.provider_status || catalog.service || null;
      if (remote_service && !remote_service.available) remote_status = remote_service.detail;
      else if (!models.length) remote_status = 'AC cloud image generation is not configured on the server.';
      else if (!models.some(model=>model.available)) remote_status = 'AC cloud image generation is disabled on the server.';
    } catch {
      remote_status = 'Could not load AC cloud model availability. Try refreshing.';
    }
    return {connected:true, handle:credits.handle, account_id:id,
      remaining:credits.remaining, purchased:credits.purchased, remote, models, remote_status, remote_service};
  }
  return async input => {
    if (input.action === 'login') {
      if (session.signedIn) {
        try { return await status(); }
        catch (error) {
          if (![401,403].includes(error.status) && !/session expired|session refresh failed|Sign in again/.test(error.message)) throw error;
        }
      }
      await session.login();
      return status();
    }
    if (input.action === 'status') return status();
    if (input.action === 'done') {
      const {token,account,id} = await authorized();
      if (input.account_id !== id) throw Error('Your AC account changed. Sign in again before Done.');
      return finishPainting({folder:input.folder, accountId:id, handle:account.handle, token, fetch, site});
    }
    if (input.action === 'move') {
      const {token,id} = await authorized();
      if (input.account_id !== id) throw Error('Your AC account changed. Sign in again before generating.');
      const image = (await readFile(input.before)).toString('base64');
      return request('/api/nopaint-inference', token, {requestId:input.requestId,
        image, seed:input.seed, strength:input.strength, quote:input.quote, model:input.model});
    }
    throw Error('Unknown AC account action');
  };
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  setDefaultResultOrder('ipv4first');
  // Retain sockets across the app's 30-second account refresh. Two connections
  // let account reads continue while a paid generation is still in flight.
  const {Agent,setGlobalDispatcher}=await import('undici');
  setGlobalDispatcher(new Agent({connections:2, keepAliveTimeout:90_000,
    keepAliveMaxTimeout:90_000, connect:{timeout:20_000,keepAlive:true,keepAliveInitialDelay:10_000}}));
  const bridge = createAccountBridge();
  async function run(input) {
    try { return await bridge(input); }
    catch (error) {
      return {error:error.status>=500?'AC is temporarily unavailable. Retrying…':error.message,
        code:error.status>=500?'offline':error.code, status:error.status};
    }
  }
  if (process.argv.includes('--serve')) {
    // One private pipe per local game server. No token-bearing HTTP listener.
    const lines=createInterface({input:process.stdin});
    lines.on('line',async line=>{
      let envelope;
      try { envelope=JSON.parse(line); } catch { return; }
      if (typeof envelope.id!=='string' || envelope.id.length>64 || !envelope.input) return;
      const result=await run(envelope.input);
      process.stdout.write(JSON.stringify({id:envelope.id,result})+'\n');
    });
    lines.on('close',()=>process.exit(0));
  } else {
    let input = '';
    for await (const chunk of process.stdin) input += chunk;
    const result=await run(JSON.parse(input));
    console.log(JSON.stringify(result));
    if (result.error) process.exitCode=1;
  }
}
