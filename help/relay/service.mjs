// Private, durable transport for Aesel's existing provider adapters.
import http from 'node:http';
import {randomUUID, createHash} from 'node:crypto';
import {mkdirSync, readFileSync, writeFileSync, appendFileSync, renameSync, existsSync} from 'node:fs';
import {join} from 'node:path';
import {pathToFileURL} from 'node:url';
import {ClaudeServer} from '../../aesel/src/claude-server.mjs';
import {AppServer} from '../../aesel/src/app-server.mjs';

const uuid = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i;
const fail = (status, message) => Object.assign(new Error(message), {status});
const json = (res, status, data) => {res.writeHead(status, {'content-type':'application/json','cache-control':'no-store'});res.end(JSON.stringify(data));};
function save(path, value) {writeFileSync(path+'.tmp', JSON.stringify(value), {mode:0o600});renameSync(path+'.tmp', path);}
async function body(req) {
  let text = '';
  for await (const chunk of req) {text += chunk;if (Buffer.byteLength(text)>8*1024*1024) throw fail(413,'Request too large');}
  try {return JSON.parse(text || '{}');} catch {throw fail(400,'Invalid JSON');}
}

export function ownerAuth({adminSub, domain='aesthetic.us.auth0.com', fetch=globalThis.fetch}) {
  if (!adminSub) throw Error('ADMIN_SUB is required');
  const cache = new Map();
  return async header => {
    if (!/^Bearer \S+$/.test(header || '')) throw fail(401,'Sign in to Aesthetic Computer');
    const key=createHash('sha256').update(header).digest('hex');
    if ((cache.get(key)||0)>Date.now()) return;
    const response=await fetch(`https://${domain}/userinfo`, {headers:{authorization:header},signal:AbortSignal.timeout(10000)});
    if (!response.ok) throw fail(401,'Session expired');
    const user=await response.json();
    if (user.sub!==adminSub || user.email_verified!==true) throw fail(403,'This relay is private');
    if (cache.size>=100) cache.clear();
    cache.set(key,Date.now()+60000);
  };
}

export function createRelay({root, authorize, factory, maxActive=2}={}) {
  if (!root || !authorize) throw Error('State directory and authorization are required');
  mkdirSync(root,{recursive:true,mode:0o700});
  const sessions=new Map();
  function persist(s) {save(join(s.dir,'session.json'),s.meta);}
  function event(s,type,value) {
    const entry={seq:++s.seq,at:new Date().toISOString(),type,value};
    appendFileSync(join(s.dir,'events.jsonl'),JSON.stringify(entry)+'\n',{mode:0o600});
    s.events.push(entry);
  }
  function load(id) {
    if (!uuid.test(id)) throw fail(400,'Invalid session id');
    if (sessions.has(id)) return sessions.get(id);
    const dir=join(root,id);
    if (!existsSync(join(dir,'session.json'))) throw fail(404,'Session not found');
    const meta=JSON.parse(readFileSync(join(dir,'session.json'),'utf8'));
    const events=existsSync(join(dir,'events.jsonl'))?readFileSync(join(dir,'events.jsonl'),'utf8').trim().split('\n').filter(Boolean).map(line=>JSON.parse(line)):[];
    const s={dir,meta,events,seq:events.at(-1)?.seq||0,engine:null,pending:new Map()};
    sessions.set(id,s);
    if (meta.busy) {
      meta.busy=false;
      for (const request of Object.values(meta.requests)) if(request.status==='running') request.status='interrupted';
      event(s,'notification',{method:'turn/completed',params:{turn:{id:meta.turnId,status:'interrupted',error:{message:'Relay restarted; your saved input is available to retry.'}}}});
      persist(s);
    }
    return s;
  }
  async function engine(s) {
    clearTimeout(s.idleTimer);
    if(s.engine && !s.engine.closed) return s.engine;
    if(s.opening) return s.opening;
    s.opening=(async()=>{
      const options={cwd:join(s.dir,'workspace'),resumeThreadId:s.meta.engineThreadId||'',model:s.meta.model,effort:s.meta.effort,
        developerInstructions:s.meta.instructions,tools:true};
      // Only the service's subscription credentials reach the child. Never
      // forward HTTP credentials, client environment, commands or local paths.
      const e=factory?factory(s.meta.provider,options):s.meta.provider==='claude'
        ?new ClaudeServer({...options,command:process.env.CLAUDE_BIN||'claude'})
        :new AppServer({...options,command:process.env.CODEX_BIN||'codex'});
      s.engine=e;
      e.on('notification',value=>{
        // CLI turn counters restart with each process. The accepted request UUID
        // is stable across idle shutdown, reconnection and provider resume.
        value=structuredClone(value);
        if(value.params?.turn) value.params.turn.id=s.meta.currentRequest;
        if(value.params?.turnId) value.params.turnId=s.meta.currentRequest;
        if(value.params?.threadId) value.params.threadId=s.meta.id;
        if(value.method==='turn/started') s.meta.turnId=value.params?.turn?.id;
        if(value.method==='turn/completed') {
          s.meta.busy=false;
          const request=s.meta.requests[s.meta.currentRequest];
          if(request) request.status=value.params?.turn?.status||'failed';
          s.pending.clear();
          s.idleTimer=setTimeout(()=>{if(!s.meta.busy){e.close();s.engine=null;}},120000);s.idleTimer.unref();
        }
        event(s,'notification',value);persist(s);
      });
      e.on('request',value=>{s.pending.set(String(value.id),value);event(s,'request',value);});
      e.on('fatal',()=>{
        s.meta.busy=false;
        const request=s.meta.requests[s.meta.currentRequest];if(request)request.status='failed';
        s.pending.clear();event(s,'fatal',{message:'Provider connection failed; saved input can be retried.'});persist(s);
      });
      const result=await e.connect();
      s.meta.engineThreadId=e.threadId||result.thread.id;persist(s);
      return e;
    })().catch(error=>{s.engine?.close();s.engine=null;throw error;}).finally(()=>{s.opening=null;});
    return s.opening;
  }
  const server=http.createServer(async(req,res)=>{
    try {
      const url=new URL(req.url,'http://localhost');
      if(url.pathname==='/health' && req.method==='GET') return json(res,200,{ok:true,service:'aesel-relay',version:1,busy:[...sessions.values()].filter(s=>s.meta.busy).length});
      await authorize(req.headers.authorization);
      if(url.pathname==='/api/aesel/sessions' && req.method==='POST') {
        const input=await body(req);
        if(!['claude','codex'].includes(input.provider))throw fail(400,'Choose claude or codex');
        if(input.provider==='codex' && !factory && process.env.CODEX_ENABLED!=='1') throw fail(503,'Codex needs a server-side login');
        for(const key of ['model','effort','instructions'])if(input[key]!=null && (typeof input[key]!=='string'||input[key].length>(key==='instructions'?64000:100)))throw fail(400,`Invalid ${key}`);
        const id=randomUUID(),dir=join(root,id);
        mkdirSync(join(dir,'workspace'),{recursive:true,mode:0o700});
        const meta={id,provider:input.provider,model:input.model||undefined,effort:input.effort||'',instructions:input.instructions||'',requests:{},busy:false,created:new Date().toISOString()};
        save(join(dir,'session.json'),meta);
        return json(res,201,{thread:{id},provider:meta.provider});
      }
      const match=url.pathname.match(/^\/api\/aesel\/sessions\/([^/]+)(?:\/(turn|respond|interrupt))?$/);
      if(!match)throw fail(404,'Not found');
      const s=load(match[1]),action=match[2];
      if(req.method==='GET' && !action) {
        const after=Number(url.searchParams.get('after')||0);
        if(!Number.isSafeInteger(after)||after<0)throw fail(400,'Invalid event cursor');
        return json(res,200,{thread:{id:s.meta.id},provider:s.meta.provider,busy:s.meta.busy,events:s.events.filter(e=>e.seq>after).slice(0,500),pending:[...s.pending.values()],requests:s.meta.requests});
      }
      if(req.method!=='POST')throw fail(405,'Method not allowed');
      const input=await body(req);
      if(action==='turn') {
        if(!uuid.test(input.requestId||''))throw fail(400,'requestId must be a UUID');
        if(Object.hasOwn(s.meta.requests,input.requestId))return json(res,200,s.meta.requests[input.requestId]);
        if(typeof input.text!=='string'||!input.text.trim()||input.text.length>500000)throw fail(400,'Invalid turn text');
        const images=input.images||[];
        if(!Array.isArray(images)||images.length>8||images.some(i=>!i||!['image/png','image/jpeg','image/webp','image/gif'].includes(i.mimeType)||typeof i.data!=='string'||!/^[A-Za-z0-9+/]*={0,2}$/.test(i.data)))throw fail(400,'Invalid images');
        if(s.meta.busy)throw fail(409,'This session is already working');
        if([...sessions.values()].filter(v=>v.meta.busy).length>=maxActive)throw fail(429,'Relay is busy; retry shortly');
        // Save the complete request before spawning or acknowledging it. Losing
        // a network connection never loses an accepted drawing or repeats a turn.
        save(join(s.dir,input.requestId+'.json'),input);
        s.meta.busy=true;s.meta.currentRequest=input.requestId;
        s.meta.requests[input.requestId]={requestId:input.requestId,status:'running'};persist(s);
        json(res,202,s.meta.requests[input.requestId]);
        void (async()=>{
          try {const e=await engine(s);await e.startTurn(input.text,{images});}
          catch {s.meta.busy=false;s.meta.requests[input.requestId].status='failed';event(s,'fatal',{message:'Could not start provider; input was saved. Check server authentication.'});persist(s);}
        })();
        return;
      }
      if(action==='respond') {
        if(!s.pending.has(String(input.id)))throw fail(409,'Approval is no longer pending');
        const pending=s.pending.get(String(input.id));
        if(!input.result || typeof input.result!=='object' || Array.isArray(input.result))throw fail(400,'Invalid response');
        if(pending.method.endsWith('/requestApproval')&&!['accept','acceptForSession','decline','cancel'].includes(input.result.decision))throw fail(400,'Invalid approval decision');
        s.engine.respond(input.id,input.result);s.pending.delete(String(input.id));
        event(s,'approval',{id:input.id,decision:input.result.decision});return json(res,200,{ok:true});
      }
      if(action==='interrupt') {await s.engine?.interrupt();return json(res,200,{ok:true});}
      throw fail(404,'Not found');
    } catch(error) {if(!res.headersSent)json(res,error.status||500,{error:error.status?error.message:'Relay request failed'});else res.end();}
  });
  server.on('close',()=>{for(const s of sessions.values()){clearTimeout(s.idleTimer);s.engine?.close();}});
  return server;
}

if(process.argv[1] && import.meta.url===pathToFileURL(process.argv[1]).href) {
  // A private subscription relay must never silently fall through to paid API keys.
  for(const key of ['ANTHROPIC_API_KEY','ANTHROPIC_AUTH_TOKEN','OPENAI_API_KEY','OPENROUTER_API_KEY'])delete process.env[key];
  const server=createRelay({root:process.env.AESEL_RELAY_STATE||'/var/lib/aesel-relay',authorize:ownerAuth({adminSub:process.env.ADMIN_SUB,domain:process.env.AUTH0_DOMAIN})});
  server.listen(Number(process.env.AESEL_RELAY_PORT||3006),'127.0.0.1',()=>console.log('Aesel relay listening on loopback'));
  for(const signal of ['SIGTERM','SIGINT'])process.on(signal,()=>{server.close();setTimeout(()=>process.exit(0),1000).unref();});
}
