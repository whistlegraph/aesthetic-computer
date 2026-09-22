// User-owned execution helper. Native apps use authenticated loopback RPC;
// the sandboxed app never receives CLI login files or launches a shell.
import {createServer} from 'node:http';
import {randomBytes, timingSafeEqual, createHash} from 'node:crypto';
import {mkdirSync, readFileSync, writeFileSync, renameSync, existsSync, lstatSync, watch, accessSync, constants} from 'node:fs';
import {join, delimiter, resolve} from 'node:path';
import {homedir} from 'node:os';
import {pathToFileURL} from 'node:url';
import {BACKENDS} from '../src/backends.mjs';
import {codexModels, pickerModels} from '../src/provider-picker.mjs';
import {validatePieceSource} from '../src/revisions.mjs';
import {nativeInstructions} from './prompt.mjs';
import {nativeInputPixels} from './preview.mjs';

const LIMIT = 1024 * 1024;
const digest = value => createHash('sha256').update(value).digest('hex');
const validID = value => typeof value === 'string' && /^[a-zA-Z0-9_-]{1,100}$/.test(value);
function requireID(value) { if (!validID(value)) throw new Error('Invalid session or operation ID'); return value; }
function atomic(file, value) {
  const temporary = `${file}.tmp`;
  writeFileSync(temporary, JSON.stringify(value), {mode:0o600});
  renameSync(temporary, file);
}
function readJSON(file, fallback) {
  try { return JSON.parse(readFileSync(file, 'utf8')); } catch (error) {
    if (error.code === 'ENOENT') return fallback;
    throw new Error('Host checkpoint could not be read; keep the workspace and repair its checkpoint before continuing.');
  }
}
function executable(name) {
  const configured=process.env[`AESEL_${name.toUpperCase()}_COMMAND`];
  if(configured) { try { accessSync(configured,constants.X_OK); return configured; } catch {} }
  for (const folder of (process.env.PATH || '').split(delimiter)) {
    const file = join(folder, name);
    try { accessSync(file, constants.X_OK); if (lstatSync(file).isFile() || lstatSync(file).isSymbolicLink()) return file; } catch {}
  }
  return null;
}
function boundedText(value, name, limit = LIMIT) {
  if (typeof value !== 'string' || Buffer.byteLength(value) > limit) throw new Error(`${name} exceeds its size limit`);
  return value;
}

export class NativeHost {
  constructor({root, engineFactory, discover, capturePreview=nativeInputPixels} = {}) {
    this.root = root || join(homedir(), 'Library/Application Support/Aesel Host');
    mkdirSync(this.root, {recursive:true,mode:0o700});
    if (lstatSync(this.root).isSymbolicLink()) throw new Error('Host storage must not be a symlink');
    this.sessions = new Map();
    this.engineFactory = engineFactory || ((provider, options) => new BACKENDS[provider].Engine(options));
    this.discover = discover || executable;
    this.capturePreview = capturePreview;
    this.catalog = null;
  }

  async capabilities() {
    if (this.catalog && Date.now() - this.catalog.at < 30000) return this.catalog.value;
    const providers = [];
    for (const id of ['claude','codex']) {
      const command = this.discover(id);
      let catalog = [], error = '';
      if (command && id === 'codex') {
        try { catalog = await codexModels({command}); } catch (failure) { error = failure.message; }
      }
      const model = BACKENDS[id].defaultModel;
      providers.push({id,available:!!command,model,
        models:pickerModels({backend:id,model,catalog}).map(x=>({id:x.id,title:x.label})),
        notice:!command ? `Install and sign in to ${id} on this Mac.` : error,
      });
    }
    const value = {schema:1,providers,media:['piece'],transport:'loopback',features:['stream','interrupt','approvals','reconnect']};
    this.catalog = {at:Date.now(),value};
    return value;
  }

  session(id) {
    requireID(id);
    if (this.sessions.has(id)) return this.sessions.get(id);
    const directory = join(this.root, 'workspaces', id);
    mkdirSync(directory,{recursive:true,mode:0o700});
    if (lstatSync(directory).isSymbolicLink()) throw new Error('Unsafe workspace');
    const file = join(directory,'piece.mjs'), checkpoint = join(directory,'host.json');
    const stored = readJSON(checkpoint, {});
    if (existsSync(checkpoint) && (stored.schema !== 1 || !Array.isArray(stored.events) || !stored.operations || typeof stored.operations !== 'object')) throw new Error('Invalid host checkpoint');
    const value = {id,directory,file,checkpoint,provider:stored.provider || '',model:stored.model || '',
      threadId:stored.threadId || '',sequence:stored.sequence || 0,events:stored.events || [],
      operations:stored.operations || {},engine:null,active:null,approvals:new Map(),sourceHash:stored.sourceHash || '',timer:null};
    for (const job of Object.values(value.operations)) {
      if (job.status === 'running') { job.status = 'interrupted'; job.error = 'The host restarted. This request was not replayed.'; }
    }
    this.sessions.set(id,value);
    this.checkpoint(value);
    value.watcher = watch(directory, (_, name) => {
      if (name !== 'piece.mjs') return;
      clearTimeout(value.sourceTimer);
      value.sourceTimer = setTimeout(()=>this.source(value).catch(error=>this.event(value,{type:'error',message:error.message})),100);
    });
    return value;
  }

  checkpoint(s) {
    clearTimeout(s.timer);
    atomic(s.checkpoint,{schema:1,provider:s.provider,model:s.model,threadId:s.threadId,
      sequence:s.sequence,events:s.events,operations:s.operations,sourceHash:s.sourceHash});
  }

  event(s, value) {
    const event = {...value,sequence:++s.sequence,operation:s.active?.id || null,at:Date.now()};
    s.events.push(event);
    while (s.events.length > 512 || Buffer.byteLength(JSON.stringify(s.events)) > 4 * LIMIT) s.events.shift();
    clearTimeout(s.timer);
    s.timer = setTimeout(()=>{try { this.checkpoint(s); } catch { s.storageError = 'Host checkpoint could not be saved. Stop and free disk space before continuing.'; }},100);
    return event;
  }

  async source(s) {
    if (!existsSync(s.file)) return;
    const info = lstatSync(s.file);
    if (!info.isFile() || info.isSymbolicLink() || info.size > LIMIT) throw new Error('Piece output must be a regular file smaller than 1 MB');
    const source = readFileSync(s.file,'utf8'), hash = digest(source);
    if (hash === s.sourceHash) return;
    await validatePieceSource(source,s.file);
    s.sourceHash = hash;
    this.event(s,{type:'source',source,hash});
  }

  async configure(p) {
    const s = this.session(p.sessionID);
    if (s.active || s.configuring) throw new Error('Wait for the current turn before changing provider or source');
    if (s.storageError) throw new Error(s.storageError);
    s.configuring = true;
    try {
      if (!['claude','codex'].includes(p.provider)) throw new Error('Unsupported host provider');
      const choice = (await this.capabilities()).providers.find(x=>x.id===p.provider);
      if (!choice?.available) throw new Error(`Install and sign in to ${p.provider} on this Mac.`);
      const source = boundedText(p.source,'Source'), model = boundedText(p.model ?? '', 'Model',200);
      if (!choice.models.some(x=>x.id===model)) throw new Error('Choose a model from the connected provider');
      const context=boundedText(p.context || '', 'Conversation handoff',32768);
      await validatePieceSource(source,s.file);
      if (existsSync(s.file) && lstatSync(s.file).isSymbolicLink()) throw new Error('Unsafe source path');
      writeFileSync(s.file,source,{mode:0o600});
      s.sourceHash = digest(source);
      if (s.engine && (s.provider !== p.provider || s.model !== model)) {
        const old=s.engine;s.engine=null;old.close();
      }
      if (s.provider !== p.provider) s.threadId = '';
      s.provider = p.provider; s.model = model;s.context=context;
      this.checkpoint(s);
      return {sessionID:s.id,sequence:s.sequence,provider:s.provider,model:s.model};
    } finally { s.configuring = false; }
  }

  async connect(s) {
    if (s.engine) return;
    const instructions = nativeInstructions(s.context);
    const engine = this.engineFactory(s.provider,{cwd:s.directory,command:this.discover(s.provider),model:s.model,
      resumeThreadId:s.threadId,developerInstructions:instructions,recoveryInstructions:instructions,
      environment:{AESEL_NATIVE_SESSION:s.id}});
    s.engine = engine;
    engine.on('notification', event => {
      if (s.engine !== engine || this.closed) return;
      this.event(s,{type:'notification',...event});
      s.threadId = engine.threadId || s.threadId;
      if (event.method === 'turn/completed') void this.finish(s,event.params?.turn?.status || 'completed',event.params?.turn?.error?.message);
    });
    engine.on('request', request => {
      if (s.engine !== engine || !s.active) return;
      const id = String(request.id);
      const event=this.event(s,{type:'approval',id,method:request.method,params:request.params});
      s.approvals.set(id,{id:request.id,operation:s.active.id,event});
    });
    engine.on('fatal', error => { if(s.engine===engine) void this.finish(s,'failed',error.message); });
    engine.on('exit', () => { if(s.engine===engine) { s.engine=null; if(s.active) void this.finish(s,'failed','Provider exited during the turn'); } });
    try {
      await Promise.race([engine.connect(),new Promise((_,reject)=>{
        s.connectTimer=setTimeout(()=>reject(new Error('Provider connection timed out')),30000);
      })]);
      if (s.engine !== engine) throw new Error('Provider exited while connecting');
      s.threadId = engine.threadId || s.threadId;
    } catch(error) { if(s.engine===engine) { s.engine=null; engine.close(); } throw error; }
    finally { clearTimeout(s.connectTimer); }
  }

  async finish(s,status,error) {
    if (!s.active || s.finishing) return;
    s.finishing = true;
    const operation = s.active;
    try { await this.source(s); } catch(failure) { error = failure.message; status = 'failed'; }
    operation.status = status; operation.error = error || ''; operation.finishedAt = Date.now();
    for(const pending of s.approvals.values()) { try { s.engine?.respond(pending.id,{decision:'decline'}); } catch {} }
    clearTimeout(s.interruptTimer);
    s.approvals.clear();
    this.event(s,{type:'done',status,error:error || ''});
    s.active = null; s.finishing = false;
    try { this.checkpoint(s); } catch { s.storageError = 'Host checkpoint could not be saved. Free disk space before continuing.'; }
  }

  async rpc(method,p={}) {
    if(this.closed)throw new Error('The host is shutting down');
    if (method === 'capabilities') return this.capabilities();
    if (method === 'configure') return this.configure(p);
    const s = this.session(p.sessionID);
    if (method === 'turn') {
      requireID(p.operationID);
      if (s.operations[p.operationID]) return s.operations[p.operationID];
      if (s.active || s.configuring) throw new Error('The workspace is busy');
      if (s.storageError) throw new Error(s.storageError);
      if (!s.provider) throw new Error('Choose a provider before sending');
      const text = boundedText(p.text,'Prompt',32768);
      if (!text.trim()) throw new Error('The prompt is empty');
      if (Object.keys(s.operations).length >= 1000) throw new Error('Start a new thread; this one has reached its operation limit');
      const operation = {id:p.operationID,status:'running',startedAt:Date.now()};
      s.active = operation; s.operations[operation.id] = operation;
      this.checkpoint(s); // Record before launching: retries can never replay a turn.
      void (async()=>{
        try {
          await this.connect(s);
          if (s.active !== operation) return;
          if (operation.cancelRequested) { await this.finish(s,'interrupted'); return; }
          const pixels = await this.capturePreview(s.id);
          if (s.active !== operation) return;
          if (operation.cancelRequested) { await this.finish(s,'interrupted'); return; }
          operation.started = true;
          await s.engine.startTurn(text+pixels.context,{images:pixels.images});
        }
        catch(error) { await this.finish(s,'failed',error.message); }
      })();
      return operation;
    }
    if (method === 'events') {
      const after = Number.isInteger(p.after) && p.after >= 0 ? p.after : 0;
      return {storageError:s.storageError || '',approvals:[...s.approvals.values()].map(x=>x.event),sequence:s.sequence,oldest:s.events[0]?.sequence ?? s.sequence,
        events:s.events.filter(x=>x.sequence>after),operation:p.operationID?s.operations[p.operationID] || null:s.active};
    }
    if (method === 'interrupt') {
      requireID(p.operationID);
      if (!s.operations[p.operationID]) {
        s.operations[p.operationID]={id:p.operationID,status:'interrupted',error:'Cancelled before host acceptance',finishedAt:Date.now()};
        this.checkpoint(s);return {requested:true};
      }
      if (!s.active || s.active.id !== p.operationID) return {requested:true};
      s.active.cancelRequested = true;
      if (s.active.started) {
        const operation=s.active;
        s.interruptTimer=setTimeout(()=>{
          if(s.active!==operation)return;
          const engine=s.engine;s.engine=null;engine?.close();
          void this.finish(s,'interrupted','The provider did not acknowledge Stop; its process was closed.');
        },10000);
        await s.engine?.interrupt();
      }
      return {requested:true};
    }
    if (method === 'approval') {
      const pending = s.approvals.get(String(p.id));
      if (!pending || !s.active || pending.operation !== s.active.id || p.operationID !== s.active.id) throw new Error('This approval has expired');
      if (!['accept','decline','cancel'].includes(p.decision)) throw new Error('Unsupported approval decision');
      s.approvals.delete(String(p.id)); s.engine.respond(pending.id,{decision:p.decision});
      return {accepted:true};
    }
    throw new Error('Unsupported host method');
  }

  close() {
    if(this.closed)return;this.closed=true;
    for(const s of this.sessions.values()) {
      s.watcher.close();clearTimeout(s.sourceTimer);clearTimeout(s.timer);clearTimeout(s.connectTimer);clearTimeout(s.interruptTimer);
      const engine=s.engine;s.engine=null;engine?.close();this.checkpoint(s);s.active=null;
    }
  }
}

export function serveHost({host,token,port=0}={}) {
  if(typeof token!=='string'||token.length<32) throw new Error('A private host credential is required');
  const expected=Buffer.from(`Bearer ${token}`);
  const server=createServer(async(request,response)=>{
    response.setHeader('Content-Type','application/json');response.setHeader('Cache-Control','no-store');
    const supplied=Buffer.from(request.headers.authorization || '');
    if(request.headers.origin || supplied.length!==expected.length || !timingSafeEqual(supplied,expected)) {
      response.writeHead(401);response.end(JSON.stringify({error:'Unauthorized'}));return;
    }
    if(request.method!=='POST'||request.url!=='/rpc') {response.writeHead(404);response.end('{}');return;}
    try {
      let length=0;const chunks=[];
      for await(const chunk of request) {length+=chunk.length;if(length>2*LIMIT)throw new Error('Request too large');chunks.push(chunk);}
      const input=JSON.parse(Buffer.concat(chunks).toString('utf8'));
      const result=await host.rpc(input.method,input.params);
      response.end(JSON.stringify({result}));
    } catch(error) {response.writeHead(400);response.end(JSON.stringify({error:error.message}));}
  });
  return new Promise((resolve,reject)=>{server.once('error',reject);server.listen(port,'127.0.0.1',()=>resolve(server));});
}

async function main() {
  const host=new NativeHost();
  const configFile=join(host.root,'connection.json');
  const previous=readJSON(configFile,{});
  const token=typeof previous.token==='string'&&previous.token.length>=32?previous.token:randomBytes(32).toString('hex');
  const server=await serveHost({host,token,port:8776});
  const config={schema:1,url:`http://127.0.0.1:${server.address().port}/rpc`,token};
  atomic(configFile,config);
  const appDirectory=join(homedir(),'Library/Containers/computer.aesthetic.aesel.native/Data/Library/Application Support/computer.aesthetic.aesel.native');
  mkdirSync(appDirectory,{recursive:true,mode:0o700});atomic(join(appDirectory,'host.json'),config);
  console.log('Aesel host ready on loopback. Connection saved for Aesel Native.');
  for(const signal of ['SIGTERM','SIGINT'])process.on(signal,()=>{host.close();server.close(()=>process.exit(0));});
}
if(process.argv[1]&&import.meta.url===pathToFileURL(resolve(process.argv[1])).href)main().catch(error=>{console.error(error.message);process.exitCode=1;});
