#!/usr/bin/env node
// Native Aesel automation through its same-user, sandbox-local RPC mailbox.
import { readFile, writeFile, rename, unlink, stat, lstat } from 'node:fs/promises';
import { join, resolve } from 'node:path';
import { homedir } from 'node:os';
import { randomUUID } from 'node:crypto';
import { pathToFileURL } from 'node:url';
import { httpPort, serveHttp, serveStdio } from '../../toolchain/mcp/http-front.mjs';

export const DEFAULT_DIRECTORY = join(homedir(), 'Library/Containers/computer.aesthetic.aesel.native/Data/Library/Application Support/computer.aesthetic.aesel.native/automation');
const pause = ms => new Promise(resolve => setTimeout(resolve, ms));

export async function request(method, params = {}, options = {}) {
  if (method === 'action' && (typeof params.expectedSessionID !== 'string' || !params.expectedSessionID.trim())) {
    throw new Error('expectedSessionID is required; read aesel_state and use session.id before acting');
  }
  const directory = options.directory || process.env.AESEL_AUTOMATION_DIR || DEFAULT_DIRECTORY;
  const metadata = await lstat(directory).catch(() => { throw new Error('Aesel automation is unavailable. Start an MCP-enabled Aesel Native build; this tool never opens or focuses the app.'); });
  if (!metadata.isDirectory() || metadata.isSymbolicLink() || (metadata.mode & 0o077) || metadata.uid !== process.getuid()) throw new Error('Automation directory must be private and owned by the current user');
  const instance = JSON.parse(await readFile(join(directory, 'instance.json'), 'utf8'));
  if (instance.schema !== 1 || !Number.isInteger(instance.pid) || typeof instance.instance !== 'string') throw new Error('Invalid Aesel instance record');
  try { process.kill(instance.pid, 0); } catch { throw new Error('Aesel is not running; instance record is stale'); }
  const id = randomUUID();
  const input = join(directory, 'requests', `${id}.json`);
  const output = join(directory, 'responses', `${id}.json`);
  const temporary = `${input}.tmp`;
  const message = JSON.stringify({ schema: 1, id, instance: instance.instance, method, params, createdAt: Date.now() / 1000 });
  if (Buffer.byteLength(message) > 65536) throw new Error('Request exceeds 64 KiB');
  await writeFile(temporary, message, { mode: 0o600, flag: 'wx' });
  await rename(temporary, input);
  const deadline = Date.now() + (options.timeoutMs ?? 20000);
  try {
    while (Date.now() < deadline) {
      const size = await stat(output).then(s => s.size).catch(() => null);
      if (size !== null) {
        if (size > 24 * 1024 * 1024) throw new Error('Response exceeds 24 MiB');
        const response = JSON.parse(await readFile(output, 'utf8'));
        if (response.id !== id) throw new Error('Response ID mismatch');
        if (response.error) throw new Error(response.error);
        return response.result;
      }
      await pause(80);
    }
    throw new Error('Aesel request timed out; no UI activation attempted');
  } finally {
    await Promise.all([input, output, temporary].map(file => unlink(file).catch(() => {})));
  }
}

const object = properties => ({ type: 'object', properties, additionalProperties: false });
export const TOOLS = [
  { name: 'aesel_map', description: 'Map the running native app: stable control IDs, enabled states, visible screen and overlays. Unsupported features remain explicitly disabled. Does not activate the app.', inputSchema: object({}), annotations: {readOnlyHint: true} },
  { name: 'aesel_state', description: 'Read Aesel UI and session state, piece revision, preview URL/query flags, draft length and saved-thread metadata. Does not return tokens, transcript text or source.', inputSchema: object({}), annotations: {readOnlyHint: true} },
  { name: 'aesel_act',
    description: 'Operate a named enabled control from aesel_map. First read aesel_state; pass its session.id as expectedSessionID. composer.set accepts text; session.resume accepts sessionId; provider.select accepts provider; model.select accepts model (empty string means CLI default); ui.scale accepts scale; window.resize and preview.resize accept width/height. turn.reconnect checks an existing turn without resending it. Send can consume provider usage or AC credits and auto-publish; piece.publish writes publicly; credits.buy opens App Store confirmation; piece.open opens a browser. Use only when that specific action is authorized. Returns acceptance and observed state; async work may still be pending. Never focuses the main app.',
    inputSchema: { ...object({
      id: {type:'string'}, expectedSessionID: {type:'string',minLength:1},
      text: {type:'string',maxLength:32768}, sessionId: {type:'string'},
      provider: {type:'string',enum:['ac','claude','codex']}, model: {type:'string'},
      scale: {type:'number',minimum:0.7,maximum:1.75},
      width: {type:'number',minimum:96,maximum:2400}, height: {type:'number',minimum:72,maximum:1800}
    }), required:['id','expectedSessionID'] },
    annotations: {readOnlyHint:false, destructiveHint:true, openWorldHint:true} },
  { name: 'aesel_events', description: 'Read the bounded local diagnostic event ring after a sequence number. Event kinds and timestamps only; no prompt, source, token or raw bridge payload. No analytics export.', inputSchema: object({after:{type:'integer',minimum:0}}), annotations:{readOnlyHint:true} },
  { name: 'aesel_preview', description: 'Inspect the actual embedded WebKit URL, query flags, runtime readiness and canvas dimensions. No arbitrary JavaScript execution.', inputSchema: object({}), annotations:{readOnlyHint:true} },
  { name: 'aesel_capture', description: 'Capture native app chrome, notebook WebKit, or preview WebKit without focusing or opening a window. Capture each visible surface for acceptance. Optional window capture requires existing Screen Recording permission and never requests it. Captures can contain visible user content.', inputSchema: object({target:{type:'string',enum:['app','notebook','preview','window'],default:'app'}}), annotations:{readOnlyHint:true} },
  { name: 'aesel_eye_capture', description: 'Save one named Aesthetic Eye scenario: running-build fingerprint, UI state, app and preview PNGs. Creates an unreviewed manifest; never certifies visual quality. Use a new directory after rebuilding.', inputSchema:{...object({directory:{type:'string'},scenario:{type:'string'},bundle:{type:'string'}}),required:['directory','scenario','bundle']},annotations:{readOnlyHint:false,destructiveHint:false} },
  { name: 'aesel_eye_check', description: 'Check an Aesel Aesthetic Eye manifest. Fails stale build/evidence, missing required scenarios, absent visual review or failed checks. Does not create or infer a pass.', inputSchema:{...object({directory:{type:'string'}}),required:['directory']},annotations:{readOnlyHint:true} },
];
const METHODS = {aesel_map:'map',aesel_state:'state',aesel_act:'action',aesel_events:'events',aesel_preview:'preview',aesel_capture:'capture'};

export async function handleMessage(message, context = {}) {
  const {id, method, params} = message;
  if (id === undefined || id === null) return null;
  if (context.headers?.origin) return {jsonrpc:'2.0',id,error:{code:-32000,message:'Browser-origin requests are not accepted'}};
  try {
    let result;
    if (method === 'initialize') result = {protocolVersion:'2024-11-05',capabilities:{tools:{}},serverInfo:{name:'aesel-mcp',version:'1.0.0'}};
    else if (method === 'ping') result = {};
    else if (method === 'tools/list') result = {tools:TOOLS};
    else if (method === 'tools/call') {
      if (params?.name === 'aesel_eye_capture' || params?.name === 'aesel_eye_check') {
        const eye=await import('./aesel-eye.mjs');
        const args=params.arguments||{};
        if(typeof args.directory!=='string')throw new Error('directory is required');
        const value=params.name==='aesel_eye_check' ? await eye.check(args.directory) : await eye.capture(args.directory,args.scenario,args.bundle);
        return {jsonrpc:'2.0',id,result:{content:[{type:'text',text:JSON.stringify(value)}],...(value?.pass===false?{isError:true}:{})}};
      }
      const operation = METHODS[params?.name];
      if (!operation) throw new Error('Unknown Aesel tool');
      const response = await request(operation, params.arguments || {});
      result = operation === 'capture'
        ? {content:[{type:'image',mimeType:response.mimeType,data:response.data},{type:'text',text:JSON.stringify({...response,data:undefined})}]}
        : {content:[{type:'text',text:JSON.stringify(response)}]};
    } else return {jsonrpc:'2.0',id,error:{code:-32601,message:'Unknown MCP method'}};
    return {jsonrpc:'2.0',id,result};
  } catch (error) {
    return {jsonrpc:'2.0',id,result:{isError:true,content:[{type:'text',text:error.message}]}};
  }
}

if (process.argv[1] && import.meta.url === pathToFileURL(resolve(process.argv[1])).href) {
  const port = httpPort(process.argv, 7781);
  if (port) serveHttp({handleMessage,port,banner:'aesel-mcp'});
  else serveStdio({handleMessage,banner:'aesel-mcp'});
}
