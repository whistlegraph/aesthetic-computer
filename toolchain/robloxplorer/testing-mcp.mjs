#!/usr/bin/env node
import { pathToFileURL } from 'node:url';
import { resolve } from 'node:path';
import { serveStdio } from '../mcp/http-front.mjs';
import * as testing from './testing.mjs';
const schema = properties => ({type: 'object', properties, additionalProperties: false});
export const tools = [
  {name: 'roblox_client_log', description: 'Read only instrumented AC arena ready, accepted-swing and audio-load events from the latest local Roblox log. No raw platform logs or credentials.', inputSchema: schema({})},
  {name: 'roblox_status', description: 'Inspect the pinned arena target and whether the local Roblox client is running/focused.', inputSchema: schema({})},
  {name: 'roblox_launch', description: 'Open the pinned arena in the local Roblox app. Optionally join a known server job ID. Does not force a new server or bypass private-game access.', inputSchema: schema({serverId: {type: 'string'}, browser: {type: 'boolean'}})},
  {name: 'roblox_capture', description: 'Focus Roblox and return its window screenshot plus OCR. Save local evidence. Check expected strings without claiming a visual QA pass. Requires Slab Frame.', inputSchema: schema({focus: {type: 'boolean'}, expected: {type: 'array', maxItems: 12, items: {type: 'string', maxLength: 120}}})},
  {name: 'roblox_control', description: 'Send one bounded gameplay key while Roblox is frontmost, then capture. Do not use while chat or login is focused. No arbitrary typing.', inputSchema: {...schema({action: {type: 'string', enum: ['nade','forward','back','left','right','jump','escape']}, milliseconds: {type: 'integer', minimum: 50, maximum: 1500}}), required: ['action']}},
  {name: 'roblox_test', description: 'Run the checked-in headless suite on the pinned arena version, poll it, inspect target, or read task logs. Test creates a remote task; no place publishing.', inputSchema: {...schema({command: {type: 'string', enum: ['inspect','test','poll','logs']}, version: {type: 'string'}}), required: ['command']}},
];
const text = value => ({type: 'text', text: JSON.stringify(value, null, 2)});
let queue = Promise.resolve();
async function call(name, args) {
  const methods = {roblox_client_log: 'clientLog', roblox_status: 'status', roblox_launch: 'launch', roblox_capture: 'capture', roblox_control: 'control', roblox_test: 'api'};
  const tool = tools.find(x => x.name === name);
  if (!tool) throw new Error('Unknown tool');
  for (const key of Object.keys(args)) if (!(key in tool.inputSchema.properties)) throw new Error(`Unknown argument: ${key}`);
  const value = await testing[methods[name]](args);
  return {content: value.image ? [text(value.evidence), value.image] : [text(value)]};
}
export async function handleMessage({id, method, params}) {
  if (id === undefined) return null;
  if (method === 'initialize') return {jsonrpc:'2.0', id, result:{protocolVersion:'2024-11-05', capabilities:{tools:{}}, serverInfo:{name:'roblox-testing',version:'0.1.0'}, instructions:'Launch, capture, inspect pixels, act, capture again. OCR matches are not visual QA. Headless tests do not prove controls, audio, or rendering. Keep all actions on the pinned test experience.'}};
  if (method === 'ping') return {jsonrpc:'2.0',id,result:{}};
  if (method === 'tools/list') return {jsonrpc:'2.0',id,result:{tools}};
  if (method !== 'tools/call') return {jsonrpc:'2.0',id,error:{code:-32601,message:'Unknown method'}};
  const task = queue.then(() => call(params?.name, params?.arguments || {}));
  queue = task.catch(() => {});
  try {return {jsonrpc:'2.0',id,result:await task};}
  catch (error) {return {jsonrpc:'2.0',id,result:{isError:true,content:[text(error.message)]}};}
}
if (process.argv[1] && import.meta.url === pathToFileURL(resolve(process.argv[1])).href) {
  if (process.argv[2] === '--call') {
    const response = await handleMessage({id:1,method:'tools/call',params:{name:process.argv[3],arguments:JSON.parse(process.argv[4] || '{}')}});
    console.log(JSON.stringify({...response.result,content:response.result.content.filter(x=>x.type!=='image')},null,2));
    if (response.result.isError) process.exitCode=1;
  } else serveStdio({handleMessage,banner:'Roblox testing MCP'});
}
