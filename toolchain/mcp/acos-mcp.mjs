#!/usr/bin/env node
// Read-only ACOS checks over named SSH routes (LAN or an authenticated relay).
// ~/.ac-os/machines.json: { "julia": { "ssh": "julia-acos", "label": "Julia" } }
import { execFile } from 'node:child_process';
import { promisify } from 'node:util';
import { readFile } from 'node:fs/promises';
import { homedir } from 'node:os';
import { join } from 'node:path';
import { pathToFileURL } from 'node:url';
import { serveStdio, serveHttp, httpPort } from './http-front.mjs';

const execute=promisify(execFile);
const registry=process.env.ACOS_MACHINES||join(homedir(),'.ac-os','machines.json');
const logs={runtime:'/mnt/ac-native.log',controller:'/tmp/ac-usb-controller.log',
  bluetooth:'/tmp/bluetooth.log',startup:'/tmp/julia-device-start.log'};
const statusScript=`
set -eu
runtime=$(/bin/curl -fsS --max-time 2 http://127.0.0.1/status 2>/dev/null || /bin/curl -fsS --max-time 2 http://127.0.0.1:8080/status 2>/dev/null || printf '{}')
pad=$(/bin/busybox cat /tmp/ac-controller.json 2>/dev/null || printf '{}')
radio=$(for r in /sys/class/rfkill/rfkill*; do
  [ -f "$r/type" ] || continue
  [ "$(cat "$r/type")" = bluetooth ] || continue
  printf '%s hard=%s soft=%s\\n' "$(cat "$r/name")" "$(cat "$r/hard")" "$(cat "$r/soft")"
done)
/bin/jq -n --arg build "$(cat /etc/ac-build 2>/dev/null || true)" \
  --arg kernel "$(uname -r)" --arg uptime "$(cat /proc/uptime)" \
  --arg pid "$(/bin/busybox pidof ac-native || true)" \
  --arg bluetooth "$radio" --arg storage "$(df -h /mnt 2>/dev/null || true)" \
  --argjson runtime "$runtime" --argjson controller "$pad" \
  '{build:$build,kernel:$kernel,uptime:$uptime,runtimePid:$pid,runtime:$runtime,controller:$controller,bluetooth:$bluetooth,storage:$storage}'
`;

export function commandFor(name,args={}) {
  if(name==='acos_status')return statusScript;
  if(name==='acos_logs') {
    const source=args.source??'runtime',lines=args.lines??80;
    if(!Object.hasOwn(logs,source))throw Error('Unknown log source');
    const path=logs[source];
    if(!Number.isInteger(lines)||lines<1||lines>200)throw Error('lines must be an integer from 1 to 200');
    return `/bin/busybox tail -n ${lines} ${path}`;
  }
  throw Error('Unknown tool');
}
export function sshArgs(target,command) {
  if(!target||typeof target.ssh!=='string'||!/^[A-Za-z0-9][A-Za-z0-9._-]{0,127}$/.test(target.ssh))
    throw Error('Machine must name an SSH config alias');
  return ['-o','BatchMode=yes','-o','ConnectTimeout=8','-o','ServerAliveInterval=5',
    '-o','ServerAliveCountMax=2','-o','StrictHostKeyChecking=yes',target.ssh,command];
}
const machine={type:'string',description:'Name from acos_list.'};
const tools=[
  {name:'acos_list',description:'List configured ACOS machines and their SSH route names. Does not imply they are online.',inputSchema:{type:'object',properties:{},additionalProperties:false}},
  {name:'acos_status',description:'Read current ACOS build, running piece, controller state, Bluetooth block flags and disk usage over authenticated SSH. A failed connection is an error, not a cached online result.',inputSchema:{type:'object',properties:{machine},required:['machine'],additionalProperties:false}},
  {name:'acos_logs',description:'Read a bounded tail of an ACOS runtime, USB controller, Bluetooth or startup log over authenticated SSH.',inputSchema:{type:'object',properties:{machine,source:{type:'string',enum:Object.keys(logs)},lines:{type:'integer',minimum:1,maximum:200,default:80}},required:['machine'],additionalProperties:false}},
].map(t=>({...t,annotations:{readOnlyHint:true,destructiveHint:false,openWorldHint:true}}));

export async function handleMessage({id,method,params}) {
  if(id===undefined||id===null)return null;
  const result=value=>({jsonrpc:'2.0',id,result:value});
  if(method==='initialize')return result({protocolVersion:params?.protocolVersion||'2024-11-05',capabilities:{tools:{}},serverInfo:{name:'acos',version:'1.0.0'}});
  if(method==='ping')return result({});
  if(method==='tools/list')return result({tools});
  if(method!=='tools/call')return {jsonrpc:'2.0',id,error:{code:-32601,message:'Method not found'}};
  try {
    if(!tools.some(t=>t.name===params?.name))throw Error('Unknown tool');
    const targets=JSON.parse(await readFile(registry,'utf8'));
    let value;
    if(params.name==='acos_list')value=Object.entries(targets).map(([machine,t])=>({machine,label:t.label||machine,ssh:t.ssh}));
    else {
      const args=params.arguments||{};
      if(typeof args.machine!=='string'||!Object.hasOwn(targets,args.machine))throw Error('Unknown machine; use acos_list');
      const {stdout}=await execute('ssh',sshArgs(targets[args.machine],commandFor(params.name,args)),{timeout:25000,maxBuffer:128*1024});
      value=params.name==='acos_status'?{machine:args.machine,checkedAt:new Date().toISOString(),...JSON.parse(stdout)}:stdout;
    }
    return result({content:[{type:'text',text:typeof value==='string'?value:JSON.stringify(value,null,2)}]});
  }catch(e){
    // execFile's full message contains its arguments. Report only bounded stderr.
    const message=e.stderr?.trim().slice(0,2000)||(e.killed?'SSH check timed out':
      typeof e.code==='number'?'SSH check failed':
      e.code==='ENOENT'?'ACOS configuration or SSH executable unavailable':e.message);
    return result({isError:true,content:[{type:'text',text:message}]});
  }
}
if(process.argv[1]&&import.meta.url===pathToFileURL(process.argv[1]).href) {
  const port=httpPort(process.argv,7787);
  if(port)serveHttp({handleMessage,port,banner:'ACOS checks'});
  else serveStdio({handleMessage,banner:'ACOS checks'});
}
