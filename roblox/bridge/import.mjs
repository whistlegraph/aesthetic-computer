import {readFileSync, writeFileSync, mkdirSync} from 'node:fs';
import {createHash} from 'node:crypto';
import {config,request,id} from '../../toolchain/robloxplorer/robloxplorer.mjs';
const dir=import.meta.dirname;
mkdirSync(`${dir}/build`,{recursive:true});
const catalog=JSON.parse(readFileSync(`${dir}/catalog.json`));
const {key,userId}=config();
const headers={'x-api-key':key};
const [command='import',slug='ac-pals']=process.argv.slice(2);
const entry=catalog.items.find(x=>x.id===slug && x.approved===true);
if(!entry) throw new Error('Entry must be explicitly approved in catalog.json');
const file=`${dir}/build/${entry.id}.json`;
if(command==='import') {
  let previous;try{previous=JSON.parse(readFileSync(file));}catch{}
  if(previous?.operation) throw new Error('Upload already recorded; use poll to avoid duplicates');
  const url=new URL(entry.sourceUrl);
  if(url.origin!=='https://assets.aesthetic.computer' || !url.pathname.endsWith('.png')) throw new Error('Only approved AC asset PNGs');
  const res=await fetch(url,{redirect:'error',signal:AbortSignal.timeout(15000)});
  if(!res.ok)throw new Error(`Source HTTP ${res.status}`);
  const bytes=new Uint8Array(await res.arrayBuffer());
  if(bytes.length>10*1024*1024 || bytes.length<24 || Buffer.from(bytes.subarray(0,8)).toString('hex')!=='89504e470d0a1a0a')throw new Error('Expected PNG under 10MB');
  const body=new FormData();
  body.set('request',JSON.stringify({assetType:'Image',displayName:entry.title,description:'AC-owned artwork imported for the Aesthetic Arena media bridge.',creationContext:{creator:{userId:id(userId)}}}));
  body.set('fileContent',new Blob([bytes],{type:'image/png'}),`${entry.id}.png`);
  const operation=await request('https://apis.roblox.com/assets/v1/assets',{method:'POST',headers,body});
  const record={source:entry,sha256:createHash('sha256').update(bytes).digest('hex'),operation};
  writeFileSync(file,JSON.stringify(record,null,2)+'\n');
  console.log(JSON.stringify(operation));
} else if(command==='poll') {
  const record=JSON.parse(readFileSync(file));
  const path=record.operation.path;
  if(!/^operations\/[A-Za-z0-9-]+$/.test(path))throw new Error('Unexpected operation path');
  record.operation=await request(`https://apis.roblox.com/assets/v1/${path}`,{headers});
  writeFileSync(file,JSON.stringify(record,null,2)+'\n');
  console.log(JSON.stringify(record.operation,null,2));
} else throw new Error('Use import or poll');
