#!/usr/bin/env node
// Set the dev-only Xbox LAN relay; credentials remain in memory, never argv/logs.
import https from 'node:https';
import {readFileSync,existsSync} from 'node:fs';
import {homedir} from 'node:os';
import {resolve} from 'node:path';
import {spawnSync} from 'node:child_process';
const supplied=process.argv[2];
if(!supplied)throw Error('usage: node xbox/tools/oskiewar-relay.mjs <ws://private-ip:port/oskiewar-live | --status | cloud>');
const match=supplied.match(/^ws:\/\/((?:\d{1,3}\.){3}\d{1,3}):([1-9]\d{0,4})\/oskiewar-live$/);
if(!['--status','cloud'].includes(supplied)){
 const octets=match?.[1].split('.');
 if(!match||Number(match[2])>65535||octets.some(n=>Number(n)>255||(n.length>1&&n[0]==='0'))||
 !(octets[0]==='10'||(octets[0]==='172'&&+octets[1]>=16&&+octets[1]<=31)||
 (octets[0]==='192'&&octets[1]==='168')))throw Error('Relay must be private IPv4 with explicit port and /oskiewar-live');
}
const envPath=process.env.XBOX_DEVICE_PORTAL_ENV||resolve(homedir(),'aesthetic-computer/aesthetic-computer-vault/xbox/device-portal.env');
let source='';
if(existsSync(envPath))source=readFileSync(envPath,'utf8');
else if(existsSync(envPath+'.gpg')){
 const result=spawnSync('gpg',['--batch','--quiet','--decrypt',envPath+'.gpg'],{encoding:'utf8'});
 if(result.status!==0)throw Error('Could not decrypt Device Portal credentials');source=result.stdout;
}
const config={};
for(const line of source.split(/\r?\n/)){
 const pair=line.trim().match(/^(?:export\s+)?([A-Za-z_][A-Za-z0-9_]*)=(.*)$/);if(!pair)continue;
 let value=pair[2].trim();if((value[0]==='"'&&value.at(-1)==='"')||(value[0]==="'"&&value.at(-1)==="'"))value=value.slice(1,-1);
 config[pair[1]]=value;
}
Object.assign(config,process.env);
const host=config.XBOX_DEVICE_PORTAL_HOST,user=config.XBOX_DEVICE_PORTAL_USERNAME,password=config.XBOX_DEVICE_PORTAL_PASSWORD;
if(!host||!user||!password)throw Error('Missing Device Portal configuration');
function request(path,{method='GET',body,headers={},auto=false}={}){
 return new Promise((done,fail)=>{
  const auth=Buffer.from(`${auto?'auto-':''}${user}:${password}`).toString('base64');
  const req=https.request({hostname:host,port:config.XBOX_DEVICE_PORTAL_PORT||11443,path,method,
   rejectUnauthorized:false,headers:{Authorization:`Basic ${auth}`,...headers}},res=>{
    const chunks=[];let size=0;
    res.on('data',b=>{size+=b.length;if(size>4*1024*1024)req.destroy(Error('Portal response too large'));else chunks.push(b);});
    res.on('end',()=>res.statusCode>=200&&res.statusCode<300?done(Buffer.concat(chunks).toString()):fail(Object.assign(Error(`Portal HTTP ${res.statusCode}`),{status:res.statusCode})));
   });
  req.setTimeout(15000,()=>req.destroy(Error('Portal timeout')));req.on('error',fail);req.end(body);
 });
}
const packages=JSON.parse(await request('/api/app/packagemanager/packages')).InstalledPackages
 .filter(p=>p.PackageFamilyName==='AestheticComputer.NativeBios').sort((a,b)=>b.Version.Revision-a.Version.Revision);
const item=packages[0];if(!item)throw Error('Oskiewar not installed');
const query=new URLSearchParams({knownfolderid:'LocalAppData',packagefullname:item.PackageFullName,path:'\\LocalState'});
const base='/api/filesystem/apps/file?'+query;
const file=base+'&filename=oskiewar-relay.txt';
if(supplied==='--status'){
 try{console.log((await request(file,{auto:true})).trim()||'cloud');}
 catch(error){if(error.status===404)console.log('cloud');else throw error;}
}
else{
 const value=supplied==='cloud'?'':supplied+'\n',boundary='oskiewar-relay-'+Date.now();
 const body=`--${boundary}\r\nContent-Disposition: form-data; name="file"; filename="oskiewar-relay.txt"\r\nContent-Type: text/plain\r\n\r\n${value}\r\n--${boundary}--\r\n`;
 await request(base,{method:'POST',auto:true,body,headers:{'Content-Type':`multipart/form-data; boundary=${boundary}`,'Content-Length':Buffer.byteLength(body)}});
 const actual=await request(file,{auto:true});if(actual!==value)throw Error('Relay readback mismatch');
 console.log(JSON.stringify({package:item.PackageFullName,relay:value.trim()||'cloud',verified:true,applies:'next fresh connection; no restart performed'}));
}
