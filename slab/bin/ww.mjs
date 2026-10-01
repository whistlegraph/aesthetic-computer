#!/usr/bin/env node
// Agent-friendly access to a running Walkieware thread. Tokens never enter argv.
import {ACSession,USER_AGENT} from '../../aesel/src/ac-session.mjs';
import {WebSocket} from 'ws';
import {createHash,randomUUID} from 'node:crypto';
import {readFileSync} from 'node:fs';
const [command='list',code,...words]=process.argv.slice(2);
if(!['list','inspect','watch','ask','undo','edit','layout'].includes(command))throw Error('Usage: ww.mjs list | inspect CODE | watch CODE | ask CODE WORDS | undo CODE | edit CODE FILE BASE_VERSION BASE_HASH | layout CODE CSS_FILE');
const replacement=['edit','layout'].includes(command)?readFileSync(words[0],'utf8'):null;
if(command==='edit'&&(!/^\d+$/.test(words[1]||'')||!/^[a-f0-9]{64}$/.test(words[2]||'')))throw Error('An edit requires the version and SHA-256 from your inspected source');
const session=new ACSession(),token=await session.token();
if(!token)throw Error('Sign in with ac-login first');
const origin=process.env.WALKIE_ORIGIN||'https://aesthetic.computer';
if(command==='list'||command==='inspect') {
  const response=await fetch(origin+'/api/walkieware'+(code?'?code='+encodeURIComponent(code):''),{headers:{Authorization:`Bearer ${token}`,'User-Agent':USER_AGENT}});
  if(!response.ok)throw Error(`Thread request failed (${response.status})`);
  console.log(JSON.stringify(await response.json(),null,2));
}else {
  const ws=new WebSocket(origin.replace(/^http/,'ws')+'/api/walkieware-stream',{headers:{'User-Agent':USER_AGENT}});
  const timer=command==='watch'?null:setTimeout(()=>{console.error('Timed out; inspect before retrying');ws.close();process.exitCode=1;},185000);
  ws.on('open',()=>ws.send(JSON.stringify({type:'authenticate',role:'agent',token,code})));
  ws.on('message',raw=>{
    const m=JSON.parse(raw);
    if(command==='watch'){console.log(JSON.stringify(m));return;}
    if(m.type==='ready') {
      if(!m.online){console.error('Device offline');process.exitCode=1;ws.close();return;}
      const v=m.thread.ledger?.versions.find(v=>v.id===m.thread.ledger.head);
      if(!v){console.error('No synchronized version');process.exitCode=1;ws.close();return;}
      ws.send(JSON.stringify({type:'command',id:randomUUID(),action:command,text:command==='edit'?'Remote source edit':words.join(' '),source:command==='edit'?replacement:null,css:command==='layout'?replacement:undefined,baseVersion:command==='edit'?Number(words[1]):v.id,baseHash:command==='edit'?words[2]:createHash('sha256').update(v.source).digest('hex')}));
    }
    if(['result','error','accepted'].includes(m.type))console.log(JSON.stringify(m));
    if(m.type==='result'||m.type==='error'){if(m.type==='error'||!m.ok)process.exitCode=1;ws.close();}
  });
  ws.on('error',()=>{console.error('Walkieware connection failed');process.exitCode=1;});
  ws.on('close',()=>clearTimeout(timer));
}
