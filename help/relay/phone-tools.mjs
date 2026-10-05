// MCP adapter for tools that execute in the owner's phone, never on this host.
import {createInterface} from 'node:readline';
async function call(path,input) {
 const r=await fetch(process.env.AESEL_PHONE_URL+'/'+path,{method:input?'POST':'GET',headers:{authorization:'Bearer '+process.env.AESEL_PHONE_SECRET,'content-type':'application/json'},...(input?{body:JSON.stringify(input)}:{})});
 if(!r.ok)throw Error('Phone tool channel unavailable');return r.json();
}
createInterface({input:process.stdin}).on('line',async line=>{
 let m;try{m=JSON.parse(line);}catch{return;}if(m.id===undefined)return;
 try {
  let result;
  if(m.method==='initialize')result={protocolVersion:m.params?.protocolVersion||'2025-06-18',capabilities:{tools:{}},serverInfo:{name:'phone',version:'1'}};
  else if(m.method==='tools/list')result=await call('tools');
  else if(m.method==='tools/call')result=await call('call',m.params);
  else if(m.method==='ping')result={};else throw Error('Unknown method');
  process.stdout.write(JSON.stringify({jsonrpc:'2.0',id:m.id,result})+'\n');
 }catch(e){process.stdout.write(JSON.stringify({jsonrpc:'2.0',id:m.id,error:{code:-32000,message:e.message}})+'\n');}
});
