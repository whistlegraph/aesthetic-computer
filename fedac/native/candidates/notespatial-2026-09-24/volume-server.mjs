// One composition gain for the six native players and browser SUB.
import http from 'node:http';
const hosts=[237,242,238,236,241,239].map(n=>`192.168.1.${n}`);
let state={percent:100,id:`volume-${Date.now()}`};
http.createServer(async(req,res)=>{
 res.setHeader('Access-Control-Allow-Origin','*');res.setHeader('Access-Control-Allow-Headers','Content-Type');res.setHeader('Access-Control-Allow-Methods','GET,POST,OPTIONS');res.setHeader('Cache-Control','no-store');
 const send=(code,value)=>{res.writeHead(code,{'Content-Type':'application/json'});res.end(JSON.stringify(value));};
 if(req.method==='OPTIONS')return send(200,{});
 if(req.url!=='/volume')return send(404,{});
 if(req.method==='GET')return send(200,state);
 if(req.method!=='POST')return send(405,{});
 try{
  let raw='';for await(const c of req){raw+=c;if(raw.length>1024)throw Error('Body too large');}
  const {percent}=JSON.parse(raw);if(!Number.isFinite(percent)||percent<0||percent>100)return send(400,{error:'percent must be 0..100'});
  const next={percent,id:`volume-${Date.now()}`};state=next;
  const nodes=await Promise.all(hosts.map(async host=>{try{const r=await fetch(`http://${host}/pieces/composition-volume.json`,{method:'PUT',body:JSON.stringify(next),signal:AbortSignal.timeout(1200)});return {host,accepted:r.ok};}catch{return {host,accepted:false};}}));
  send(nodes.every(n=>n.accepted)?200:207,{...next,nodes});
 }catch(e){send(400,{error:e.message});}
}).listen(8795,'0.0.0.0',()=>console.log('Composition volume: POST :8795/volume {"percent":0..100}'));
