import { createInterface } from 'node:readline';
import { Artifacts } from './artifacts.mjs';
const at=process.argv.indexOf('--cwd');
const store=new Artifacts(at>=0?process.argv[at+1]:process.cwd());
const tools=[
  {name:'artifact_context',description:'Read the selected Easel artifact and its supported action schemas. Call before editing pictures, sounds, papers or GameBoy projects.',inputSchema:{type:'object',properties:{}}},
  {name:'artifact_action',description:'Apply a supported action to the selected artifact. Validated output becomes a new version. Remote image generation/editing is available when requested by the user; visual-QA approval uses direct user controls.',inputSchema:{type:'object',properties:{action:{type:'string'},input:{type:'object'}},required:['action']}},
];
let queue=Promise.resolve();
createInterface({input:process.stdin,crlfDelay:Infinity}).on('line',line=>{
  queue=queue.then(async()=>{
    let message;
    try{message=JSON.parse(line);}catch{return;}
    if(message.id===undefined)return;
    let result;
    try {
      if(message.method==='initialize')result={protocolVersion:message.params?.protocolVersion||'2025-06-18',capabilities:{tools:{}},serverInfo:{name:'easel-media',version:'1'}};
      else if(message.method==='tools/list')result={tools};
      else if(message.method==='ping')result={};
      else if(message.method==='tools/call'){
        const {name,arguments:args={}}=message.params || {};
        let value;
        if(name==='artifact_context')value={context:await store.context(),tools:await store.tools()};
        else if(name==='artifact_action')value=await store.run(args.action,args.input||{});
        else throw new Error('Unknown artifact tool');
        result={content:[{type:'text',text:JSON.stringify(value)}]};
      }else throw new Error('Unknown method');
    }catch(error){result={isError:true,content:[{type:'text',text:error.message}]};}
    process.stdout.write(JSON.stringify({jsonrpc:'2.0',id:message.id,result})+'\n');
  }).catch(()=>{});
});
