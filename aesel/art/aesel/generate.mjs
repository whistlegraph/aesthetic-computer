// Paid, explicit model route. Never retries a paid POST or swaps providers.
import {readFileSync,writeFileSync,existsSync} from 'node:fs';
import {fileURLToPath} from 'node:url';
import {plan,generate,FAL_FLARE} from '../../media/picture/illy.mjs';
const output=fileURLToPath(new URL('./sheet-source.png',import.meta.url));
const request=plan({provider:'fal',model:FAL_FLARE,prompt:readFileSync(new URL('./prompt.txt',import.meta.url),'utf8'),size:'1024x1024',quality:'high'});
if(!process.argv.includes('--generate')){console.log(JSON.stringify({...request,prompt:undefined,output},null,2));process.exit(0);}
if(existsSync(output))throw new Error('Existing artwork retained; choose a new output for a deliberate reroll.');
let key=process.env.FAL_KEY;
if(!key){const vault=process.env.AC_VAULT_ENV;if(!vault)throw new Error('Set FAL_KEY or AC_VAULT_ENV outside the project.');const line=readFileSync(vault,'utf8').split('\n').find(x=>x.startsWith('FAL_KEY='));key=line?.slice(8).trim().replace(/^['"]|['"]$/g,'');}
if(!key)throw new Error('fal credential unavailable');
const result=await generate(request,{env:{FAL_KEY:key},onSubmitted:async queue=>{writeFileSync(output+'.queue.json',JSON.stringify(queue),{mode:0o600});console.log('Submitted fal sprite sheet.');}});
writeFileSync(output,result.bytes);writeFileSync(output+'.illy.json',JSON.stringify(result.provenance,null,2));console.log('Saved '+output);
