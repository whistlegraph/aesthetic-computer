#!/usr/bin/env node
// Run on Lith with the system environment. Sequential, public tapes only; no ATProto publication.
import {writeFile} from 'node:fs/promises';
import {connect,closePool} from '../../system/backend/database.mjs';
import {sourceRecord,mediaFile} from '../../system/backend/mime-media.mjs';
const apply=process.argv.includes('--apply');
const outIndex=process.argv.indexOf('--out'),out=outIndex>=0?process.argv[outIndex+1]:null;
const base=process.env.OVEN_URL||'https://oven.aesthetic.computer';
const {db}=await connect();const results=[];
const wait=ms=>new Promise(resolve=>setTimeout(resolve,ms));
async function size(url){const r=await fetch(url,{method:'HEAD',signal:AbortSignal.timeout(20000)});if(!r.ok)throw Error(`Asset HTTP ${r.status}`);return Number(r.headers.get('content-length'))||null;}
try{
 const tapes=db.collection('tapes');
 const candidates=await tapes.find({kind:{$ne:'mp4'},mp4Status:{$in:['pending','oven-failed']},slug:{$type:'string',$ne:''},status:{$ne:'wip'},private:{$ne:true},hidden:{$ne:true},deleted:{$ne:true},nuked:{$ne:true},draft:{$ne:true},visibility:{$in:[null,'public']}}).sort({when:-1}).limit(20).toArray();
 console.log(JSON.stringify({apply,candidates:candidates.map(t=>t.code)}));
 if(apply&&!process.env.OVEN_CALLBACK_SECRET)throw Error('Missing oven callback credential');
 for(const candidate of candidates){
  const row={code:candidate.code,before:candidate.mp4Status};results.push(row);
  const tape=await sourceRecord(db,`tape_${candidate._id}`);if(!tape||tape.mp4Status==='complete'){row.result='skipped';continue;}
  try{
   const zipUrl=mediaFile('tape',tape).url;row.sourceBytes=await size(zipUrl);
   if(!apply){row.result='ready';console.log(JSON.stringify(row));continue;}
   const status=await(await fetch(base+'/status',{signal:AbortSignal.timeout(10000)})).json();
   if(status.active?.length){row.result='converter busy';console.log(JSON.stringify(row));break;}
   const response=await fetch(base+'/bake',{method:'POST',headers:{'Content-Type':'application/json'},signal:AbortSignal.timeout(20000),body:JSON.stringify({mongoId:String(tape._id),slug:tape.slug,code:tape.code,zipUrl,callbackUrl:'https://aesthetic.computer/api/oven-complete?previewOnly=1',callbackSecret:process.env.OVEN_CALLBACK_SECRET})});
   if(!response.ok)throw Error(`Converter HTTP ${response.status}`);
   await tapes.updateOne({_id:tape._id,mp4Status:{$ne:'complete'}},{$set:{mp4Status:'oven-processing',mp4RequestedAt:new Date()}});
   const started=Date.now();console.log(JSON.stringify({code:tape.code,result:'processing',sourceBytes:row.sourceBytes}));
   while(Date.now()-started<600000){
    await wait(5000);
    const current=await tapes.findOne({_id:tape._id});
    if(current?.mp4Status==='complete'){
     row.result='complete';row.previewBytes=await size(mediaFile('tape',current).url);row.poster=Boolean(current.thumbnailUrl);row.elapsedSeconds=Math.round((Date.now()-started)/1000);break;
    }
    if(current?.mp4Status==='oven-failed'){row.result='conversion failed';break;}
   }
   if(!row.result)row.result='timed out';
  }catch(e){row.result=e.message;}
  console.log(JSON.stringify(row));
  if(out)await writeFile(out,JSON.stringify({checkedAt:new Date(),previewOnly:true,results},null,2)+'\n');
  if(row.result==='timed out')break;
 }
}finally{if(out)await writeFile(out,JSON.stringify({checkedAt:new Date(),previewOnly:true,results},null,2)+'\n');await closePool();}
