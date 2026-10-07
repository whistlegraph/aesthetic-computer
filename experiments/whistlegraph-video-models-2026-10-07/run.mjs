import {readFileSync, writeFileSync, existsSync} from 'node:fs';
import {dirname, join} from 'node:path';
import {fileURLToPath} from 'node:url';
import {falKey, uploadToFalStorage} from '../../pop/lib/fal.mjs';
const archive=dirname(fileURLToPath(import.meta.url));
const dir=process.argv[3] || archive;
const save=(name,value)=>writeFileSync(join(dir,name),JSON.stringify(value,null,2)+'\n');
const read=name=>JSON.parse(readFileSync(join(dir,name),'utf8'));
const auth={Authorization:`Key ${falKey()}`,'Content-Type':'application/json'};
const env=readFileSync(new URL('../../aesthetic-computer-vault/.devcontainer/envs/devcontainer.env',import.meta.url),'utf8');
const admin=env.split('\n').find(x=>x.startsWith('FAL_ADMIN_KEY=')).slice('FAL_ADMIN_KEY='.length).trim().replace(/^['"]|['"]$/g,'');
async function balance(){const r=await fetch('https://api.fal.ai/v1/account/billing?expand=credits',{headers:{Authorization:`Key ${admin}`},signal:AbortSignal.timeout(20000)});if(!r.ok)throw Error(`billing ${r.status}`);return {checked_at:new Date().toISOString(),...(await r.json()).credits};}
const prompt=`Video 1 is a real Whistlegraph performance: white chalk on a textured green surface, a real hand, close handheld phone framing, and an unaccompanied casual human voice. Use it as the reference for the material, hand, camera and live sound. Make a NEW five-second drawing performance on a blank patch of the same green surface. One continuous shot, one hand holding one piece of white chalk. The hand draws one continuous mountain line from left to right with exactly three triangular peaks. From 0 to 0.5 seconds the chalk approaches the blank surface. From 0.5 to 3.8 seconds draw the three peaks in sequence: first peak up and down, second peak up and down, third peak up and down. From 3.8 to 4.5 seconds continue into a short flat valley; then lift the chalk and hold the finished drawing until 5 seconds. THE ONE RULE: sound and drawing are the same gesture. A casual human whistles one rising-then-falling note for each peak, then a low held note for the flat valley. Every rise in pitch coincides with an upward stroke, every fall with a downward stroke. Dry chalk scratching occurs exactly while the chalk touches and moves across the surface; it stops completely when the chalk lifts. All marks grow exclusively from the moving chalk tip, stay attached to the surface and persist unchanged. The whole hand, chalk tip and whole drawing remain visible. No extra marks, no pre-drawn mountains, no disappearing strokes, no music, speech, text, subtitles, logos, cuts or transitions. Keep the imperfect, intimate phone-recording quality of the reference.`;
const models=[
 {name:'seedance-2.5',endpoint:'bytedance/seedance-2.5/reference-to-video',estimate_usd:2.84,input:{task:'reference',prompt:prompt.replace('Video 1','@Video1'),duration:'5',resolution:'720p',aspect_ratio:'9:16',generate_audio:true,seed:10072026}},
 {name:'h3-max',endpoint:'minimax/h3-max/reference-to-video',estimate_usd:1.07,input:{prompt,duration:5,resolution:'768P',aspect_ratio:'9:16',prompt_expansion_mode:'disabled',seed:10072026}},
 {name:'wan-3.0',endpoint:'alibaba/wan-3.0/reference-to-video',estimate_usd:1.00,input:{prompt,duration:5,resolution:'720p',aspect_ratio:'9:16',audio:true,enable_prompt_expansion:false,enable_thinking:true,seed:10072026}}
];
const mode=process.argv[2];
if(mode==='prepare'){
 if(existsSync(join(dir,'plan.json'))) throw Error('A plan already exists here. Use a fresh directory containing reference.mp4.');
 save('balance-before.json',await balance());
 let uploaded;
 if(existsSync(join(dir,'reference-upload.json')))uploaded=read('reference-upload.json');
 else {uploaded={url:await uploadToFalStorage(join(dir,'reference.mp4')),source:'https://www.tiktok.com/@whistlegraph/video/7119595740988460330',source_segment_seconds:[0,5]};save('reference-upload.json',uploaded);}
 for(const m of models){m.input[m.name==='seedance-2.5'?'video_urls':'reference_video_urls']=[uploaded.url];}
 save('plan.json',{created_at:new Date().toISOString(),expected_total_usd:4.91,source:uploaded,models});
 writeFileSync(join(dir,'prompt.txt'),prompt+'\n');
 console.log('Prepared three 5-second takes. Estimate $4.91. Balance:',read('balance-before.json'));
}else if(mode==='submit'){
 for(const m of read('plan.json').models){
  if(existsSync(join(dir,`${m.name}.mp4`))){console.log(m.name,'already downloaded; skipping');continue;}
  const path=`${m.name}.queue.json`;
  if(existsSync(join(dir,path))){console.log(m.name,'already recorded; skipping');continue;}
  save(path,{state:'submitting',endpoint:m.endpoint,submitted_at:new Date().toISOString()});
  // Never retry ambiguous submissions: the provider could already be billing.
  const r=await fetch(`https://queue.fal.run/${m.endpoint}`,{method:'POST',headers:auth,body:JSON.stringify(m.input),signal:AbortSignal.timeout(60000)});
  const body=await r.json();save(path,{state:r.ok?'queued':'rejected',endpoint:m.endpoint,http_status:r.status,...body});
  console.log(m.name,r.status,body);
 }
}else if(mode==='poll'){
 for(const m of read('plan.json').models){
  if(existsSync(join(dir,`${m.name}.mp4`))){console.log(m.name,'downloaded');continue;}
  const q=read(`${m.name}.queue.json`);if(!q.status_url){console.log(m.name,q.state);continue;}
  const r=await fetch(q.status_url,{headers:auth,signal:AbortSignal.timeout(20000)});const s=await r.json();save(`${m.name}.status.json`,s);console.log(m.name,s);
  if(s.status==='COMPLETED'){
   const rr=await fetch(q.response_url,{headers:auth,signal:AbortSignal.timeout(20000)});const result=await rr.json();save(`${m.name}.result.json`,{http_status:rr.status,...result});
   if(!rr.ok||!result.video?.url){console.log(m.name,'result failed',result);continue;}
   const video=await fetch(result.video.url,{signal:AbortSignal.timeout(60000)});if(!video.ok)throw Error(`download ${video.status}`);
   writeFileSync(join(dir,`${m.name}.mp4`),Buffer.from(await video.arrayBuffer()));console.log(m.name,'saved');
  }
 }
 save('balance-after.json',await balance());console.log('Balance:',read('balance-after.json'));
}else{console.log('Usage: node run.mjs prepare | submit | poll [output-directory]');}
