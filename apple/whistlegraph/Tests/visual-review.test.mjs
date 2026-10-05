import test from 'node:test';
import assert from 'node:assert/strict';
import {validateFrames,parseVerdict,reviewVisualResult,reviewWithRepair} from '../Resources/Web/visual-review.mjs';
const png='iVBORw0KGgo=';
const evidence=()=>({sourceHash:'hash',renderID:7,frames:[0,800,1600,2400].map(atMs=>({atMs,width:320,height:240,png}))});
const pass={passed:true,observations:'The four frames show the entire butterfly rotating together.',findings:[]};
const fail={passed:false,observations:'The wings orbit an upright body.',findings:['Rotate the body, wings and markings with one transform.']};
test('rejects stale, incomplete, untimed or oversized frame evidence before inference',()=>{
  assert.equal(validateFrames(evidence(),'hash',7).length,4);
  for(const change of [e=>e.sourceHash='old',e=>e.renderID=6,e=>e.frames.pop(),e=>e.frames[3].atMs=1700,e=>e.frames[1].atMs=0,e=>e.frames[0].width=400,e=>e.frames[0].png='not png',e=>e.error='backgrounded']){
    const e=evidence();change(e);assert.throws(()=>validateFrames(e,'hash',7));
  }
});
test('a verdict must contain observations and cannot pass with findings',()=>{
  assert.deepEqual(parseVerdict(JSON.stringify(pass)),pass);
  assert.deepEqual(parseVerdict('```json\n'+JSON.stringify(fail)+'\n```'),fail);
  for(const v of [{passed:true}, {...pass,observations:''},{...pass,findings:['broken']},{...fail,findings:[]}])assert.throws(()=>parseVerdict(JSON.stringify(v)));
});
test('a failed visual result gets one repair and fresh inspection; a second failure remains failed',async()=>{
  for(const final of [pass,fail]){
    const events=[];
    const result=await reviewWithRepair({cancelled:()=>false,inspect:async()=>{events.push('inspect');return events.length===1?fail:final;},repair:async v=>{assert.deepEqual(v,fail);events.push('repair');}});
    assert.deepEqual(events,['inspect','repair','inspect']);assert.equal(result.passed,final.passed);
  }
});
test('missing evidence and cancellation cannot purchase a repair or become success',async()=>{
  let repairs=0;
  await assert.rejects(reviewWithRepair({cancelled:()=>false,inspect:async()=>{throw Error('capture unavailable');},repair:async()=>repairs++}),/unavailable/);
  await assert.rejects(reviewWithRepair({cancelled:()=>true,inspect:async()=>pass,repair:async()=>repairs++}),/stopped/);
  assert.equal(repairs,0);
});
test('stop during the second inspection cannot turn a repaired frame into success',async()=>{
  let stopped=false,inspections=0;
  await assert.rejects(reviewWithRepair({cancelled:()=>stopped,repair:async()=>{},inspect:async()=>{
    if(++inspections===1)return fail;
    stopped=true;return pass;
  }}),/stopped/);
});
function stream(verdict,stop='end_turn'){
  const events=[{type:'content_block_delta',delta:{type:'text_delta',text:JSON.stringify(verdict)}}, ...(stop?[{type:'message_delta',delta:{stop_reason:stop}}]:[])];
  const bytes=new TextEncoder().encode(events.map(e=>'data: '+JSON.stringify(e)+'\r\n\r\n').join(''));
  return new Response(new ReadableStream({start(c){for(let i=0;i<bytes.length;i+=7)c.enqueue(bytes.slice(i,i+7));c.close();}}));
}
test('sends actual PNG frames in order and parses split SSE; truncated verdicts fail closed',async()=>{
  for(const stop of ['end_turn','max_tokens',null]){
    const call=reviewVisualResult({evidence:evidence(),sourceHash:'hash',renderID:7,source:'source',request:'spin it',history:[],model:'model',token:'token',fetch:async(url,options)=>{
      const body=JSON.parse(options.body);assert.equal(body.messages[0].content.filter(b=>b.type==='image').length,4);
      assert.match(body.system,/rotation must rotate the whole subject/);assert.equal(options.headers.Authorization,'Bearer token');
      return stream(pass,stop);
    }});
    if(stop==='end_turn')assert.equal((await call).passed,true);else await assert.rejects(call,/did not finish/);
  }
});
test('personal visual review sends all four images through the private relay only',async()=>{
 let submitted;
 const result=await reviewVisualResult({evidence:evidence(),sourceHash:'hash',renderID:7,source:'source',request:'spin it',history:[],model:'anthropic/claude-opus-5',token:'owner',personalRelay:true,fetch:async(url,options)=>{
  assert.ok(url.startsWith('https://help.aesthetic.computer/api/aesel/'));
  if(url.endsWith('/sessions')){const create=JSON.parse(options.body);assert.deepEqual(create.clientTools,[]);assert.equal(create.effort,'low');return Response.json({thread:{id:'session'}});}
  if(url.endsWith('/turn')){submitted=JSON.parse(options.body);return Response.json({status:'running'});}
  return Response.json({pending:[],events:[{seq:1,type:'notification',value:{method:'item/agentMessage/delta',params:{delta:JSON.stringify(pass)}}},{seq:2,type:'notification',value:{method:'turn/completed',params:{turn:{status:'completed'}}}}]});
 }});
 assert.equal(result.passed,true);assert.equal(submitted.images.length,4);assert.deepEqual(submitted.images.map(i=>i.data),[png,png,png,png]);
});

test('personal review survives the old 45-second deadline and still accepts a late verdict',async t=>{
 t.mock.timers.enable({apis:['setTimeout']});
 let signal,finish,ready;const waiting=new Promise(r=>ready=r);
 const call=reviewVisualResult({evidence:evidence(),sourceHash:'hash',renderID:7,source:'source',request:'drawing',history:[],model:'anthropic/claude-opus-5',token:'owner',personalRelay:true,fetch:async(url,options)=>{
  if(url.endsWith('/sessions'))return Response.json({thread:{id:'session'}});
  if(url.endsWith('/turn'))return Response.json({status:'running'});
  signal=options.signal;return new Promise((resolve,reject)=>{finish=resolve;signal.addEventListener('abort',()=>reject(signal.reason),{once:true});ready();});
 }});
 await waiting;t.mock.timers.tick(60000);assert.equal(signal.aborted,false);
 finish(Response.json({pending:[],events:[{seq:1,type:'notification',value:{method:'item/agentMessage/delta',params:{delta:JSON.stringify(pass)}}},{seq:2,type:'notification',value:{method:'turn/completed',params:{turn:{status:'completed'}}}}]}));
 assert.equal((await call).passed,true);
});
test('a real review timeout preserves failure with an actionable recovery message',async t=>{
 t.mock.timers.enable({apis:['setTimeout']});
 let ready;const waiting=new Promise(r=>ready=r);
 const call=reviewVisualResult({evidence:evidence(),sourceHash:'hash',renderID:7,source:'source',request:'drawing',history:[],model:'model',token:'owner',fetch:async(url,options)=>new Promise((resolve,reject)=>{
  options.signal.addEventListener('abort',()=>reject(options.signal.reason),{once:true});ready();
 })});
 const check=assert.rejects(call,/Visual review timed out.*checkpoint remain saved/);
 await waiting;t.mock.timers.tick(45000);await check;
});
