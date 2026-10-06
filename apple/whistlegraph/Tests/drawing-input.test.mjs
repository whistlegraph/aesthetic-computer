import test from 'node:test';
import assert from 'node:assert/strict';
import {normalizeDrawing,withDrawing,inputData,drawingEvidence,drawingGeometry} from '../Resources/Web/drawing-input.mjs';
import {inferenceRequest} from '../Resources/Web/inference-input.mjs';
import {musicalPrompt} from '../Resources/Web/musical-input.mjs';
import {selectedBranch} from '../Resources/Web/branch-context.mjs';
const sketch={schema:'whistlegraph-drawing/v1',id:'11111111-1111-4111-8111-111111111111',revision:3,aspect:4/3,strokes:[[[100,100,0],[200,300,200],[400,100,450]],[[400,100,1000],[450,100,1100,900]]],speechStartMs:-500};
const sound={transcript:'make it move',words:[{text:'move',atMs:1500,durationMs:200}],sound:{schema:'walkieware-sound/v1',durationMs:2000,audibleMs:1200,frames:[{atMs:0,rms:.2,pitchHz:200},{atMs:1500,rms:.8,pitchHz:330}],onsetsMs:[1500],recordingID:'22222222-2222-4222-8222-222222222222'}};
test('typed and drawing-only requests preserve a bounded vector attachment in one request',()=>{
 const stored=withDrawing('make this bounce',sketch),parsed=inputData(stored);
 assert.equal(parsed.transcript,'make this bounce');assert.deepEqual(parsed.drawing.strokes,sketch.strokes);
 assert.match(inferenceRequest(stored),/make this bounce/);assert.match(inferenceRequest(withDrawing('',sketch)),/Infer the intended edit from these chalk gestures/);
 assert.doesNotMatch(inferenceRequest(stored),new RegExp(sketch.id));
 assert.equal(withDrawing('hello',null),'hello');
});
test('gesture and sound timing survive ordinary speech and recording identity stays local',()=>{
 const stored=withDrawing(musicalPrompt(sound),sketch),prompt=inferenceRequest(stored);
 assert.match(prompt,/speechStartMs":-500/);assert.match(prompt,/"atMs":1500/);assert.match(prompt,/"rms":0.8/);
 assert.doesNotMatch(prompt,/22222222|11111111|recordingID/);
 assert.equal(inputData(stored).sound.recordingID,sound.sound.recordingID);
 assert.doesNotMatch(inferenceRequest(musicalPrompt(sound)),/rms/,'ordinary speech without drawing still excludes sound');
});
test('sampling preserves stroke endpoints, pen lifts, pressure and a strict point budget',()=>{
 const points=Array.from({length:1200},(_,i)=>[i%1000,100,i*2,500]);
 const result=normalizeDrawing({...sketch,strokes:[points]});
 assert.ok(result.strokes[0].length<=320);assert.deepEqual(result.strokes[0][0],points[0]);assert.deepEqual(result.strokes[0].at(-1),points.at(-1));
 assert.equal(normalizeDrawing(sketch).strokes[0][0].length,3,'finger pressure is absent');
});
test('malformed or oversized strokes and clocks are rejected before provider submission',()=>{
 for(const strokes of [[],Array(33).fill([[0,0,0]]),[[[1001,0,0]]],[[[0,0,0],[0,0,-1]]],[[[0,0,0,NaN]]],[[[0,0,20]],[[0,0,10]]]])assert.throws(()=>normalizeDrawing({...sketch,strokes}));
 assert.throws(()=>normalizeDrawing({...sketch,speechStartMs:Infinity}));
 assert.match(drawingEvidence(sketch),/ambiguous gestures/);
 assert.throws(()=>withDrawing('x'.repeat(20000),sketch),/too large/);
});
test('selected branch descriptions retain the presence of drawings without raw coordinate prose',()=>{
 const context=selectedBranch({head:1,versions:[{id:0,parent:null,source:''},{id:1,parent:0,request:withDrawing('a bird',sketch),source:''}]});
 assert.equal(context.history[0].request,'a bird [with drawing]');
});
test('provider geometry fits every edge with one scale across portrait, square and landscape',()=>{
 for(const aspect of [.25,.5,1,1.333,4]){
  const original={...sketch,aspect,strokes:[[[0,0,0],[1000,1000,200,900]]]};
  const evidence=drawingGeometry(original);
  assert.equal(evidence.width,1000);assert.ok(Math.abs(evidence.width/evidence.height-aspect)<.00001);
  assert.deepEqual(evidence.strokes[0].at(-1),[evidence.width,evidence.height,200,900]);
  for(const [w,h] of [[600,450],[300,600]]){
   const scale=Math.min(w/evidence.width,h/evidence.height);
   for(const [x,y] of evidence.strokes.flat()){
    assert.ok(x*scale<=w+.001);assert.ok(y*scale<=h+.001);
   }
  }
  assert.deepEqual(original.strokes[0].at(-1),[1000,1000,200,900],'stored coordinates stay normalized');
  assert.equal(evidence.speechStartMs,sketch.speechStartMs);
 }
});
test('compact model observations retain full bounds, endpoints, dot timing and alignment',()=>{
 const points=Array.from({length:30},(_,i)=>[i*10,i===7?1000:200,i*100]);
 const input={...sketch,aspect:2,strokes:[points,[[390,580,4000],[390,580,4300]]]};
 const evidence=JSON.parse(drawingEvidence(input).split('\n').at(-1));
 assert.equal(evidence.strokes[0].samples.length,6);
 assert.deepEqual(evidence.strokes[0].bounds,[0,100,290,500],'bounds include extrema omitted from sample trajectory');
 assert.deepEqual(evidence.strokes[0].samples[0],[0,100,0]);
 assert.deepEqual(evidence.strokes[0].samples.at(-1),[290,100,2900]);
 assert.deepEqual(evidence.strokes[1].samples,[[390,290,4000],[390,290,4300]]);
 assert.equal(evidence.speechStartMs,-500);
});

test('a performance keeps timed words, sound, and drawing on their stated clocks',async()=>{
 const {musicalPrompt}=await import('../Resources/Web/musical-input.mjs');
 const request=musicalPrompt({transcript:'like this',words:[{text:'this',atMs:1200,durationMs:200}],sound:{schema:'walkieware-sound/v1',durationMs:3000,audibleMs:2000,frames:[{atMs:1200,rms:0.1,pitchHz:440}],onsetsMs:[1200]},performance:{schema:'whistlegraph-performance/v1',timeline:'audio-start'}});
 const drawing={schema:'whistlegraph-drawing/v1',id:'11111111-1111-4111-8111-111111111111',revision:2,aspect:4/3,strokes:[[[10,10,0],[20,20,1200]]],speechStartMs:-300};
 const combined=withDrawing(request,drawing),parsed=inputData(combined);
 assert.equal(parsed.performance.timeline,'audio-start');assert.equal(parsed.words[0].atMs,1200);assert.equal(parsed.drawing.speechStartMs,-300);
 assert.match(combined,/audioTimeMs = point\[2\] - drawing.speechStartMs/);
 assert.match(inferenceRequest(combined),/whistlegraph-performance\/v1/);
 assert.match(inferenceRequest(combined),/"speechStartMs":-300/);
});
