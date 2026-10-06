import test from 'node:test';
import assert from 'node:assert/strict';
import {execFileSync} from 'node:child_process';
import {mkdtempSync,readFileSync,rmSync,writeFileSync} from 'node:fs';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import {fileURLToPath} from 'node:url';
import vm from 'node:vm';

function benchmark(t,requireSavedModel=true){
  const directory=mkdtempSync(join(tmpdir(),'oskiewar-benchmark-test-'));
  t.after(()=>rmSync(directory,{recursive:true,force:true}));
  const source=join(directory,'fixture.js'),output=join(directory,'benchmark.js');
  writeFileSync(source,`// @bundle-native-account
let freeskateLook = (Date.now()>>>0)||1;
let debugHitboxes=false;
const players=[{x:2840,z:0}];
function freeskateCourseNow(){return 'desert';}
function sim(){lifecycle.sim++;lifecycle.pads=[gamepad(0),gamepad(1)];}
function paint(){lifecycle.paint++;}
`);
  const args=[fileURLToPath(new URL('./benchmark-native.mjs',import.meta.url)),
    '--source',source,'--out',output,'--label','saved-model-test'];
  if(requireSavedModel)args.push('--require-saved-model');
  const generated=JSON.parse(execFileSync(process.execPath,args,{encoding:'utf8'}));
  assert.equal(generated.requireSavedModel,requireSavedModel);
  assert.equal(generated.durationSeconds,56);
  assert.equal(generated.waitTimeoutSeconds,requireSavedModel?30:0);
  assert.equal(generated.maxDurationSeconds,requireSavedModel?86:56);
  assert.equal(generated.controllerInputAborts,true);
  const code=readFileSync(output,'utf8');
  assert.ok(code.includes('// @bundle-native-account'),'native account bundling survives instrumentation');
  let now=100000;
  const events=[],lifecycle={sim:0,paint:0},pads=[
    {connected:true,down:[],leftX:0,leftY:0,rightX:0,rightY:0,leftTrigger:0,rightTrigger:0},
    {connected:true,down:[],leftX:0,leftY:0,rightX:0,rightY:0,leftTrigger:0,rightTrigger:0},
  ];
  const context=vm.createContext({
    Date:{now:()=>now},lifecycle,gamepad:index=>pads[index],
    runtime:()=>({frameMs:16.7,renderCpuMs:.5,presentMs:4,hz:60,refreshHz:60}),
    telemetry:(name,payload)=>events.push({name,payload}),
  });
  vm.runInContext(code,context);
  return {context,events,pads,lifecycle,
    advance:ms=>{now+=ms;},sim:()=>context.sim(),
    paints:(count=120)=>{for(let i=0;i<count;i++)context.paint();},
    ready:()=>{context.__oskiewarLocalPractice=true;},
    named:name=>events.filter(event=>event.name===name),
  };
}

test('saved-model benchmark starts its warmup when the model arrives',t=>{
  const b=benchmark(t);
  b.sim();b.paints();b.advance(25000);b.sim();b.paints();
  assert.equal(b.events.length,0,'waiting frames neither start nor sample the benchmark');
  assert.deepEqual(Array.from(b.context.gamepad(0).down),[]);
  assert.ok(b.lifecycle.sim>0&&b.lifecycle.paint>0,'account loading and rendering keep running');
  b.ready();b.sim();b.paints();
  const begin=JSON.parse(b.named('BENCH_BEGIN')[0].payload);
  assert.equal(begin.savedModel,true);assert.equal(begin.debug,true);
  assert.equal(b.named('BENCH_SAMPLE').length,0);
  b.advance(13999);b.sim();b.paints();
  assert.equal(b.named('BENCH_SAMPLE').length,0,'the pre-model wait is excluded from warmup');
  b.advance(1);b.sim();b.paints();
  const samples=b.named('BENCH_SAMPLE');
  assert.equal(samples.length,1);
  assert.equal(JSON.parse(samples[0].payload).elapsedMs,14000);
  b.advance(42000);b.sim();b.sim();b.paints();
  assert.equal(b.named('BENCH_END').length,1);
  assert.equal(b.context.gamepad(0),b.pads[0],'completion restores live input');
});

test('saved-model timeout aborts once and restores both live pad inputs',t=>{
  const b=benchmark(t);
  b.sim();b.paints();b.advance(29999);b.sim();
  assert.equal(b.named('BENCH_ABORT').length,0);
  b.advance(1);b.sim();b.paints();
  assert.equal(b.named('BENCH_ABORT').length,1);
  assert.equal(b.named('BENCH_ABORT')[0].payload,'saved model unavailable');
  for(let i=0;i<2;i++)assert.equal(b.context.gamepad(i),b.pads[i]);
  b.ready();b.advance(60000);
  for(let i=0;i<3;i++){b.sim();b.paints();}
  assert.deepEqual(b.events.map(event=>event.name),['BENCH_ABORT'],'late readiness cannot restart an aborted run');
});

test('benchmarks without the saved-model flag retain their immediate start',t=>{
  const b=benchmark(t,false);
  b.sim();
  assert.equal(JSON.parse(b.named('BENCH_BEGIN')[0].payload).savedModel,false);
  b.advance(14000);b.sim();b.paints();
  assert.equal(b.named('BENCH_SAMPLE').length,1);
});

test('deliberate buttons, either stick, or either trigger abort on the same simulation tick',t=>{
  const inputs=[{down:['A']},{leftX:.26},{leftY:-.26},{rightX:-.26},{rightY:.26},
    {leftX:.2,leftY:.2},{leftTrigger:.16},{rightTrigger:.16}];
  for(const [i,input] of inputs.entries()){
    const b=benchmark(t);
    if(i%2)b.ready(); // Exercise both saved-model waiting and timed measurement.
    b.sim();b.paints();
    const seat=i%2;
    Object.assign(b.pads[seat],input);b.sim();
    assert.equal(b.lifecycle.pads[seat],b.pads[seat],'the input that interrupted the benchmark reaches the game');
    assert.equal(vm.runInContext('debugHitboxes',b.context),false);
    assert.equal(b.named('BENCH_ABORT').length,1);
    assert.equal(b.named('BENCH_ABORT')[0].payload,'controller input');
    b.advance(60000);b.sim();b.sim();b.paints();
    assert.equal(b.named('BENCH_ABORT').length,1,'held input does not repeatedly abort');
    assert.equal(b.named('BENCH_SAMPLE').length,0);
    assert.equal(b.named('BENCH_END').length,0);
    for(let seat=0;seat<2;seat++)assert.equal(b.context.gamepad(seat),b.pads[seat]);
  }
});

test('a direct controller poll also aborts immediately without waiting for another sim tick',t=>{
  const b=benchmark(t);b.ready();b.sim();
  b.pads[1].down=['RightShoulder'];
  assert.equal(b.context.gamepad(1),b.pads[1]);
  assert.equal(b.named('BENCH_ABORT').length,1);
  assert.equal(b.context.gamepad(),b.pads[0]);
  b.sim();assert.equal(b.named('BENCH_ABORT').length,1);
});

test('small drift and exact input thresholds do not abort a benchmark',t=>{
  const b=benchmark(t);b.ready();
  Object.assign(b.pads[0],{leftX:.25,rightY:-.25,leftTrigger:.15,rightTrigger:.15});
  Object.assign(b.pads[1],{leftX:.03,leftY:-.04,rightX:.02,rightY:.01});
  b.sim();b.advance(14000);b.sim();b.paints();
  assert.equal(b.named('BENCH_ABORT').length,0);
  assert.equal(b.named('BENCH_SAMPLE').length,1);
  assert.equal(b.lifecycle.pads[0].leftX,0,'small drift stays neutral during the measurement');
});
