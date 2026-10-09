import {createRozPlan,ROZ_SOURCE} from "./roz-plan.mjs";
import {prepareRozAssets,renderRozCPU} from "./roz-assets.mjs";
import {createRozGPU} from "./roz-gpu.mjs";
import {createRozClock} from "./roz-clock.mjs";
import {highlightSource} from "./highlight-source.mjs";
import {createWanderer} from "../scene/wanderer-host.mjs";

const $=id=>document.getElementById(id);
const canvas=$("gpu"),cpuCanvas=$("cpu"),pause=$("pause"),stats=$("stats");
function viewportSize(){const scale=Math.max(4,innerWidth/512,innerHeight/512);return {width:Math.max(32,Math.round(innerWidth/scale)),height:Math.max(32,Math.round(innerHeight/scale))};}
let {width,height}=viewportSize(),assets=prepareRozAssets(width,height);
const clock=createRozClock();
const reference=$("reference"),sceneCanvas=$("scene"),picker=$("piece");
const records=new Map([...TOP_HITS.pieces,...Object.values(TOP_HITS.dependencies)].map(p=>[p.code,p]));
let selected="roz",wanderer,selection=0,sceneLast=0,sceneFrames=0,sceneStats=0;
const referenceBase=location.protocol==="file:"?"https://aesthetic.computer":location.origin;
const option=(value,label,group)=>{const item=document.createElement("option");item.value=value;item.textContent=label;group.append(item);};
const experiment=document.createElement("optgroup");experiment.label="Prepared WebGPU";picker.append(experiment);
option("roz","$roz",experiment);option("wanderer","Wanderer · 3D",experiment);
const hits=document.createElement("optgroup");hits.label="Top 100 · AC reference";picker.append(hits);
TOP_HITS.pieces.forEach((p,i)=>option(p.code==="roz"?"reference:roz":p.code,`${i+1}. $${p.code}${p.embeds?" ↳":""}`,hits));
function sendReference(type,extra={}){reference.contentWindow?.postMessage({type,...extra},referenceBase);}
const warmSources=Object.fromEntries([...records].map(([id,p])=>[id,p.source]));
let referenceBooted=false,referenceCode="bop",wantedCode=null,navigating=false,switchStarted=0,switchMs=null,loadTimer;
function armLoading(){clearTimeout(loadTimer);loadTimer=setTimeout(()=>{if(!reference.hidden&&(navigating||!referenceBooted)){stats.textContent="Waiting";$("notice").textContent="The player has not reported ready. Restart retries this piece.";}},8000);}
function navigateReference(code){
  wantedCode=code;
  if(!referenceBooted||navigating){armLoading();return;}
  if(referenceCode===code){sendReference("ac:request-fps");sendReference("kidlisp-resume");stats.textContent="Running";return;}
  navigating=true;referenceCode=code;switchStarted=performance.now();armLoading();
  sendReference("kidlisp-resume");sendReference("ac:navigate",{to:`$${code}`});
}
function showSource(code) {
  const p=records.get(code);highlightSource($("source"),code==="wanderer"?SCENE_SOURCE:p?.source||ROZ_SOURCE);
}
async function selectPiece(value) {
  const epoch=++selection;selected=value;paused=false;pause.textContent="Pause";pause.setAttribute("aria-label","Pause animation");
  picker.value=value;history.replaceState(null,"",`#${value}`);clock.reset();resetStats();
  wanderer?.enable(false);sendReference("kidlisp-pause");reference.hidden=true;
  canvas.hidden=cpuCanvas.hidden=sceneCanvas.hidden=true;$("notice").textContent="";$("timing").textContent="";
  const code=value.replace(/^reference:/,"");showSource(code);$("sources").replaceChildren();
  const link=$("permalink");link.hidden=value==="wanderer";link.href=`https://aesthetic.computer/$${code}`;
  if(value==="roz") {
    canvas.hidden=backend!=="webgpu";cpuCanvas.hidden=backend!=="cpu";dirty=true;
    $("profile").textContent="roz-feedback-v1 · 60 updates/s · Seed 1 · WebGPU with CPU fallback. Resize restarts feedback.";
    stats.textContent=backend==="webgpu"?"WebGPU":"CPU";
  }else if(value==="wanderer") {
    $("profile").textContent="Experimental scene-v1 · WebGPU · Click the image, WASD to walk, drag to look. Double-click locks the pointer; Esc releases it. Geometry comes from this source.";
    stats.textContent="Preparing…";
    try {
      if(!wanderer)wanderer=await createWanderer(sceneCanvas,SCENE_SOURCE,SCENE_SHADER);
      if(epoch!==selection)return;
      sceneCanvas.hidden=false;wanderer.enable(true);sceneLast=sceneStats=performance.now();sceneFrames=0;
      $("notice").textContent="Click + WASD · Drag to look";
    }catch(error){if(epoch===selection){$("notice").textContent=error.message;stats.textContent="Unavailable";}}
  }else {
    $("profile").textContent="AC reference runtime · Original source and inclusions. This piece uses its own timing and renderer.";
    stats.textContent="Loading…";reference.hidden=false;
    navigateReference(code);
  }
  const visited=new Set(),ordered=[];
  function walk(id){if(visited.has(id))return;visited.add(id);ordered.push(id);for(const child of TOP_HITS.audit.edges[id]||[])walk(child);}
  if(records.has(code))walk(code);
  if(ordered.length>1)for(const id of ordered){const b=document.createElement("button");b.textContent=`$${id}`;b.onclick=()=>showSource(id);$("sources").append(b);}
  publish();
}
picker.onchange=()=>void selectPiece(picker.value);
reference.onload=()=>{sendReference("ac:request-fps");};
addEventListener("message",e=>{
  if(e.source!==reference.contentWindow||e.origin!==referenceBase)return;
  if(e.data?.type==="boot-log"&&/^ready:/.test(e.data.message||"")) {
    // The blank init disk also sends ready: prompt; wait for the actual root.
    if(!String(e.data.message).includes(`$${referenceCode}`))return;
    if(!referenceBooted){referenceBooted=true;sendReference("ac:warm-cache",{codes:warmSources});}
    navigating=false;switchMs=switchStarted?performance.now()-switchStarted:null;clearTimeout(loadTimer);
    sendReference("ac:request-fps");
    if(wantedCode&&wantedCode!==referenceCode){navigateReference(wantedCode);return;}
    if(reference.hidden||paused)sendReference("kidlisp-pause");
    else {stats.textContent="Running";$("notice").textContent="";$("timing").textContent=switchMs===null?"":`${Math.round(switchMs)} ms to ready · cached sources`;}
    publish();
  }
  if(!reference.hidden&&e.data?.type==="ac:fps-report"&&Number.isFinite(e.data.fps)&&!navigating)stats.textContent=`${Math.round(e.data.fps)} fps`;
});
let plan=createRozPlan({width,height}),gpu,cpu,paused=false,dirty=true,resetPending=false,inflight;
let frame=0,notice="",backend="webgpu",lastStats=0,lastFrame=0,presents=0,generation=0,resizePending=false;
let gpuTotal=0,gpuSamples=0;
const cpuLayer=new OffscreenCanvas(width,height);
let background=new ImageData(new Uint8ClampedArray(assets.initial.buffer),width,height);
canvas.width=cpuCanvas.width=width;canvas.height=cpuCanvas.height=height;
highlightSource($("source"),ROZ_SOURCE);
const setupGPU=(a,c)=>createRozGPU(a,c,ROZ_GRAPH_SHADER,ROZ_DISPLAY_SHADER);
const freshCPU=()=>({width,height,pixels:new Uint8ClampedArray(assets.initial.buffer.slice(0))});
function resetStats(){lastStats=performance.now();lastFrame=frame;presents=gpuTotal=gpuSamples=0;generation++;}
function fallback(error) {
  notice=error.message;gpu?.dispose();gpu=null;backend="cpu";cpu=freshCPU();plan=createRozPlan({width,height});frame=0;clock.reset();resetStats();
  canvas.hidden=true;cpuCanvas.hidden=selected!=="roz";
  $("notice").textContent=`CPU fallback · ${notice}. Feedback restarted.`;
}
const publish=()=>window.rozPreview={frame,paused,backend,notice,width,height,selected,reference:{ready:referenceBooted,code:referenceCode,wanted:wantedCode,navigating,switchMs},scene:wanderer?.state,pending:gpu?.pending??0};
function drawCPU() {
  const ctx=cpuCanvas.getContext("2d");
  ctx.putImageData(background,0,0);
  cpuLayer.getContext("2d").putImageData(new ImageData(cpu.pixels,width,height),0,0);
  ctx.drawImage(cpuLayer,0,0);
}
async function draw(count) {
  if(resizePending) {
    resizePending=false;await gpu?.drain();gpu?.dispose();gpu=null;
    ({width,height}=viewportSize());assets=prepareRozAssets(width,height);
    canvas.width=cpuCanvas.width=cpuLayer.width=width;canvas.height=cpuCanvas.height=cpuLayer.height=height;
    background=new ImageData(new Uint8ClampedArray(assets.initial.buffer),width,height);
    if(backend==="webgpu")try{gpu=await setupGPU(assets,canvas);}catch(error){fallback(error);}
    resetPending=true;
  }
  if(resetPending) {
    plan=createRozPlan({width,height});gpu?.reset();cpu=freshCPU();frame=0;resetPending=false;clock.reset();resetStats();count=0;
  }
  const frames=Array.from({length:count},()=>plan.next().nodes);
  if(gpu) {
    const owner=gpu,epoch=generation;
    const result=await gpu.renderFrames(frames.length?frames:[[]],{waitForCompletion:false});
    result.completion.then(ms=>{if(ms!==null&&gpu===owner&&generation===epoch){gpuTotal+=ms;gpuSamples++;}});
  } else {
    for(const nodes of frames)renderRozCPU(cpu,nodes);
    drawCPU();
  }
  frame+=count;presents++;
  const now=performance.now();
  if(count && now-lastStats>=500) {
    const seconds=(now-lastStats)/1000,updates=Math.round((frame-lastFrame)/seconds),fps=Math.round(presents/seconds);
    stats.textContent=`${fps} fps`;
    $("timing").textContent=gpuSamples?`${updates} updates/s · ${gpu?"WebGPU":"CPU"} · ${(gpuTotal/gpuSamples).toFixed(2)} ms per completed GPU submission. Each submission may contain several ordered updates.`:"";
    lastFrame=frame;lastStats=now;presents=gpuTotal=gpuSamples=0;
  }
  publish();
}
function tick(now) {
  const hidden=document.hidden;
  if(selected!=="roz") {
    clock.reset();
    if(selected==="wanderer"&&wanderer&&!sceneCanvas.hidden&&!hidden)try {
      if(wanderer.render(now,{paused}))sceneFrames++;
      if(now-sceneStats>=500){stats.textContent=`${Math.round(sceneFrames*1000/(now-sceneStats))} fps`;sceneStats=now;sceneFrames=0;}
      publish();
    }catch(error){$("notice").textContent=error.message;sceneCanvas.hidden=true;stats.textContent="Stopped";}
    requestAnimationFrame(tick);return;
  }
  const blocked=!!inflight||(gpu?.pending??0)>=2;
  const count=clock.tick(now,{paused:paused||hidden,blocked});
  if(!hidden&&!blocked&&(dirty||count)) {
    dirty=false;
    inflight=draw(count).catch(error=>{
      if(gpu){fallback(error);dirty=true;}else{paused=true;pause.textContent="Play";$("notice").textContent=error.message;}
      publish();
    }).finally(()=>{inflight=null;});
  }
  requestAnimationFrame(tick);
}
pause.onclick=()=>{paused=!paused;pause.textContent=paused?"Play":"Pause";pause.setAttribute("aria-label",paused?"Resume animation":"Pause animation");clock.reset();resetStats();if(!reference.hidden)sendReference(paused?"kidlisp-pause":"kidlisp-resume");publish();};
$("reset").onclick=()=>{if(selected==="roz"){resetPending=true;dirty=true;}else if(selected==="wanderer")wanderer?.reset();else {referenceBooted=false;navigating=false;referenceCode=selected.replace(/^reference:/,"");reference.src=`${referenceBase}/$${referenceCode}?nogap=true&nolabel=true&density=4&noauth=true`;armLoading();}};
let resizeTimer;
addEventListener("resize",()=>{clearTimeout(resizeTimer);resizeTimer=setTimeout(()=>{resizePending=true;dirty=true;},150);});

async function boot() {
  reference.src=`${referenceBase}/$bop?nogap=true&nolabel=true&density=4&noauth=true`;
  try{gpu=await setupGPU(assets,canvas);}catch(error){fallback(error);}
  resetStats();await selectPiece([...picker.options].some(o=>o.value===location.hash.slice(1))?location.hash.slice(1):"roz");publish();requestAnimationFrame(tick);
  window.rozHarness={
    pause:async()=>{paused=true;pause.textContent="Play";await inflight;await gpu?.drain();clock.reset();publish();},
    async context({width=128,height=128,seed=1}={}) {
      const start=performance.now(),a=prepareRozAssets(width,height);
      const c=document.createElement("canvas");c.width=width;c.height=height;
      const g=await setupGPU(a,c);let p=createRozPlan({width,height,seed});
      let b={width,height,pixels:new Uint8ClampedArray(a.initial.buffer.slice(0))};
      return {preparationMs:performance.now()-start,info:g.info,
        async step({reference=true,readback=true,count=1,waitForCompletion=true}={}) {
          const start=performance.now(),commands=Array.from({length:count},()=>p.next()),controlMs=performance.now()-start;
          const cpuStart=performance.now();if(reference)for(const command of commands)renderRozCPU(b,command.nodes);
          const cpuMs=performance.now()-cpuStart;
          const result=await g.renderFrames(commands.map(c=>c.nodes),{readback,waitForCompletion});
          return {...result,controlMs,cpuMs,commands,expected:reference?b.pixels.slice():undefined};
        },
        render:(nodes,options)=>g.render(nodes,options),renderFrames:(frames,options)=>g.renderFrames(frames,options),
        reset(){g.reset();p=createRozPlan({width,height,seed});b={width,height,pixels:new Uint8ClampedArray(a.initial.buffer.slice(0))};},
        drain:()=>g.drain(),dispose:()=>g.dispose(),
      };
    },
  };
}
boot().catch(error=>{$("notice").textContent=error.message;});
addEventListener("pagehide",()=>{gpu?.dispose();wanderer?.dispose();},{once:true});
