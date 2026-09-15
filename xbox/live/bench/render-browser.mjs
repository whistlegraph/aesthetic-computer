import { pack } from './pack.mjs';
import WebGLScene from '/scene3d-webgl.mjs';
const frames = await (await fetch('/trace.json')).json();
const canvas = document.querySelector('#canvas'), gpuCanvas = document.querySelector('#gpu');
const context = canvas.getContext('2d', { alpha: false });
const max = Math.max(...frames.map(f => f.triangles.length / 12));
const renderer = new WebGLScene(gpuCanvas, { maxTriangles: max });
const gl = renderer.gl;
gl.disable(gl.DEPTH_TEST); // This comparison preserves Canvas painter order.
const timer = gl.getExtension('EXT_disjoint_timer_query_webgl2');
const debug = gl.getExtension('WEBGL_debug_renderer_info');
const nextFrame = () => new Promise(requestAnimationFrame);
const stats = values => {
  if (!values.length) return null;
  const v = values.slice().sort((a,b)=>a-b);
  return { samples: v.length, p50: v[Math.floor(v.length*.5)], p95: v[Math.min(v.length-1,Math.floor(v.length*.95))], p99: v[Math.min(v.length-1,Math.floor(v.length*.99))] };
};
const retained = frames.map(frame => { const v = new Float32Array(frame.triangles.length / 12 * 18); pack(frame,v); return v; });
function drawCanvas(frame, width, height) {
  context.setTransform(width/1920,0,0,height/1080,0,0);
  context.fillStyle = `rgb(${frame.clear.join(',')})`; context.fillRect(0,0,1920,1080);
  let ink = '', pending = false;
  const t = frame.triangles;
  for (let i=0;i<t.length;i+=12) {
    const color = `rgb(${t[i+9]},${t[i+10]},${t[i+11]})`;
    if (pending && color !== ink) { context.fill(); pending=false; }
    if (!pending) { context.fillStyle=color; context.beginPath(); ink=color; pending=true; }
    context.moveTo(t[i],t[i+1]);
    if ((t[i+3]-t[i])*(t[i+7]-t[i+1]) < (t[i+6]-t[i])*(t[i+4]-t[i+1])) {
      context.lineTo(t[i+6],t[i+7]); context.lineTo(t[i+3],t[i+4]);
    } else { context.lineTo(t[i+3],t[i+4]); context.lineTo(t[i+6],t[i+7]); }
    context.closePath();
  }
  if (pending) context.fill();
}
function drawGpu(frame,index,mode) {
  renderer.scene.triangleCount = frame.triangles.length/12;
  if(mode === 'webgl-stream') pack(frame,renderer.scene.vertices);
  else renderer.scene.vertices.set(retained[index]);
  renderer.present({ clear:[...frame.clear.map(v=>v/255),1] });
}
async function run(mode,width,height,count) {
  canvas.hidden=mode!=='canvas';gpuCanvas.hidden=mode==='canvas';
  canvas.width=width;canvas.height=height;renderer.resize(width,height);
  const cpu=[], intervals=[], completed=[], gpu=[], queries=[];
  const draw=i=>mode==='canvas'?drawCanvas(frames[i],width,height):drawGpu(frames[i],i,mode);
  for(let i=0;i<30;i++){await nextFrame();draw(i%frames.length);}
  let previous=await nextFrame(), disjoint=false;
  for(let i=0;i<count;i++) {
    const now=await nextFrame();intervals.push(now-previous);previous=now;
    const query=mode!=='canvas'&&timer?gl.createQuery():null;
    if(query) gl.beginQuery(timer.TIME_ELAPSED_EXT,query);
    const start=performance.now();draw(i%frames.length);cpu.push(performance.now()-start);
    if(query){gl.endQuery(timer.TIME_ELAPSED_EXT);queries.push(query);}
    if(timer&&gl.getParameter(timer.GPU_DISJOINT_EXT))disjoint=true;
  }
  // Separate forced-completion diagnostic: intentionally stalls, never mixed with rAF samples.
  for(let i=0;i<20;i++) {
    await nextFrame();const start=performance.now();draw(i%frames.length);
    if(mode==='canvas')context.getImageData(0,0,1,1);else gl.finish();
    completed.push(performance.now()-start);
  }
  for(let pass=0;queries.length&&pass<60;pass++) {
    await nextFrame();
    if(gl.getParameter(timer.GPU_DISJOINT_EXT))disjoint=true;
    for(let i=queries.length-1;i>=0;i--) if(gl.getQueryParameter(queries[i],gl.QUERY_RESULT_AVAILABLE)) {
      gpu.push(gl.getQueryParameter(queries[i],gl.QUERY_RESULT)/1e6);gl.deleteQuery(queries[i]);queries.splice(i,1);
    }
  }
  const pendingQueries=queries.length;for(const q of queries)gl.deleteQuery(q);
  if (gl.isContextLost()) throw new Error('WebGL context lost during benchmark');
  const error = gl.getError();
  if (error !== gl.NO_ERROR) throw new Error('WebGL error: ' + error);
  // Same frame for the comparison screenshots; no simulation runs here.
  draw(20);
  return {mode,width,height,cpuMs:stats(cpu),rafIntervalMs:stats(intervals),
    callbacksOver25ms:intervals.filter(v=>v>25).length,completionProbeMs:stats(completed),
    gpuMs:disjoint?null:stats(gpu),gpuDisjoint:disjoint,pendingQueries,
    completionProbe:mode==='canvas'?'1px getImageData forces readback; includes readback overhead':'gl.finish forces GPU completion; excludes display/compositor'};
}
async function idle(count) {
  canvas.hidden = false; gpuCanvas.hidden = true;
  const intervals = []; let previous = await nextFrame();
  for (let i = 0; i < count; i++) {
    const now = await nextFrame(); intervals.push(now - previous); previous = now;
  }
  return { rafIntervalMs: stats(intervals), callbacksOver25ms: intervals.filter(v => v > 25).length };
}
globalThis.bench={run,idle,hardware:()=>({userAgent:navigator.userAgent,
  webglVersion:gl.getParameter(gl.VERSION),renderer:debug?gl.getParameter(debug.UNMASKED_RENDERER_WEBGL):gl.getParameter(gl.RENDERER),
  gpuTimerAvailable:!!timer,antialias:gl.getContextAttributes().antialias,devicePixelRatio,
  visibility:document.visibilityState})};
