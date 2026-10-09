import {compileScene,createSceneState} from "./scene-plan.mjs";

// A prepared scene; animation updates packed values, never compiles shaders.
export async function createWanderer(canvas,source,shader) {
  const plan=compileScene(source),state=createSceneState(plan);
  if(!navigator.gpu)throw new Error("The experimental scene needs WebGPU");
  const adapter=await navigator.gpu.requestAdapter();
  if(!adapter)throw new Error("No WebGPU adapter");
  const device=await adapter.requestDevice(),context=canvas.getContext("webgpu");
  const owned=[],listeners=new AbortController();let failure,disposed=false,pending=0;
  device.addEventListener("uncapturederror",e=>failure=e.error.message);
  device.lost.then(info=>{if(!disposed)failure=info.message||"WebGPU device lost";});
  try {
    const module=device.createShaderModule({code:shader});
    const errors=(await module.getCompilationInfo()).messages.filter(m=>m.type==="error");
    if(errors.length)throw new Error(errors.map(m=>`${m.lineNum}: ${m.message}`).join("\n"));
    const format=navigator.gpu.getPreferredCanvasFormat();
    const pipeline=await device.createRenderPipelineAsync({layout:"auto",vertex:{module,entryPoint:"vertex"},fragment:{module,entryPoint:"fragment",targets:[{format}]}});
    context.configure({device,format,alphaMode:"opaque"});
    const uniform=device.createBuffer({size:80,usage:GPUBufferUsage.UNIFORM|GPUBufferUsage.COPY_DST});
    const shapes=device.createBuffer({size:state.primitives.byteLength,usage:GPUBufferUsage.STORAGE|GPUBufferUsage.COPY_DST});
    owned.push(uniform,shapes);
    const group=device.createBindGroup({layout:pipeline.getBindGroupLayout(0),entries:[{binding:0,resource:{buffer:uniform}},{binding:1,resource:{buffer:shapes}}]});
    const bytes=new ArrayBuffer(80),u=new Uint32Array(bytes),f=new Float32Array(bytes);
    let camera=[...plan.camera],yaw=0,pitch=-.07,time=0,previous=null,enabled=false,dragging=false;
    const keys=new Set(),on=(target,type,fn)=>target.addEventListener(type,fn,{signal:listeners.signal});
    on(window,"keydown",e=>{if(enabled&&document.activeElement===canvas&&["KeyW","KeyA","KeyS","KeyD","ArrowUp","ArrowDown","ArrowLeft","ArrowRight"].includes(e.code)){keys.add(e.code);e.preventDefault();}});
    on(window,"keyup",e=>keys.delete(e.code));
    on(window,"blur",()=>keys.clear());
    on(document,"visibilitychange",()=>{keys.clear();previous=null;});
    on(canvas,"pointerdown",e=>{if(enabled){canvas.focus();dragging=true;canvas.setPointerCapture(e.pointerId);}});
    on(canvas,"pointerup",()=>dragging=false);
    on(canvas,"pointercancel",()=>dragging=false);
    on(canvas,"pointermove",e=>{if(enabled&&(dragging||document.pointerLockElement===canvas)){yaw+=e.movementX*.004;pitch=Math.max(-1.3,Math.min(1.3,pitch-e.movementY*.004));}});
    on(canvas,"dblclick",()=>{if(enabled)canvas.requestPointerLock()?.catch(()=>{});});
    const reset=()=>{camera=[...plan.camera];yaw=0;pitch=-.07;time=0;previous=null;keys.clear();};
    function render(now,{paused=false}={}) {
      if(disposed||failure)throw new Error(failure||"Scene disposed");
      const dt=previous===null||paused?0:Math.min(.05,Math.max(0,(now-previous)/1000));previous=now;
      if(pending>=2)return false;
      time=(time+dt)%86400;state.update(time);
      let forward=Number(keys.has("KeyW")||keys.has("ArrowUp"))-Number(keys.has("KeyS")||keys.has("ArrowDown"));
      let side=Number(keys.has("KeyD"))-Number(keys.has("KeyA"));
      yaw+=(Number(keys.has("ArrowRight"))-Number(keys.has("ArrowLeft")))*dt*1.8;
      const step=dt*plan.walk/Math.max(1,Math.hypot(forward,side));
      const x=camera[0]+(Math.sin(yaw)*forward+Math.cos(yaw)*side)*step;
      const z=camera[2]+(Math.cos(yaw)*forward-Math.sin(yaw)*side)*step;
      if(!state.blocked(x,camera[1],camera[2]))camera[0]=x;
      if(!state.blocked(camera[0],camera[1],z))camera[2]=z;
      const scale=Math.max(4,innerWidth/512,innerHeight/512);
      const width=Math.max(32,Math.round(innerWidth/scale)),height=Math.max(32,Math.round(innerHeight/scale));
      if(canvas.width!==width||canvas.height!==height){canvas.width=width;canvas.height=height;}
      u.set([width,height,plan.primitives.length,0]);f.set([...camera,yaw],4);
      f.set([pitch,width/height,Math.tan(Math.PI/6),time],8);f.set([...plan.sun,0],12);f.set([...plan.sky.map(c=>c/255),plan.fog],16);
      device.queue.writeBuffer(uniform,0,bytes);device.queue.writeBuffer(shapes,0,state.primitives);
      const encoder=device.createCommandEncoder();
      const pass=encoder.beginRenderPass({colorAttachments:[{view:context.getCurrentTexture().createView(),loadOp:"clear",storeOp:"store"}]});
      pass.setPipeline(pipeline);pass.setBindGroup(0,group);pass.draw(3);pass.end();
      device.queue.submit([encoder.finish()]);pending++;
      device.queue.onSubmittedWorkDone().catch(e=>failure=e.message).finally(()=>pending--);
      return true;
    }
    return {plan,render,reset,
      enable(value){enabled=value;keys.clear();previous=null;if(!value&&document.pointerLockElement===canvas)document.exitPointerLock();},
      get state(){return {camera:[...camera],yaw,pitch,time,pending};},
      async drain(){await device.queue.onSubmittedWorkDone();},
      dispose(){disposed=true;listeners.abort();owned.forEach(b=>b.destroy());context.unconfigure();device.destroy();},
    };
  }catch(error){listeners.abort();owned.forEach(b=>b.destroy());context?.unconfigure();device.destroy();throw error;}
}
