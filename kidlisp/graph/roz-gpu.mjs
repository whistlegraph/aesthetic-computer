import {validateRozNodes} from "./roz-nodes.mjs";

export const MAX_ROZ_BATCH = 4;
const SLOTS = MAX_ROZ_BATCH * 6;

// Ordered feedback stays in two persistent buffers. The live path submits
// without awaiting completion; the host bounds queued submissions to two.
export async function createRozGPU(assets, canvas, computeSource, displaySource) {
  if (!navigator.gpu) throw new Error("WebGPU is unavailable");
  const adapter = await navigator.gpu.requestAdapter();
  if (!adapter) throw new Error("No WebGPU adapter");
  const device = await adapter.requestDevice();
  const owned = [];
  let disposed = false, busy = false, failed, pending = 0;
  device.addEventListener("uncapturederror", e => { failed = e.error.message; });
  device.lost.then(info => { if (!disposed) failed = `WebGPU device lost: ${info.message}`; });
  const make = (data, usage) => {
    const buffer = device.createBuffer({size:data.byteLength, usage:usage | GPUBufferUsage.COPY_DST});
    device.queue.writeBuffer(buffer, 0, data); owned.push(buffer); return buffer;
  };
  const context = canvas.getContext("webgpu");
  try {
    if (!context) throw new Error("No WebGPU canvas");
    const compute = device.createShaderModule({code:computeSource});
    const display = device.createShaderModule({code:displaySource});
    for (const module of [compute, display]) {
      const errors = (await module.getCompilationInfo()).messages.filter(m => m.type === "error");
      if (errors.length) throw new Error(errors.map(m => `${m.lineNum}: ${m.message}`).join("\n"));
    }
    const pipeline = await device.createComputePipelineAsync({layout:"auto", compute:{module:compute, entryPoint:"main"}});
    const format = navigator.gpu.getPreferredCanvasFormat();
    context.configure({device, format, alphaMode:"opaque"});
    const present = await device.createRenderPipelineAsync({layout:"auto", vertex:{module:display, entryPoint:"vertex"}, fragment:{module:display, entryPoint:"fragment", targets:[{format}]}});
    const storage = GPUBufferUsage.STORAGE;
    const images = [make(assets.initial,storage | GPUBufferUsage.COPY_SRC),make(assets.initial,storage | GPUBufferUsage.COPY_SRC)];
    const background = make(assets.initial,storage), lut = make(assets.circleLUT,storage);
    const tables = {};
    for (const [name,data] of Object.entries(assets.maps)) tables[name] = make(data,storage);
    for (const [radius,data] of Object.entries(assets.masks)) tables[`circle:${radius}`] = make(data,storage);
    tables.contrast = make(assets.contrast,storage); tables.none = make(new Uint32Array([0]),storage);
    const controlBytes = new ArrayBuffer(SLOTS * 256);
    const u = new Uint32Array(controlBytes), signed = new Int32Array(controlBytes);
    const controls = make(u, GPUBufferUsage.UNIFORM);
    const view = make(new Uint32Array([assets.width,assets.height,0,0]), GPUBufferUsage.UNIFORM);
    const groups = new Map();
    for (let slot=0;slot<SLOTS;slot++) for (let direction=0;direction<2;direction++) for (const [name,table] of Object.entries(tables)) {
      groups.set(`${slot}:${direction}:${name}`, device.createBindGroup({layout:pipeline.getBindGroupLayout(0),entries:[
        {binding:0,resource:{buffer:controls,offset:slot*256,size:48}},
        {binding:1,resource:{buffer:images[direction]}}, {binding:2,resource:{buffer:images[1-direction]}},
        {binding:3,resource:{buffer:table}}, {binding:4,resource:{buffer:lut}},
      ]}));
    }
    const presentGroups = images.map(image => device.createBindGroup({layout:present.getBindGroupLayout(0),entries:[
      {binding:0,resource:{buffer:view}}, {binding:1,resource:{buffer:image}}, {binding:2,resource:{buffer:background}},
    ]}));
    let active = 0;
    const ready = () => {
      if (disposed || failed) throw new Error(failed || "Graph disposed");
      if (busy) throw new Error("Graph already in flight");
    };
    const reset = () => {
      ready();
      device.queue.writeBuffer(images[0],0,assets.initial);device.queue.writeBuffer(images[1],0,assets.initial);active=0;
    };
    async function renderFrames(frames, {readback=false, waitForCompletion=true}={}) {
      ready();
      if (!Array.isArray(frames) || frames.length<1 || frames.length>MAX_ROZ_BATCH) throw new TypeError("Invalid graph frame batch");
      frames.forEach(validateRozNodes);
      if (pending>=2) throw new Error("GPU submission budget exceeded");
      if (canvas.width<1 || canvas.height<1 || canvas.width>1024 || canvas.height>1024) throw new RangeError("Invalid display size");
      busy=true;let read;
      try {
        const start=performance.now(),encoder=device.createCommandEncoder();
        let nextActive=active,slot=0;
        for (const nodes of frames) for (const n of nodes) {
          const offset=slot*64;let table="none",kind;
          u[offset]=assets.width;u[offset+1]=assets.height;
          if(n.op==="spin"||n.op==="zoom"){kind=0;table=n.op==="spin"?`spin:${n.value}`:"zoom";}
          else if(n.op==="line"){kind=1;u.set(n.color,offset+4);}
          else if(n.op==="circle"){kind=2;table=`circle:${n.radius}`;u.set(n.color,offset+4);}
          else if(n.op==="contrast"){kind=3;table="contrast";}
          else if(n.op==="scroll"){kind=4;signed[offset+8]=n.x;signed[offset+9]=n.y;}
          const group=groups.get(`${slot}:${nextActive}:${table}`);
          u[offset+2]=kind;
          const pass=encoder.beginComputePass();pass.setPipeline(pipeline);pass.setBindGroup(0,group);
          pass.dispatchWorkgroups(Math.ceil(assets.width*assets.height/256));pass.end();
          nextActive=1-nextActive;slot++;
        }
        if(slot)device.queue.writeBuffer(controls,0,controlBytes,0,slot*256);
        const pass=encoder.beginRenderPass({colorAttachments:[{view:context.getCurrentTexture().createView(),loadOp:"clear",storeOp:"store",clearValue:{r:0,g:0,b:0,a:1}}]});
        pass.setPipeline(present);pass.setBindGroup(0,presentGroups[nextActive]);pass.draw(3);pass.end();
        if(readback){read=device.createBuffer({size:assets.initial.byteLength,usage:GPUBufferUsage.MAP_READ|GPUBufferUsage.COPY_DST});encoder.copyBufferToBuffer(images[nextActive],0,read,0,assets.initial.byteLength);}
        device.queue.submit([encoder.finish()]);active=nextActive;pending++;
        const submitMs=performance.now()-start;
        const completion=device.queue.onSubmittedWorkDone().then(()=>performance.now()-start).catch(error=>{failed=`WebGPU device lost: ${error.message}`;return null;}).finally(()=>{pending--;});
        const completedMs=(waitForCompletion||readback)?await completion:null;
        if(failed)throw new Error(failed);
        let rgba;
        if(read){await read.mapAsync(GPUMapMode.READ);rgba=new Uint8ClampedArray(read.getMappedRange().slice(0));read.unmap();}
        return {submitMs,completedMs,completion,rgba,passes:slot};
      } finally {read?.destroy();busy=false;}
    }
    return {
      info:{vendor:adapter.info?.vendor,architecture:adapter.info?.architecture},
      get pending(){return pending;}, get failure(){return failed;},
      reset, renderFrames, render:(nodes,options)=>renderFrames([nodes],options),
      async drain(){await device.queue.onSubmittedWorkDone();},
      dispose(){disposed=true;owned.forEach(b=>b.destroy());context.unconfigure();device.destroy();},
    };
  } catch(error) {owned.forEach(b=>b.destroy());context?.unconfigure();device.destroy();throw error;}
}
