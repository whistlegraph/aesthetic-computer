import {PixelProfiler} from '../../system/public/aesthetic.computer/lib/pixel-type.mjs';
const shader=`struct View {size:vec2u,hud:vec2u}
@group(0) @binding(0) var<uniform> view:View;
@group(0) @binding(1) var<storage,read> pixels:array<u32>;
@vertex fn vertex(@builtin(vertex_index) i:u32)->@builtin(position) vec4f {let p=array<vec2f,3>(vec2f(-1,-1),vec2f(3,-1),vec2f(-1,3));return vec4f(p[i],0,1);}
@fragment fn fragment(@builtin(position) position:vec4f)->@location(0) vec4f {
let origin=max(vec2i(0),vec2i(view.size)-vec2i(view.hud)-vec2i(2));let p=vec2i(position.xy)-origin;
if(any(p<vec2i(0))||any(p>=vec2i(view.hud))){discard;}
let c=pixels[u32(p.x)+u32(p.y)*view.hud.x];return vec4f(f32(c&255u),f32((c>>8u)&255u),f32((c>>16u)&255u),255)/255.0;}`;
export async function createPixelProfilerGPU(device,format){
 const profiler=new PixelProfiler(),module=device.createShaderModule({code:shader});
 const pipeline=await device.createRenderPipelineAsync({layout:'auto',vertex:{module,entryPoint:'vertex'},fragment:{module,entryPoint:'fragment',targets:[{format}]}});
 const pixels=device.createBuffer({size:200*14*4,usage:GPUBufferUsage.STORAGE|GPUBufferUsage.COPY_DST});
 const view=device.createBuffer({size:16,usage:GPUBufferUsage.UNIFORM|GPUBufferUsage.COPY_DST});
 const bind=device.createBindGroup({layout:pipeline.getBindGroupLayout(0),entries:[{binding:0,resource:{buffer:view}},{binding:1,resource:{buffer:pixels}}]});
 let version=-1;const control=new Uint32Array(4);
 return {profiler,draw(encoder,target,width,height){
  if(version!==profiler.version){device.queue.writeBuffer(pixels,0,profiler.hud.pixels);version=profiler.version;}
  control.set([width,height,profiler.hud.width,profiler.hud.height]);device.queue.writeBuffer(view,0,control);
  const pass=encoder.beginRenderPass({colorAttachments:[{view:target,loadOp:'load',storeOp:'store'}]});pass.setPipeline(pipeline);pass.setBindGroup(0,bind);pass.draw(3);pass.end();
 },dispose(){pixels.destroy();view.destroy();}};
}
