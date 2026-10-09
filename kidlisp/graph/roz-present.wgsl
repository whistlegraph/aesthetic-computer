struct View { size:vec2u, pad:vec2u }
@group(0) @binding(0) var<uniform> view:View;
@group(0) @binding(1) var<storage,read> pixels:array<u32>;
@group(0) @binding(2) var<storage,read> background:array<u32>;
struct Vertex { @builtin(position) position:vec4f, @location(0) uv:vec2f }
fn unpack(p:u32)->vec4f{return vec4f(f32(p&255u),f32((p>>8u)&255u),f32((p>>16u)&255u),f32(p>>24u))/255.0;}
@vertex fn vertex(@builtin(vertex_index) i:u32)->Vertex {
  let points=array<vec2f,3>(vec2f(-1,-1),vec2f(3,-1),vec2f(-1,3));
  let p=points[i];return Vertex(vec4f(p,0,1),vec2f((p.x+1.0)/2.0,(1.0-p.y)/2.0));
}
@fragment fn fragment(v:Vertex)->@location(0) vec4f {
  let p=min(vec2u(v.uv*vec2f(view.size)),view.size-vec2u(1));
  let i=p.x+p.y*view.size.x;let color=unpack(pixels[i]);
  return vec4f(color.rgb*color.a+unpack(background[i]).rgb*(1.0-color.a),1);
}
