struct Control { size: vec2u, kind: u32, radius: u32, color: vec4u, scroll: vec2i, pad: vec2u }
@group(0) @binding(0) var<uniform> control: Control;
@group(0) @binding(1) var<storage,read> inputPixels: array<u32>;
@group(0) @binding(2) var<storage,read_write> outputPixels: array<u32>;
@group(0) @binding(3) var<storage,read> lookup: array<u32>;
@group(0) @binding(4) var<storage,read> circleLUT: array<u32>;
fn rgba(p: u32) -> vec4u {return vec4u(p&255u,(p>>8u)&255u,(p>>16u)&255u,p>>24u);}
fn packed(c: vec4u) -> u32 {return c.x|(c.y<<8u)|(c.z<<16u)|(c.w<<24u);}
fn wrap(x:i32,size:i32)->u32{return u32(((x%size)+size)%size);}
@compute @workgroup_size(256)
fn main(@builtin(global_invocation_id) id:vec3u){
  let i=id.x; if(i>=control.size.x*control.size.y){return;}
  let x=i%control.size.x;let y=i/control.size.x;
  var color=rgba(inputPixels[i]);
  switch control.kind {
    case 0u: {outputPixels[i]=inputPixels[lookup[i]];return;}
    case 1u: {
      if(x==control.size.x/2u){let a=control.color.w;let inv=255u-a;color=vec4u((control.color.xyz*a+color.xyz*inv)>>vec3u(8u),a+((color.w*inv)>>8u));}
    }
    case 2u: {
      for(var n=0u;n<lookup[i];n++){
        let base=color.w*256u;
        let r=circleLUT[base+color.x];let g=circleLUT[base+color.y];let b=circleLUT[base+color.z];
        color=vec4u((r>>select(0u,8u,control.color.x>0u))&255u,(g>>select(0u,8u,control.color.y>0u))&255u,(b>>select(0u,8u,control.color.z>0u))&255u,(r>>16u)&255u);
      }
    }
    case 3u: {if(color.w>0u){color=vec4u(lookup[color.x],lookup[color.y],lookup[color.z],color.w);}}
    case 4u: {
      let sx=wrap(i32(x)+control.scroll.x,i32(control.size.x));
      let sy=wrap(i32(y)-control.scroll.y,i32(control.size.y));
      outputPixels[i]=inputPixels[sx+sy*control.size.x];return;
    }
    default: {}
  }
  outputPixels[i]=packed(color);
}
