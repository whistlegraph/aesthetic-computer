struct Frame { size:vec2u,count:u32,pad:u32,camera:vec4f,look:vec4f,sun:vec4f,sky:vec4f }
struct Shape { position:vec4f,size:vec4f,color:vec4f,pad:vec4f }
struct Hit { distance:f32,index:i32 }
@group(0) @binding(0) var<uniform> frame:Frame;
@group(0) @binding(1) var<storage,read> shapes:array<Shape>;
@vertex fn vertex(@builtin(vertex_index) i:u32)->@builtin(position) vec4f {
  let p=array<vec2f,3>(vec2f(-1,-1),vec2f(3,-1),vec2f(-1,3));return vec4f(p[i],0,1);
}
fn trace(origin:vec3f,direction:vec3f)->Hit {
  var best=Hit(60.0,-1);
  for(var i=0u;i<frame.count;i++) {
    let shape=shapes[i];let local=origin-shape.position.xyz;var t=1000.0;
    switch u32(shape.position.w) {
      case 0u: {
        let b=dot(local,direction);let c=dot(local,local)-shape.size.x*shape.size.x;let d=b*b-c;
        if(d>=0.0){let root=sqrt(d);t=-b-root;if(t<.001){t=-b+root;}}
      }
      case 1u: {
        let inv=select(vec3f(-1),vec3f(1),direction>=vec3f(0))/max(abs(direction),vec3f(0.000001));
        let lo=(-shape.size.xyz-local)*inv;let hi=(shape.size.xyz-local)*inv;
        let near=min(lo,hi);let far=max(lo,hi);let enter=max(near.x,max(near.y,near.z));let leave=min(far.x,min(far.y,far.z));
        if(leave>=max(enter,0.0)){t=select(enter,leave,enter<.001);}
      }
      default: {if(abs(direction.y)>.000001){t=-local.y/direction.y;}}
    }
    if(t>.001&&t<best.distance){best=Hit(t,i32(i));}
  }
  return best;
}
fn normal(p:vec3f,shape:Shape)->vec3f {
  let local=p-shape.position.xyz;
  if(shape.position.w==0.0){return normalize(local);}
  if(shape.position.w==2.0){return vec3f(0,1,0);}
  let relative=abs(local/shape.size.xyz);
  if(relative.x>=relative.y&&relative.x>=relative.z){return vec3f(sign(local.x),0,0);}
  if(relative.y>=relative.z){return vec3f(0,sign(local.y),0);}return vec3f(0,0,sign(local.z));
}
fn sky(direction:vec3f)->vec3f{return frame.sky.rgb*(.7+.6*max(direction.y,0.0));}
@fragment fn fragment(@builtin(position) pixel:vec4f)->@location(0) vec4f {
  let uv=(pixel.xy/vec2f(frame.size)*2.0-1.0)*vec2f(frame.look.y,-1.0);
  let yaw=frame.camera.w;let pitch=frame.look.x;
  let forward=vec3f(sin(yaw)*cos(pitch),sin(pitch),cos(yaw)*cos(pitch));
  let right=vec3f(cos(yaw),0,-sin(yaw));let up=cross(forward,right);
  var direction=normalize(forward+frame.look.z*(uv.x*right+uv.y*up));
  var origin=frame.camera.xyz;var color=vec3f(0);var weight=1.0;
  let sun=normalize(frame.sun.xyz);
  for(var bounce=0u;bounce<2u;bounce++) {
    let hit=trace(origin,direction);
    if(hit.index<0){color+=weight*sky(direction);break;}
    let shape=shapes[hit.index];let p=origin+direction*hit.distance;let n=normal(p,shape);
    var albedo=shape.color.rgb;
    if(shape.color.w>0.0){let cell=floor(p.x/shape.color.w)+floor(p.z/shape.color.w);albedo*=select(.45,1.0,(i32(cell)%2)==0);}
    let shadow=trace(p+n*.005,sun).index>=0;
    let diffuse=max(dot(n,sun),0.0)*select(1.0,.08,shadow);
    let reflected=reflect(direction,n);
    let shine=pow(max(dot(reflected,sun),0.0),48.0)*select(.45,0.0,shadow);
    let lit=albedo*(.18+.82*diffuse)+vec3f(shine);
    let fog=1.0-exp(-hit.distance*frame.sky.w);
    let surface=mix(lit,frame.sky.rgb,fog);
    let mirror=select(shape.size.w,0.0,bounce==1u);
    color+=weight*(1.0-mirror)*surface;weight*=mirror;
    if(weight<.01){break;}origin=p+n*.005;direction=reflected;
  }
  return vec4f(color,1);
}
