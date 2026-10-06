// Presentation only: generated colors/accessories never enter the simulation.
export const rgb = value => [1, 3, 5].map(i => parseInt(value.slice(i, i + 2), 16));
export function validateFighter(fighter) {
  const a = fighter?.appearance;
  if (fighter?.version !== 1 || fighter.recipe !== 'oskiewar-capsule-fighter-v1' ||
      !/^[a-f0-9]{64}$/.test(fighter.hash) || !a ||
      ['skin', 'hair', 'shirt', 'pants', 'shoes'].some(k => !/^#[a-f0-9]{6}$/i.test(a[k])) ||
      !['none', 'short', 'long', 'curly'].includes(a.hairStyle) ||
      !['short', 'long'].includes(a.sleeves) || typeof a.beard !== 'boolean' || typeof a.glasses !== 'boolean')
    throw Error('Invalid fighter preview.');
  return { ...a, ...Object.fromEntries(['skin', 'hair', 'shirt', 'pants', 'shoes'].map(k => [k, rgb(a[k])])) };
}

// One articulated mesh for both the consent preview and the playable rig.
// Coordinates use the game's downward Y. Bone-local Y spans 0..1; transverse
// dimensions stay in world units. No model values enter collision or movement.
const cache = new Map();
const tint = (color, amount) => color.map(c => Math.round(c + (amount > 0 ? 255 - c : c) * amount));
export function fighterParts(a) {
  const key = JSON.stringify(a);
  if (cache.has(key)) return cache.get(key);
  const parts = {};
  let faces;
  const quad = (a,b,c,d,color) => faces.push({points:[a,d,c,b],color});
  function ellipsoid(center, scale, color, cols=12, rows=8) {
    const at = (j,i) => {
      const lat=j/rows*Math.PI, lon=i/cols*Math.PI*2;
      return [center[0]+Math.cos(lon)*Math.sin(lat)*scale[0], center[1]-Math.cos(lat)*scale[1], center[2]+Math.sin(lon)*Math.sin(lat)*scale[2]];
    };
    for(let j=0;j<rows;j++) for(let i=0;i<cols;i++) quad(at(j,i),at(j,i+1),at(j+1,i+1),at(j+1,i),color);
  }
  function rings(profile,color,cols=12) {
    const at=(r,i)=>[Math.cos(i/cols*Math.PI*2)*r[1],r[0],Math.sin(i/cols*Math.PI*2)*r[2]];
    for(let j=0;j<profile.length-1;j++) for(let i=0;i<cols;i++) quad(at(profile[j],i),at(profile[j],i+1),at(profile[j+1],i+1),at(profile[j+1],i),color);
  }
  const part=(name,draw)=>{faces=[];draw();parts[name]=faces;};
  part('torso',()=>{
    rings([[0,8,7],[.07,19,10],[.22,21,12],[.52,18,12],[.83,16,11],[1,17,11],[1.03,0,0]],a.shirt,16);
    // Collar and placket add readable clothing construction at game scale.
    for(const side of [-1,1]) quad([side*2,.03,7.5],[side*9,.06,9],[side*8,.25,12.4],[side*.8,.12,11],tint(a.shirt,-.1));
    quad([-.6,.15,12.2],[.6,.15,12.2],[.6,.85,11.8],[-.6,.85,11.8],tint(a.shirt,-.075));
    for(const y of [.32,.51,.7]) ellipsoid([0,y,12.5],[.7,.014,.4],tint(a.shirt,-.28),6,4);
  });
  part('neck',()=>rings([[0,5,5],[.15,6,6],[.85,6.5,6],[1,5,5]],a.skin));
  part('pelvis',()=>ellipsoid([0,5,0],[17,7,11],a.pants));
  part('upper-arm',()=>{
    const shirtEnd=a.sleeves==='long'?1:.52;
    rings([[0,0,0],[.08,8,8],[.4,7.8,8],[shirtEnd,6.7,7]],a.shirt);
    if(shirtEnd<1) rings([[shirtEnd,6.4,6.5],[.8,6,6],[1,5.8,5.8]],a.skin);
  });
  part('forearm',()=>{
    const color=a.sleeves==='long'?a.shirt:a.skin;
    rings([[0,5.8,5.8],[.25,6.7,6.5],[.78,5.3,5.2],[.94,4.8,4.8],[1,4.8,4.8]],color);
    if(a.sleeves==='long') rings([[.88,5.5,5.4],[.98,5.3,5.3]],tint(color,-.1));
  });
  part('hand',()=>{ellipsoid([0,3,0],[5.4,7.2,4],a.skin,10,6);ellipsoid([4,1,2],[2.5,4,2.5],a.skin,8,4);});
  part('thigh',()=>rings([[0,8,9],[.1,10,11],[.5,9.7,10],[.9,7.8,8],[1,7.5,8]],a.pants));
  part('shin',()=>{
    rings([[0,7.5,8],[.3,8,8.5],[.75,6.7,7],[1,6.5,6.5]],a.pants);
    rings([[.93,6.9,6.9],[1,6.9,6.9]],tint(a.pants,-.14));
  });
  part('shoe',()=>{
    ellipsoid([0,-1,4],[7.5,5,13],tint(a.shoes,.07),12,6);
    ellipsoid([0,2,4],[7.8,2.2,13.4],tint(a.shoes,.18),12,4);
    ellipsoid([0,-3,1],[5,3,6],a.shoes,8,4);
  });
  part('head',()=>{
    // Less spherical jaw, a brow, ears and a projecting nose.
    rings([[-23,0,0],[-21,10,10],[-16,16,15],[-8,18,16],[0,17,16],[9,15,14],[17,11,10],[20,5,6],[21,0,0]],a.skin,20);
    for(const side of [-1,1]) {
      ellipsoid([side*17,1,0],[3.4,5.3,3.3],a.skin,8,6);
      ellipsoid([side*6.6,-2,14.8],[4,1.9,1.65],[245,240,230],10,6);
      ellipsoid([side*6.6,-1.9,16.3],[1.4,1.5,.7],[57,49,43],8,6);
      ellipsoid([side*6.4,-6,14.9],[4.2,.85,1],a.hair,8,4);
    }
    ellipsoid([0,1,15.8],[2.3,4.9,2.5],a.skin,10,6);
    ellipsoid([0,4,17.3],[3,2.1,2.2],tint(a.skin,-.03),10,6);
    ellipsoid([0,10,12.7],[4.8,.7,.9],tint(a.skin,-.32),10,4);
    ellipsoid([0,11.2,12.3],[4,.65,.8],tint(a.skin,-.12),10,4);
    if(a.hairStyle!=='none') {
      // Back volume is behind the head; the front stays open below the part.
      ellipsoid([0,-13,-5],[18.8,12,13.6],a.hair,16,8);
      if(a.hairStyle==='long') {
        ellipsoid([0,-.5,-11],[18.5,24,8.5],a.hair,14,8);
        for(const side of [-1,1]) {
          ellipsoid([side*17.4,0,-2],[5,20,10],a.hair,10,8);
          ellipsoid([side*18.3,13,0],[3.8,11,7],tint(a.hair,.035),8,6);
        }
      }
      if(a.hairStyle==='curly') for(let i=0;i<10;i++) {
        const t=i/10*Math.PI*2;
        ellipsoid([Math.cos(t)*14,-17+Math.sin(t)*3,Math.sin(t)*10],[6.5,7,6.5],tint(a.hair,i%2*.045),8,6);
      }
      else {
        // Two swept lobes leave a narrow visible part, instead of a helmet.
        ellipsoid([-8,-19,5],[10,6.5,12],tint(a.hair,.055),12,6);
        ellipsoid([10,-17,5],[8,8,11.5],a.hair,12,6);
        for(const side of [-1,1]) ellipsoid([side*15,-11,8],[4,9,6],tint(a.hair,.025),10,6);
      }
    }
    if(a.beard) ellipsoid([0,13,7],[12.5,9,8],a.hair,14,6);
    if(a.glasses) {
      for(const side of [-1,1]) {
        // Open lenses keep eyes visible.
        for(let i=0;i<12;i++) {
          const t=i/12*Math.PI*2;
          ellipsoid([side*7+Math.cos(t)*5,-2+Math.sin(t)*3.6,17],[1.2,1,.8],[28,32,39],6,4);
        }
      }
      ellipsoid([0,-2.7,17],[2.8,.65,.8],[28,32,39],8,4);
    }
  });
  const model={key,parts,version:2};
  if(cache.size>=8) cache.delete(cache.keys().next().value);
  cache.set(key,model); return model;
}
const vector=(x,y,z)=>({x,y,z});
const cross=(a,b)=>vector(a.y*b.z-a.z*b.y,a.z*b.x-a.x*b.z,a.x*b.y-a.y*b.x);
const unit=a=>{const n=Math.hypot(a.x,a.y,a.z)||1;return vector(a.x/n,a.y/n,a.z/n);};
export function poseFighter(model, world, {yaw=0,headless=false,hasPart=()=>true}={}) {
  const instances=[], front=vector(Math.sin(yaw),0,Math.cos(yaw));
  const frame=(bone)=>{
    const axis=vector(bone.x2-bone.x1,bone.y2-bone.y1,bone.z2-bone.z1),down=unit(axis);
    // At a punch pointing straight at the camera, keep the frame nonsingular.
    let right=cross(down,front); if(Math.hypot(right.x,right.y,right.z)<.05) right=cross(down,vector(0,1,0));
    right=unit(right); const forward=unit(cross(right,down));
    return [right,axis,forward];
  };
  const add=(name,origin,axes)=>instances.push({name,faces:model.parts[name],origin,axes});
  const endpoint=(b,n)=>vector(b['x'+n],b['y'+n],b['z'+n]);
  const torso=world.segments.find(b=>b.role==='torso');
  const basis=torso?frame(torso):[vector(Math.cos(yaw),0,-Math.sin(yaw)),vector(0,1,0),front];
  const rigid=[basis[0],unit(basis[1]),basis[2]];
  for(const bone of world.segments) {
    if(bone.hidden||bone.hitboxOnly||!hasPart(bone.part))continue;
    const name=bone.role.replace(/^(left|right|lead|rear|attack|rest|grab|item)-/,'');
    if(!model.parts[name])continue;
    add(name,endpoint(bone,1),frame(bone));
    if(name==='torso')add('pelvis',endpoint(bone,2),rigid);
    if(name==='forearm')add('hand',endpoint(bone,2),[...frame(bone).map(unit)]);
    if(name==='shin')add('shoe',endpoint(bone,2),rigid);
  }
  if(!headless) {
    const scale=(world.head.radius||22)/22;
    add('head',world.head,rigid.map(v=>vector(v.x*scale,v.y*scale,v.z*scale)));
  }
  return instances;
}
export function previewPose() {
  const segments=[];
  const bone=(role,part,a,b)=>segments.push({role,part,x1:a[0],y1:a[1],z1:a[2],x2:b[0],y2:b[1],z2:b[2]});
  bone('torso','torso',[0,-137,0],[0,-90,0]);
  bone('neck','torso',[0,-151,0],[0,-135,0]);
  for(const side of [-1,1]){
    const name=side<0?'left':'right';
    bone(name+'-upper-arm',name+'-arm',[side*19,-133,0],[side*28,-101,0]);
    bone(name+'-forearm',name+'-arm',[side*28,-101,0],[side*30,-72,3]);
    bone(name+'-thigh',name+'-leg',[side*10,-90,0],[side*12,-47,1]);
    bone(name+'-shin',name+'-leg',[side*12,-47,1],[side*13,-5,0]);
  }
  return {head:{x:0,y:-174,z:0,radius:22},segments};
}
export function fighterMesh(appearance) {
  return poseFighter(fighterParts(appearance),previewPose()).flatMap(({faces,origin:o,axes:a})=>faces.map(face=>({color:face.color,points:face.points.map(([x,y,z])=>[
    (o.x+a[0].x*x+a[1].x*y+a[2].x*z)/62,
    -(o.y+a[0].y*x+a[1].y*y+a[2].y*z+99)/62,
    (o.z+a[0].z*x+a[1].z*y+a[2].z*z)/62])})));
}
// The module loads through the wizard before the game boots. Native builds
// without the account UI retain their existing renderer.
globalThis.__oskiewarFighterModel={build:fighterParts,pose:poseFighter,version:2};
// Use a depth buffer for intersecting sleeves, hair and clothing. Sorting
// whole faces alone produces spikes at the waist and lets back hair leak through.
let previewContext;
function previewSurface(mesh) {
  const canvas=previewContext?.canvas || document.createElement('canvas');canvas.width=640;canvas.height=760;
  const gl=canvas.getContext('webgl',{alpha:true,antialias:true,preserveDrawingBuffer:true});
  if(!gl)return null;
  if(previewContext){gl.deleteBuffer(previewContext.buffer);gl.deleteProgram(previewContext.program);}
  const shader=(type,source)=>{const s=gl.createShader(type);gl.shaderSource(s,source);gl.compileShader(s);return s;};
  const program=gl.createProgram();
  gl.attachShader(program,shader(gl.VERTEX_SHADER,`
    attribute vec3 position; attribute vec3 color; attribute vec3 normal;
    uniform float angle; varying vec3 ink;
    void main(){
      float c=cos(angle),s=sin(angle);
      mat3 turn=mat3(c,0.,-s,0.,1.,0.,s,0.,c);
      vec3 p=turn*position,n=normalize(turn*normal);
      float diffuse=max(0.,dot(n,normalize(vec3(-.4,.65,.65))));
      ink=min(vec3(1.),color*(.75+.25*diffuse)+pow(diffuse,18.)*.05);
      float w=7.-p.z;
      gl_Position=vec4(p.x*4.2875,p.y*3.610526-.010526*w,1.1*w-2.1,w);
    }`));
  gl.attachShader(program,shader(gl.FRAGMENT_SHADER,`precision mediump float;varying vec3 ink;void main(){gl_FragColor=vec4(ink,1.);}`));
  gl.linkProgram(program);
  for(const s of gl.getAttachedShaders(program))gl.deleteShader(s);
  if(!gl.getProgramParameter(program,gl.LINK_STATUS))return null;
  const vertices=[];
  for(const f of mesh)for(const ids of [[0,1,2],[0,2,3]]) {
    const [a,b,c]=ids.map(i=>f.points[i]);
    const u=b.map((v,i)=>v-a[i]),v=c.map((n,i)=>n-a[i]);
    const n=[u[2]*v[1]-u[1]*v[2],u[0]*v[2]-u[2]*v[0],u[1]*v[0]-u[0]*v[1]],length=Math.hypot(...n);
    if(length<1e-8)continue;
    for(const point of [a,b,c])vertices.push(...point,...f.color.map(c=>c/255),...n.map(v=>v/length));
  }
  gl.useProgram(program);const buffer=gl.createBuffer();gl.bindBuffer(gl.ARRAY_BUFFER,buffer);gl.bufferData(gl.ARRAY_BUFFER,new Float32Array(vertices),gl.STATIC_DRAW);
  for(const [i,name] of ['position','color','normal'].entries()) {const at=gl.getAttribLocation(program,name);gl.enableVertexAttribArray(at);gl.vertexAttribPointer(at,3,gl.FLOAT,false,36,i*12);}
  const angleUniform=gl.getUniformLocation(program,'angle');gl.enable(gl.DEPTH_TEST);
  previewContext={canvas,buffer,program};
  return angle=>{gl.clearColor(0,0,0,0);gl.clear(gl.COLOR_BUFFER_BIT|gl.DEPTH_BUFFER_BIT);gl.uniform1f(angleUniform,angle);gl.drawArrays(gl.TRIANGLES,0,vertices.length/9);return canvas;};
}
export function mountFighterPreview(host, fighter) {
  const appearance=validateFighter(fighter), mesh=fighterMesh(appearance);
  const canvas=document.createElement('canvas'); canvas.width=640;canvas.height=760;
  canvas.style.cssText='position:static;inset:auto;display:block;width:100%;height:auto;max-height:38vh;object-fit:contain;border-radius:22px;background:#e8eef8;box-shadow:inset 0 1px 0 #fff,0 1px 2px #15335b15';
  canvas.setAttribute('aria-label','Generated fighter preview. Use the rotation slider to inspect all sides.');
  const slider=document.createElement('input'); slider.type='range';slider.min='-180';slider.max='180';slider.value='-18';
  slider.setAttribute('aria-label','Rotate fighter');slider.style.cssText='width:100%;margin:14px 0 0;accent-color:#0866ff';
  host.replaceChildren(canvas,slider);const ctx=canvas.getContext('2d'),surface=previewSurface(mesh);
  function paint(){
    const angle=Number(slider.value)*Math.PI/180,c=Math.cos(angle),s=Math.sin(angle);
    const bg=ctx.createLinearGradient(0,0,640,760);bg.addColorStop(0,'#f9fbff');bg.addColorStop(.55,'#e8effb');bg.addColorStop(1,'#d7e3f5');ctx.fillStyle=bg;ctx.fillRect(0,0,640,760);
    const halo=ctx.createRadialGradient(270,270,10,320,340,330);halo.addColorStop(0,'#ffffffda');halo.addColorStop(1,'#ffffff00');ctx.fillStyle=halo;ctx.fillRect(0,0,640,760);
    ctx.fillStyle='#ffffffa8';ctx.beginPath();ctx.ellipse(320,701,160,27,0,0,Math.PI*2);ctx.fill();
    const shadow=ctx.createRadialGradient(320,700,3,320,700,105);shadow.addColorStop(0,'#3d527e50');shadow.addColorStop(1,'#3d527e00');ctx.save();ctx.translate(0,550);ctx.scale(1,.215);ctx.fillStyle=shadow;ctx.fillRect(180,570,280,240);ctx.restore();
    if(surface){ctx.drawImage(surface(angle),0,0);return;}
    const rotate=([x,y,z])=>[x*c+z*s,y,z*c-x*s];
    const projected=mesh.map(f=>({color:f.color,points:f.points.map(rotate)}));
    projected.sort((a,b)=>a.points.reduce((n,p)=>n+p[2],0)-b.points.reduce((n,p)=>n+p[2],0));
    for(const face of projected){
      const [a,b,d]=face.points;const u=b.map((v,i)=>v-a[i]),v=d.map((n,i)=>n-a[i]);
      const normal=[u[1]*v[2]-u[2]*v[1],u[2]*v[0]-u[0]*v[2],u[0]*v[1]-u[1]*v[0]],length=Math.hypot(...normal)||1;
      const diffuse=Math.max(0,-(normal[0]*-.4+normal[1]*.65+normal[2]*.65)/length);
      const light=.75+.25*diffuse,shine=Math.pow(diffuse,18)*13;
      ctx.fillStyle=`rgb(${face.color.map(v=>Math.min(255,Math.round(v*light+shine))).join(',')})`;
      ctx.beginPath();face.points.forEach(([x,y,z],i)=>{const scale=196*7/(7-z);ctx[i?'lineTo':'moveTo'](320+x*scale,384-y*scale);});ctx.closePath();ctx.fill();
    }
  }
  slider.addEventListener('input',paint);paint();return appearance;
}
