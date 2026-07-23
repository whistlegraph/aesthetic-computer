#!/usr/bin/env node
// Convert a bounded triangle GLB into a self-contained Native BIOS material piece.
// Textures are sampled offline per face; runtime shading supplies light, specular,
// and procedural skybox reflection without exposing file or network APIs to JS.

import { readFileSync, writeFileSync } from "node:fs";
import sharp from "sharp";

const [source, destination, skinBakePath] = process.argv.slice(2);
if (!source || !destination) {
  throw new Error("usage: glb-to-native-material.mjs <simplified.glb> <piece.js> [skin-bake.json]");
}
const skinBake = skinBakePath ? JSON.parse(readFileSync(skinBakePath, "utf8")) : null;
if (skinBake && (skinBake.triangles > 2400 || skinBake.frames?.length < 2))
  throw new Error("skin bake must contain 2+ frames and at most 2400 triangles");

const bytes = readFileSync(source);
if (bytes.toString("ascii", 0, 4) !== "glTF" || bytes.readUInt32LE(4) !== 2) {
  throw new Error("expected a glTF 2.0 binary file");
}

let json;
let binary;
for (let offset = 12; offset + 8 <= bytes.length;) {
  const length = bytes.readUInt32LE(offset);
  const type = bytes.readUInt32LE(offset + 4);
  const chunk = bytes.subarray(offset + 8, offset + 8 + length);
  if (type === 0x4e4f534a) json = JSON.parse(chunk.toString("utf8").replace(/\0+$/, ""));
  if (type === 0x004e4942) binary = chunk;
  offset += 8 + length;
}
if (!json || !binary || json.extensionsRequired?.length) {
  throw new Error("GLB must contain uncompressed JSON and BIN chunks");
}

const component = {
  5121: { bytes: 1, read: "readUInt8" },
  5123: { bytes: 2, read: "readUInt16LE" },
  5125: { bytes: 4, read: "readUInt32LE" },
  5126: { bytes: 4, read: "readFloatLE" },
};
const widths = { SCALAR: 1, VEC2: 2, VEC3: 3, VEC4: 4 };

function accessor(index) {
  const spec = json.accessors[index];
  const view = json.bufferViews[spec?.bufferView];
  const kind = component[spec?.componentType];
  const width = widths[spec?.type];
  if (!spec || !view || !kind || !width || spec.sparse) {
    throw new Error(`unsupported accessor ${index}`);
  }
  const stride = view.byteStride || kind.bytes * width;
  const start = (view.byteOffset || 0) + (spec.byteOffset || 0);
  return Array.from({ length: spec.count }, (_, row) => {
    const value = Array.from({ length: width }, (_, column) =>
      binary[kind.read](start + row * stride + column * kind.bytes));
    return width === 1 ? value[0] : value;
  });
}

async function decodedImage(textureIndex) {
  const texture = json.textures?.[textureIndex];
  const image = json.images?.[texture?.source];
  const view = json.bufferViews?.[image?.bufferView];
  if (!view) return null;
  const encoded = binary.subarray(view.byteOffset || 0, (view.byteOffset || 0) + view.byteLength);
  const { data, info } = await sharp(encoded).removeAlpha().raw()
    .toBuffer({ resolveWithObject: true });
  return { data, width: info.width, height: info.height, channels: info.channels };
}

function texel(image, uv) {
  if (!image) return [0, 0, 0];
  const wrap = (value) => ((value % 1) + 1) % 1;
  const x = Math.min(image.width - 1, Math.floor(wrap(uv[0]) * image.width));
  const y = Math.min(image.height - 1, Math.floor((1 - wrap(uv[1])) * image.height));
  const offset = (y * image.width + x) * image.channels;
  return [image.data[offset], image.data[offset + 1], image.data[offset + 2]];
}

const primitive = json.meshes?.[0]?.primitives?.find((item) => (item.mode ?? 4) === 4);
if (!primitive || primitive.attributes?.POSITION === undefined ||
    primitive.attributes?.TEXCOORD_0 === undefined || primitive.indices === undefined) {
  throw new Error("GLB needs one indexed triangle primitive with POSITION and TEXCOORD_0");
}
const positions = accessor(primitive.attributes.POSITION);
const uvs = accessor(primitive.attributes.TEXCOORD_0);
const indices = accessor(primitive.indices);
if (indices.length % 3 || indices.length / 3 > 4096) {
  throw new Error("material piece requires 1-4096 triangles");
}

const material = json.materials?.[primitive.material || 0] || {};
const pbr = material.pbrMetallicRoughness || {};
const [baseImage, metallicImage, emissiveImage] = await Promise.all([
  decodedImage(pbr.baseColorTexture?.index),
  decodedImage(pbr.metallicRoughnessTexture?.index),
  decodedImage(material.emissiveTexture?.index),
]);

const min = [0, 1, 2].map((axis) => Math.min(...positions.map((point) => point[axis])));
const max = [0, 1, 2].map((axis) => Math.max(...positions.map((point) => point[axis])));
const center = min.map((value, axis) => (value + max[axis]) / 2);
const normalization = 2 / Math.max(...max.map((value, axis) => value - min[axis]), 1e-9);
const round = (value, places = 4) => {
  const factor = 10 ** places;
  return Math.round(value * factor) / factor;
};
const vertices = positions.map((point) => point.map((value, axis) =>
  round((value - center[axis]) * normalization)));

const faceRows = [];
for (let offset = 0; offset < indices.length; offset += 3) {
  const ids = [indices[offset], indices[offset + 1], indices[offset + 2]];
  const average = (image) => {
    const samples = ids.map((id) => texel(image, uvs[id]));
    return [0, 1, 2].map((channel) => Math.round(
      samples.reduce((sum, sample) => sum + sample[channel], 0) / samples.length));
  };
  const base = average(baseImage);
  const metalRough = average(metallicImage);
  const emissive = average(emissiveImage);
  // glTF packs roughness in G and metallic in B. Jeffrey asked for shine, so
  // retain texture variation but cap roughness into a glossy range.
  const roughness = round(Math.min(0.28, Math.max(0.08, metalRough[1] / 255 * 0.35)), 3);
  const metallic = round(Math.max(0.72, metalRough[2] / 255), 3);
  faceRows.push([...ids, ...base, metallic, roughness, ...emissive]);
}

let random = 0x50414c53;
const next = () => {
  random = (Math.imul(random, 1664525) + 1013904223) >>> 0;
  return random / 0x100000000;
};
const stars = Array.from({ length: 72 }, () => [
  Math.round(next() * 1920), Math.round(120 + next() * 650),
  next() > 0.82 ? 3 : 2, Math.round(125 + next() * 120),
]);

let code = `// Pals Material Skybox, 26.07.22
// Connected Pals GLB plus the 24-joint ThespianJas Meshy idle skin.
const vertices=${JSON.stringify(vertices)};
const faces=${JSON.stringify(faceRows)};
const stars=${JSON.stringify(stars)};
const jeffrey=${skinBake ? JSON.stringify(skinBake) : "null"};
let rotationX=0,rotationY=0,frameCount=0,perfStarted=0,fpsLabel="-- FPS",frameMsLabel="-- MS";
let audioOn=true,lastSoundAt=-1,scenePulse=0,lastMidiEvent=0,lastButton="--";
let playerX=1315,playerVelocity=0,playerFacing=1,cameraX=0,runPhase=0;
let attractDirection=-1,lastSimTime=0,lastPlayerInput=-1e9,attractMode=true;
const submitTriangleBatch=triangles3d,triangleBatch=new Float32Array(4096*12);
const submitTextureBatch=texturedTriangles3d,textureBatch=new Float32Array(2048*18);
const submitSpriteBatch=sprites3d,spriteBatch=new Float32Array(512*8);
let triangleBatchIndex=0,textureBatchIndex=0,spriteBatchIndex=0;
function emitTriangle(x1,y1,z1,x2,y2,z2,x3,y3,z3,r,g,b){
  if(triangleBatchIndex>=triangleBatch.length)return;
  const i=triangleBatchIndex;
  triangleBatch[i]=x1;triangleBatch[i+1]=y1;triangleBatch[i+2]=z1;
  triangleBatch[i+3]=x2;triangleBatch[i+4]=y2;triangleBatch[i+5]=z2;
  triangleBatch[i+6]=x3;triangleBatch[i+7]=y3;triangleBatch[i+8]=z3;
  triangleBatch[i+9]=r;triangleBatch[i+10]=g;triangleBatch[i+11]=b;
  triangleBatchIndex=i+12;
}
function emitTexturedTriangle(x1,y1,z1,u1,v1,x2,y2,z2,u2,v2,x3,y3,z3,u3,v3,r,g,b){
  if(textureBatchIndex>=textureBatch.length)return;
  const i=textureBatchIndex;
  textureBatch[i]=x1;textureBatch[i+1]=y1;textureBatch[i+2]=z1;textureBatch[i+3]=u1;textureBatch[i+4]=v1;
  textureBatch[i+5]=x2;textureBatch[i+6]=y2;textureBatch[i+7]=z2;textureBatch[i+8]=u2;textureBatch[i+9]=v2;
  textureBatch[i+10]=x3;textureBatch[i+11]=y3;textureBatch[i+12]=z3;textureBatch[i+13]=u3;textureBatch[i+14]=v3;
  textureBatch[i+15]=r;textureBatch[i+16]=g;textureBatch[i+17]=b;
  textureBatchIndex=i+18;
}
function emitSprite(x,y,z,size,r,g,b,frame){
  if(spriteBatchIndex>=spriteBatch.length)return;
  const i=spriteBatchIndex;
  spriteBatch[i]=x;spriteBatch[i+1]=y;spriteBatch[i+2]=z;spriteBatch[i+3]=size;
  spriteBatch[i+4]=r;spriteBatch[i+5]=g;spriteBatch[i+6]=b;spriteBatch[i+7]=frame;
  spriteBatchIndex=i+8;
}
function boot(){
  oscillator(55,.06);
  telemetry("PALS_MATERIAL_BOOT","pals=${indices.length / 3} jeffrey=${skinBake ? skinBake.triangles : 0} joints=${skinBake ? skinBake.joints : 0} material=uv-texture lights=3 shadows=1 stencil=d24s8 fog=depth sprites=point post=fullscreen attract=2.5d audio=harmonic-xaudio2");
}
function sim(){
  const pad=gamepad(),run=runtime(),t=run.monotonicUs/1000000;
  const dt=lastSimTime?clamp(t-lastSimTime,0,.05):1/60;lastSimTime=t;
  let direction=0;
  if(pad.down.includes("ArrowLeft")){direction=-1;lastPlayerInput=t;}
  if(pad.down.includes("ArrowRight")){direction=1;lastPlayerInput=t;}
  if(pad.down.includes("ArrowUp"))rotationX-=0.035;
  if(pad.down.includes("ArrowDown"))rotationX+=0.035;
  attractMode=t-lastPlayerInput>6;
  if(attractMode){
    if(playerX<820)attractDirection=1;
    if(playerX>1650)attractDirection=-1;
    direction=attractDirection;
  }
  playerVelocity+=direction*980*dt;
  playerVelocity*=Math.pow(direction ? .48 : .025,dt);
  playerVelocity=clamp(playerVelocity,-285,285);
  playerX=clamp(playerX+playerVelocity*dt,760,1710);
  if(Math.abs(playerVelocity)>8)playerFacing=playerVelocity<0?-1:1;
  runPhase+=Math.abs(playerVelocity)*dt*.038;
  cameraX+=((playerX-1315)*.3-cameraX)*Math.min(1,dt*2.6);
  scenePulse*=.92;
  if(run.midiEvents>lastMidiEvent){lastMidiEvent=run.midiEvents;scenePulse=1;}
  if(audioOn&&t-lastSoundAt>.05){
    const roots=[55,65.406,73.416,82.407];
    const root=roots[Math.floor(t/4)%roots.length];
    oscillator(root*(1+.012*Math.sin(t*.37)+.004*Math.sin(t*1.71)),.06+scenePulse*.04);
    lastSoundAt=t;
  }
}
function clamp(value,low=0,high=255){return Math.max(low,Math.min(high,value))}
function rotate(point,ax,ay){
  const x=point[0]*Math.cos(ay)-point[2]*Math.sin(ay);
  const z=point[0]*Math.sin(ay)+point[2]*Math.cos(ay);
  const y=point[1]*Math.cos(ax)-z*Math.sin(ax);
  return [x,y,point[1]*Math.sin(ax)+z*Math.cos(ax)];
}
function skybox(t){
  wipe(3,5,18);
  box(0,700,1920,4,64,38,86);
  box(0,704,1920,7,32,22,58);
  for(let index=0;index<stars.length;index++){
    const star=stars[index];
    const pulse=.62+.38*Math.sin(t*.7+star[0]*.017+star[1]*.009);
    const glow=Math.floor(star[3]*pulse);
    const drift=index<22?(t*(2+index%4))%1920:0;
    const x=(star[0]+drift)%1920,y=star[1]+(index<22?Math.sin(t*.45+index)*5:0);
    box(x,y,star[2],star[2],glow*.72,glow*.82,glow);
  }
  for(let comet=0;comet<3;comet++){
    const phase=(t*(.052+comet*.009)+comet*.34)%1;
    const x=phase*2240-160,y=150+comet*155+phase*115;
    line(x,y,x-112,y-55,2,142,122,208);
    line(x,y,x-62,y-31,2,210,198,255);
    box(x-2,y-2,5,5,240,236,255);
  }
  for(let lane=-5;lane<=5;lane++)line(960,710,960+lane*245,1080,1,21,16,42);
  for(let row=0;row<6;row++){
    const y=725+Math.pow(row/5,1.7)*345;
    line(0,y,1920,y,1,18+row*2,14+row,38+row*3);
  }
}
function orbitalScene(t,run){
  const pulse=1+scenePulse*.42;
  // Three faceted moons orbit behind the Pals mesh. Their rings and facets are
  // emitted through the same GPU triangle list as the GLB.
  const moons=[[350,315,66,.19],[1560,655,48,-.27],[1510,250,32,.34]];
  for(let moon=0;moon<moons.length;moon++){
    const base=moons[moon],phase=t*(base[3]+moon*.035)+moon*2.1;
    const cx=base[0]+Math.cos(phase)*55,cy=base[1]+Math.sin(phase*.83)*34;
    const radius=base[2]*pulse,z=.9+moon*.08,segments=16;
    for(let i=0;i<segments;i++){
      const a=phase+i*Math.PI*2/segments,b=phase+(i+1)*Math.PI*2/segments;
      const shade=.55+.45*Math.sin(a*2.7+t*.41);
      triangle3d(cx,cy,z,cx+Math.cos(a)*radius,cy+Math.sin(a)*radius,z,cx+Math.cos(b)*radius,cy+Math.sin(b)*radius,z,
        34+shade*72,28+shade*58,92+shade*112);
    }
    for(let i=0;i<24;i++){
      const a=phase*.55+i*Math.PI*2/24,b=phase*.55+(i+1)*Math.PI*2/24;
      const rx=radius*1.82,ry=radius*.38,thick=2.6+moon;
      triangle3d(cx+Math.cos(a)*rx,cy+Math.sin(a)*ry,z-.035,cx+Math.cos(b)*rx,cy+Math.sin(b)*ry,z-.035,
        cx+Math.cos(b)*(rx-thick),cy+Math.sin(b)*(ry-thick*.3),z-.035,88,68,156);
      triangle3d(cx+Math.cos(a)*rx,cy+Math.sin(a)*ry,z-.035,cx+Math.cos(b)*(rx-thick),cy+Math.sin(b)*(ry-thick*.3),z-.035,
        cx+Math.cos(a)*(rx-thick),cy+Math.sin(a)*(ry-thick*.3),z-.035,138,106,208);
    }
  }
  // A living dust field: tiny depth-layered diamonds that drift in parallax.
  for(let i=0;i<84;i++){
    const phase=t*(.05+(i%7)*.008)+i*1.731;
    const x=(i*227+phase*91)%1840+40,y=125+((i*97)%720)+Math.sin(phase)*18;
    const size=(1.5+(i%4))*(i%17===0?pulse:1),z=.72+(i%5)*.045;
    const glow=70+(i%6)*24;
    triangle3d(x,y-size,z,x+size,y,z,x,y+size,z,glow*.64,glow*.54,glow);
    triangle3d(x,y-size,z,x,y+size,z,x-size,y,z,glow*.38,glow*.46,glow*.82);
  }
  // Clock-synchronized energy ribbon. Every console using /api/clock sees the
  // same phase, while local monotonic time keeps motion smooth between syncs.
  const clockT=run.unixMs/1000;
  for(let lane=0;lane<3;lane++)for(let i=0;i<28;i++){
    const x0=i*72-40,x1=x0+76;
    const y0=760+lane*54+Math.sin(clockT*.72+i*.46+lane*2)*18;
    const y1=760+lane*54+Math.sin(clockT*.72+(i+1)*.46+lane*2)*18;
    const width=3+scenePulse*5,z=.82+lane*.025;
    triangle3d(x0,y0-width,z,x1,y1-width,z,x1,y1+width,z,24+lane*22,72+lane*18,104+lane*38);
    triangle3d(x0,y0-width,z,x1,y1+width,z,x0,y0+width,z,42+lane*28,98+lane*18,142+lane*36);
  }
}
function animatedJeffrey(t){
  if(!jeffrey)return;
  const moving=Math.abs(playerVelocity)>18;
  const clipTime=moving?runPhase*.32:t;
  const position=(clipTime%jeffrey.duration)/jeffrey.duration*jeffrey.frameCount;
  const frame0=Math.floor(position)%jeffrey.frameCount,frame1=(frame0+1)%jeffrey.frameCount,mix=position-Math.floor(position);
  const source0=jeffrey.frames[frame0],source1=jeffrey.frames[frame1];
  const lean=clamp(playerVelocity/285,-1,1)*-.12;
  const angle=-.22+Math.sin(t*.23)*.08+lean,cos=Math.cos(angle),sin=Math.sin(angle);
  const world=source0.map((point,index)=>{
    const next=source1[index],x=(point[0]+(next[0]-point[0])*mix)*playerFacing;
    const y=point[1]+(next[1]-point[1])*mix,z=point[2]+(next[2]-point[2])*mix;
    return [x*cos-z*sin,y,x*sin+z*cos];
  });
  const cx=playerX-cameraX,ground=722-(moving?Math.abs(Math.sin(runPhase*Math.PI))*10:0),scale=260;
  const points=world.map((point)=>[cx+point[0]*scale,ground-point[1]*scale,point[2]+.05]);
  // Project a bounded subset onto the floor for a moving cast-shadow pass.
  for(let index=0;index<jeffrey.faces.length;index+=4){
    const face=jeffrey.faces[index],a=world[face[0]],b=world[face[1]],c=world[face[2]];
    const shadow=(point)=>[cx+point[0]*scale+92,735+point[2]*82,.98];
    const p0=shadow(a),p1=shadow(b),p2=shadow(c);
    triangle3d(p0[0],p0[1],p0[2],p1[0],p1[1],p1[2],p2[0],p2[1],p2[2],9,6,20);
  }
  // Stencil-style expanded silhouette. Revision 18 keeps this as a geometry
  // pass; the native D24S8 post pipeline upgrades it to hardware stencil.
  for(const face of jeffrey.faces){
    const a=world[face[0]],b=world[face[1]],c=world[face[2]];
    const ux=b[0]-a[0],uy=b[1]-a[1],uz=b[2]-a[2],vx=c[0]-a[0],vy=c[1]-a[1],vz=c[2]-a[2];
    const nz=ux*vy-uy*vx;if(nz*playerFacing>=0)continue;
    const expand=(point)=>[cx+point[0]*scale*1.035,ground-point[1]*scale*1.035,point[2]+.11];
    const p0=expand(a),p1=expand(b),p2=expand(c);
    triangle3d(p0[0],p0[1],p0[2],p1[0],p1[1],p1[2],p2[0],p2[1],p2[2],38,8,58);
  }
  const lights=[
    [Math.sin(t*.71)*.72,-.48,.7,1,.48,.82],
    [Math.cos(t*.43)*-.66,.18,.74,.42,.8,1],
    [Math.sin(t*.29+.7)*.38,.78,.5,1,.32,.42],
  ];
  for(const light of lights){const length=Math.hypot(light[0],light[1],light[2]);light[0]/=length;light[1]/=length;light[2]/=length;}
  for(const face of jeffrey.faces){
    const a=world[face[0]],b=world[face[1]],c=world[face[2]];
    const ux=b[0]-a[0],uy=b[1]-a[1],uz=b[2]-a[2],vx=c[0]-a[0],vy=c[1]-a[1],vz=c[2]-a[2];
    let nx=uy*vz-uz*vy,ny=uz*vx-ux*vz,nz=ux*vy-uy*vx;
    const length=Math.hypot(nx,ny,nz);if(length<.00001||nz*playerFacing>=0)continue;
    nx=-nx*playerFacing/length;ny=-ny*playerFacing/length;nz=-nz*playerFacing/length;
    let red=.28,green=.28,blue=.32;
    for(const light of lights){
      const amount=Math.max(0,nx*light[0]+ny*light[1]+nz*light[2]);
      red+=amount*light[3]*.52;green+=amount*light[4]*.52;blue+=amount*light[5]*.52;
    }
    const spec=Math.pow(Math.max(0,nx*.2+ny*-.35+nz*.92),34)*.74;
    const fog=clamp((a[2]+b[2]+c[2])/3*.24+.1,0,.42);
    red=red*(1-fog)+.16*fog+spec;green=green*(1-fog)+.15*fog+spec*.86;blue=blue*(1-fog)+.3*fog+spec;
    const p0=points[face[0]],p1=points[face[1]],p2=points[face[2]];
    const uv0=jeffrey.uvs[face[0]],uv1=jeffrey.uvs[face[1]],uv2=jeffrey.uvs[face[2]];
    emitTexturedTriangle(p0[0],p0[1],p0[2],uv0[0],uv0[1],p1[0],p1[1],p1[2],uv1[0],uv1[1],p2[0],p2[1],p2[2],uv2[0],uv2[1],clamp(red*255),clamp(green*255),clamp(blue*255));
  }
  // Real 24-joint idle skeleton, interpolated from the same baked clip. Lines
  // are submitted before GPU skin triangles, so the body naturally occludes them.
  const joints0=jeffrey.jointFrames[frame0],joints1=jeffrey.jointFrames[frame1];
  const jointPoints=joints0.map((point,index)=>{
    const next=joints1[index],x=point[0]+(next[0]-point[0])*mix;
    const y=point[1]+(next[1]-point[1])*mix,z=point[2]+(next[2]-point[2])*mix;
    return [cx+((x*playerFacing)*cos-z*sin)*scale,ground-y*scale];
  });
  for(const bone of jeffrey.bones){const a=jointPoints[bone[0]],b=jointPoints[bone[1]];line(a[0],a[1],b[0],b[1],2,102,242,214);}
  // Two point-sampled billboard emitters tied to animated ankle/hand space.
  for(let emitter=0;emitter<2;emitter++)for(let i=0;i<34;i++){
    const life=(t*(.38+emitter*.08)+i*.071)%1,phase=i*2.399+emitter*1.7;
    const x=cx+(emitter?88:-84)+Math.sin(phase+t*.7)*life*85;
    const y=ground-18-life*310+Math.cos(phase*1.3)*22;
    const size=2+((i+emitter)%4),z=-.18+life*.5;
    const r=emitter?255:72,g=emitter?96:228,b=emitter?156:255;
    emitSprite(x,y,z,size*2,r,g,b,(i+emitter)&1);
  }
  if(moving)for(let i=0;i<18;i++){
    const life=(runPhase*.7+i*.083)%1;
    emitSprite(cx-playerFacing*(28+life*92),ground-4-life*34,.7,5+(i%3)*3,174,126,224,i&1);
  }
}
function paint(){
  const run=runtime(),t=run.monotonicUs/1000000;
  triangleBatchIndex=0;textureBatchIndex=0;spriteBatchIndex=0;
  if(!perfStarted)perfStarted=t;
  frameCount++;
  if(t-perfStarted>=2){
    const measuredFps=frameCount/(t-perfStarted);
    fpsLabel=measuredFps.toFixed(1)+" FPS";
    frameMsLabel=(1000/measuredFps).toFixed(1)+" MS";
    telemetry("PALS_MATERIAL_PERF","fps="+fpsLabel.slice(0,-4));
    perfStarted=t;frameCount=0;
  }
  skybox(t);
  orbitalScene(t,run);
  animatedJeffrey(t);
  const ax=rotationX+Math.sin(t*.31)*.1;
  const ay=rotationY+t*.72;
  const world=vertices.map((point)=>rotate(point,ax,ay));
  const points=world.map((point)=>{
    const scale=930/(3.45+point[2]);
    return [960-cameraX*.38+point[0]*scale,530-point[1]*scale,point[2]];
  });
  const light=[Math.sin(t*.31)*.48,-.56,.72];
  const lightLength=Math.hypot(light[0],light[1],light[2]);
  light[0]/=lightLength;light[1]/=lightLength;light[2]/=lightLength;
  const half=[light[0],light[1],light[2]+1];
  const halfLength=Math.hypot(half[0],half[1],half[2]);
  half[0]/=halfLength;half[1]/=halfLength;half[2]/=halfLength;
  for(const face of faces){
    const a=world[face[0]],b=world[face[1]],c=world[face[2]];
    const ux=b[0]-a[0],uy=b[1]-a[1],uz=b[2]-a[2];
    const vx=c[0]-a[0],vy=c[1]-a[1],vz=c[2]-a[2];
    let nx=uy*vz-uz*vy,ny=uz*vx-ux*vz,nz=ux*vy-uy*vx;
    const length=Math.hypot(nx,ny,nz);if(length<.00001)continue;
    nx/=length;ny/=length;nz/=length;
    if(nz>=0)continue;
    nx=-nx;ny=-ny;nz=-nz;
    const diffuse=.38+.68*Math.max(0,nx*light[0]+ny*light[1]+nz*light[2]);
    const rough=face[7],metal=face[6];
    const specular=Math.pow(Math.max(0,nx*half[0]+ny*half[1]+nz*half[2]),12+56*(1-rough))*(.55+.75*metal);
    const fresnel=Math.pow(1-Math.max(0,nz),2.4)*(.24+.4*metal);
    const sky=(.2+.8*Math.max(0,.5-.5*ny))*(.26+.34*metal);
    const rim=Math.pow(Math.max(0,-nx*.62-ny*.18+nz*.28),2)*(.16+.24*metal);
    const p0=points[face[0]],p1=points[face[1]],p2=points[face[2]];
    const red=clamp(face[3]*diffuse+face[8]*.22+255*specular+78*fresnel+58*sky+42*rim);
    const green=clamp(face[4]*diffuse+face[9]*.22+240*specular+48*fresnel+52*sky+52*rim);
    const blue=clamp(face[5]*diffuse+face[10]*.22+255*specular+128*fresnel+132*sky+122*rim);
    triangle3d(p0[0],p0[1],p0[2],p1[0],p1[1],p1[2],p2[0],p2[1],p2[2],red,green,blue);
  }
  submitTriangleBatch(triangleBatch,triangleBatchIndex/12);
  submitTextureBatch(textureBatch,textureBatchIndex/18);
  submitSpriteBatch(spriteBatch,spriteBatchIndex/8);
  const audioLatency=run.audioLatencyMs>0?run.audioLatencyMs.toFixed(1)+" MS":"MEASURING";
  const audioSubmit=Number(run.audioSubmitUs||0),inputToAudio=Number(run.inputToAudioUs||0);
  const midi=run.midiInputs>0?run.midiInputs+" IN / NOTE "+run.midiNote+" / #"+(run.midiEvents||0):"NO MIDI INPUT";
  const clock=run.clockSynced?(run.clockOffsetMs>=0?"+":"")+run.clockOffsetMs+" MS / RTT "+run.clockRttMs+" MS":"SYNCING";
  box(36,34,520,132,8,10,29);
  line(36,92,556,92,2,92,66,126);
  systemWrite(fpsLabel+" / "+frameMsLabel,52,46,25,224,218,255);
  systemWrite("AUDIO "+audioLatency+" / SUBMIT "+audioSubmit.toFixed(1)+" US / GLITCH "+(run.audioGlitches||0)+"\\nMIDI "+midi+" / EVENT>VOICE "+inputToAudio.toFixed(1)+" US\\nNET CLOCK "+clock,52,104,17,176,194,228);
  box(1384,34,500,384,7,9,25);
  line(1384,98,1884,98,2,92,66,126);
  systemWrite("STACK / LIVE PIPELINE",1412,52,27,234,228,255);
  systemWrite("PALS + UV-TEXTURED JEFFREY\\n> 24-JOINT 2.5D ATTRACT RUN\\n> D-PAD LEFT / RIGHT CONTROL\\n> 3 LIGHTS + D24S8 STENCIL\\n> DEPTH FOG + POINT SPRITES\\n> FULLSCREEN POST SHADER\\n> 3 BATCHED GPU DRAWS\\n> XAUDIO2 + MIDI + NET CLOCK\\n> PRESENT(1) / 60 HZ VSYNC",1412,116,19,190,176,224);
}
function act(button){
  lastButton=button;scenePulse=1;
  lastPlayerInput=runtime().monotonicUs/1000000;
  if(button==="Menu"){
    audioOn=!audioOn;
    if(audioOn)oscillator(55,.06);else oscillatorStop();
  }else{
    const notes={A:220,B:277.18,X:329.63,Y:440,ArrowLeft:164.81,ArrowRight:196,ArrowUp:246.94,ArrowDown:146.83};
    if(notes[button])synth(notes[button],.14);
  }
  telemetry("PALS_MATERIAL_BUTTON",button+" audio="+audioOn);
}
function leave(){oscillatorStop();telemetry("PALS_MATERIAL_LEAVE","ok")}
`;

// The generated scene uses a reusable typed stream so every apparent
// triangle3d call remains readable in this template but becomes a cheap JS
// buffer append in the published piece.
code = code.replaceAll("triangle3d(", "emitTriangle(");

writeFileSync(destination, code);
console.log(JSON.stringify({ source, destination, vertices: vertices.length,
  triangles: faceRows.length, bytes: Buffer.byteLength(code), textures: {
    baseColor: !!baseImage, metallicRoughness: !!metallicImage, emissive: !!emissiveImage,
  }, skin: skinBake ? { source: skinBake.source, frames: skinBake.frameCount,
    vertices: skinBake.vertices, triangles: skinBake.triangles, joints: skinBake.joints } : null }));
