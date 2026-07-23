#!/usr/bin/env node
// Convert a bounded triangle GLB into a self-contained Native BIOS material piece.
// Textures are sampled offline per face; runtime shading supplies light, specular,
// and procedural skybox reflection without exposing file or network APIs to JS.

import { readFileSync, writeFileSync } from "node:fs";
import { MeshoptSimplifier } from "meshoptimizer";
import sharp from "sharp";

const [source, destination, skinBakePath, headBakePath] = process.argv.slice(2);
if (!source || !destination) {
  throw new Error("usage: glb-to-native-material.mjs <simplified.glb> <piece.js> [skin-bake.json]");
}
const skinBake = skinBakePath ? JSON.parse(readFileSync(skinBakePath, "utf8")) : null;
const headBake = headBakePath ? JSON.parse(readFileSync(headBakePath, "utf8")) : null;
if (skinBake && (skinBake.triangles > 2400 || skinBake.frames?.length < 2))
  throw new Error("skin bake must contain 2+ frames and at most 2400 triangles");
if (headBake && (headBake.triangles > 6500 || headBake.points?.length < 3))
  throw new Error("head bake must contain 3+ vertices and at most 6500 triangles");

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
let positions = accessor(primitive.attributes.POSITION);
let uvs = accessor(primitive.attributes.TEXCOORD_0);
let indices = accessor(primitive.indices);
if (indices.length % 3) {
  throw new Error("material piece requires 1-4096 triangles");
}
if (indices.length / 3 > 4096) {
  await MeshoptSimplifier.ready;
  const targetTriangles = Math.max(566, Math.floor(indices.length / 3 * .05));
  const [lod] = MeshoptSimplifier.simplifySloppy(Uint32Array.from(indices),
    Float32Array.from(positions.flat()), 3, null, targetTriangles * 3, 1);
  const used = [...new Set(lod)];
  const remap = new Map(used.map((id, index) => [id, index]));
  positions = used.map((id) => positions[id]);
  uvs = used.map((id) => uvs[id]);
  indices = Array.from(lod, (id) => remap.get(id));
}
if (indices.length / 3 > 4096)
  throw new Error("material simplification did not reach the 4096-triangle runtime limit");

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

let code = `// Jeffrey Portrait Skybox, 26.07.23
// Camera-facing ThespianJas portrait, colored terrain, Pals, and fireflies.
const vertices=${JSON.stringify(vertices)};
const faces=${JSON.stringify(faceRows)};
const stars=${JSON.stringify(stars)};
const jeffrey=${skinBake ? JSON.stringify(skinBake) : "null"};
const jeffreyHead=${headBake ? JSON.stringify(headBake) : "null"};
let rotationX=0,rotationY=0,frameCount=0,perfStarted=0,fpsLabel="-- FPS",frameMsLabel="-- MS";
let audioOn=true,lastSoundAt=-1,scenePulse=0,lastMidiEvent=0,lastButton="--";
let playerX=960,playerDepth=.7,playerVelocityX=0,playerVelocityDepth=0,playerFacing=1,cameraX=0,runPhase=0;
let attractDirection=-1,lastSimTime=0,lastPlayerInput=-1e9,attractMode=true;
const submitTriangleBatch=triangles3d,triangleBatch=new Float32Array(8192*12);
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
  telemetry("PALS_MATERIAL_BOOT","pals=${indices.length / 3} jeffrey=${skinBake ? skinBake.triangles : 0} joints=${skinBake ? skinBake.joints : 0} material=uv-texture portrait=front lights=3 terrain=color fireflies=world midi=note-gate-bend-cc audio=sine-xaudio2");
}
function sim(){
  const pad=gamepad(),run=runtime(),t=run.monotonicUs/1000000;
  const dt=lastSimTime?clamp(t-lastSimTime,0,.05):1/60;lastSimTime=t;
  let directionX=0,directionDepth=0;
  if(pad.down.includes("ArrowLeft")){directionX=-1;lastPlayerInput=t;}
  if(pad.down.includes("ArrowRight")){directionX=1;lastPlayerInput=t;}
  if(pad.down.includes("ArrowUp")){directionDepth=-1;lastPlayerInput=t;}
  if(pad.down.includes("ArrowDown")){directionDepth=1;lastPlayerInput=t;}
  attractMode=t-lastPlayerInput>6;
  if(attractMode){
    if(playerX<510)attractDirection=1;
    if(playerX>1410)attractDirection=-1;
    directionX=attractDirection;
    directionDepth=clamp(.57+Math.sin(t*.41)*.2-playerDepth,-1,1);
  }
  playerVelocityX+=directionX*1120*dt;
  playerVelocityDepth+=directionDepth*2.2*dt;
  playerVelocityX*=Math.pow(directionX ? .42 : .018,dt);
  playerVelocityDepth*=Math.pow(directionDepth ? .36 : .012,dt);
  playerVelocityX=clamp(playerVelocityX,-330,330);
  playerVelocityDepth=clamp(playerVelocityDepth,-.72,.72);
  playerX=clamp(playerX+playerVelocityX*dt,390,1530);
  playerDepth=clamp(playerDepth+playerVelocityDepth*dt,.05,1);
  if(Math.abs(playerVelocityX)>8)playerFacing=playerVelocityX<0?-1:1;
  const playerSpeed=Math.hypot(playerVelocityX,playerVelocityDepth*430);
  runPhase+=playerSpeed*dt*.0049;
  cameraX+=((playerX-960)*.16-cameraX)*Math.min(1,dt*2.6);
  scenePulse*=.92;
  if(run.midiEvents>lastMidiEvent){lastMidiEvent=run.midiEvents;scenePulse=1;}
  if(audioOn&&run.midiGate){
    const bend=((run.midiPitchBend||8192)-8192)/8192*2;
    const mod=run.midiControl===1?(run.midiControlValue||0)/127:0;
    const vibrato=Math.sin(t*(5+mod*3))*mod*.22;
    oscillator(440*Math.pow(2,((run.midiNote||69)-69+bend+vibrato)/12),
      Math.max(.02,(run.midiVelocity||1)/127*.34));
    lastSoundAt=t;
  }else if(audioOn&&t-lastSoundAt>.05){
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
  wipe(30,19,74);
  const skyBands=[[43,28,101],[56,36,128],[76,48,151],[104,64,162],[147,86,166],[205,124,162],[242,166,151]];
  for(let band=0;band<skyBands.length;band++){
    const color=skyBands[band],y=band*100;
    box(0,y,1920,104,color[0],color[1],color[2]);
  }
  box(0,690,1920,15,255,168,126);
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
  const groundBands=[[38,35,93],[34,52,105],[28,70,112],[24,88,113],[29,105,106],[45,119,96]];
  for(let row=0;row<groundBands.length;row++){
    const color=groundBands[row],y=705+row*63;
    box(0,y,1920,66,color[0],color[1],color[2]);
  }
  for(let lane=-5;lane<=5;lane++)line(960,710,960+lane*245,1080,1,87,156,151);
  for(let row=0;row<6;row++){
    const y=725+Math.pow(row/5,1.7)*345;
    line(0,y,1920,y,1,70+row*8,121+row*7,130+row*5);
  }
}
function fireflies(t){
  // Environmental emitters live in world space, independent of Jeffrey's
  // joints. Three depth layers create slow firefly parallax across the field.
  for(let i=0;i<108;i++){
    const layer=i%3,phase=i*2.399+t*(.19+layer*.055);
    const drift=(t*(18+layer*13)+i*173)%2100-90;
    const x=(drift+Math.sin(phase*.73)*76+2100)%2100-90;
    const y=170+(i*83)%710+Math.sin(phase)*42;
    const pulse=.35+.65*Math.pow(.5+.5*Math.sin(t*2.1+i*.91),3);
    const size=(2+layer*2+pulse*4),z=.58+layer*.16;
    const warm=i%5===0;
    emitSprite(x,y,z,size,warm?255:132,warm?194:255,warm?94:205,i&1);
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
    const radius=base[2]*pulse,z=.9+moon*.08,segments=12;
    for(let i=0;i<segments;i++){
      const a=phase+i*Math.PI*2/segments,b=phase+(i+1)*Math.PI*2/segments;
      const shade=.55+.45*Math.sin(a*2.7+t*.41);
      triangle3d(cx,cy,z,cx+Math.cos(a)*radius,cy+Math.sin(a)*radius,z,cx+Math.cos(b)*radius,cy+Math.sin(b)*radius,z,
        34+shade*72,28+shade*58,92+shade*112);
    }
    for(let i=0;i<16;i++){
      const a=phase*.55+i*Math.PI*2/16,b=phase*.55+(i+1)*Math.PI*2/16;
      const rx=radius*1.82,ry=radius*.38,thick=2.6+moon;
      triangle3d(cx+Math.cos(a)*rx,cy+Math.sin(a)*ry,z-.035,cx+Math.cos(b)*rx,cy+Math.sin(b)*ry,z-.035,
        cx+Math.cos(b)*(rx-thick),cy+Math.sin(b)*(ry-thick*.3),z-.035,88,68,156);
      triangle3d(cx+Math.cos(a)*rx,cy+Math.sin(a)*ry,z-.035,cx+Math.cos(b)*(rx-thick),cy+Math.sin(b)*(ry-thick*.3),z-.035,
        cx+Math.cos(a)*(rx-thick),cy+Math.sin(a)*(ry-thick*.3),z-.035,138,106,208);
    }
  }
  // A living dust field: tiny depth-layered diamonds that drift in parallax.
  for(let i=0;i<48;i++){
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
  for(let lane=0;lane<3;lane++)for(let i=0;i<20;i++){
    const x0=i*101-50,x1=x0+106;
    const y0=760+lane*54+Math.sin(clockT*.72+i*.46+lane*2)*18;
    const y1=760+lane*54+Math.sin(clockT*.72+(i+1)*.46+lane*2)*18;
    const width=3+scenePulse*5,z=.82+lane*.025;
    triangle3d(x0,y0-width,z,x1,y1-width,z,x1,y1+width,z,24+lane*22,72+lane*18,104+lane*38);
    triangle3d(x0,y0-width,z,x1,y1+width,z,x0,y0+width,z,42+lane*28,98+lane*18,142+lane*36);
  }
}
function animatedJeffrey(t){
  if(!jeffrey)return;
  const moving=Math.hypot(playerVelocityX,playerVelocityDepth*430)>18;
  const frames=moving&&jeffrey.runFrames?jeffrey.runFrames:jeffrey.frames;
  const jointFrames=moving&&jeffrey.runJointFrames?jeffrey.runJointFrames:jeffrey.jointFrames;
  const clipDuration=moving&&jeffrey.runDuration?jeffrey.runDuration:jeffrey.duration;
  const clipFrames=moving&&jeffrey.runFrameCount?jeffrey.runFrameCount:jeffrey.frameCount;
  const clipTime=moving?runPhase:t;
  const position=(clipTime%clipDuration)/clipDuration*clipFrames;
  const frame0=Math.floor(position)%clipFrames,frame1=(frame0+1)%clipFrames,mix=position-Math.floor(position);
  const source0=frames[frame0],source1=frames[frame1];
  // Portrait framing: keep the generated face square to the camera and allow
  // only a small living idle turn. D-pad motion shifts the portrait without
  // flipping it into a profile view.
  const angle=Math.PI+Math.sin(t*.23)*.018+playerFacing*.012,cos=Math.cos(angle),sin=Math.sin(angle);
  const world=source0.map((point,index)=>{
    const next=source1[index],x=point[0]+(next[0]-point[0])*mix;
    const y=point[1]+(next[1]-point[1])*mix,z=point[2]+(next[2]-point[2])*mix;
    return [x*cos-z*sin,y,x*sin+z*cos];
  });
  const cx=playerX-cameraX*.08;
  const scale=570+playerDepth*210;
  const ground=930+playerDepth*190-(moving?Math.abs(Math.sin(runPhase*Math.PI*2))*7:0);
  const points=world.map((point)=>[cx+point[0]*scale,ground-point[1]*scale,point[2]+.05]);
  // Project a bounded subset onto the floor for a moving cast-shadow pass.
  for(let index=0;index<jeffrey.faces.length;index+=8){
    const face=jeffrey.faces[index],a=world[face[0]],b=world[face[1]],c=world[face[2]];
    const shadow=(point)=>[cx+point[0]*scale+70+playerDepth*30,700+playerDepth*220+point[2]*(58+playerDepth*36),.98];
    const p0=shadow(a),p1=shadow(b),p2=shadow(c);
    triangle3d(p0[0],p0[1],p0[2],p1[0],p1[1],p1[2],p2[0],p2[1],p2[2],9,6,20);
  }
  // The native D24S8 target now records the visible body directly; the
  // fullscreen stencil pass supplies the living edge without duplicating the
  // entire mesh as expanded geometry.
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
    const length=Math.hypot(nx,ny,nz);if(length<.00001||nz>=0)continue;
    nx=-nx/length;ny=-ny/length;nz=-nz/length;
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
  // Real 24-joint skeleton, interpolated from the active idle/run clip. Lines
  // are submitted before GPU skin triangles, so the body naturally occludes them.
  const joints0=jointFrames[frame0],joints1=jointFrames[frame1];
  const jointPoints=joints0.map((point,index)=>{
    const next=joints1[index],x=point[0]+(next[0]-point[0])*mix;
    const y=point[1]+(next[1]-point[1])*mix,z=point[2]+(next[2]-point[2])*mix;
    return [cx+(x*cos-z*sin)*scale,ground-y*scale];
  });
  for(const bone of jeffrey.bones){const a=jointPoints[bone[0]],b=jointPoints[bone[1]];line(a[0],a[1],b[0],b[1],2,102,242,214);}
  detailedJeffreyHead(t,cx,ground,scale,moving);
}
function detailedJeffreyHead(t,cx,ground,bodyScale,moving){
  if(!jeffreyHead)return;
  const yaw=Math.PI+Math.sin(t*.19)*.012+playerFacing*.008;
  const cosine=Math.cos(yaw),sine=Math.sin(yaw),lean=moving?playerVelocityX/330*.045:0;
  const world=jeffreyHead.points.map((point)=>{
    const x=point[0]*cosine-point[2]*sine,y=point[1]+point[0]*lean;
    return [x,y,point[0]*sine+point[2]*cosine];
  });
  const headScale=bodyScale*.47,centerY=ground-bodyScale*.91;
  const points=world.map((point)=>[cx+point[0]*headScale,centerY-point[1]*headScale,point[2]*.16-1.08]);
  const light=[Math.sin(t*.53)*.34,-.42,-.84];
  const lightLength=Math.hypot(...light);for(let i=0;i<3;i++)light[i]/=lightLength;
  for(const face of jeffreyHead.faces){
    const a=world[face[0]],b=world[face[1]],c=world[face[2]];
    const ux=b[0]-a[0],uy=b[1]-a[1],uz=b[2]-a[2],vx=c[0]-a[0],vy=c[1]-a[1],vz=c[2]-a[2];
    let nx=uy*vz-uz*vy,ny=uz*vx-ux*vz,nz=ux*vy-uy*vx;
    const length=Math.hypot(nx,ny,nz);if(length<.00001)continue;
    const toward=nz>0?-1:1;nx=nx/length*toward;ny=ny/length*toward;nz=nz/length*toward;
    const diffuse=.5+.64*Math.max(0,nx*light[0]+ny*light[1]+nz*light[2]);
    const spec=Math.pow(Math.max(0,nx*.1+ny*-.25+nz*-.96),28)*(1-(face[7]||.5))*.46;
    const rim=Math.pow(1-Math.abs(nz),2.2)*.18;
    const p0=points[face[0]],p1=points[face[1]],p2=points[face[2]];
    triangle3d(p0[0],p0[1],p0[2],p1[0],p1[1],p1[2],p2[0],p2[1],p2[2],
      clamp(face[3]*diffuse+(face[8]||0)*.12+255*spec+86*rim),
      clamp(face[4]*diffuse+(face[9]||0)*.12+240*spec+62*rim),
      clamp(face[5]*diffuse+(face[10]||0)*.12+255*spec+112*rim));
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
  fireflies(t);
  animatedJeffrey(t);
  const ax=rotationX+Math.sin(t*.31)*.1;
  const ay=rotationY+t*.72;
  const world=vertices.map((point)=>rotate(point,ax,ay));
  const points=world.map((point)=>{
    const scale=470/(3.45+point[2]);
    return [1510-cameraX*.08+point[0]*scale,310-point[1]*scale,point[2]];
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
  const midi=run.midiInputs>0?run.midiInputs+" IN / "+(run.midiGate?"ON ":"OFF ")+"NOTE "+run.midiNote+" / BEND "+(run.midiPitchBend||8192)+" / CC "+(run.midiControl||0)+":"+(run.midiControlValue||0):(run.midiStatus||"NO MIDI INPUT").toUpperCase();
  const clock=run.clockSynced?(run.clockOffsetMs>=0?"+":"")+run.clockOffsetMs+" MS / RTT "+run.clockRttMs+" MS":"SYNCING";
  box(30,28,370,52,27,21,70);
  systemWrite(fpsLabel+" / "+frameMsLabel,48,38,23,245,232,255);
  box(1140,28,750,78,27,21,70);
  systemWrite("MIDI "+midi+" / "+inputToAudio.toFixed(0)+" US\\nAUDIO "+audioLatency+" / CLOCK "+clock,1160,38,18,227,224,255);
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
