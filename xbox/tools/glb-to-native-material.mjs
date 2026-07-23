#!/usr/bin/env node
// Convert a bounded triangle GLB into a self-contained Native BIOS material piece.
// Textures are sampled offline per face; runtime shading supplies light, specular,
// and procedural skybox reflection without exposing file or network APIs to JS.

import { readFileSync, writeFileSync } from "node:fs";
import sharp from "sharp";

const [source, destination] = process.argv.slice(2);
if (!source || !destination) {
  throw new Error("usage: glb-to-native-material.mjs <simplified.glb> <piece.js>");
}

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

const code = `// Pals Material Skybox, 26.07.22
// Connected ${indices.length / 3}-triangle derivative of the recovered Meshy GLB.
const vertices=${JSON.stringify(vertices)};
const faces=${JSON.stringify(faceRows)};
const stars=${JSON.stringify(stars)};
let rotationX=0,rotationY=0;
function boot(){telemetry("PALS_MATERIAL_BOOT","triangles=${indices.length / 3} material=glb-textures glossy skybox procedural hud=0")}
function sim(){
  const pad=gamepad();
  if(pad.down.includes("ArrowLeft"))rotationY-=0.035;
  if(pad.down.includes("ArrowRight"))rotationY+=0.035;
  if(pad.down.includes("ArrowUp"))rotationX-=0.035;
  if(pad.down.includes("ArrowDown"))rotationX+=0.035;
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
  for(let band=0;band<30;band++){
    const mix=band/29;
    const r=5+Math.floor(24*mix*mix),g=8+Math.floor(14*mix),b=28+Math.floor(32*mix);
    box(0,band*36,1920,37,r,g,b);
  }
  box(0,700,1920,5,90,54,112);
  box(0,705,1920,8,42,28,70);
  for(const star of stars){
    const pulse=.62+.38*Math.sin(t*.7+star[0]*.017+star[1]*.009);
    const glow=Math.floor(star[3]*pulse);
    box(star[0],star[1],star[2],star[2],glow*.72,glow*.82,glow);
  }
  for(let lane=-7;lane<=7;lane++)line(960,710,960+lane*190,1080,1,32,24,56);
  for(let row=0;row<8;row++){
    const y=720+Math.pow(row/7,1.7)*350;
    line(0,y,1920,y,1,25+row*3,19+row*2,48+row*4);
  }
}
function paint(){
  const t=runtime().monotonicUs/1000000;
  skybox(t);
  const ax=rotationX+Math.sin(t*.17)*.09;
  const ay=rotationY+Math.sin(t*.13)*.34;
  const world=vertices.map((point)=>rotate(point,ax,ay));
  const points=world.map((point)=>{
    const scale=930/(3.45+point[2]);
    return [960+point[0]*scale,530-point[1]*scale,point[2]];
  });
  const order=faces.map((_,index)=>index);
  order.sort((left,right)=>{
    const a=faces[left],b=faces[right];
    return (points[b[0]][2]+points[b[1]][2]+points[b[2]][2])-
      (points[a[0]][2]+points[a[1]][2]+points[a[2]][2]);
  });
  const light=[Math.sin(t*.31)*.48,-.56,.72];
  const lightLength=Math.hypot(light[0],light[1],light[2]);
  light[0]/=lightLength;light[1]/=lightLength;light[2]/=lightLength;
  const half=[light[0],light[1],light[2]+1];
  const halfLength=Math.hypot(half[0],half[1],half[2]);
  half[0]/=halfLength;half[1]/=halfLength;half[2]/=halfLength;
  for(const index of order){
    const face=faces[index],a=world[face[0]],b=world[face[1]],c=world[face[2]];
    const ux=b[0]-a[0],uy=b[1]-a[1],uz=b[2]-a[2];
    const vx=c[0]-a[0],vy=c[1]-a[1],vz=c[2]-a[2];
    let nx=uy*vz-uz*vy,ny=uz*vx-ux*vz,nz=ux*vy-uy*vx;
    const length=Math.hypot(nx,ny,nz);if(length<.00001)continue;
    nx/=length;ny/=length;nz/=length;
    if(nz<0){nx=-nx;ny=-ny;nz=-nz}
    const diffuse=.16+.72*Math.max(0,nx*light[0]+ny*light[1]+nz*light[2]);
    const rough=face[7],metal=face[6];
    const specular=Math.pow(Math.max(0,nx*half[0]+ny*half[1]+nz*half[2]),12+56*(1-rough))*(.55+.75*metal);
    const fresnel=Math.pow(1-Math.max(0,nz),3)*(.16+.32*metal);
    const sky=Math.max(0,.5-.5*ny)*(.14+.24*metal);
    const p0=points[face[0]],p1=points[face[1]],p2=points[face[2]];
    const red=clamp(face[3]*diffuse+face[8]*.22+255*specular+70*fresnel+32*sky);
    const green=clamp(face[4]*diffuse+face[9]*.22+240*specular+42*fresnel+34*sky);
    const blue=clamp(face[5]*diffuse+face[10]*.22+255*specular+110*fresnel+88*sky);
    triangle3d(p0[0],p0[1],p0[2],p1[0],p1[1],p1[2],p2[0],p2[1],p2[2],red,green,blue);
  }
}
function act(button){telemetry("PALS_MATERIAL_BUTTON",button)}
function leave(){telemetry("PALS_MATERIAL_LEAVE","ok")}
`;

writeFileSync(destination, code);
console.log(JSON.stringify({ source, destination, vertices: vertices.length,
  triangles: faceRows.length, bytes: Buffer.byteLength(code), textures: {
    baseColor: !!baseImage, metallicRoughness: !!metallicImage, emissive: !!emissiveImage,
  } }));
