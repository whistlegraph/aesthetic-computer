// Prepare fixed geometry once using the actual software rasterizer. Integer
// gather maps retain its rounding/wrapping quirks exactly, including zoom's
// Float32 coordinate staging. No full-image CPU work in steady GPU frames.
import * as graph from "../../system/public/aesthetic.computer/lib/graph.mjs";
export function prepareRozAssets(width,height) {
  if(![width,height].every(n=>Number.isInteger(n)&&n>=32&&n<=512))throw new RangeError("Invalid graph dimensions");
  const count=width*height;
  const buffer={width,height,pixels:new Uint8ClampedArray(count*4)};
  const words=new Uint32Array(buffer.pixels.buffer);
  graph.setBuffer(buffer);graph.unmask();graph.blendMode("blend");
  graph.color("fade:red-blue-black-blue-red");graph.clear();
  const initial=words.slice(),maps={};
  for(const [key,apply] of [["spin:-2",()=>graph.spin(-2)],["spin:-1",()=>graph.spin(-1)],["spin:1",()=>graph.spin(1)],["spin:2",()=>graph.spin(2)],["zoom",()=>graph.zoom(1.1)]]) {
    for(let i=0;i<count;i++)words[i]=(i|0xff000000)>>>0;
    apply(); maps[key]=Uint32Array.from(words,w=>w&0xffffff);
  }
  const masks={};
  for(const radius of [2,4,8]) {
    words.fill(0xff000000);graph.color(255,255,255,1);graph.circle(width/2,height/2,radius,true);
    masks[radius]=Uint32Array.from(words,w=>w&255);
    if(masks[radius].some(n=>n>16))throw new Error("Primitive coverage exceeds graph bound");
  }
  const circleLUT=new Uint32Array(256*256);
  const byte=new Uint8ClampedArray(1);
  for(let a=0;a<256;a++)for(let value=0;value<256;value++) {
    let low,high,alpha;
    if(a===255){low=(247*value)>>8;high=(9*255+247*value)>>8;alpha=255;}
    else {
      const src=8/255,dst=a/255,combined=src+(1-src)*dst;
      byte[0]=(value*(1-src)*dst)/(combined+1e-10);low=byte[0];
      byte[0]=(255*src+value*(1-src)*dst)/(combined+1e-10);high=byte[0];
      byte[0]=combined*255;alpha=byte[0];
    }
    circleLUT[a*256+value]=(low|(high<<8)|(alpha<<16))>>>0;
  }
  const contrast=Uint32Array.from({length:256},(_,i)=>Math.max(0,Math.min(255,Math.round(((i/255-.5)*1.05+.5)*255))));
  return {width,height,initial,maps,masks,circleLUT,contrast};
}

// Independent reference playback of resolved graph commands. Use in tests or
// fallback only; graph.mjs is module-global, so callers run serially.
export function renderRozCPU(buffer,nodes) {
  graph.setBuffer(buffer);graph.unmask();graph.blendMode("blend");
  for(const node of nodes) {
    if(node.op==="line"){graph.color(...node.color);graph.line(buffer.width/2,0,buffer.width/2,buffer.height);}
    else if(node.op==="circle"){graph.color(...node.color);graph.circle(buffer.width/2,buffer.height/2,node.radius,true);}
    else if(node.op==="spin")graph.spin(node.value);
    else if(node.op==="zoom")graph.zoom(node.value);
    else if(node.op==="contrast")graph.contrast(node.value);
    else if(node.op==="scroll"){graph.resetScrollState();graph.scroll(node.x,node.y);}
    else throw new Error("Unsupported reference graph node");
  }
  return buffer.pixels;
}
