import { KidLisp } from "../../system/public/aesthetic.computer/lib/kidlisp.mjs";
import { KidLispExecution } from "../../system/public/aesthetic.computer/lib/kidlisp-execution.mjs";
import { cssColors } from "../../system/public/aesthetic.computer/lib/num.mjs";

// A pinned first effectful graph profile, not a general KidLisp compiler.
export const ROZ_SOURCE = `fade:red-blue-black-blue-red
ink (? rainbow white 0) (1s... 24 64)
line w/2 0 w/2 h
(spin (2s... -1.125 1.125)) (zoom 1.1)
(0.5s (contrast 1.05))
(scroll (? -0.1 0 0.1) (? -0.1 0 0.1))
ink (? cyan yellow magenta) 8
circle w/2 h/2 (? 2 4 8)`;
export const ROZ_GRAPH = Object.freeze({ version: 1, profile: "roz-feedback-v1", source: ROZ_SOURCE,
  initial: "fade:red-blue-black-blue-red", nodes: ["line", "spin", "zoom", "contrast?", "scroll?", "circle"],
  feedback: "previous frame → line → spin → zoom → contrast → scroll → circle → next frame",
  limits: { dimension: 512, nodes: 6, steps: 10000, frames: 1000000 },
});

export function createRozPlan({ source = ROZ_SOURCE, width = 256, height = 256, seed = 1, stepMs = 1000 / 60 } = {}) {
  if (source.trim() !== ROZ_SOURCE) throw new TypeError("roz-feedback-v1 accepts only the pinned $roz source");
  if (![width,height].every(n=>Number.isInteger(n)&&n>=32&&n<=512)||!Number.isInteger(seed)||seed<0||seed>0xffffffff) throw new RangeError("Invalid graph dimensions or seed");
  const execution = new KidLispExecution({seed, stepMs, maxSteps:10000});
  const lisp = new KidLisp({execution});
  lisp.module(source,true); lisp.cacheInitiated = true;
  let frame = 0, spin = 0, sx = 0, sy = 0, rainbowIndex = 0, rainbowUsed = false, nodes;
  let ink = [255,255,255,255];
  const palette = ["red","orange","yellow","green","blue","indigo","violet"].map(name=>cssColors[name]);
  const api = {
    screen:{width,height}, clock:execution.clock,
    ink(...args) {
      while(Array.isArray(args[0])) args=args[0];
      let [color,alpha=255]=args;
      if(color==="rainbow") { if(!rainbowUsed){rainbowIndex=(rainbowIndex+1)%7;rainbowUsed=true;} color=palette[rainbowIndex]; }
      else if(typeof color==="string") color=cssColors[color];
      else if(typeof color==="number") color=[color,color,color];
      if(!Array.isArray(color)) throw new TypeError("Unresolved graph ink");
      ink=[...color.slice(0,3),alpha];return api;
    },
    line(x0,y0,x1,y1) { if(x0!==width/2||x1!==width/2||y0!==0||y1!==height)throw new Error("Unsupported graph line"); nodes.push({op:"line",color:[...ink]}); },
    circle(x,y,r,fill) { if(x!==width/2||y!==height/2||![2,4,8].includes(r)||fill!==true)throw new Error("Unsupported graph circle"); nodes.push({op:"circle",radius:r,color:[...ink]}); },
    spin(steps) { spin+=steps;const whole=Math.floor(spin);spin-=whole;if(whole)nodes.push({op:"spin",value:whole}); },
    zoom(level) { if(level!==1.1)throw new Error("Unsupported graph zoom");nodes.push({op:"zoom",value:level}); },
    contrast(level) { if(level!==1.05)throw new Error("Unsupported graph contrast");nodes.push({op:"contrast",value:level}); },
    scroll(dx,dy) {sx+=dx;sy+=dy;const x=Math.trunc(sx),y=Math.trunc(sy);sx-=x;sy-=y;if(x||y)nodes.push({op:"scroll",x,y});},
  };
  return { graph:ROZ_GRAPH, width,height,seed,
    next() {
      if(frame>=ROZ_GRAPH.limits.frames)throw new RangeError("Graph frame budget exhausted");
      execution.beginFrame(frame);lisp.frameCount=frame+1;nodes=[];rainbowUsed=false;
      lisp.evaluate(lisp.ast,execution.bindApi(api),undefined,undefined,true);
      if(execution.state.error)throw execution.state.error;
      if(nodes.length>6)throw new Error("Graph node budget exceeded");
      return {frame:frame++,timeMs:execution.timeMs,nodes,steps:execution.state.steps};
    },
  };
}
