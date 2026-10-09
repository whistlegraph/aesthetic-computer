import {KidLisp} from "../../system/public/aesthetic.computer/lib/kidlisp.mjs";
import {KidLispExecution} from "../../system/public/aesthetic.computer/lib/kidlisp-execution.mjs";

// A separate experimental profile, not additional forms silently accepted by
// the shipping evaluator. Values become a reusable DAG and packed primitives.
export function compileScene(source) {
  if(typeof source!=="string"||source.length>32768)throw new RangeError("Scene source budget exceeded");
  const parser=new KidLisp({execution:new KidLispExecution({maxDepth:64})});
  const ast=parser.parse(source);
  if(ast.length!==1||ast[0][0]!=="scene")throw new TypeError("Expected one scene form");
  const plan={version:1,profile:"scene-v1",source,values:[],primitives:[],camera:[0,1.6,-6],walk:3,sky:[18,26,44],sun:[-.6,.9,-.5],fog:.018};
  const cache=new Map();let ink=[255,255,255],mirror=0,checker=0,work=0;
  const literal=n=>{if(!Number.isFinite(n)||Math.abs(n)>10000)throw new TypeError("Scene requires bounded finite numbers");return n;};
  function value(expr,env) {
    if(typeof expr==="string"&&Object.hasOwn(env,expr))expr=env[expr];
    let node;
    if(typeof expr==="number")node={op:"constant",value:literal(expr)};
    else if(expr==="time")node={op:"time"};
    else if(Array.isArray(expr)&&["+","-","*","sin","cos"].includes(expr[0])) {
      const [op,...args]=expr;
      if(args.length<1||args.length>8||(["sin","cos"].includes(op)&&args.length!==1))throw new TypeError(`Invalid scene arithmetic ${op}`);
      node={op,args:args.map(a=>value(a,env))};
    }else throw new TypeError(`Unsupported scene expression: ${JSON.stringify(expr)}`);
    const key=JSON.stringify(node);if(cache.has(key))return cache.get(key);
    if(plan.values.length>=512)throw new RangeError("Scene value budget exceeded");
    const index=plan.values.push(node)-1;cache.set(key,index);return index;
  }
  function statements(forms,env={},depth=0) {
    if(depth>16)throw new RangeError("Scene repeat depth exceeded");
    for(const form of forms) {
      if(++work>1024)throw new RangeError("Scene expansion budget exceeded");
      if(!Array.isArray(form))throw new TypeError("Scene statements must be lists");
      const [op,...args]=form;
      if(op==="repeat") {
        const [count,name,...body]=args;
        if(!Number.isInteger(count)||count<1||count>16||typeof name!=="string"||name==="time"||!body.length)throw new TypeError("Scene repeat needs a static count, binding and body");
        for(let i=0;i<count;i++)statements(body,{...env,[name]:i},depth+1);
      }else if(["sphere","box3","ground"].includes(op)) {
        const arity={sphere:4,box3:6,ground:1}[op];
        if(args.length!==arity)throw new TypeError(`Invalid ${op} arity`);
        if(plan.primitives.length>=32)throw new RangeError("Scene primitive budget exceeded");
        plan.primitives.push({op,values:args.map(a=>value(a,env)),ink:[...ink],mirror,checker});
      }else if(["camera","sky","sun","ink"].includes(op)) {
        if(args.length!==3)throw new TypeError(`Invalid ${op} arity`);
        const values=args.map(literal);
        if(["ink","sky"].includes(op)&&values.some(n=>n<0||n>255))throw new RangeError("Scene colors must be 0–255");
        if(op==="ink")ink=values;else plan[op]=values;
      }else if(["mirror","checker","walk","fog"].includes(op)) {
        if(args.length!==1)throw new TypeError(`Invalid ${op} arity`);
        const n=literal(args[0]);
        const max={mirror:1,checker:16,walk:10,fog:1}[op];
        if(n<0||n>max)throw new RangeError(`Invalid ${op} range`);
        if(op==="mirror")mirror=n;else if(op==="checker")checker=n;else plan[op]=n;
      }else throw new TypeError(`Unsupported scene statement: ${op}`);
    }
  }
  statements(ast[0].slice(1));
  if(!plan.primitives.length||Math.hypot(...plan.sun)<.001)throw new TypeError("Scene needs geometry and a sun direction");
  return plan;
}

export function createSceneState(plan) {
  if(plan.profile!=="scene-v1")throw new TypeError("Unsupported scene plan");
  const values=new Float64Array(plan.values.length),primitives=new Float32Array(32*16);
  return {primitives,
    update(time) {
      if(!Number.isFinite(time)||time<0||time>86400)throw new RangeError("Scene time outside 0–86400 seconds");
      for(let i=0;i<plan.values.length;i++) {
        const n=plan.values[i],a=n.args?.map(j=>values[j]);
        values[i]=n.op==="constant"?n.value:n.op==="time"?time:n.op==="sin"?Math.sin(a[0]):n.op==="cos"?Math.cos(a[0]):n.op==="+"?a.reduce((x,y)=>x+y,0):n.op==="*"?a.reduce((x,y)=>x*y,1):a.length===1?-a[0]:a.slice(1).reduce((x,y)=>x-y,a[0]);
        if(!Number.isFinite(values[i])||Math.abs(values[i])>1e6)throw new RangeError("Scene arithmetic exceeded finite bounds");
      }
      plan.primitives.forEach((p,i)=>{
        const a=p.values.map(j=>values[j]),o=i*16;
        const size=p.op==="sphere"?[a[3],a[3],a[3]]:p.op==="box3"?a.slice(3):[0,0,0];
        if(p.op!=="ground"&&size.some(n=>n<=0||n>1000))throw new RangeError("Scene shape dimensions must be positive");
        primitives.set(p.op==="ground"?[0,a[0],0,2]:[a[0],a[1],a[2],p.op==="sphere"?0:1],o);
        primitives.set([...size,p.mirror],o+4);primitives.set([...p.ink.map(c=>c/255),p.checker],o+8);
      });
      return primitives;
    },
    // Conservative walking clearance around declared spheres and boxes.
    blocked(x,y,z) {
      for(let i=0;i<plan.primitives.length;i++) {
        const o=i*16,t=primitives[o+3];if(t===2)continue;
        const dx=x-primitives[o],dy=y-primitives[o+1],dz=z-primitives[o+2];
        if(t===0&&Math.hypot(dx,dy,dz)<primitives[o+4]+.25)return true;
        if(t===1&&Math.abs(dx)<primitives[o+4]+.25&&Math.abs(dy)<primitives[o+5]+.25&&Math.abs(dz)<primitives[o+6]+.25)return true;
      }
      return false;
    },
  };
}
