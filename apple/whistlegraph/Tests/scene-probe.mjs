// Bounded deterministic execution supplements (does not replace) phone review.
import vm from 'node:vm';
import {parse} from '../Resources/Web/easel/src/vendor/acorn.mjs';
export function probeScene(source,{animated=true,attachedRope=false,minDrawingCalls=3}={}){
 const tree=parse(source,{ecmaVersion:'latest',sourceType:'module'}), exported={};let script=source;
 for(const n of [...tree.body].reverse()){
  if(n.type==='ImportDeclaration'||n.type==='ExportDefaultDeclaration')throw Error('Scene must be self-contained');
  if(n.type==='ExportNamedDeclaration'){
   if(n.declaration){if(n.declaration.id)exported[n.declaration.id.name]=n.declaration.id.name;for(const d of n.declaration.declarations||[])if(d.id.type==='Identifier')exported[d.id.name]=d.id.name;script=script.slice(0,n.start)+script.slice(n.declaration.start);}
   else{for(const s of n.specifiers)exported[s.exported.name]=s.local.name;script=script.slice(0,n.start)+script.slice(n.end);}
  }
 }
 if(!exported.paint)throw Error('No paint lifecycle');
 const results=[];
 for(const [width,height] of [[240,180],[320,240]]){
  const probe=`
  const screen={width:${width},height:${height}};let calls=[],color=[],clears=0;const shapes=new Set();
  const api={screen,store:{}, wipe(...args){clears++;calls.push(['wipe',args]);return api;},ink(...args){color=args;return api;}};
  for(const name of ['circle','box','tri','line','write','plot','ellipse'])api[name]=(...args)=>{if(args.flat().some(v=>typeof v==='number'&&!Number.isFinite(v)))throw Error('Non-finite '+name);calls.push([name,color,args]);return api;};
  ${script}
  ${exported.boot?`${exported.boot}(api);`:''}
  let minCalls=Infinity,maxCalls=0;
  for(let frame=0;frame<720;frame++){calls=[];clears=0;${exported.sim?`${exported.sim}(api);`:''}if(${exported.paint}(api)===false)throw Error('Paint frozen');if(!clears)throw Error('Frame not cleared');
  if(${attachedRope}){
   const colored=(rgb)=>calls.filter(c=>c[0]==='line'&&rgb.every((v,i)=>c[1].flat()[i]===v));
   const arms=colored([250,214,178]).slice(-2),rope=colored([250,250,250]);
   if(arms.length!==2||rope.length<12)throw Error('Missing arms or rope');
   const left=arms[0][2].slice(2,4),right=arms[1][2].slice(2,4);
   const start=rope[0][2].slice(0,2),end=rope.at(-1)[2].slice(2,4);
   if(Math.hypot(left[0]-start[0],left[1]-start[1])>.01||Math.hypot(right[0]-end[0],right[1]-end[1])>.01)throw Error('Rope detached from moving hands at frame '+frame);
  }
  minCalls=Math.min(minCalls,calls.length);maxCalls=Math.max(maxCalls,calls.length);shapes.add(JSON.stringify(calls));}
  JSON.stringify({width:screen.width,height:screen.height,frames:720,distinctFrames:shapes.size,minCalls,maxCalls});`;
  const result=JSON.parse(new vm.Script(probe).runInContext(vm.createContext({},{codeGeneration:{strings:false,wasm:false}}),{timeout:2000}));
  if(result.minCalls<minDrawingCalls)throw Error('Scene missing drawing');
  if(animated&&result.distinctFrames<8)throw Error('Insufficient motion');
  results.push(result);
 }
 return {passed:true,results,scope:'720 frames at two sizes; finite geometry, complete redraw, drawing variation. Semantic correctness requires visual review.'};
}
