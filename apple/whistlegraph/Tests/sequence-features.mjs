import vm from 'node:vm';
import {parse} from '../Resources/Web/easel/src/vendor/acorn.mjs';
import {balls} from '../Resources/Web/sequence-benchmark.mjs';

// Behavioral probes of the actual generated source, separate from the real-phone
// renderer. No network/files/process or host functions are exposed to the VM.
export function testFeatures(source,index) {
 const errors=[];
 let tree;try{tree=parse(source,{ecmaVersion:'latest',sourceType:'module'});}catch(e){return {passed:false,errors:[e.message]};}
 let script=source;const exported={};
 for(const node of [...tree.body].reverse()){
  if(node.type==='ImportDeclaration'||node.type==='ExportDefaultDeclaration')return {passed:false,errors:['Scenario must be a self-contained named-export piece']};
  if(node.type==='ExportNamedDeclaration'){
   if(node.declaration){if(node.declaration.id)exported[node.declaration.id.name]=node.declaration.id.name;for(const d of node.declaration.declarations||[])if(d.id.type==='Identifier')exported[d.id.name]=d.id.name;script=script.slice(0,node.start)+script.slice(node.declaration.start);}
   else {for(const spec of node.specifiers)exported[spec.exported.name]=spec.local.name;script=script.slice(0,node.start)+script.slice(node.end);}
  }
 }
 if(!exported.sim||!exported.paint)return {passed:false,errors:['Animation must export sim and paint']};
 const expected=balls.slice(0,Math.ceil(index/4)).map((b,i)=>({...b,trail:index>=i*4+2,highlight:index>=i*4+3,counter:index>=i*4+4}));
 for(const [width,height] of [[240,180],[320,240]]){
  const context=vm.createContext({}, {codeGeneration:{strings:false,wasm:false}});
  const probe=`
  const expected=${JSON.stringify(expected)};
  const screen={width:${width},height:${height}};
  let color=[255,255,255,255],circles=[],texts=[];
  const api={screen,
    wipe(){return api;},
    ink(...v){if(Array.isArray(v[0]))v=v[0]; if(typeof v[0]==='string'){const names={white:[255,255,255],black:[0,0,0],pink:[255,192,203]};color=[...(names[v[0]]||[-1,-1,-1]),v[1]??255];}else color=[v[0],v[1],v[2],v[3]??255];return api;},
    circle(x,y,r,filled){circles.push({x,y,r,filled,color:[...color]});return api;},
    write(text,...args){texts.push(String(text));return api;}
  };
  ${script}
  ${exported.boot?`${exported.boot}(api);`:''}
  const seen=expected.map(()=>({positions:new Set(),trail:false,highlights:0,counters:[],vxSigns:new Set(),vySigns:new Set(),previous:null}));
  const failures=new Set();
  for(let frame=0;frame<720;frame++){
    ${exported.sim}(api);circles=[];texts=[];if(${exported.paint}(api)===false)failures.add('paint returned false, freezing animation');
    expected.forEach((ball,i)=>{
      const matching=circles.filter(c=>c.color.slice(0,3).every((v,k)=>v===ball.rgb[k]));
      if(matching.length>13)failures.add(ball.name+': trail exceeds 12 positions');
      const bodies=matching.filter(c=>c.color[3]>=254&&c.filled===true);
      if(bodies.length!==1){failures.add(ball.name+': expected one opaque filled body');return;}
      const body=bodies[0], state=seen[i];
      if(![body.x,body.y,body.r].every(Number.isFinite)||body.r<=0)failures.add(ball.name+': invalid geometry');
      if(body.x-body.r < -1||body.x+body.r>screen.width+1||body.y-body.r < -1||body.y+body.r>screen.height+1)failures.add(ball.name+': body outside screen');
      state.positions.add(body.x.toFixed(2)+','+body.y.toFixed(2));
      if(state.previous){const dx=body.x-state.previous.x,dy=body.y-state.previous.y;if(Math.abs(dx)>.01)state.vxSigns.add(Math.sign(dx));if(Math.abs(dy)>.01)state.vySigns.add(Math.sign(dy));}state.previous=body;
      if(matching.some(c=>c.color[3]>=20&&c.color[3]<=180&&Math.hypot(c.x-body.x,c.y-body.y)>1))state.trail=true;
      if(circles.some(c=>c.color.slice(0,3).every(v=>v===255)&&c.color[3]>=254&&c.r<body.r*.5&&c.r>0&&Math.hypot(c.x-body.x,c.y-body.y)+c.r<=body.r+1))state.highlights++;
      const prefix=ball.name+':';const text=texts.find(t=>t.startsWith(prefix));if(text){const n=Number(text.slice(prefix.length).trim());if(Number.isInteger(n))state.counters.push(n);}
    });
  }
  expected.forEach((ball,i)=>{const state=seen[i];
   if(state.positions.size<100||state.vxSigns.size<2||state.vySigns.size<2)failures.add(ball.name+': movement and both-axis wall reversals required');
   if(ball.trail&&!state.trail)failures.add(ball.name+': no visible-alpha trail behind body');
   if(!ball.trail&&state.trail)failures.add(ball.name+': trail added before requested');
   if(!ball.highlight&&state.highlights>600)failures.add(ball.name+': highlight added before requested');
   if(!ball.counter&&state.counters.length>20)failures.add(ball.name+': counter added before requested');
   if(ball.highlight&&state.highlights<650)failures.add(ball.name+': highlight missing or outside body');
   if(ball.counter&&state.counters.some((v,j)=>j>0&&v<state.counters[j-1]))failures.add(ball.name+': counter decreased');
   if(ball.counter&&(state.counters.length<600||Math.max(...state.counters)<=Math.min(...state.counters)))failures.add(ball.name+': wall-hit counter not visible/increasing');
  });
  JSON.stringify([...failures]);`;
  try{const failures=JSON.parse(new vm.Script(probe).runInContext(context,{timeout:2000}));errors.push(...failures.map(s=>`${width}x${height}: ${s}`));}
  catch(e){errors.push(`${width}x${height}: ${e.message}`);}
 }
 return {passed:errors.length===0,errors,framesPerSize:720,sizes:[[240,180],[320,240]],ballCount:expected.length};
}
