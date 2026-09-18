// Shared pose-atlas choreography. Every crop excludes the old easel columns.
(function(scope){
 const STATES=['idle','awake','sleeping','working','running'];
 const CROP=Object.freeze({cellWidth:64,cellHeight:64,columns:4,width:46,height:64});
 const finite=(n,min,max)=>typeof n==='number'&&Number.isFinite(n)&&n>=min&&n<=max;
 function validateActions(value,{expectedCount}={}){
  if(!Array.isArray(value)||value.length>256||(expectedCount!==undefined&&value.length!==expectedCount))throw Error('Invalid action count');
  const ids=new Set();
  for(const action of value){
   if(!action||!/^[a-z][a-z0-9-]{1,63}$/.test(action.id)||ids.has(action.id))throw Error('Invalid or duplicate action ID');ids.add(action.id);
   if(typeof action.label!=='string'||!action.label.trim()||action.label.length>100||typeof action.category!=='string'||action.category.length>40||typeof action.loop!=='boolean'||!Array.isArray(action.states)||!action.states.length||action.states.some(s=>!STATES.includes(s)))throw Error(`Invalid action metadata: ${action.id}`);
   if(!Array.isArray(action.frames)||action.frames.length<3||action.frames.length>64)throw Error(`Invalid frames: ${action.id}`);
   for(const f of action.frames){
    if(!f||!Number.isInteger(f.pose)||f.pose<0||f.pose>15||!Number.isInteger(f.duration)||!finite(f.duration,80,3000))throw Error(`Invalid pose/timing: ${action.id}`);
    for(const name of ['x','y'])if(f[name]!==undefined&&!finite(f[name],-128,128))throw Error(`Invalid ${name}`);
    if(f.rotate!==undefined&&!finite(f.rotate,-180,180))throw Error('Invalid rotation');
    for(const name of ['scaleX','scaleY'])if(f[name]!==undefined&&!finite(f[name],.25,3))throw Error(`Invalid ${name}`);
    if(f.flip!==undefined&&typeof f.flip!=='boolean')throw Error('Invalid flip');
    for(const name of ['prop','effect'])if(f[name]!==undefined&&(typeof f[name]!=='string'||!/^[a-z][a-z0-9-]{0,47}$/.test(f[name])))throw Error(`Invalid ${name}`);
   }
  }return value;
 }
 function cropPose(pose){if(!Number.isInteger(pose)||pose<0||pose>15)throw Error('Invalid pose');return {x:(pose%4)*64,y:Math.floor(pose/4)*64,width:CROP.width,height:CROP.height};}
 function sampleAction(action,elapsed=0,{reducedMotion=false}={}){
  const total=action.frames.reduce((sum,f)=>sum+f.duration,0),time=Math.max(0,Number.isFinite(elapsed)?elapsed:0);
  const done=!action.loop&&time>=total;let offset=action.loop?time%total:Math.min(time,total-1),index=0;
  if(reducedMotion)index=0;
  else for(let i=0;i<action.frames.length;i++){index=i;if(offset<action.frames[i].duration)break;offset-=action.frames[i].duration;}
  const frame=action.frames[index];
  return {id:action.id,label:action.label,category:action.category,pose:frame.pose,crop:cropPose(frame.pose),x:reducedMotion?0:frame.x||0,y:reducedMotion?0:frame.y||0,rotate:reducedMotion?0:frame.rotate||0,flip:!!frame.flip,scaleX:reducedMotion?1:frame.scaleX??1,scaleY:reducedMotion?1:frame.scaleY??1,...(!reducedMotion&&frame.prop?{prop:frame.prop}:{}),...(!reducedMotion&&frame.effect?{effect:frame.effect}:{}),frameIndex:index,duration:total,done,nextDelay:reducedMotion||done?null:frame.duration-offset};
 }
 function hashSeed(seed){let h=2166136261;for(const c of String(seed)){h^=c.charCodeAt(0);h=Math.imul(h,16777619);}return h>>>0;}
 function chooseAction(actions,{state='idle',seed=0,previous}={}){
  const eligible=actions.filter(a=>a.states.includes(state));if(!eligible.length)return null;
  const candidates=eligible.length>1?eligible.filter(a=>a.id!==previous):eligible;
  return candidates[hashSeed(seed)%candidates.length];
 }
 async function loadActions({urls=['donkey-actions/idle.json','donkey-actions/work.json','donkey-actions/movement.json'],fetch=scope.fetch?.bind(scope),expectedCount=128}={}){
  if(!fetch)throw Error('No action loader');
  const groups=await Promise.all(urls.map(async url=>{const response=await fetch(url);if(!response.ok)throw Error(`Cannot load donkey actions: ${url}`);return validateActions(await response.json());}));
  return validateActions(groups.flat(),{expectedCount});
 }
 function createActionPlayer(actions,{seed=0,state='idle',now=0,reducedMotion=false}={}){
  validateActions(actions);let sequence=0,started=now,selected=chooseAction(actions,{state,seed:`${seed}:0`});
  function select(at,nextState){const previous=selected?.id;state=nextState;selected=chooseAction(actions,{state,seed:`${seed}:${++sequence}`,previous});started=at;}
  return {
   setState(next,at=0){if(!STATES.includes(next))throw Error('Unknown donkey state');if(next!==state)select(at,next);},
   setReducedMotion(value){reducedMotion=!!value;},
   play(id,at=0){const action=actions.find(a=>a.id===id);if(!action)throw Error('Unknown action');selected=action;started=at;},
   sample(at=0){if(!selected)return null;return sampleAction(selected,at-started,{reducedMotion});},
   next(at=0){select(at,state);return this.sample(at);},
   get action(){return selected?.id||null;},get state(){return state;},
  };
 }
 const api={STATES,CROP,validateActions,cropPose,sampleAction,chooseAction,loadActions,createActionPlayer};
 if(typeof module!=='undefined'&&module.exports)module.exports=api;else scope.DonkeyActions=api;
})(typeof window==='undefined'?globalThis:window);
