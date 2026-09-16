import {spawn} from 'node:child_process';
import {createInterface} from 'node:readline';
import {BACKENDS} from './backends.mjs';

// Read the signed-in CLI's catalog without starting a thread or inference turn.
export function codexModels({command='codex',args=['app-server','--listen','stdio://'],cwd=process.cwd()}={}) {
  return new Promise((resolve,reject)=>{
    const child=spawn(command,args,{cwd,stdio:['pipe','pipe','ignore']});
    const models=[]; let done=false;
    const finish=(error)=>{if(done)return;done=true;clearTimeout(timer);child.kill();error?reject(error):resolve(models);};
    const timer=setTimeout(()=>finish(new Error('Codex model list timed out')),8000);
    const send=value=>child.stdin.write(JSON.stringify(value)+'\n');
    child.on('error',finish);child.on('exit',()=>finish(new Error('Codex model list unavailable')));child.stdin.on('error',finish);
    createInterface({input:child.stdout}).on('line',line=>{let m;try{m=JSON.parse(line);}catch{return;}
      if(m.error)return finish(new Error(m.error.message||'Codex model list failed'));
      if(m.id===1){send({method:'initialized',params:{}});send({id:2,method:'model/list',params:{limit:100}});}
      if(m.id===2){models.push(...(m.result?.data||[]));if(m.result?.nextCursor&&models.length<500)send({id:2,method:'model/list',params:{limit:100,cursor:m.result.nextCursor}});else finish();}
    });
    send({id:1,method:'initialize',params:{clientInfo:{name:'easel-settings',version:'1'},capabilities:{}}});
  });
}
export function pickerModels(p) {
  const backend=BACKENDS[p.backend];
  const choices=p.backend==='ac'?Object.entries(backend.models).map(([label,id])=>({id,label}))
    :p.backend==='claude'?['fable','opus','sonnet','haiku'].map(id=>({id,label:id}))
    :[{id:'',label:'CLI default'},...(p.catalog||[]).filter(x=>!x.hidden).map(x=>({id:x.model,label:x.displayName||x.model}))];
  if(!choices.some(x=>x.id===p.model))choices.unshift({id:p.model,label:p.model||'CLI default'});
  return choices;
}
export function pickerEfforts(p) {
  if(p.backend==='ac')return [''];
  if(p.backend==='claude')return ['', 'low','medium','high','xhigh','max'];
  const selected=p.catalog?.find(x=>p.model?x.model===p.model:x.isDefault);
  return ['',...(selected?.supportedReasoningEfforts||[]).map(x=>x.reasoningEffort)];
}
export function pickerKey(p,key) {
  if(key==='\x1b'||key==='\x03')return {action:'cancel'};
  if(key==='\r'||key==='\n')return p.row===3?{action:'apply'}:{...p,row:p.row+1};
  if(['\t','\x1b[B','\x1b[A','\x1b[Z'].includes(key))return {...p,row:(p.row+(['\x1b[A','\x1b[Z'].includes(key)?3:1))%4};
  if(!['\x1b[C','\x1b[D',' '].includes(key)||p.row===3)return p;
  const direction=key==='\x1b[D'?-1:1;
  const cycle=(values,value)=>values[(Math.max(0,values.indexOf(value))+direction+values.length)%values.length];
  if(p.row===0){const backend=cycle(['ac','claude','codex'],p.backend);return {...p,backend,model:BACKENDS[backend].defaultModel,effort:''};}
  if(p.row===1)return {...p,model:cycle(pickerModels(p).map(x=>x.id),p.model),effort:''};
  return {...p,effort:cycle(pickerEfforts(p),p.effort)};
}
export function pickerLines(p) {
  return ['Model settings','',...[
    `Provider   ${p.backend==='ac'?'AC hosted':p.backend+' · your CLI account'}`,
    `Model      ${pickerModels(p).find(x=>x.id===p.model)?.label||p.model}`,
    `Effort     ${p.backend==='ac'?'provider managed':p.effort||'default'}`,
    'Apply',
  ].map((line,i)=>(p.row===i?'› ':'  ')+line),'',p.loading?'Loading Codex models…':p.error||'↑ ↓ / Tab field · ← → choice · Enter next/apply · Esc cancel',
  'Custom model: /model NAME · Settings apply to the next turn'];
}

export function drawerOptions(p) {
  if(p.row===0)return [
    {id:'ac',label:'AC hosted'},
    {id:'claude',label:'Bring your own provider · Claude'},
    {id:'codex',label:'Bring your own provider · Codex'},
  ];
  if(p.row===1)return pickerModels(p);
  if(p.row===2)return pickerEfforts(p).map(id=>({id,label:id|| (p.backend==='ac'?'Provider managed':'Model default')}));
  return [{id:'apply',label:'Apply settings'}];
}
export function drawerIndex(p) {
  const value=[p.backend,p.model,p.effort,'apply'][p.row];
  return Math.max(0,drawerOptions(p).findIndex(x=>x.id===value));
}
export function drawerSelect(p,index) {
  const choice=drawerOptions(p)[index];if(!choice)return p;
  if(p.row===3)return {action:'apply'};
  const next=p.row===0?{...p,backend:choice.id,model:BACKENDS[choice.id].defaultModel,effort:'',row:1}
    :p.row===1?{...p,model:choice.id,effort:'',row:2}:{...p,effort:choice.id,row:3};
  return {...next,index:drawerIndex(next)};
}
export function drawerKey(p,key) {
  if(key==='\x1b'||key==='\x03')return {action:'cancel'};
  if(key==='\r'||key==='\n')return drawerSelect(p,p.index??drawerIndex(p));
  if(['\t','\x1b[Z','\x1b[C','\x1b[D'].includes(key)){
    const row=(p.row+(['\x1b[Z','\x1b[D'].includes(key)?3:1))%4;
    const next={...p,row};return {...next,index:drawerIndex(next)};
  }
  if(key==='\x1b[A'||key==='\x1b[B'){
    const count=drawerOptions(p).length;
    return {...p,index:((p.index??drawerIndex(p))+(key==='\x1b[A'?-1:1)+count)%count};
  }
  return p;
}
