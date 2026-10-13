import {PieceVersions} from './piece-versions.mjs';

// Each project owns its ledger and publishing state. Existing single-project
// keys become the legacy project without rewriting or discarding their data.
export class WareProjects {
 constructor(storage,root){
  this.storage=storage;this.root=root;this.indexKey=root+'-projects';
  const saved=storage.getItem(this.indexKey);
  this.index=saved?JSON.parse(saved):{format:1,current:'legacy',projects:[{id:'legacy'}]};
  if(this.index.format!==1||!Array.isArray(this.index.projects)||this.index.projects.length>128||!this.index.projects.some(p=>p.id===this.index.current)||this.index.projects.some(p=>!this.validID(p.id))||new Set(this.index.projects.map(p=>p.id)).size!==this.index.projects.length)throw Error('Invalid project library');
 }
 validID(id){return typeof id==='string'&&/^[a-z0-9-]{1,64}$/.test(id);}
 get current(){return this.index.current;}
 key(id=this.current){if(!this.index.projects.some(p=>p.id===id))throw Error('Project unavailable');return id==='legacy'?this.root:this.root+'-project-'+id;}
 summary(id,live){
  const key=this.key(id),ledger=live?.ledger??JSON.parse(this.storage.getItem(key+'-versions'));
  if(!ledger?.versions?.length)return {id,code:'',name:'untitled',utterance:'',versions:0,updatedAt:'',current:id===this.current};
  const head=ledger.versions.find(v=>v.id===ledger.head);if(!head)throw Error('Invalid project history');
  const source=live?null:JSON.parse(head.source),codes=live?null:JSON.parse(this.storage.getItem(key+'-codes')||'{}');
  return {id,code:live?.code??codes?.[head.id]??'',name:live?.name??source?.title??'untitled',utterance:head.request||'',versions:ledger.versions.filter(v=>v.id>0).length,updatedAt:head.createdAt,current:id===this.current};
 }
 list(live){return [this.summary(this.current,live),...this.index.projects.filter(p=>p.id!==this.current).map(p=>({...p,current:false}))];}
 save(next){this.storage.setItem(this.indexKey,JSON.stringify(next));this.index=next;}
 new(initialSource){
  if(this.index.projects.length>=128)throw Error('This phone has reached its project limit. Export and remove an older project first.');
  const parked=this.summary(this.current);let id;
  do{id=globalThis.crypto?.randomUUID?.()??('p-'+Date.now().toString(36)+'-'+Math.random().toString(36).slice(2));}while(this.index.projects.some(p=>p.id===id));
  const key=this.root+'-project-'+id;
  const versions=new PieceVersions(this.storage,key+'-versions',initialSource);
  const next={...this.index,current:id,projects:[...this.index.projects.map(p=>p.id===parked.id?parked:p),{id}]};
  try{this.save(next);}catch(error){this.storage.removeItem(key+'-versions');throw error;}
  return versions;
 }
 open(id){
  if(id===this.current)return;
  this.key(id);const parked=this.summary(this.current);this.summary(id);
  this.save({...this.index,current:id,projects:this.index.projects.map(p=>p.id===parked.id?parked:p)});
 }
 remove(id){
  if(id===this.current)throw Error('Open another project before removing this one');
  const key=this.key(id);this.save({...this.index,projects:this.index.projects.filter(p=>p.id!==id)});
  for(const suffix of ['-versions','-codes','-attempt'])this.storage.removeItem(key+suffix);
 }
}
export const requestsNewProject=(text,ware='doom')=>ware==='roblox'?/^(?:please\s+)?(?:(?:make|create|start)\s+)?(?:a\s+)?new\s+(?:empty\s+)?(?:roblox\s+)?room[.!]?$/i.test(String(text).trim()):/^(?:please\s+)?(?:(?:make|create|start)\s+)?(?:a\s+)?new\s+(?:empty\s+)?(?:doom\s+)?wad[.!]?$/i.test(String(text).trim());
