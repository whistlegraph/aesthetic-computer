import {randomInt, createHash} from 'node:crypto';

export const validID = value => typeof value === 'string' && /^[a-f0-9-]{36}$/i.test(value);
// New Whistlegraph threads use wg; persisted ww identifiers remain readable.
export const THREAD_CODE_PREFIX = 'wg';
export const validCode = value => typeof value === 'string' && /^(?:wg|ww)[a-z]{5,12}$/i.test(value);
export function pronounceableCode() {
  const consonants='bdfghklmnprstvz', vowels='aeiou';
  let word='';
  for (let i=0;i<5;i++) word+=(i%2?vowels:consonants)[randomInt(i%2?vowels.length:consonants.length)];
  return THREAD_CODE_PREFIX+word[0].toUpperCase()+word.slice(1);
}
export const sourceHash = source => createHash('sha256').update(source).digest('hex');
export function validateLedger(ledger) {
  if(ledger?.format!==1 || !Array.isArray(ledger.versions) || !ledger.versions.length || ledger.versions.length>256) throw Error('Invalid version history');
  if(Buffer.byteLength(JSON.stringify(ledger))>8_000_000) throw Error('Version history exceeds 8 MB');
  const ids=new Set();
  for(const v of ledger.versions) {
    if(!Number.isSafeInteger(v.id)||v.id<0||ids.has(v.id)||typeof v.source!=='string'||Buffer.byteLength(v.source)>500_000||!(v.parent===null||ids.has(v.parent))||!(v.request==null||typeof v.request==='string'&&v.request.length<=20000)) throw Error('Invalid version');
    ids.add(v.id);
  }
  if(!ids.has(ledger.head)) throw Error('Missing head');
  return {format:1,head:ledger.head,versions:ledger.versions.map(({id,parent,source,request,createdAt,layers})=>({id,parent,source,request:request??null,createdAt:typeof createdAt==='string'?createdAt:'',layers:Number.isInteger(layers)?layers:0}))};
}
// A single Mongo document makes head + immutable version additions atomic.
export function mongoWhistlegraphStore(collection, {name=pronounceableCode}={}) {
  let indexes;
  const ready=()=>indexes??=collection.createIndex({codeKey:1},{unique:true});
  return {
    async open(owner,id) {
      if(!validID(id))throw Error('Invalid thread ID');
      await ready();
      const existing=await collection.findOne({_id:id,owner});
      if(existing)return existing;
      for(let attempt=0;attempt<100;attempt++) {
        const code=name(), row={_id:id,owner,code,codeKey:code.toLowerCase(),revision:0,ledger:null,updatedAt:new Date().toISOString()};
        try {await collection.insertOne(row);return row;} catch(error) {
          if(error.code!==11000)throw error;
          const claimed=await collection.findOne({_id:id});
          if(claimed){if(claimed.owner!==owner)throw Error('Thread unavailable');return claimed;}
        }
      }
      throw Error('Unable to reserve a name');
    },
    async read(owner,code) {if(!validCode(code))return null;return collection.findOne({owner,codeKey:code.toLowerCase()});},
    async list(owner) {return collection.find({owner},{projection:{owner:0,ledger:0}}).sort({updatedAt:-1}).limit(100).toArray();},
    async state(owner,id,state){await collection.updateOne({_id:id,owner},{$set:{diagnostics:state}});},
    async save(owner,id,revision,ledger) {
      ledger=validateLedger(ledger);
      const previous=await collection.findOne({_id:id,owner,revision});
      if(!previous)return null;
      for(const v of previous.ledger?.versions||[]) {
        const next=ledger.versions.find(n=>n.id===v.id);
        if(!next||JSON.stringify(next)!==JSON.stringify(v))throw Error('Saved versions are immutable');
      }
      const result=await collection.updateOne({_id:id,owner,revision},{$set:{ledger,updatedAt:new Date().toISOString()},$inc:{revision:1}});
      return result.modifiedCount?{...previous,ledger,revision:revision+1}:null;
    }
  };
}
export function publicThread(row) {
  if(!row)return null;
  const head=row.ledger?.versions.find(v=>v.id===row.ledger.head);
  return {id:row._id,code:row.code,revision:row.revision,ledger:row.ledger,head:head?.id,sourceHash:head?sourceHash(head.source):null,updatedAt:row.updatedAt,diagnostics:row.diagnostics};
}
