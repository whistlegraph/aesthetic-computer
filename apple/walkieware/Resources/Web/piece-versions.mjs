// One durable version per successful ask; intermediate layers stay provisional.
export class PieceVersions {
 constructor(storage,key,initialSource=''){
  this.storage=storage;this.key=key;
  const saved=storage.getItem(key);
  if(saved){this.value=JSON.parse(saved);if(this.value.format!==1||!Array.isArray(this.value.versions)||!this.head)throw Error('Invalid piece history');}
  else {
   let cached;try{cached=JSON.parse(storage.getItem(key.replace(/-versions$/,'')+'-cloud-ledger'));}catch{}
   this.value=cached?.format===1&&cached.head===0&&cached.versions?.length===1&&cached.versions[0].source===initialSource?cached:{format:1,head:0,versions:[{id:0,parent:null,source:initialSource,request:null,createdAt:new Date().toISOString()}]};
   this.persist(this.value);
  }
 }
 get head(){return this.value.versions.find(v=>v.id===this.value.head);}
 persist(next){this.storage.setItem(this.key,JSON.stringify(next));this.value=next;return this.head;}
 commit({source,request,layers,parent=this.value.head,requestID}){
  if(requestID){const saved=this.value.versions.find(v=>v.requestID===requestID);if(saved)return saved;}
  if(parent!==this.value.head)throw Error('Version changed during this ask');
  const version={...(requestID?{requestID}:{}),id:Math.max(...this.value.versions.map(v=>v.id))+1,parent,source,request,layers,createdAt:new Date().toISOString()};
  return this.persist({...this.value,head:version.id,versions:[...this.value.versions,version]});
 }
 checkout(id){if(!this.value.versions.some(v=>v.id===id))throw Error('Version unavailable');return this.persist({...this.value,head:id});}
 undo(){if(this.head.parent===null)return this.head;return this.persist({...this.value,head:this.head.parent});}
}
