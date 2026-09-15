export const MAX_FRAME_BYTES = 16 * 1024 * 1024;
export function validateMetadata(value, id) {
  if (!value || value.id !== id || !['picture','sound','paper','gameboy','piece'].includes(value.kind)
    || !Number.isSafeInteger(value.sequence) || value.sequence < 1
    || !Number.isSafeInteger(value.version) || value.version < 1
    || !['live','stopped'].includes(value.status) || typeof value.mime !== 'string') throw new Error('Invalid preview metadata');
  return value;
}
async function readFrame(response) {
  if (Number(response.headers?.get('content-length')) > MAX_FRAME_BYTES) throw new Error('Preview is too large');
  if (!response.body?.getReader) { const bytes=await response.arrayBuffer(); if(bytes.byteLength>MAX_FRAME_BYTES)throw new Error('Preview is too large');return bytes; }
  const reader=response.body.getReader(), chunks=[];let length=0;
  try { while(true){const{done,value}=await reader.read();if(done)break;length+=value.length;if(length>MAX_FRAME_BYTES){await reader.cancel();throw new Error('Preview is too large');}chunks.push(value);} }
  finally {reader.releaseLock();}
  const bytes=new Uint8Array(length);let offset=0;for(const chunk of chunks){bytes.set(chunk,offset);offset+=chunk.length;}return bytes.buffer;
}
export class LiveWatch {
  constructor({id,fetch=globalThis.fetch.bind(globalThis),onFrame=async()=>{},onStatus=()=>{},onEnd=()=>{},onError=()=>{},schedule=(fn,delay)=>globalThis.setTimeout(fn,delay),cancel=timer=>globalThis.clearTimeout(timer)}={}) {
    if(!/^[a-f0-9]{32}$/.test(id||''))throw new Error('This watch link is incomplete');
    Object.assign(this,{id,fetch,onFrame,onStatus,onEnd,onError,schedule,cancel});
    this.visible=true;this.running=false;this.sequence=0;this.retry=750;this.epoch=0;
  }
  start(){if(this.running)return;this.running=true;this.queue(0);}
  stop(){this.running=false;this.epoch++;this.cancel(this.timer);this.controller?.abort();}
  setVisible(visible){this.visible=visible;this.epoch++;this.cancel(this.timer);this.controller?.abort();if(visible&&this.running)this.queue(0);}
  queue(delay){this.cancel(this.timer);if(this.running&&this.visible)this.timer=this.schedule(()=>this.poll(),delay);}
  async poll(){
    if(!this.running||!this.visible||this.inFlight)return;
    this.inFlight=true;const epoch=this.epoch,controller=new AbortController();this.controller=controller;
    const timeout=this.schedule(()=>controller.abort(),15000);
    let delay=750;
    try{
      const options={credentials:'omit',cache:'no-store',referrerPolicy:'no-referrer',signal:controller.signal};
      const response=await this.fetch(`/api/easel-live?id=${this.id}`,options);
      if(response.status===404||response.status===410){this.stop();this.onEnd(response.status===410?'stopped':'expired');return;}
      if(!response.ok)throw new Error('Could not connect to this preview');
      const metadata=validateMetadata(await response.json(),this.id);
      if(epoch!==this.epoch||!this.running)return;
      if(metadata.status==='stopped'){this.stop();this.onEnd('stopped');return;}
      this.onStatus(metadata,this.sequence);
      if(metadata.sequence>this.sequence){
        const frame=await this.fetch(`/api/easel-live?id=${this.id}&frame=${metadata.sequence}`,options);
        if(frame.status===409){delay=250;return;}
        if(frame.status===404||frame.status===410){this.stop();this.onEnd(frame.status===410?'stopped':'expired');return;}
        if(!frame.ok)throw new Error('Could not load the new version');
        const bytes=await readFrame(frame);
        if(epoch!==this.epoch||!this.running)return;
        await this.onFrame(metadata,bytes,controller.signal);
        if(epoch!==this.epoch||!this.running)return;
        this.sequence=metadata.sequence;
        this.onStatus(metadata,this.sequence);
      }
      this.retry=750;
    }catch(error){
      if(epoch===this.epoch&&this.running&&this.visible){this.onError(error.name==='AbortError'?'Connection timed out; reconnecting':error.message);delay=this.retry;this.retry=Math.min(10000,this.retry*2);}
    }finally{
      this.cancel(timeout);this.inFlight=false;if(this.controller===controller)this.controller=null;
      if(this.running&&this.visible)this.queue(epoch===this.epoch?delay:0);
    }
  }
}
