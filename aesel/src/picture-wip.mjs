// Pictures use the same public WIP/code/state as AC and No Paint.
import { readFile, writeFile, mkdir, rename, lstat } from 'node:fs/promises';
import { join } from 'node:path';
import { randomBytes, createHash } from 'node:crypto';
import { gzipSync } from 'node:zlib';
import { SITE, USER_AGENT } from './ac-session.mjs';
import { decode } from '../media/picture/png.mjs';
import { createNoPaintPiece, createNoPaintProposalLayer, appendNoPaintLayer, sameNoPaintPixels, JSZip } from '../media/picture/ac-tools.mjs';
const queues = new WeakMap();
const hash = bytes => createHash('sha256').update(bytes).digest('hex');
const owner = session => session.read?.()?.user?.sub || session.handle || 'guest';
const filename = (artifacts, id) => join(artifacts.root, 'painting-wips', `${id}.json`);
export async function pictureWipRecord(artifacts, id) {
  if (!/^[a-f0-9-]{36}$/.test(id || '')) throw new Error('Invalid picture identity.');
  const file = filename(artifacts,id);
  try {
    if ((await lstat(file)).isSymbolicLink()) throw new Error('WIP record cannot be a symlink.');
    return JSON.parse(await readFile(file,'utf8'));
  } catch (error) { if(error.code==='ENOENT')return null;throw error; }
}
export async function savePictureWipRecord(artifacts, id, record) {
  const dir=join(artifacts.root,'painting-wips');
  await mkdir(dir,{recursive:true,mode:0o700});
  if((await lstat(dir)).isSymbolicLink())throw new Error('WIP directory cannot be a symlink.');
  const temp=filename(artifacts,id)+`.${randomBytes(8).toString('hex')}.tmp`;
  await writeFile(temp,JSON.stringify(record),{mode:0o600,flag:'wx'});
  await rename(temp,filename(artifacts,id));
}
export function pictureWipAddress(record, site=SITE) {
  if(!record?.code)return null;
  return {code:record.code,tag:`#${record.code}`,status:record.status,route:`${site}/#${record.code}`,scanUrl:`${new URL(site).host}/#${record.code}`,steps:record.steps,revision:record.revision};
}
export async function pictureState(current) {
  let piece;
  for(let version=1;version<=current.version;version++) {
    const root=join(current.root,'..',`v${version}`);
    const revision=JSON.parse(await readFile(join(root,'revision.json'),'utf8'));
    const bytes=await readFile(join(root,'composite.png'));
    if(hash(bytes)!==revision.hashes['composite.png'])throw new Error('Painting history hash mismatch.');
    const image=decode(bytes), canvas={width:image.width,height:image.height,pixels:new Uint8ClampedArray(image.data)};
    if(!piece) {piece=createNoPaintPiece({seed:current.id,...canvas,role:current.parent?'fork':'easel'});piece.layers[0].timestamp=revision.createdAt;}
    else if(!sameNoPaintPixels(piece.composite,canvas)) {
      piece={...piece,width:canvas.width,height:canvas.height};
      const state=JSON.parse(await readFile(join(root,'picture.json'),'utf8'));
      const layer=createNoPaintProposalLayer({piece,proposal:{kind:'easel',summary:revision.summary,brush:state.layers.at(-1)?.brush,score:state.layers.at(-1)?.score},pixels:canvas.pixels,pixelMode:'composite'});
      layer.label=`easel~${version}`;layer.timestamp=revision.createdAt;
      piece=appendNoPaintLayer(piece,layer,canvas.pixels);
    }
  }
  if(current.parent)piece.parent=current.parent;
  return piece;
}
export function encodePictureState(piece) {
  return gzipSync(Buffer.from(JSON.stringify({format:'aesthetic.computer/painting-state',version:1,piece},(_,v)=>ArrayBuffer.isView(v)?{$pixels:Buffer.from(v.buffer,v.byteOffset,v.byteLength).toString('base64')}:v))).toString('base64');
}
export function syncPictureWip(options) {
  const {artifacts}=options;
  const running=(queues.get(artifacts)||Promise.resolve()).catch(()=>{}).then(()=>sync(options));
  queues.set(artifacts,running);return running;
}
async function sync({artifacts,session,fetch=globalThis.fetch,site=SITE}) {
  const current=await artifacts.selected();if(current?.kind!=='picture')return null;
  let record=await pictureWipRecord(artifacts,current.id);
  if(record && record.owner!==owner(session))throw new Error('Sign into the account that owns this WIP, or start a copy.');
  const piece=await pictureState(current);
  const request=async body=>{
    const token=session.signedIn?await session.token():null;
    const response=await fetch(`${site}/api/painting-wip`,{method:'POST',headers:{'Content-Type':'application/json','User-Agent':USER_AGENT,...(token?{Authorization:`Bearer ${token}`}:{})},body:JSON.stringify(body),signal:AbortSignal.timeout(45000)});
    const result=await response.json();if(!response.ok)throw new Error(result.error||`Painting save failed (${response.status}).`);return result;
  };
  if(!record) {
    record={id:randomBytes(24).toString('hex'),key:randomBytes(24).toString('hex'),owner:owner(session),status:'wip',revision:0};
    await savePictureWipRecord(artifacts,current.id,record);
  }
  if(!record.code) {
    Object.assign(record,await request({action:'create',id:record.id,key:record.key,width:piece.width,height:piece.height,initialLayers:1,parent:current.parent}));
    await savePictureWipRecord(artifacts,current.id,record);
  }
  if(record.status==='done')return {record,piece,artifactId:current.id};
  const state=encodePictureState(piece), digest=hash(state);
  if(record.hash!==digest) {
    Object.assign(record,await request({action:'save',code:record.code,key:record.key,revision:record.revision,state}),{hash:digest});
    await savePictureWipRecord(artifacts,current.id,record);
  }
  return {record,piece,artifactId:current.id};
}

export async function pictureRecording(piece) {
  const zip=new JSZip();
  const steps=[];
  for(let i=0;i<piece.layers.length;i++) {
    const layer=piece.layers[i], p=layer.pixels;
    const step=`${layer.timestamp || i} - ${layer.label || 'easel~'+i}`;
    steps.push({step});
    const {encode}=await import('../media/picture/png.mjs');
    zip.file(`${step}.png`,encode({width:p.width,height:p.height,data:Buffer.from(p.data)}));
  }
  zip.file('painting.json',JSON.stringify(steps));
  return zip.generateAsync({type:'nodebuffer',compression:'DEFLATE'});
}
