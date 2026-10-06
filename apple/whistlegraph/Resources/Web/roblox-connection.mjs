import {validateRoom} from './room-schema.mjs';
export function launchURL(value){
  const url=new URL(value);
  if(url.protocol!=='https:'||url.hostname!=='www.roblox.com'||url.pathname!=='/share'||url.username||url.password||url.port||!url.searchParams.get('code'))throw Error('The Roblox launch link is invalid.');
  return url.href;
}
// Journal before sending. Replaying a lost response uses the same idempotency
// key; a later edit waits until that previous save has been reconciled.
export async function saveForLaunch({storage,owner,room,request,uuid=()=>crypto.randomUUID()}){
  if(!owner)throw Error('Wait for your account to connect.');
  const key='whistlegraph-roblox-cloud-'+owner,source=JSON.stringify(validateRoom(room));
  const settle=async pending=>{
    validateRoom(pending.room);
    const result=await request('POST',pending);
    if(!Number.isSafeInteger(result.revision)||result.revision!==pending.expectedRevision+1)throw Error('The room save was not acknowledged.');
    const url=launchURL(result.launchURL);
    storage.setItem(key,JSON.stringify({revision:result.revision,source:JSON.stringify(pending.room)}));
    storage.removeItem(key+'-pending');return {...result,launchURL:url};
  };
  const saved=storage.getItem(key+'-pending');
  if(saved){const pending=JSON.parse(saved),result=await settle(pending);if(JSON.stringify(pending.room)===source)return result;}
  const prior=JSON.parse(storage.getItem(key)||'null');
  const pending={action:'save',expectedRevision:prior?.revision||0,room:JSON.parse(source),requestId:uuid()};
  storage.setItem(key+'-pending',JSON.stringify(pending));
  return settle(pending);
}
