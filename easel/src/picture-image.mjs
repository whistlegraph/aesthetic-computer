import { FAL_FLARE } from '../media/picture/illy.mjs';
import { ACSession, SITE, USER_AGENT } from './ac-session.mjs';
// The hosted path uses AC sign-in. Provider credentials stay on the server.
export async function hostedPictureImage(plan,{reference,jobId,fetchImpl=fetch,session=new ACSession(),site=SITE}={}) {
  if (!(plan.provider==='openai' && plan.model==='gpt-image-2') && !(plan.provider==='fal' && plan.model===FAL_FLARE)) throw new Error('This model requires your own provider key; it is not available through AC hosted images.');
  const token=await session.token();
  const response=await fetchImpl(`${site}/api/easel-image`,{method:'POST',headers:{Authorization:`Bearer ${token}`,'Content-Type':'application/json','User-Agent':USER_AGENT},body:JSON.stringify({...plan,jobId,reference:reference?.toString('base64')}),signal:AbortSignal.timeout(330000)});
  const result=await response.json();
  if(!response.ok)throw new Error(result.error||`Image request failed (${response.status}).`);
  if(!result.png)throw new Error('The image server returned no image.');
  return {bytes:Buffer.from(result.png,'base64'),provenance:result.provenance};
}
