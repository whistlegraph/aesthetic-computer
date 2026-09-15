import sharp from 'sharp';
import { generate as generateProviderImage, FAL_FLARE } from '../../easel/media/picture/illy.mjs';
export const hostedImageModels = Object.freeze({openai: ['gpt-image-2'], fal: [FAL_FLARE]});
export function imageRequest(input) {
  if (!/^[a-zA-Z0-9_-]{16,80}$/.test(input.jobId || '')) throw new Error('Invalid image job.');
  if (typeof input.prompt !== 'string' || !input.prompt.trim() || input.prompt.length > 16000) throw new Error('Provide an image prompt.');
  const provider=input.provider || 'openai';
  const model=input.model || (provider === 'openai' ? 'gpt-image-2' : null);
  if (!hostedImageModels[provider]?.includes(model)) throw new Error('Unsupported hosted image provider/model.');
  if (provider === 'fal' && input.reference) throw new Error('This fal model supports generation only, not reference edits.');
  const size=input.size || '1024x1024', quality=input.quality || 'medium';
  if (!['1024x1024','1536x1024','1024x1536'].includes(size) || !['low','medium','high'].includes(quality)) throw new Error('Unsupported image settings.');
  if(input.reference && (typeof input.reference!=='string' || input.reference.length>12*1024*1024 || !/^[A-Za-z0-9+/]+={0,2}$/.test(input.reference)))throw new Error('Invalid reference image.');
  return {jobId:input.jobId,prompt:input.prompt,provider,model,size,quality,reference:input.reference};
}
export async function generateEaselImage(input,{key,fetchImpl=fetch}={}) {
  input=imageRequest(input);
  if (!key) throw new Error('Selected image provider is not configured.');
  if (input.provider === 'fal') {
    const result=await generateProviderImage({...input, mode:'generate'}, {env:{FAL_KEY:key},fetchImpl,signal:AbortSignal.timeout(300000)});
    const bytes=await sharp(result.bytes,{limitInputPixels:16*1024*1024}).ensureAlpha().png().toBuffer();
    if(bytes.length>8*1024*1024)throw new Error('Generated image is too large.');
    return {png:bytes.toString('base64'),provenance:{provider:'fal',model:input.model,mode:'generate',requestId:result.provenance.requestId,usage:result.provenance.usage,completedAt:result.provenance.completedAt}};
  }
  let body, headers={Authorization:`Bearer ${key}`};
  const parameters={model:input.model,prompt:input.prompt,size:input.size,quality:input.quality,n:1,output_format:'png'};
  if(input.reference) {
    const image=await sharp(Buffer.from(input.reference,'base64'),{limitInputPixels:16*1024*1024}).png().toBuffer();
    body=new FormData();for(const [name,value] of Object.entries(parameters))body.append(name,String(value));
    body.append('image[]',new Blob([image],{type:'image/png'}),'painting.png');
  } else {headers['Content-Type']='application/json';body=JSON.stringify(parameters);}
  const response=await fetchImpl(`https://api.openai.com/v1/images/${input.reference?'edits':'generations'}`,{method:'POST',headers,body,signal:AbortSignal.timeout(300000)});
  if(!response.ok)throw new Error(`Image provider returned HTTP ${response.status}. The request was not retried.`);
  const result=await response.json();
  if(!result.data?.[0]?.b64_json)throw new Error('Image provider returned no image.');
  const bytes=await sharp(Buffer.from(result.data[0].b64_json,'base64'),{limitInputPixels:16*1024*1024}).ensureAlpha().png().toBuffer();
  if(bytes.length>8*1024*1024)throw new Error('Generated image is too large.');
  return {png:bytes.toString('base64'),provenance:{provider:'openai',model:input.model,mode:input.reference?'edit':'generate',requestId:response.headers.get('x-request-id'),usage:result.usage||null,completedAt:new Date().toISOString()}};
}
