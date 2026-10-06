import {createHash} from 'node:crypto';
import {braincellsFromCost} from './easel-paid-credits.mjs';

// Reviewed image-in/image-out profiles. Discovery is broader than this allowlist.
export const profiles = {
  'black-forest-labs/flux.2-klein-4b': {name:'FLUX.2 Klein 4B', options:{aspect_ratio:'1:1', output_format:'png', seed:true}},
  'google/gemini-3.1-flash-image': {name:'Nano Banana 2', options:{aspect_ratio:'1:1', resolution:'512'}},
  'openai/gpt-image-1-mini': {name:'GPT Image 1 Mini', options:{aspect_ratio:'1:1', quality:'low'}},
  'openai/gpt-image-2.5-flare': {name:'GPT Image 2.5 Flare', options:{aspect_ratio:'1:1', quality:'low'}},
};

export function openRouterOffers(configuration='[]') {
  let settings;
  try { settings=JSON.parse(configuration); } catch { return []; }
  if (!Array.isArray(settings)) return [];
  const found=new Set();
  return settings.flatMap(entry=>{
    if (!entry || typeof entry!=="object") return [];
    const {model,usd}=entry;
    const profile=Object.hasOwn(profiles,model)?profiles[model]:null;
    if (!profile || !Number.isFinite(usd) || usd<=0 || usd>1 || found.has(model)) return [];
    found.add(model);
    const braincells=braincellsFromCost(usd);
    const quote=createHash('sha256').update(JSON.stringify([model,'openrouter-256-rgb-v1',profile.options,braincells])).digest('hex');
    return [{id:'ac-openrouter:'+model, name:profile.name, model, provider:'openrouter', location:'AC cloud · OpenRouter',
      braincells, quote, size:[256,256], previews:false}];
  });
}

export function createOpenRouterProvider({key, fetch=globalThis.fetch}) {
  return async (input, offer)=>{
    const profile=Object.hasOwn(profiles,offer.model)?profiles[offer.model]:null;
    if (!profile) throw Error('Unconfigured image model');
    const amount={'.25':'small','.5':'medium','.75':'large'}[String(input.strength).replace(/^0/,'')];
    const {seed,...options}=profile.options;
    const response=await fetch('https://openrouter.ai/api/v1/images', {
      method:'POST', redirect:'error', signal:AbortSignal.timeout(180_000),
      headers:{Authorization:`Bearer ${key}`, 'Content-Type':'application/json'},
      body:JSON.stringify({model:offer.model, ...options, ...(seed?{seed:input.seed}:{}), n:1,
        prompt:`Make exactly one ${amount} abstract painting move on this image: change a texture, color relationship, shape, or spatial arrangement. Preserve most of the existing image. Return the complete updated image. Do not add text, borders, or a depicted scene.`,
        input_references:[{type:'image_url',image_url:{url:'data:image/png;base64,'+input.image}}],
      }),
    });
    if (!response.ok) throw Object.assign(Error(`Image provider unavailable (HTTP ${response.status})`),{status:502});
    // Bound bytes before parsing; never follow a provider-controlled image URL.
    let length=0; const chunks=[];
    for await(const chunk of response.body) {
      length+=chunk.length;
      if(length>24_000_000) throw Error('Image response too large');
      chunks.push(chunk);
    }
    const result=JSON.parse(Buffer.concat(chunks).toString()), image=result.data?.[0];
    if (!image || typeof image.b64_json!=='string' || !/^[A-Za-z0-9+/]+={0,2}$/.test(image.b64_json)
        || (image.media_type && !['image/png','image/jpeg','image/webp'].includes(image.media_type))) throw Error('Invalid image response');
    const {default:sharp}=await import('sharp');
    const output=await sharp(Buffer.from(image.b64_json,'base64'), {limitInputPixels:16_777_216})
      .resize(256,256,{fit:'fill'}).removeAlpha().png().toBuffer();
    return {image:output.toString('base64'), model:offer.model, width:256, height:256,
      provider_cost_usd:Number.isFinite(result.usage?.cost)?result.usage.cost:null};
  };
}
