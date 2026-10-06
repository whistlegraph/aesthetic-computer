import {createHash} from 'node:crypto';
import {braincellsFromCost} from './easel-paid-credits.mjs';
import {imageInputBound} from './easel-input-images.mjs';
import {movePrompt, PROMPT_VERSION} from './nopaint-move-prompt.mjs';

export const MODEL = 'fal-ai/flux-2/klein/4b/edit';
const fail = (status, message) => Object.assign(Error(message), {status});
// A server-configured flat tariff, disclosed before submission. No inferred
// megapixel minimum or unverified provider cost is silently billed to users.
export function moveOffer(usd) {
  if (!Number.isFinite(usd) || usd <= 0 || usd > 1) return null;
  const braincells = braincellsFromCost(usd);
  const quote = createHash('sha256').update(JSON.stringify([MODEL, '256-rgb-v2', PROMPT_VERSION, braincells])).digest('hex');
  return {id:'ac-klein', name:'FLUX.2 Klein 4B', model:MODEL, location:'AC cloud',
    braincells, quote, size:[256,256], previews:false};
}
export function moveInput(body) {
  if (!body || typeof body !== 'object' || Array.isArray(body)) throw fail(400, 'Expected a move');
  if (!/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i.test(body.requestId || '')) throw fail(400, 'Move ID must be a UUID');
  if (![.25,.5,.75].includes(body.strength) || !Number.isInteger(body.seed) || body.seed < 0 || body.seed >= 2**31) throw fail(400, 'Invalid move settings');
  try { imageInputBound({messages:[{type:'image',source:{type:'base64',media_type:'image/png',data:body.image}}]}); }
  catch { throw fail(400, 'Expected a complete PNG image'); }
  const png = Buffer.from(body.image,'base64');
  if (png.readUInt32BE(16)!==256 || png.readUInt32BE(20)!==256 || png[24]!==8 || ![2,6].includes(png[25])) throw fail(400, 'Expected a 256×256 RGB PNG');
  const hash = createHash('sha256').update(png).update(JSON.stringify([body.seed,body.strength,body.quote])).digest('hex');
  return {...body, hash};
}

export function createNoPaintProvider({key, fetch=globalThis.fetch, sleep=ms=>new Promise(r=>setTimeout(r,ms))}) {
  return async input => {
    const signal = AbortSignal.timeout(180_000);
    let handle;
    const request = async (url, method='GET', body) => {
      const target = new URL(url);
      if (target.origin !== 'https://queue.fal.run' || target.username || target.password) throw fail(502, 'Unexpected provider URL');
      const response = await fetch(url, {method, redirect:'error', signal,
        headers:{Authorization:`Key ${key}`, 'Content-Type':'application/json'},
        ...(body ? {body:JSON.stringify(body)} : {})});
      if (!response.ok) throw fail(502, `Image provider unavailable (HTTP ${response.status})`);
      return response.json();
    };
    try {
      handle = await request('https://queue.fal.run/'+MODEL, 'POST', {
        prompt:movePrompt(input),
        image_urls:['data:image/png;base64,'+input.image], image_size:{width:256,height:256},
        seed:input.seed, num_images:1, num_inference_steps:4, output_format:'png',
        sync_mode:true, enable_safety_checker:true,
      });
      while (true) {
        signal.throwIfAborted();
        const state = await request(handle.status_url);
        if (state.status === 'COMPLETED') {
          if (state.error) throw fail(502, 'Image generation failed');
          break;
        }
        if (!['IN_QUEUE','IN_PROGRESS'].includes(state.status)) throw fail(502, 'Unexpected provider status');
        await sleep(500);
      }
      const result = await request(handle.response_url), image = result.images?.[0]?.url;
      // sync_mode keeps image downloads and arbitrary external URLs out of this gateway.
      if (typeof image !== 'string' || image.length > 8_000_000 || !/^data:image\/png;base64,[A-Za-z0-9+/]+={0,2}$/.test(image)) throw fail(502, 'Image provider returned an invalid PNG');
      const {default:sharp} = await import('sharp');
      const output = await sharp(Buffer.from(image.split(',')[1],'base64'), {limitInputPixels:4_194_304})
        .resize(256,256,{fit:'fill'}).removeAlpha().png().toBuffer();
      return {image:output.toString('base64'), model:MODEL, width:256, height:256};
    } catch (error) {
      if (handle?.cancel_url) {
        try {
          const url = new URL(handle.cancel_url);
          if (url.origin === 'https://queue.fal.run' && !url.username && !url.password) await fetch(url, {
            method:'PUT', redirect:'error', headers:{Authorization:`Key ${key}`}, signal:AbortSignal.timeout(5000),
          });
        } catch {}
      }
      throw error;
    }
  };
}
