// Which parameter shape, if any, turns DeepSeek v4.1 flash's thinking off on
// OpenRouter? Reads OPENROUTER_API_KEY from vault/lith/.env in-process (never
// printed), sends one small request per shape, prints status + usage only.
// Run: node probe-openrouter.mjs [model]
import {readFileSync} from 'node:fs';
const env=readFileSync(new URL('../../../vault/lith/.env',import.meta.url),'utf8');
const key=env.match(/^OPENROUTER_API_KEY=(.+)$/m)?.[1]?.trim().replace(/^["']|["']$/g,'');
if(!key)throw Error('no OPENROUTER_API_KEY in vault/lith/.env');
const model=process.argv[2]||'deepseek/deepseek-v4.1-flash';
const prompt='Write a JavaScript function that draws a filled circle at the center of a screen object with width and height, using ink() and circle(). Code only.';
const headers={Authorization:`Bearer ${key}`,'Content-Type':'application/json','anthropic-version':'2023-06-01','HTTP-Referer':'https://aesthetic.computer','X-Title':'Walkieware probe'};
const shapes=[
  ['messages · baseline',{}],
  ['messages · reasoning.effort none',{reasoning:{effort:'none'}}],
  ['messages · reasoning.enabled false',{reasoning:{enabled:false}}],
  ['messages · thinking disabled',{thinking:{type:'disabled'}}],
  ['messages · reasoning.max_tokens 0',{reasoning:{max_tokens:0}}],
  ['chat · reasoning.effort none',null],
  ['chat · reasoning.enabled false',{enabled:false}],
];
for(const [label,extra] of shapes){
  const t0=performance.now();
  const chat=label.startsWith('chat');
  const url=chat?'https://openrouter.ai/api/v1/chat/completions':'https://openrouter.ai/api/v1/messages';
  const body=chat?{model,max_tokens:600,messages:[{role:'user',content:prompt}],reasoning:extra||{effort:'none'}}:{model,max_tokens:600,messages:[{role:'user',content:prompt}],...extra};
  try{
    const r=await fetch(url,{method:'POST',headers,body:JSON.stringify(body)});
    const text=await r.text();let j;try{j=JSON.parse(text);}catch{}
    const usage=j?.usage||{};
    const think=usage.output_tokens_details?.thinking_tokens??usage.completion_tokens_details?.reasoning_tokens??'?';
    const out=usage.output_tokens??usage.completion_tokens??'?';
    const stop=j?.stop_reason||j?.choices?.[0]?.finish_reason||'';
    const err=j?.error?.message||(r.ok?'':text.slice(0,120));
    console.log(label.padEnd(36),'→',r.status,String(Math.round(performance.now()-t0)).padStart(6)+'ms','thinking',String(think).padStart(5),'out',String(out).padStart(5),stop,err);
  }catch(e){console.log(label.padEnd(36),'→ fetch failed',e.message);}
}
