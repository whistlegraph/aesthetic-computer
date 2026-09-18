// Token colors only: never execute source or accept terminal escapes from it.
import {tokenizer} from './vendor/acorn.mjs';
const cache=new Map();
export function syntaxSpans(source) {
  if(cache.has(source))return cache.get(source);
  const spans=[];
  const scan=tokenizer(source,{ecmaVersion:'latest',onComment:(block,text,start,end)=>spans.push({start,end,tone:'muted'})});
  try {
    while(true){
      const t=scan.getToken(),label=t.type.label;if(label==='eof')break;
      const tone=t.type.keyword?'prompt':['string','regexp','template','`'].includes(label)?'status':label==='num'?'highlight':label==='name'?'soft':null;
      if(tone)spans.push({start:t.start,end:t.end,tone});
    }
  } catch { /* Streaming/incomplete source remains readable. */ }
  spans.sort((a,b)=>a.start-b.start);
  if(cache.size>=32)cache.delete(cache.keys().next().value);
  cache.set(source,spans);return spans;
}
export function syntaxLine(source,spans,start,end,paint){
  let at=start,out='';
  for(const span of spans){
    if(span.end<=start)continue;if(span.start>=end)break;
    const from=Math.max(start,span.start),to=Math.min(end,span.end);
    if(from>at)out+=paint('text',source.slice(at,from));
    out+=paint(span.tone,source.slice(from,to),span);at=to;
  }
  return out+paint('text',source.slice(at,end));
}
