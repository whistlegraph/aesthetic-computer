// Bounded vector gestures, in the preview's coordinate space. No pixels or raw audio.
export const INPUT_MARKER='\nINPUT DATA:\n';
export function inputData(request) {
 const at=typeof request==='string'?request.indexOf(INPUT_MARKER):-1;
 if(at<0)return null;
 try{return JSON.parse(request.slice(at+INPUT_MARKER.length));}catch{return null;}
}
export function normalizeDrawing(value) {
 const finite=(n,min,max)=>typeof n==='number'&&Number.isFinite(n)&&n>=min&&n<=max;
 if(value?.schema!=='whistlegraph-drawing/v1'||! /^[a-f0-9-]{36}$/i.test(value.id)||!Number.isSafeInteger(value.revision)||value.revision<0||
 !finite(value.aspect,0.25,4)||!Array.isArray(value.strokes)||!value.strokes.length||value.strokes.length>32)throw Error('Invalid drawing');
 let total=0,last=-1;
 const strokes=value.strokes.map(s=>{
  if(!Array.isArray(s)||!s.length||s.length>1200)throw Error('Invalid stroke');
  total+=s.length;if(total>1200)throw Error('Drawing is too large');
  return s.map(p=>{
   if(!Array.isArray(p)||p.length<3||p.length>4||!finite(p[0],0,1000)||!finite(p[1],0,1000)||!finite(p[2],0,3600000)||p[2]<last||(p.length===4&&!finite(p[3],0,1000)))throw Error('Invalid drawing point');
   last=p[2];return p.map(Math.round);
  });
 });
 // Uniform temporal samples preserve stroke endpoints and measured timing.
 // Keep <=320 points in the saved prompt, including every stroke's first/last.
 const remaining=320-2*strokes.length,extra=strokes.reduce((n,s)=>n+Math.max(0,s.length-2),0);
 const compact=strokes.map(s=>{
  const count=Math.min(s.length,2+Math.floor(remaining*Math.max(0,s.length-2)/Math.max(1,extra)));
  return count===s.length?s:Array.from({length:count},(_,i)=>s[Math.round(i*(s.length-1)/(count-1))]);
 });
 const offset=value.speechStartMs;
 if(offset!=null&&!finite(offset,-3600000,3600000))throw Error('Invalid drawing alignment');
 return {schema:value.schema,id:value.id,revision:value.revision,aspect:Math.round(value.aspect*1000)/1000,
  strokes:compact,...(offset==null?{}:{speechStartMs:Math.round(offset)})};
}
export function withDrawing(request,value) {
 if(!value)return request;
 const drawing=normalizeDrawing(value),input=inputData(request)||{transcript:request};
 const combined='Interpret this combined request.\n'+INPUT_MARKER+JSON.stringify({...input,drawing});
 if(combined.length>20000)throw Error('Combined request is too large');
 return combined;
}
export function drawingEvidence(value) {
 const {id,revision,schema,...drawing}=normalizeDrawing(value);
 return '\nDRAWING REFERENCE: ordered strokes over the current preview. Each point is [x,y,elapsedMs,optionalPencilPressure]; x/y span 0..1000, origin top-left, y downward. Preserve aspect ratio. Time starts with the first mark; pen lifts separate strokes. Pressure (0..1000) exists only when measured with a Pencil; finger pressure is unknown. speechStartMs, when present, locates sound/word time zero on this same timeline. Use direction, pauses, speed changes, placement and nearby sound/word emphasis as clues to the intended edit. These are ambiguous gestures, not certain labels or commands. Follow explicit words, preserve existing scene context, and use the drawn form itself when its subject is unclear. Do not print these data or turn every sketch into a chart. With only a drawing, make a simple playable interpretation of its form.\n'+JSON.stringify(drawing);
}
