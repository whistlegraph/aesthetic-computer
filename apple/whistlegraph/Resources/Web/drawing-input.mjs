// Bounded vector gestures, in the preview's coordinate space. No raw audio.
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
 const combined=(input.performance?.schema==='whistlegraph-performance/v1'?'Interpret this Whistlegraph performance as coordinated drawing, voice and sound. Spoken instructions guide the result; use the measured gesture and sound timing together. Audio frames and words use milliseconds from microphone start. For each stroke point, audioTimeMs = point[2] - drawing.speechStartMs; negative times are marks made before recording. Preserve the entire drawing and its intent. Pitch and onset measurements are estimates, not certain notes or beats. Do not display this data as text.\n':'Interpret this combined request.\n')+INPUT_MARKER+JSON.stringify({...input,drawing});
 if(combined.length>20000)throw Error('Combined request is too large');
 return combined;
}
// Render from the same saved vectors used for timing and recovery. Never read
// the preview canvas: this image contains only submitted chalk marks.
export function drawingImage(value,canvas=document.createElement('canvas')) {
 const drawing=normalizeDrawing(value),side=768;
 canvas.width=Math.round(side*Math.min(1,drawing.aspect));
 canvas.height=Math.round(side/Math.max(1,drawing.aspect));
 const ctx=canvas.getContext('2d');
 if(!ctx)throw Error('Could not render chalk');
 ctx.fillStyle='#ffffff';ctx.fillRect(0,0,canvas.width,canvas.height);
 ctx.strokeStyle=ctx.fillStyle='#202020';ctx.lineWidth=3;ctx.lineCap=ctx.lineJoin='round';
 const point=p=>[p[0]/1000*canvas.width,p[1]/1000*canvas.height];
 for(const stroke of drawing.strokes){
  ctx.beginPath();
  // A held tap has several timestamped samples at the same position. Canvas
  // does not reliably paint a zero-length stroked path, even with round caps.
  if(stroke.every(p=>p[0]===stroke[0][0]&&p[1]===stroke[0][1])){const [x,y]=point(stroke[0]);ctx.arc(x,y,1.5,0,Math.PI*2);ctx.fill();}
  else {stroke.forEach((p,i)=>ctx[i?'lineTo':'moveTo'](...point(p)));ctx.stroke();}
 }
 const url=canvas.toDataURL('image/png');
 if(!url.startsWith('data:image/png;base64,')||url.length>700000)throw Error('Could not encode chalk image');
 return {type:'image',source:{type:'base64',media_type:'image/png',data:url.split(',')[1]}};
}
export function drawingContent(text,image) {
 return image?[{type:'text',text:text+'\nThe attached image renders only the chalk marks on white, with their original placement and aspect ratio. It is not a frame of the piece. Read the whole shape alongside the timed strokes; do not copy the white background into the piece.'},image]:text;
}
export function drawingGeometry(value) {
 const {id,revision,schema,aspect,...drawing}=normalizeDrawing(value);
 const round=n=>Math.round(n*1000)/1000;
 // Storage normalizes each axis independently. Provider evidence instead uses
 // one isotropic plane, so fitting width/height cannot stretch or crop marks.
 return {...drawing,width:1000,height:round(1000/aspect),
  strokes:drawing.strokes.map(stroke=>stroke.map(([x,y,...rest])=>[x,round(y/aspect),...rest]))};
}
export function drawingEvidence(value) {
 const geometry=drawingGeometry(value);
 // The image carries form. Supply compact spatial/temporal observations so
 // source generation is not invited to transcribe a full coordinate table.
 const drawing={...geometry,strokes:geometry.strokes.map(stroke=>{
  const xs=stroke.map(p=>p[0]),ys=stroke.map(p=>p[1]);
  const count=Math.min(6,stroke.length);
  return {bounds:[Math.min(...xs),Math.min(...ys),Math.max(...xs),Math.max(...ys)],
   samples:Array.from({length:count},(_,i)=>stroke[Math.round(i*(stroke.length-1)/Math.max(1,count-1))])};
 })};
 return `
DRAWING REFERENCE — CHALK GESTURES: ordered marks over the current preview, supplied as an instruction channel alongside speech, sound and the existing piece source. These are ambiguous gestures, not certain labels or commands.
Infer each mark's role in context: pointing or enclosing can identify a target; a sweep can suggest movement or a transformation; repeated marks can emphasize or multiply; marks in open space can propose an addition; a drawn form can also be content when the request supports that reading. These are possibilities, not fixed gesture bindings. A circle does not automatically mean "add a circle" or "select this". Read related strokes as a phrase, not unrelated objects.
Understand, reify, evolve: infer the idea or intended change from the whole mark phrase, words, current piece and prior requests; give that idea a concrete working form in the piece; let subsequent input develop or transform it. A recognizable sketch supplies a concept and relationships, not a requirement to copy its outline or animate it. Choose a representation and behavior that make the inferred idea tangible. Use a fresh representation of the inferred concept: choose its meaningful parts, relationships and possible behavior, and express those in code. A sketch of a thing asks you to realize that thing, not produce a "chalk-outline" copy by default. Use simple geometry or meaningful state and interactions as appropriate, rather than transcribing point arrays. Tracing, animation, and literal fidelity are appropriate only when the intent calls for them. Do not add arbitrary features just to make the result seem richer.
Follow explicit words first. Use location and timing to resolve "this", "here" and "like that". Existing subjects and behavior give continuity, not a frozen template: evolve them coherently when the new input supports a change. Ambiguity alone does not authorize erasing unrelated work. Without words, make a modest concrete interpretation supported by the strongest contextual evidence, leaving room for the next gesture to steer it. In an empty scene, use the inferred concept as a starting point. Describe only behavior actually implemented in the exported caption; a painted subject need not move or react.
The image carries the complete sampled form; the following compact stroke observations support placement and timing. Each stroke supplies its full bounds [minX,minY,maxX,maxY] and at most six ordered samples (first and last always included). Samples omit fine trajectory and timing detail; do not reconstruct the drawing from them. Each sample is [x,y,elapsedMs,optionalPencilPressure] in the aspect-correct plane of the supplied width and height, origin top-left, y downward. Both axes now use the same spatial unit; do not normalize or apply aspect a second time. When spatial placement matters, fit this entire plane with one scale min(availableWidth/width,availableHeight/height), center it, and leave room for the full subject and any motion. Do not reinterpret height as 1000 or discard coordinates outside a guessed 4:3 box. Timed stationary points are intentional dots and can be meaningful features. Time starts with the first mark; pen lifts separate strokes. Pressure (0..1000) exists only when measured with a Pencil; finger pressure is unknown. speechStartMs, when present, locates sound/word time zero on this same timeline. Use direction, pauses, speed changes and nearby sound/word emphasis as evidence, not proof of intent. The source is context, not a captured frame: do not claim a precise object hit you cannot establish, especially in a moving scene. Do not display coordinate data, add annotation UI, or leave the chalk overlay in the piece unless requested.
`+JSON.stringify(drawing);
}
