import {spaceMoves} from './space-moves.mjs';
// Explicit debug performance scenario. Each prompt edits the previous real piece.
export const balls = [
 {name:'Pink',rgb:[255,105,180]}, {name:'Cyan',rgb:[0,220,255]},
 {name:'Orange',rgb:[255,150,40]}, {name:'Green',rgb:[70,230,100]},
 {name:'Purple',rgb:[180,100,255]}, {name:'Yellow',rgb:[255,230,50]},
 {name:'Red',rgb:[245,60,70]}, {name:'Blue',rgb:[70,110,255]}
];
export const moves = balls.flatMap((ball,i)=>[
 `${i===0?'Start a fresh scene with only the existing pink circle. Animate it':'Add one more independent bouncing ball named '+ball.name}. Use exact RGB (${ball.rgb.join(',')}) for its opaque filled body. Make its radius 6% of the smaller screen dimension, and bounce within the full visible screen with radius-aware bounds. Move in both axes. ${i===0?'Clear previous unrelated features.':'Preserve all previous balls and their features.'} Ensure exactly ${i+1} balls total. The new ${ball.name} ball must start as a plain body: no trail, highlight, or counter yet. Those are separate upcoming edits. Use per-ball feature flags so enabling one does not change the others.`,
 `Add a visible fading trail specifically behind the ${ball.name} ball. Use its body RGB with ink alpha between 20 and 180 on the AC 0–255 scale. Keep 12 recent positions, draw trails before bodies, and preserve earlier features.`,
 `Add a small opaque white circular highlight inside the ${ball.name} ball, offset toward its upper left, about a quarter of its radius. Preserve earlier features.`,
 `Add a readable wall-hit counter for ${ball.name}. Draw the text "${ball.name}: N" with write(), replacing N with the increasing integer collision count. Keep it on screen; use two columns for multiple counters. Preserve earlier features.`
]);
export const scenarioContract = 'Use only standard JavaScript and the AC boot, sim, paint lifecycle with screen, wipe, ink, circle, and write APIs for this benchmark. Use numeric RGB colors. ink alpha is 0–255, not 0–1. Call sim once per frame; velocities should be 1–3 pixels per frame. Clear black and redraw each paint; do not return false from paint. Keep all geometry finite. Use the default write font, without a custom typeface. No external assets, clocks, randomness, network, or publishing. Size relative to screen. Preserve every feature from earlier moves.';
export function checksFor({finished,changed,painted,errors,source}) {
 return {finished,sourceChanged:changed,committedSourcePainted:painted,noRuntimeErrors:errors.length===0,sourceBounded:source.length<=100000};
}
export const sceneMoves = [
 'Replace the ball scene with a simple pixel girl standing on green ground against blue sky. Dark hair, pink dress, stick arms and legs. Large centered figure. Static. No rope yet. Keep this first drawing minimal.',
 'Animate the same girl gently jumping in place in a steady repeating rhythm. Feet clearly leave the ground and return. Keep her appearance and background; no rope yet.',
 'Add a skipping rope held at both hands. Animate a continuous curved rope turning around her, passing beneath her feet while she is airborne and overhead between jumps. Keep the girl and jumping rhythm. Draw enough connected line segments for a smooth rope, with strong contrast.',
 'Refine the same scene: small synchronized hand rotations, knees bending on landing, and pigtails bobbing. Preserve the continuous turning rope and jump timing. Crucial: compute left and right hand coordinates once and use those same coordinates as the exact first and last rope points every frame. Interpolate rope x and baseline y between those endpoints before adding the curved swing. Never leave the rope at old fixed hand positions.',
 'Polish the scene with a soft oval ground shadow that becomes smaller when she rises, a few simple flowers at the edges, and a small smiling face with clearly visible dark square eyes and a line smile below the hair. Keep the girl and rope clearly readable and the looping action smooth. No text or UI.'
];
const sceneContract = 'Use standard AC JavaScript lifecycle boot/sim/paint. Read screen dimensions at paint/sim time, use relative sizing. Use documented wipe, ink, line, box and circle drawing APIs and numeric RGB. No external assets, network, audio, randomness, publishing, or clock dependencies. Always redraw the complete scene each paint. Use a frame counter in sim for smooth deterministic animation. Preserve all previous scene features unless this ask changes them.';
export async function runSequence(api) {
 const delay=ms=>new Promise(resolve=>setTimeout(resolve,ms));
 while(!window.walkiewareAccountReady||!api.ready())await delay(100);
 const start=window.__walkiewareSequenceStart||1;
 const prompts=window.__walkiewareSpace?spaceMoves:window.__walkiewareScene?sceneMoves:moves;
 const contract=window.__walkiewareSpace||window.__walkiewareScene?sceneContract:scenarioContract;
 for(let index=start;index<=prompts.length;index++) {
  const before=api.source();const events=[];const errors=[];let finished=false;
  const began=performance.now();
  window.__walkiewareSequenceEvent=(event,fields={})=>{
   events.push({event,ms:Math.round(performance.now()-began),...fields});
   if(event==='generationFinished')finished=true;
   if(event==='runtimeError')errors.push(fields.message||'Preview error');
  };
  document.getElementById('live-keep').click();
  const deadline=setTimeout(()=>api.interrupt(),window.__walkiewareScene||window.__walkiewareSpace?120000:60000);
  try {await api.ask(window.__walkiewareSpace&&index===1?prompts[0]:prompts[index-1]+' '+contract+' Apply the change using small edit_piece layers; avoid rewriting unchanged code.');}
  finally {clearTimeout(deadline);}
  // Give the committed module time to load and expose delayed runtime failures.
  for(let n=0;n<50&&!api.painted();n++)await delay(100);
  await delay(750);
  const source=api.source();
  const report={index,total:prompts.length,prompt:prompts[index-1],requiresMotion:window.__walkiewareSpace?index>=5:!window.__walkiewareScene||index>1,motionReview:window.__walkiewareSpace&&index>=5,model:api.model,source,events,errors,
   scope:'Successive text edits on physical iPhone; audio excluded. Runtime and animation checks plus screenshots; feature semantics require visual review.',
   checks:{...checksFor({finished,changed:source.trim()!==before.trim(),painted:api.painted(),errors,source}),oneVersionPerAsk:events.filter(e=>e.event==='versionCommitted').length===1}};
  const result=await new Promise(resolve=>{
   window.__walkiewareSequenceCaptureDone=resolve;
   const rect=document.getElementById('live-piece').getBoundingClientRect();
   window.webkit.messageHandlers.walkie.postMessage({action:'sequenceCapture',id:'sequence',report,rect:{x:rect.x,y:rect.y,width:rect.width,height:rect.height}});
  });
  window.__walkiewareSequenceEvent=null;
  document.getElementById('live-phase').textContent=`Move ${index}/${prompts.length} ${result.passed?'passed':'failed'}`;
  if(!result.passed){if(events.some(e=>e.event==='versionCommitted'))document.getElementById('live-undo').click();return;}
 }
 document.getElementById('live-keep').click();
 document.getElementById('live-phase').textContent=`${prompts.length} moves tested`;
}
