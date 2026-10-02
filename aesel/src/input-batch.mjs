// Only Enter-submitted lines enter this queue; an unfinished draft never does.
export const INPUT_SETTLE_MS = 650;
// Trim submitted lines, leaving indentation and spacing inside the message.
// The live editor keeps its spaces so the next word can still be typed.
export function trimSubmittedInput(value) {
 return String(value ?? '').replace(/\r\n?/g, '\n').replace(/[\t ]+$/gm, '').trim();
}
export function takeSubmittedBatch(queue) {
 if(!queue.length)return [];
 // A command goes alone. So does anything that is not a typed line — an inbox
 // message queued as an object — which ends the batch before it.
 const command=(item)=>typeof item==='string'&&item.startsWith('/');
 if(typeof queue[0]!=='string')return [];
 const stop=queue.findIndex((item,index)=>index>0&&(command(item)||typeof item!=='string'));
 const count=command(queue[0])?1:(stop<0?queue.length:stop);
 return queue.splice(0,count);
}
export function inputBatchDelay(lastInputAt, now=Date.now()) {
 return Math.max(0,INPUT_SETTLE_MS-(now-lastInputAt));
}
