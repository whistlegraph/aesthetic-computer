// Only Enter-submitted lines enter this queue; an unfinished draft never does.
export const INPUT_SETTLE_MS = 650;
export function takeSubmittedBatch(queue) {
 if(!queue.length)return [];
 const count=queue[0].startsWith('/')?1:(queue.findIndex(text=>text.startsWith('/'))<0?queue.length:queue.findIndex(text=>text.startsWith('/')));
 return queue.splice(0,count);
}
export function inputBatchDelay(lastInputAt, now=Date.now()) {
 return Math.max(0,INPUT_SETTLE_MS-(now-lastInputAt));
}
