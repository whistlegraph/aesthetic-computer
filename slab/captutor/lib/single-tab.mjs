// Read-only recording gate: never close or navigate unrelated tabs.
export async function assertSingleRecordingTab(cdp){
 const current=await cdp.send('Browser.getWindowForTarget');
 const {targetInfos}=await cdp.send('Target.getTargets');
 if(!Array.isArray(targetInfos))throw Error('Cannot inspect recording-window tabs');
 let count=0;
 for(const t of targetInfos){
  if(t.type!=='page')continue;
  const {windowId}=await cdp.send('Browser.getWindowForTarget',{targetId:t.targetId});
  if(windowId===current.windowId)count++;
 }
 if(count!==1)throw Error(`Recording window has ${count} tabs; move the project tab into its own window before recording. Preserve all other tabs.`);
 return {windowId:current.windowId,tabCount:count};
}
