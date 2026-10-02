import './harness-tui-fixture.mjs';
import {appendFileSync,writeFileSync} from 'node:fs';
import {join,dirname} from 'node:path';
import {BACKENDS} from '../src/backends.mjs';
import {FrameDiff} from '../src/frame-diff.mjs';
const root=dirname(process.env.AESEL_TEST_LOG);
const draw=FrameDiff.prototype.update;
FrameDiff.prototype.update=function(frame,columns){
  writeFileSync(join(root,'frame.json'),JSON.stringify({frame,columns}));
  return draw.call(this,frame,columns);
};
BACKENDS.codex.Engine.prototype.startTurn=async function(text){
  const emit=(method,params)=>this.emit('notification',{method,params});
  emit('turn/started',{turn:{id:'activity-turn'}});
  emit('item/agentMessage/delta',{itemId:'progress',delta:'I’m checking the interface.'});
  for(let i=0;i<12;i++) {
    const item={id:`tool-${i}`,type:'commandExecution',command:'RAW_SCRIPT_DO_NOT_PAINT',commandActions:[{type:i%2?'search':'read'}]};
    emit('item/started',{item});
    emit('item/commandExecution/outputDelta',{itemId:item.id,delta:'RAW_OUTPUT_DO_NOT_PAINT'});
    emit('item/completed',{item:{...item,exitCode:0}});
  }
  emit('item/started',{item:{id:'last-tool',type:'mcpToolCall',tool:'ac_preview'}});
  emit('thread/tokenUsage/updated',{threadId:'fixture',turnId:'activity-turn',tokenUsage:{total:{totalTokens:6400,reasoningOutputTokens:800},last:{totalTokens:6400,reasoningOutputTokens:800}}});
  appendFileSync(process.env.AESEL_TEST_LOG,JSON.stringify({event:'activity-ready'})+'\n');
  // The test releases this through a file after checking resize and the live frame.
  const {existsSync}=await import('node:fs');
  while(!existsSync(join(root,'finish')))await new Promise(resolve=>setTimeout(resolve,30));
  emit('item/completed',{item:{id:'last-tool',type:'mcpToolCall',tool:'ac_preview',status:'completed'}});
  emit('item/agentMessage/delta',{itemId:'final',delta:'The interface is ready.'});
  emit('turn/completed',{turn:{id:'activity-turn',status:'completed'}});
};
