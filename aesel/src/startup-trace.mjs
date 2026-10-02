import {writeFileSync} from 'node:fs';

// Opt-in local timings only. Accumulate in memory so tracing does not add disk
// work before the prompt. No prompts, paths, credentials or tool data are logged.
const events=[];
export function markStartup(phase){
  if(process.env.AESEL_STARTUP_TRACE)events.push({phase,ms:performance.now(),...(phase==='imports'?{entry:process.env.AESEL_STARTUP_ENTRY||'tui.mjs'}:{})});
}
export function flushStartupTrace(){
  if(process.env.AESEL_STARTUP_TRACE){
    try{writeFileSync(process.env.AESEL_STARTUP_TRACE,JSON.stringify(events)+'\n',{mode:0o600});}catch{}
  }
}
