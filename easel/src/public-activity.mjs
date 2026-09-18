// Public agent text and observed tool stages only; never private reasoning or tool payloads.
export function toolActivity(item={}) {
 const name=String(item.tool||'').split(' · ')[0];
 if(item.type==='fileChange')return 'editing the piece';
 if(/(?:^|__)ac_frame$/.test(name))return 'looking at the preview';
 if(/(?:^|__)ac_preview$/.test(name))return 'checking the preview';
 if(/(?:^|__)(?:Read|read_file|ac_symbol|ac_outline)$/.test(name))return 'reading source';
 if(/(?:^|__)(?:ac_api|ac_examples|ac_references|Grep|Glob)$/.test(name))return 'looking up details';
 if(item.type==='commandExecution')return 'running a command';
 return 'using a tool';
}
export function publicActivity(state) {
 if(state.connectionNotice)return state.connectionNotice;
 if(state.previewNotice&&!state.busy)return state.previewNotice.replace('editing · ','code needs correction · ');
 if(!state.busy)return '';
 if(state.status==='approval')return 'waiting for approval';
 if(state.status==='interrupting')return 'stopping';
 const text=String(state.activityText||'').replace(/\s+/g,' ').trim();
 if(text)return text.length>160?'…'+text.slice(-159):text;
 return state.activityStage||({preparing:'preparing',connecting:'connecting',waiting:'waiting for the agent',generating:'composing a reply',approval:'waiting for approval',interrupting:'stopping'}[state.status])||'working';
}
