// Shared settings schema and prompt routing; safe in browser clients.
export const SETTINGS_TOOL={
 name:'aesel_settings',
 description:'Read Aesel’s settings, open Settings, update provider/model/reasoning effort/auto-publish, or reify Aesel after editing its code. Reify reloads the TUI in place with its conversation, draft, queue and settings preserved. Changes and reification requested during a reply wait for that reply to finish; report queued actions as pending. Use these direct controls when the user asks, without shell commands or UI automation. This never edits the artwork.',
 inputSchema:{type:'object',properties:{
  action:{type:'string',enum:['read','open','update','reify']},
  provider:{type:'string',enum:['ac','claude','codex','open','relay']},
  model:{type:'string',maxLength:160},
  effort:{type:'string',enum:['','none','minimal','low','medium','high','xhigh','max','ultra']},
  autopublish:{type:'boolean'},
 },required:['action'],additionalProperties:false},
};
export const HARNESS_INSTRUCTIONS='For questions about Aesel itself—its provider, model, settings, account state, or controls—answer briefly in chat without changing the artwork. Use aesel_settings with action read for current settings, action open to open Settings, and action update to apply the requested provider, model, reasoning effort, or auto-publish change. Use action reify when asked to reload Aesel after editing its code: the TUI saves this session and reloads after the current reply and uploads finish, preserving the conversation, editor draft, queue, settings and Slab identity. These are direct controls: no shell configuration edits or UI automation. Change settings or reify only when requested. Report queued actions as pending, never as already applied. If a control is unavailable, state that limitation.';
export const PIECE_INSTRUCTIONS='Your primary output is runnable code for the current Aesthetic Computer piece. Treat ordinary prompts as requests to express the result in that piece, especially the first prompt on a blank canvas. For example, “3+3 = ?” means edit the piece to visibly print “6” (or “3 + 3 = 6”), then briefly answer in chat. A chat-only answer does not complete that piece request. Meaningful answer text is the artwork, not a decorative label. Read and preserve existing source, make the smallest useful code change, and inspect the preview. Explicit requests for conversation or an explanation, and introspective questions about Aesel or its settings, can stay in chat without an artwork edit.';

export function validateSettingsRequest(value){
 if(!value||typeof value!=='object'||Array.isArray(value))throw Error('Expected a settings request');
 const keys=['action','provider','model','effort','autopublish'];
 if(Object.keys(value).some(key=>!keys.includes(key))||!['read','open','update','reify'].includes(value.action))throw Error('Unsupported settings action');
 if(value.action!=='update'&&Object.keys(value).length!==1)throw Error('Only update accepts setting values');
 if(value.action==='update'&&Object.keys(value).length===1)throw Error('Choose a setting to update');
 if(value.provider!==undefined&&!['ac','claude','codex','open','relay'].includes(value.provider))throw Error('Unknown provider');
 if(value.model!==undefined&&(typeof value.model!=='string'||value.model.length>160||/[\x00-\x1f\x7f]/.test(value.model)))throw Error('Invalid model');
 if(value.effort!==undefined&&!SETTINGS_TOOL.inputSchema.properties.effort.enum.includes(value.effort))throw Error('Invalid reasoning effort');
 if(value.autopublish!==undefined&&typeof value.autopublish!=='boolean')throw Error('Invalid auto-publish setting');
 return value;
}
// Obvious harness-only requests do not need a canvas capture before inference.
// This only skips visual context; the model still interprets the requested action.
export function isHarnessRequest(text){
 const value=String(text).trim().toLowerCase().replace(/[?.!]+$/,'');
 if (/^(?:please )?(?:reify|reload|restart)(?: aesel| yourself| the tui)?$/.test(value)) return true;
 return /^(?:please )?(?:(?:open|show)(?: me)? (?:your |the |aesel(?:’s|'s)? )?settings(?: panel)?|switch (?:the )?provider|(?:switch|change)(?: (?:your|the))? (?:provider|model)(?: to .+)?|(?:switch to|use) (?:claude|codex|ac|aesthetic computer)(?: .*)?|(?:set|change|use) (?:your |the )?(?:reasoning )?effort(?: .*)?|(?:enable|disable|turn on|turn off) auto-?publish)$/.test(value)
 || /^(?:what|which) (?:provider|model) (?:are you(?: using| running)?|is aesel using)$/.test(value)
 || /^(?:what|which|how|are|can|do)\b.*\b(?:your|aesel(?:’s|'s)?)\b.*\b(?:settings|provider|model|reasoning effort|auto-?publish)\b/.test(value);
}
