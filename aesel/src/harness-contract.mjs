// Shared settings schema and prompt routing; safe in browser clients.
import {harnessText} from './harness-text.mjs';
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
export const HARNESS_INSTRUCTIONS=harnessText('harness-instructions');
export const PIECE_INSTRUCTIONS=harnessText('piece-instructions');

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
