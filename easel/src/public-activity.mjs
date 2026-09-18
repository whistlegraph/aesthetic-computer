// Public agent text and observed tool stages only; never private reasoning or tool payloads.
const toolTypes = new Set(['fileChange', 'commandExecution', 'mcpToolCall', 'dynamicToolCall']);

export function toolActivity(item = {}) {
  const name = String(item.tool || '').split(' · ')[0];
  if (item.type === 'fileChange' || /(?:^|__)(?:write_piece|Write|Edit|apply_patch)$/.test(name)) return "I'm editing the piece";
  if (/(?:^|__)ac_frame$/.test(name)) return "I'm looking at the preview";
  if (/(?:^|__)ac_preview$/.test(name)) return "I'm checking the preview";
  if (/(?:^|__)(?:Read|read_file|ac_symbol|ac_outline)$/.test(name)) return "I'm reading the source";
  if (/(?:^|__)(?:ac_api|ac_examples|ac_references|Grep|Glob)$/.test(name)) return "I'm looking up details";
  if (item.type === 'commandExecution' || /(?:^|__)(?:Bash|exec_command)$/.test(name)) return "I'm running a command";
  return "I'm using a tool";
}

// Retain other active tools when parallel calls finish out of order. A completion
// without a matching start (e.g. harness advice) must not replace live activity.
export function observeToolActivity(state, method, item) {
  if (!toolTypes.has(item?.type) || !item.id) return;
  state.activityTools ||= new Map();
  if (method === 'item/started') {
    state.activityTools.set(item.id, toolActivity(item));
    state.activityText = '';
    state.activityMessageId = null;
  } else if (method === 'item/completed') {
    if (!state.activityTools.delete(item.id)) return;
    if (!state.activityTools.size && ['tool', 'writing'].includes(state.status)) state.status = 'working';
  } else return;
  state.activityStage = [...state.activityTools.values()].at(-1) || '';
}

export function publicActivity(state) {
  if (state.connectionNotice) return "I'm offline; your changes are kept here";
  if (state.previewNotice && !state.busy) return "I need to fix the code; I've kept the previous preview";
  if (!state.busy) return '';
  if (state.status === 'approval') return "I'm waiting for your approval";
  if (state.status === 'interrupting') return "I'm stopping";
  const text = String(state.activityText || '').replace(/\s+/g, ' ').trim();
  if (text) return text;
  return state.activityStage || ({
    preparing: "I'm on my way",
    connecting: "I'm connecting",
    waiting: "I'm waiting for a response",
    generating: "I'm putting my reply together",
    composing: "I'm preparing my next step",
    writing: "I'm saving the changes",
  }[state.status]) || "I'm working on it";
}
