// Operational notices stay in Settings; the page belongs to the conversation.
export function notebookConversationEntry(entry) {
  return ['user', 'assistant', 'error'].includes(entry.kind) && !entry.activityOnly;
}
