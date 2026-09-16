// Only completed conversational text enters .easel exports; no tools, account
// objects, provider state, file paths, or hidden prompts are copied from the UI.
import { createHash } from 'node:crypto';
export function transcriptMessages(entry, {backend,model} = {}) {
  if (!entry || !['user','assistant'].includes(entry.kind) || typeof entry.text !== 'string' || typeof entry.id !== 'string') return [];
  const base = createHash('sha256').update(entry.id).digest('hex');
  const chunks = []; let text = '';
  for(const character of entry.text) { if(text.length + character.length > 32768) { chunks.push(text);text=''; } text += character; }
  if(text || !chunks.length)chunks.push(text);
  return chunks.map((text,index)=>({type:'message',id:`message_${base.slice(0,48)}_${index}`,role:entry.kind,text,...(backend?{backend}:{}),...(model?{model}:{})}));
}
