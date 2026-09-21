// Portable shareable .easel JSON Lines; never a provider-session-state dump.
export const TRANSCRIPT_FORMAT = 'aesthetic.easel';
export const MAX_DOCUMENT_BYTES = 8 * 1024 * 1024;
export const MAX_BATCH_BYTES = 256 * 1024;
const ID = /^[a-zA-Z0-9_-]{1,80}$/;
function object(value, keys) {
  if (!value || typeof value !== 'object' || Array.isArray(value)) throw new Error('Expected an object');
  for (const key of Object.keys(value)) if (!keys.includes(key)) throw new Error(`Unexpected transcript field: ${key}`);
  return value;
}
function text(value, limit, label) {
  if (typeof value !== 'string' || value.length > limit) throw new Error(`Invalid ${label}`);
  return value;
}
function id(value) { if (typeof value !== 'string' || !ID.test(value)) throw new Error('Invalid transcript ID'); return value; }
function date(value) { if (typeof value !== 'string' || !/^\d{4}-\d\d-\d\dT/.test(value) || !Number.isFinite(Date.parse(value))) throw new Error('Invalid transcript timestamp'); return new Date(value).toISOString(); }
export function validateHeader(value) {
  object(value,['type','format','version','id','createdAt','metadata','consent','provenance']);
  if(value.type!=='session'||value.format!==TRANSCRIPT_FORMAT||value.version!==1)throw new Error('Unsupported .easel format');
  const metadata=object(value.metadata??{},['medium','projectId','title']);
  if(metadata.medium!==undefined&&!['picture','sound','piece','paper','gameboy'].includes(metadata.medium))throw new Error('Invalid medium');
  if(metadata.projectId!==undefined)id(metadata.projectId);
  if(metadata.title!==undefined)text(metadata.title,160,'title');
  const consent=object(value.consent??{sharing:'private'},['sharing','id','acceptedAt','disclosureVersion']);
  if(!['private','company'].includes(consent.sharing))throw new Error('Invalid sharing setting');
  if(consent.sharing==='company') {id(consent.id);date(consent.acceptedAt);if(![1,2,3,4].includes(consent.disclosureVersion))throw new Error('Consent disclosure required');}
  const provenance=object(value.provenance??{application:'easel'},['application','version']);
  if(provenance.application!=='easel')throw new Error('Invalid provenance');
  if(provenance.version!==undefined)text(provenance.version,40,'application version');
  return {type:'session',format:TRANSCRIPT_FORMAT,version:1,id:id(value.id),createdAt:date(value.createdAt),metadata:{...metadata},consent:{...consent},provenance:{...provenance}};
}
export function validateRecord(value) {
  if(!['message','artifact','event'].includes(value?.type))throw new Error('Invalid transcript record type');
  const fields=['type','id','seq','at'];
  const extra=value.type==='message'?['role','text','model','backend']:value.type==='artifact'?['artifactId','medium','revision']:['name','status'];
  object(value,[...fields,...extra]);
  if(!Number.isSafeInteger(value.seq)||value.seq<1)throw new Error('Invalid record sequence');
  const record={type:value.type,id:id(value.id),seq:value.seq,at:date(value.at)};
  if(value.type==='message') {
    if(!['user','assistant'].includes(value.role))throw new Error('Only user and assistant messages belong in transcripts');
    Object.assign(record,{role:value.role,text:text(value.text,32768,'message text')});
    for(const key of ['model','backend'])if(value[key]!==undefined)record[key]=text(value[key],120,key);
  } else if(value.type==='artifact') {
    if(!['picture','sound','piece','paper','gameboy'].includes(value.medium))throw new Error('Invalid artifact medium');
    if(!Number.isSafeInteger(value.revision)||value.revision<1)throw new Error('Invalid artifact revision');
    Object.assign(record,{artifactId:id(value.artifactId),medium:value.medium,revision:value.revision});
  } else {
    if(!['turn-start','turn-complete','turn-interrupted','render-start','render-complete','render-failed','model-change'].includes(value.name))throw new Error('Unsupported transcript event');
    record.name=value.name;
    if(value.status!==undefined) { if(!['working','complete','interrupted','failed'].includes(value.status))throw new Error('Invalid event status');record.status=value.status; }
  }
  return record;
}
export function redactTranscriptText(value) {
  return value
    .replace(/-----BEGIN [^-]*PRIVATE KEY-----[\s\S]*?-----END [^-]*PRIVATE KEY-----/g,'[redacted private key]')
    .replace(/\bBearer\s+[A-Za-z0-9._~+\/-]+=*/gi,'Bearer [redacted]')
    .replace(/\b(?:sk-(?:proj-)?[A-Za-z0-9_-]{16,}|gh[pousr]_[A-Za-z0-9_]{20,}|github_pat_[A-Za-z0-9_]{20,}|AKIA[A-Z0-9]{16})\b/g,'[redacted credential]')
    .replace(/\beyJ[A-Za-z0-9_-]+\.[A-Za-z0-9_-]+\.[A-Za-z0-9_-]+\b/g,'[redacted token]')
    .replace(/((?:access[_-]?token|refresh[_-]?token|api[_-]?key|authorization|password|client[_-]?secret)\s*["']?\s*[:=]\s*["']?)[^\s,"'}]+/gi,'$1[redacted]');
}
export function serializeTranscript(header,records,{redact=false,maxBytes=MAX_DOCUMENT_BYTES}={}) {
  const h=validateHeader(header), list=records.map(validateRecord), seen=new Set();let seq=0;
  for(const record of list){if(seen.has(record.id)||record.seq<=seq)throw new Error('Transcript IDs and sequence must be unique and ordered');seen.add(record.id);seq=record.seq;}
  if(redact){if(h.metadata.title)h.metadata.title=redactTranscriptText(h.metadata.title);for(const r of list)if(r.type==='message')r.text=redactTranscriptText(r.text);}
  const output=[h,...list].map(v=>JSON.stringify(v)).join('\n')+'\n';
  if(Buffer.byteLength(output)>maxBytes)throw new Error('Transcript exceeds size limit');return output;
}
export function parseTranscript(source,{recoverPartial=false,maxBytes=MAX_DOCUMENT_BYTES}={}) {
  if(typeof source!=='string'||Buffer.byteLength(source)>maxBytes)throw new Error('Transcript exceeds size limit');
  if(!source.endsWith('\n')){if(!recoverPartial)throw new Error('Incomplete .easel final line');source=source.slice(0,source.lastIndexOf('\n')+1);}
  const lines=source.split('\n');lines.pop();if(!lines.length)throw new Error('Missing .easel header');
  const header=validateHeader(JSON.parse(lines.shift())),records=lines.map(line=>validateRecord(JSON.parse(line)));
  serializeTranscript(header,records,{maxBytes});return{header,records};
}
