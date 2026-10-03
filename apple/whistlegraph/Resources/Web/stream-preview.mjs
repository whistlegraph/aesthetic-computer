import {parse} from './easel/src/vendor/acorn.mjs';
import {applyPieceEdits} from './easel/src/piece-edits.mjs';

const options={ecmaVersion:'latest',sourceType:'module'};
const isPaint=node=>node.type==='ExportNamedDeclaration'&&node.declaration?.type==='FunctionDeclaration'&&node.declaration.id?.name==='paint';
// A temporary closing brace lets complete paint statements appear before the
// function finishes streaming. Never close strings, expressions or nested blocks.
// This source is preview-only; only the actual completed tool can save a version.
export function runnablePrefix(source) {
  if(!source || source.length>100000)return null;
  const ends=[source.length];
  for(let i=source.length-1;i>=0&&ends.length<80;i--) {
    if(source[i]==='}'||source[i]===';')ends.push(i+1);
  }
  for(const end of ends){
    const candidate=source.slice(0,end);
    try {if(parse(candidate,options).body.some(isPaint))return candidate;}catch{}
    if(!/[;}]$/.test(candidate.trimEnd()))continue;
    try {
      const body=parse(candidate+'\n}',options).body, last=body.at(-1);
      if(isPaint(last)&&last.declaration.body.body.length)return candidate+'\n}';
    }catch{}
  }
  return null;
}

export function partialString(json,field='source') {
  const start=json.match(new RegExp('"'+field+'"\\s*:\\s*"'));if(!start)return {value:'',complete:false};
  const raw=json.slice(start.index+start[0].length);
  let value='';for(let i=0;i<raw.length;i++){
    const c=raw[i];if(c==='"')return {value,complete:true};
    if(c!=='\\'){value+=c;continue;}
    const next=raw[++i];if(next===undefined)break;
    if(next==='u'){const hex=raw.slice(i+1,i+5);if(!/^[0-9a-f]{4}$/i.test(hex))break;value+=String.fromCharCode(parseInt(hex,16));i+=4;}
    else value+=({n:'\n',r:'\r',t:'\t',b:'\b',f:'\f','"':'"','\\':'\\','/':'/'})[next]??'';
  }return {value,complete:false};
}

// Preview only complete exact replacements against the tool's original revision.
// An incomplete replacement must never delete the remainder of the old drawing.
export function streamedEdits(json,source,revision) {
  if(json.length>200000)return null;
  const supplied=partialString(json,'revision');
  if(!supplied.complete||supplied.value!==revision)return null;
  const start=json.match(/"edits"\s*:\s*\[/);if(!start)return null;
  let quoted=false,escaped=false,depth=0,begin=-1;const edits=[];
  for(let i=start.index+start[0].length;i<json.length;i++){
    const c=json[i];
    if(quoted){if(escaped)escaped=false;else if(c==='\\')escaped=true;else if(c==='"')quoted=false;continue;}
    if(c==='"'){quoted=true;continue;}
    if(c==='{'){if(depth++===0)begin=i;}
    else if(c==='}'&&--depth===0){try{edits.push(JSON.parse(json.slice(begin,i+1)));}catch{return null;}}
    else if(c===']'&&depth===0)break;
  }
  try {const candidate=applyPieceEdits(source,edits);parse(candidate,options);return candidate;}catch{return null;}
}

export function streamedCode(json,tool) {
  if(tool!=='edit_piece')return partialString(json).value;
  return [...json.matchAll(/"replace"\s*:\s*"/g)].map(match=>partialString(json.slice(match.index),'replace').value).join('\n');
}
