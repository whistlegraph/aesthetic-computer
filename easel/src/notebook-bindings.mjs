import {parse} from './vendor/acorn.mjs';
import {createHash} from 'node:crypto';
export const bindingRevision=source=>createHash('sha256').update(source).digest('hex');
const number=node=>node?.type==='Literal'&&typeof node.value==='number'?node.value:node?.type==='UnaryExpression'&&['-','+'].includes(node.operator)&&node.argument?.type==='Literal'&&typeof node.argument.value==='number'?(node.operator==='-'?-1:1)*node.argument.value:null;
export function notebookBindings(source,file='piece.mjs'){
 const revision=bindingRevision(source),document=bindingRevision(file),bindings=[];
 if(!file.endsWith('.mjs')||source.length>500000)return {revision,document,bindings};
 let tree;try{tree=parse(source,{ecmaVersion:'latest',sourceType:'module',locations:true});}catch{return {revision,document,bindings};}
 const add=(node,kind,value,label,format='literal')=>{if(bindings.length>=160)return;bindings.push({id:`${node.start}:${node.end}`,kind,value,label:label.slice(0,100),line:node.loc.start.line,start:node.start,end:node.end,original:source.slice(node.start,node.end),format});};
 function visit(node,parent){
  if(!node||typeof node!=='object')return;
  if(node.type==='VariableDeclaration'&&node.kind==='const')for(const declaration of node.declarations){const value=number(declaration.init);if(declaration.id.type==='Identifier'&&value!==null&&Number.isFinite(value))add(declaration.init,'number',value,declaration.id.name);}
  if(node.type==='Literal'&&typeof node.value==='string'&&node.value.length<=100){
   const label=parent?.type==='VariableDeclarator'&&parent.id?.type==='Identifier'?parent.id.name:parent?.type==='Property'?(parent.key?.name||String(parent.key?.value||'color')):parent?.type==='CallExpression'?(parent.callee?.name||parent.callee?.property?.name||'color'):'color';
   add(node,'color',node.value,label);
  }
  if(node.type==='CallExpression'&&['wipe','ink','background','fill','stroke'].includes(node.callee?.name||node.callee?.property?.name)){
   const values=node.arguments.slice(0,3).map(number);
   if(values.length===3&&values.every(v=>v!==null&&v>=0&&v<=255)){const first=node.arguments[0],last=node.arguments[2];add({...first,end:last.end},'color',`rgb(${values.join(', ')})`,node.callee.name||node.callee.property.name,'rgb');}
  }
  if(node.type==='ArrayExpression'&&node.elements.length>=3&&node.elements.length<=4){
   const call=parent?.type==='CallExpression'&&(parent.callee?.name||parent.callee?.property?.name);
   const name=parent?.type==='VariableDeclarator'&&parent.id?.name;
   if(['wipe','ink','background','fill','stroke'].includes(call)||name&&/color|tint|ink|background|fill|stroke/i.test(name)){
    const values=node.elements.slice(0,3).map(number);
    if(values.every(v=>v!==null&&v>=0&&v<=255)){const first=node.elements[0],last=node.elements[2];add({...first,end:last.end},'color',`rgb(${values.join(', ')})`,name||call,'rgb-array');}
   }
  }
  for(const [key,value]of Object.entries(node)){if(['loc','start','end'].includes(key))continue;if(Array.isArray(value))for(const child of value)visit(child,node);else if(value&&typeof value==='object')visit(value,node);}
 }
 visit(tree,null);return {revision,document,bindings};
}
export function editNotebookBinding(source,{revision,document,id,value},file){
 if(bindingRevision(file)!==document)throw Error('The selected piece changed. Pick the value again.');
 if(bindingRevision(source)!==revision)throw Error('The piece changed. Pick the value again.');
 const binding=notebookBindings(source,file).bindings.find(item=>item.id===id);
 if(!binding)throw Error('That value is no longer in the piece.');
 let replacement;
 if(binding.kind==='number'){
  if(typeof value!=='number'||!Number.isFinite(value)||Math.abs(value)>Number.MAX_SAFE_INTEGER)throw Error('Enter a finite number.');replacement=String(value);
 }else{
  if(typeof value!=='string'||!/^#[a-f\d]{6}(?:[a-f\d]{2})?$/i.test(value))throw Error('Choose a valid color.');
  if(binding.format==='rgb'||binding.format==='rgb-array')replacement=[1,3,5].map(i=>parseInt(value.slice(i,i+2),16)).join(', ');
  else {const quote=binding.original[0];replacement=quote+value.toLowerCase()+quote;}
 }
 const next=source.slice(0,binding.start)+replacement+source.slice(binding.end);
 parse(next,{ecmaVersion:'latest',sourceType:'module'});
 return {source:next,binding,summary:`${binding.label}: ${binding.value} → ${value}`};
}
export function bindingRequest(input){
 if(input.length>12000)return null;const match=/^\x1b\[99;7;([\d;]+)~$/.exec(input);if(!match)return null;
 const bytes=match[1].split(';').map(Number);if(bytes.length>3000||bytes.some(n=>!Number.isInteger(n)||n<0||n>255))return null;
 try{const value=JSON.parse(Buffer.from(bytes).toString('utf8'));if(!/^[a-f\d]{64}$/.test(value.document)||!/^[a-f\d]{64}$/.test(value.revision)||!/^\d+:\d+$/.test(value.id)||!/^edit-[a-z0-9-]{1,80}$/.test(value.request))return null;return value;}catch{return null;}
}
