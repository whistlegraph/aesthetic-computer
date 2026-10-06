import {parse} from './easel/src/vendor/acorn.mjs';

// Compile complete module prefixes only. Never invent missing brackets or run
// an unfinished paint body. Runtime failures retain the last painted checkpoint.
export function runnablePrefix(source) {
  if(!source || source.length>100000)return null;
  const ends=[source.length];
  for(let i=source.length-1;i>=0&&ends.length<24;i--) {
    if(source[i]==='}'||source[i]===';')ends.push(i+1);
  }
  for(const end of ends){
    const candidate=source.slice(0,end);
    let tree;try{tree=parse(candidate,{ecmaVersion:'latest',sourceType:'module'});}catch{continue;}
    const paint=tree.body.some(node=>node.type==='ExportNamedDeclaration'&&node.declaration?.type==='FunctionDeclaration'&&node.declaration.id?.name==='paint');
    if(paint)return candidate;
  }
  return null;
}
