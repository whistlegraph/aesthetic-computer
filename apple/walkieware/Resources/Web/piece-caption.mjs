import {parse} from './easel/src/vendor/acorn.mjs';
// Read declarative text without executing any piece code.
export function pieceCaption(source) {
  try {
    for (const node of parse(source, {ecmaVersion:'latest',sourceType:'module'}).body) {
      if(node.type!=='ExportNamedDeclaration'||node.declaration?.type!=='VariableDeclaration'||node.declaration.kind!=='const')continue;
      for(const value of node.declaration.declarations) {
        if(value.id.name==='caption'&&value.init?.type==='Literal'&&typeof value.init.value==='string')return value.init.value.trim().slice(0,160);
      }
    }
  } catch {}
  return '';
}
