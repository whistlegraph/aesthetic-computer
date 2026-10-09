import {tokenize} from "../../system/public/aesthetic.computer/lib/kidlisp.mjs";

// Lexical tokens omit comments and preserve quoted strings. A dollar sign in
// prose is not an inclusion. Repeated references remain visible for auditing.
export function inclusions(source) {
  return tokenize(source).filter(t=>/^\$[a-z0-9]{3,}$/i.test(t)).map(t=>t.slice(1));
}

export function auditInclusions(records, roots=Object.keys(records)) {
  const edges=Object.fromEntries(Object.entries(records).map(([code,p])=>[code,inclusions(p.source)]));
  const missing=[],cycles=[],paths={},duplicates=[];let maxDepth=0;
  for(const [code,children]of Object.entries(edges)) {
    for(const child of new Set(children))if(!records[child])missing.push({parent:code,child});
    for(const child of new Set(children))if(children.filter(c=>c===child).length>1)duplicates.push({parent:code,child,count:children.filter(c=>c===child).length});
  }
  for(const root of roots) {
    const visited=new Set();let work=0;
    function walk(code,path) {
      if(++work>10000||path.length>64)throw new RangeError(`Inclusion audit budget exceeded at $${root}`);
      if(path.includes(code)){cycles.push([...path.slice(path.indexOf(code)),code]);return;}
      maxDepth=Math.max(maxDepth,path.length);
      if(visited.has(code)||!edges[code])return;
      visited.add(code);
      for(const child of new Set(edges[code]))walk(child,[...path,code]);
    }
    walk(root,[]);paths[root]=[...visited].filter(c=>c!==root);
  }
  return {edges,missing,cycles,duplicates,maxDepth,transitive:paths};
}
