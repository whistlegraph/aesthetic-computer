#!/usr/bin/env node
import {writeFile} from "node:fs/promises";
import {resolve} from "node:path";
import {createHash} from "node:crypto";
import {inclusions,auditInclusions} from "../conformance/inclusions.mjs";
const out=resolve(process.argv[2]||"kidlisp/conformance/top100.json");
async function get(query) {
  const response=await fetch(`https://aesthetic.computer/api/store-kidlisp?${query}`,{signal:AbortSignal.timeout(15000)});
  if(!response.ok)throw new Error(`Source lookup failed: HTTP ${response.status} (${query})`);
  return response.json();
}
const {recent}=await get("recent=true&limit=100&sort=hits");
if(!Array.isArray(recent)||recent.length!==100)throw new Error("Expected exactly 100 ranked pieces");
const records={},queue=[];
function add(code,p) {
  if(!/^[a-z0-9]{3,}$/i.test(code)||typeof p.source!=="string"||p.source.length>1000000)throw new Error("Invalid source record");
  records[code]={code,source:p.source,handle:p.handle??null,hits:p.hits??0,sha256:createHash("sha256").update(p.source).digest("hex")};
  queue.push(...inclusions(p.source));
}
recent.forEach(p=>add(p.code,p));
while(queue.length) {
  const pending=[...new Set(queue.splice(0))].filter(code=>!records[code]);
  if(Object.keys(records).length+pending.length>512)throw new Error("Dependency source budget exceeded");
  for(let i=0;i<pending.length;i+=4) {
    const codes=pending.slice(i,i+4);
    const responses=await Promise.all(codes.map(code=>get(`code=${encodeURIComponent(code)}`)));
    codes.forEach((code,j)=>add(code,responses[j]));
  }
}
const roots=recent.map(p=>p.code),audit=auditInclusions(records,roots);
const snapshot={version:1,refreshed:new Date().toISOString(),pieces:roots.map(code=>({...records[code],embeds:inclusions(records[code].source).length>0})),dependencies:Object.fromEntries(Object.entries(records).filter(([code])=>!roots.includes(code))),audit};
await writeFile(out,JSON.stringify(snapshot,null,2)+"\n");
console.log(JSON.stringify({out,roots:roots.length,sources:Object.keys(records).length,inclusions:roots.filter(c=>audit.edges[c].length).length,maxDepth:audit.maxDepth,cycles:audit.cycles,missing:audit.missing}));
