export function revisionSummary(previous,source,{restoredFrom}={}){
 if(restoredFrom!==undefined)return `Restored version ${restoredFrom}.`;
 if(previous===undefined)return 'First version.';
 const before=previous.split('\n'),after=source.split('\n');let start=0;
 while(start<before.length&&start<after.length&&before[start]===after[start])start++;
 let a=before.length-1,b=after.length-1;while(a>=start&&b>=start&&before[a]===after[b]){a--;b--;}
 const labels={paint:'drawing',sim:'motion',act:'controls',boot:'setup',beat:'rhythm',leave:'cleanup'};
 const prefix=after.slice(0,start+1).join('\n');const surrounding=[...prefix.matchAll(/(?:function\s+|(?:const|let)\s+)(paint|sim|act|boot|beat|leave)\b/g)].at(-1)?.[1];
 const changed=after.slice(start,b+1).join('\n');const areas=new Set([surrounding,...Array.from(changed.matchAll(/(?:function\s+|(?:const|let)\s+)(paint|sim|act|boot|beat|leave)\b/g),m=>m[1])].filter(Boolean).map(name=>labels[name]));
 return areas.size?`Updated ${[...areas].join(' and ')}.`:`Changed ${Math.max(a-start+1,b-start+1,1)} lines.`;
}
