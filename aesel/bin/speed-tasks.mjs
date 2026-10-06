import {runInNewContext} from 'node:vm';
export const tasks={
 ping:{prompt:'Reply with exactly: SABLE',check:text=>text.trim()==='SABLE'},
 sequence:{prompt:'List the integers 1 through 40, separated by comma and space. No other text.',check:text=>text.trim()===Array.from({length:40},(_,i)=>i+1).join(', ')},
 coding:{prompt:'Return only a JSON object with one key, "code". Its value must be a JavaScript function declaration named mergeRanges(ranges). Input is an array of inclusive integer pairs [start,end], sometimes reversed and unsorted. Return sorted disjoint normalized pairs, merging overlapping OR adjacent ranges, preserving the input without mutation. Empty input returns []. Include no imports, markdown, tools, tests, or explanation.',check:text=>{
  try{
   const object=JSON.parse(text);
   if(typeof object.code!=='string'||object.code.length>16000)return false;
   // No host objects/functions enter the guest realm. Dynamic code generation
   // is disabled and every sample is bounded. Only a boolean leaves the realm.
   return runInNewContext(`${object.code}\n(()=>{
    const cases=[
     [[],[]],
     [[[5,2]],[[2,5]]],
     [[[8,10],[1,3],[4,7]],[[1,10]]],
     [[[1,2],[4,5]],[[1,2],[4,5]]],
     [[[3,3],[3,3],[2,1]],[[1,3]]],
     [[[-1,-5],[-7,-6],[0,2],[10,9]],[[-7,2],[9,10]]],
     [[[1,10],[3,4],[12,12],[11,11]],[[1,12]]]
    ];
    return cases.every(([input,expected])=>{
     const before=JSON.stringify(input),actual=mergeRanges(input);
     return JSON.stringify(actual)===JSON.stringify(expected)&&JSON.stringify(input)===before;
    });
   })()`,Object.create(null),{timeout:100,contextCodeGeneration:{strings:false,wasm:false}})===true;
  }catch{return false;}
 }}
};
