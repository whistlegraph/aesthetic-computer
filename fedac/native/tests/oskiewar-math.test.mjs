// Native math must match the actual shared JS, bit for bit—not merely be close.
import test from 'node:test';
import assert from 'node:assert/strict';
import {readFileSync,writeFileSync,mkdtempSync,rmSync} from 'node:fs';
import {tmpdir} from 'node:os';
import {resolve,dirname,join} from 'node:path';
import {fileURLToPath} from 'node:url';
import {execFileSync} from 'node:child_process';
import vm from 'node:vm';
const root=resolve(dirname(fileURLToPath(import.meta.url)),'../../..');
test('native deterministic math matches shared JavaScript doubles',()=>{
  const dir=mkdtempSync(join(tmpdir(),'oskiewar-math-'));
  try {
    writeFileSync(join(dir,'test.c'),`
#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include "oskiewar-math.h"
int main(void) {
 int op,n; double v[32];
 while(scanf("%d %d",&op,&n)==2) {
   if(n<0 || n>32)return 2;
   for(int i=0;i<n;i++)if(scanf("%lf",&v[i])!=1)return 3;
   double x=n?v[0]:NAN,y=n>1?v[1]:NAN,r;
   switch(op){case 0:r=ow_sin(x);break;case 1:r=ow_sin(x+1.5707963267948966);break;
   case 2:r=ow_sin(x)/ow_sin(x+1.5707963267948966);break;case 3:r=ow_atan(x);break;
   case 4:r=ow_atan2(x,y);break;case 5:r=ow_atan2(x,sqrt(1-x*x));break;
   case 6:r=ow_exp(x);break;default:r=ow_hypot(v,n);break;}
   union{double d;uint64_t u;}bits={.d=r};printf("%016" PRIx64 "\\n",bits.u);
 }
 return 0;
}`);
    execFileSync(process.env.CC || 'cc',['-O2','-ffp-contract=off','-I',join(root,'fedac/native/src'),join(dir,'test.c'),'-lm','-o',join(dir,'test')]);
    const source=readFileSync(join(root,'xbox/live/oskiewar.js'),'utf8');
    const math=vm.runInNewContext('const platformMath=Math;'+source.slice(source.indexOf('function netSin('),source.indexOf('for (const [name, fallback]'))+';netMath');
    const names=Object.keys(math), cases=[];
    let seed=1924;
    const random=()=>{seed=(Math.imul(seed,1664525)+1013904223)>>>0;return seed/2**32;};
    const edges=[0,-0,1,-1,Math.PI,-Math.PI,Math.PI/2,1e-300,1e300,Infinity,-Infinity,NaN,709.4,-745.1];
    for(const [op,name] of names.entries()) {
      const values=[...edges,...Array.from({length:1600},()=>name==='asin'?random()*2-1:(random()-.5)*(random()<.5?10:1400))];
      for(const x of values){
        const args=name==='atan2'?[x,values[Math.floor(random()*values.length)]]:
          name==='hypot'?[x,random()*100-50,random()*100-50]:[x];
        cases.push({op,name,args,expected:math[name](...args)});
      }
    }
    cases.push({op:7,name:'hypot',args:[],expected:0});
    const input=cases.map(c=>[c.op,c.args.length,...c.args.map(x=>Object.is(x,-0)?'-0':String(x))].join(' ')).join('\n')+'\n';
    const lines=execFileSync(join(dir,'test'),{input,encoding:'utf8',maxBuffer:4e6}).trim().split('\n');
    assert.equal(lines.length,cases.length);
    cases.forEach((c,i)=>{
      const actual=Buffer.from(lines[i],'hex').readDoubleBE();
      assert.ok(Object.is(actual,c.expected),`${c.name}(${c.args}): ${actual} vs ${c.expected}`);
    });
  }finally{rmSync(dir,{recursive:true,force:true});}
});
