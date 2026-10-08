import {test} from 'node:test';
import assert from 'node:assert/strict';
import {alignWords} from '../lib/read-along.mjs';
const heard=(text,from,to)=>({text,offsets:{from,to}});
test('readalong retains canonical names with measured boundaries and explicit substitutions',()=>{
 const body='Aesel meets Loopboy.';
 const result=alignWords(body,[heard('Asil',0,300),heard('meets',300,600),heard('Loop',600,800),heard('Boy',800,1000)],12000);
 assert.deepEqual(result.words.map(w=>[body.slice(w.start,w.end),w.fromMs,w.toMs,w.match]),[['Aesel',12000,12300,'substitution'],['meets',12300,12600,'exact'],['Loopboy',12600,13000,'exact']]);
 assert.equal(result.alignment.substitutions,1);
});
test('missing words are omitted rather than given invented times; repeated words retain distinct offsets',()=>{
 const result=alignWords('one missing two two',[heard('one',0,100),heard('two',200,300),heard('two',400,500)]);
 assert.equal(result.alignment.missingWords,1);
 assert.deepEqual(result.words.map(w=>w.start),[0,12,16]);
});
