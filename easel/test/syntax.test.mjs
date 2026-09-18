import test from 'node:test';
import assert from 'node:assert/strict';
import {syntaxSpans,syntaxLine} from '../src/syntax.mjs';
import {renderFrame,cleanText} from '../src/render.mjs';
test('token colors distinguish comments, keywords, numbers and strings without changing text',()=>{
 const source='/* hello\nworld */ const value = "pink" + 42;',spans=syntaxSpans(source);
 assert.deepEqual(new Set(spans.map(s=>s.tone)),new Set(['muted','prompt','soft','status','highlight']));
 assert.equal(syntaxLine(source,spans,0,source.length,(_,text)=>text),source);
 assert.doesNotThrow(()=>syntaxSpans('const x = "unfinished'));
});
test('fenced source is highlighted and retains clean terminal text',()=>{
 const state={entries:[{kind:'assistant',text:'```js\nconst x = 42;\n```'}],input:'',status:'ready'};
 const plain=renderFrame(state,80,24,false),colored=renderFrame(state,80,24,true);
 assert.match(plain,/const x = 42/);assert.equal(cleanText(colored),cleanText(plain));
 assert.match(colored,/const\x1b/);
});

test('raw command source and wrapped URLs carry colors and complete hyperlink targets',()=>{
 const url='https://aesthetic.computer/@jeffrey/a-long-piece-name-for-testing';
 const state={entries:[{kind:'command',text:"node <<'JS'\nconst color = 42;\nJS"},{kind:'assistant',text:'Live at '+url}],input:'',status:'ready'};
 const output=renderFrame(state,48,30,true);
 assert.match(output,/const\x1b/);
 assert.ok(output.includes('\x1b]8;;'+url+'\x07'));
 assert.equal(cleanText(output),cleanText(renderFrame(state,48,30,false)));
});
