import test from 'node:test';import assert from 'node:assert/strict';
import {readFileSync,readdirSync} from 'node:fs';
import {HARNESS,harnessText} from '../src/harness-text.mjs';
import {parse} from '../bin/sync-harness.mjs';

test('a harness section fills its placeholders and refuses to leave a hole',()=>{
 assert.equal(harnessText('pro-piece-directory',{dir:'/p'}),'New pieces go in /p, not the repository root.');
 assert.throws(()=>harnessText('pro-piece-directory'),/needs \{\{dir\}\}/);
 assert.throws(()=>harnessText('no-such-section'),/No harness section/);
});
test('markdown sections parse by name and reject duplicates',()=>{
 assert.deepEqual(parse('# Title\n\nlead\n\n## one\n\nfirst\n\n## two\n\nsecond\n','x.md'),{one:'first',two:'second'});
 assert.throws(()=>parse('## one\n\na\n## one\n\nb\n','x.md'),/appears twice/);
 assert.throws(()=>parse('## One\n\na\n','x.md'),/lowercase-kebab/);
});
test('every harness section is placed by some prompt',()=>{
 const code=['src','native'].flatMap(dir=>readdirSync(new URL(`../${dir}/`,import.meta.url)).filter(f=>f.endsWith('.mjs')&&f!=='harness-text.mjs')
  .map(f=>readFileSync(new URL(`../${dir}/${f}`,import.meta.url),'utf8'))).join('\n');
 const orphans=Object.values(HARNESS).flatMap(Object.keys).filter(key=>!code.includes(`harnessText("${key}"`)&&!code.includes(`harnessText('${key}'`));
 assert.deepEqual(orphans,[]);
});
