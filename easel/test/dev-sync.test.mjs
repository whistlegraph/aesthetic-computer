import test from 'node:test';import assert from 'node:assert/strict';import fs from 'node:fs';import path from 'node:path';import os from 'node:os';import {execFileSync} from 'node:child_process';import {sync,dependencyKey} from '../dev/sync.mjs';
function fixture(t){
 const dir=fs.mkdtempSync(path.join(os.tmpdir(),'aesel-dev-test-')),repo=path.join(dir,'repo'),home=path.join(dir,'home');fs.mkdirSync(path.join(repo,'easel/desktop'),{recursive:true});fs.mkdirSync(home);
 const git=(...args)=>execFileSync('git',['-C',repo,...args],{encoding:'utf8',stdio:['ignore','pipe','pipe']}).trim();git('init','-q');git('config','user.name','Fixture');git('config','user.email','fixture@example.invalid');git('config','commit.gpgsign','false');
 const pkg={version:'0.7.3',dependencies:{},devDependencies:{electron:'39.2.7'}};fs.writeFileSync(path.join(repo,'easel/desktop/package.json'),JSON.stringify(pkg));fs.writeFileSync(path.join(repo,'easel/desktop/main.cjs'),'// main');
 fs.writeFileSync(path.join(home,'config.json'),JSON.stringify({repo,dependencyKey:dependencyKey(pkg)}));
 function commit(){git('add','.');git('commit','-qm','fixture');git('update-ref','refs/remotes/origin/main','HEAD');}
 commit();t.after(()=>fs.rmSync(dir,{recursive:true,force:true}));return {home,repo,commit,git};
}
test('sync installs immutable builds atomically and ignores unrelated commits',t=>{
 const f=fixture(t),args={home:f.home,fetch:false,build:()=>{}};const first=sync(args);const original=fs.realpathSync(path.join(f.home,'current'));
 fs.writeFileSync(path.join(f.repo,'unrelated.txt'),'other project');f.commit();const unrelated=sync(args);assert.equal(unrelated.installed.tree,first.installed.tree);assert.equal(fs.realpathSync(path.join(f.home,'current')),original);
 fs.writeFileSync(path.join(f.repo,'easel/desktop/main.cjs'),'// changed');f.commit();const next=sync(args);assert.notEqual(next.installed.tree,first.installed.tree);assert(fs.existsSync(original));assert.equal(fs.readFileSync(path.join(f.home,'current/easel/desktop/main.cjs'),'utf8'),'// changed');
});
test('local modifications are visible and never overwritten',t=>{const f=fixture(t),args={home:f.home,fetch:false,build:()=>{}};sync(args);const file=path.join(f.home,'current/easel/desktop/main.cjs');fs.writeFileSync(file,'local work');fs.writeFileSync(path.join(f.repo,'easel/desktop/main.cjs'),'remote work');f.commit();const status=sync(args);assert.equal(status.state,'modified');assert.equal(fs.readFileSync(file,'utf8'),'local work');});
test('failed staging keeps current build and marks freshness unknown',t=>{const f=fixture(t),args={home:f.home,fetch:false,build:()=>{}};sync(args);const original=fs.realpathSync(path.join(f.home,'current'));fs.writeFileSync(path.join(f.repo,'easel/desktop/main.cjs'),'remote');f.commit();assert.throws(()=>sync({...args,build(){throw Error('build failed');}}),/build failed/);assert.equal(fs.realpathSync(path.join(f.home,'current')),original);assert.equal(JSON.parse(fs.readFileSync(path.join(f.home,'status.json'))).online,false);});
test('retention removes only old unmodified snapshots',t=>{
 const f=fixture(t),args={home:f.home,fetch:false,build:()=>{}};sync(args);const edited=fs.realpathSync(path.join(f.home,'current'));
 const next=i=>{fs.writeFileSync(path.join(f.repo,'easel/desktop/main.cjs'),'// revision '+i);f.commit();sync(args);return fs.realpathSync(path.join(f.home,'current'));};
 const old=next(1);fs.writeFileSync(path.join(edited,'easel/desktop/main.cjs'),'local edit');for(let i=2;i<=5;i++)next(i);
 assert(fs.existsSync(edited));assert(!fs.existsSync(old));assert.equal(fs.readdirSync(path.join(f.home,'versions')).length,4);
});
