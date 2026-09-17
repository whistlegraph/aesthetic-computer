import test from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, writeFile, readFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { typeTextAsync, sendKeysAsync, termListAsync, osa } from "../bin/macos.mjs";
import { execFileSync } from "node:child_process";

test("native async operations preserve paste ordering, yield to other clients, and recover after errors", async t => {
  const dir = await mkdtemp(join(tmpdir(), "macos-tools-"));
  const savedPath = process.env.PATH;
  const log = join(dir, "commands.jsonl");
  // Stand in for SSH; record scripts without sending any real UI input.
  await writeFile(join(dir, "ssh"), `#!${process.execPath}\nimport fs from 'node:fs';
let input = ''; for await (const chunk of process.stdin) input += chunk;
fs.appendFileSync(${JSON.stringify(log)}, JSON.stringify({host:process.argv[2],command:process.argv.at(-1),input})+'\\n');
setTimeout(() => { if(input.includes('fixture-failure')) process.exit(1); else console.log('ok'); }, 35);
`, { mode: 0o755 });
  process.env.PATH = `${dir}:${savedPath}`;
  t.after(async () => { process.env.PATH = savedPath; await rm(dir, {recursive:true,force:true}); });
  const spec = {sshHost: "fixture"};
  let responsive = false;
  const heartbeat = setTimeout(() => { responsive = true; }, 10);
  const first = typeTextAsync(spec, "first", {paste:true,enter:true});
  const second = typeTextAsync(spec, "second", {paste:true,enter:true});
  assert.deepEqual(await Promise.all([first,second]), ["ok","ok"]);
  clearTimeout(heartbeat);
  assert.equal(responsive, true);
  const commands = (await readFile(log,"utf8")).trim().split('\n').map(JSON.parse);
  assert.equal(commands.length,4);
  assert.equal(commands[0].input,"first");
  assert.match(commands[1].input,/keystroke "v"[\s\S]*delay 0.06[\s\S]*key code 36/);
  assert.equal(commands[2].input,"second");
  assert.match(commands[3].input,/key code 36/);
  const fail = sendKeysAsync(spec,"fixture-failure");
  const next = termListAsync(spec);
  await assert.rejects(fail);
  assert.equal(await next,"ok");
});

test("local AppleScript bypasses the login shell and preserves stdout", {skip:process.platform!=="darwin"}, t => {
  const values = {direct:[],login:[]};
  for(let i=0;i<5;i++) {
    let start=performance.now();
    assert.equal(osa({local:true}, 'return "fixture-ok"'),"fixture-ok");
    values.direct.push(performance.now()-start);
    start=performance.now();
    assert.equal(execFileSync("bash",["-lc","osascript -"],{input:'return "fixture-ok"',encoding:"utf8"}).trim(),"fixture-ok");
    values.login.push(performance.now()-start);
  }
  for(const [name,times] of Object.entries(values)) t.diagnostic(`${name}: median ${times.sort((a,b)=>a-b)[2].toFixed(1)}ms (5 harmless AppleScript calls)`);
});
