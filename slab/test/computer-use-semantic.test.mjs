import test from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, readFile, writeFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { chromium } from "playwright-core";
import { spawn } from "node:child_process";
import { once } from "node:events";
import { SemanticBrowser } from "../lib/puppet-semantic.mjs";

// One isolated headless browser. No personal tabs, accounts, or native input.
test("semantic browser fixture", {timeout:30000}, async t => {
  const dir=await mkdtemp(join(tmpdir(),"puppet-semantic-"));
  const context=await chromium.launchPersistentContext(dir,{channel:"chrome",headless:true,args:["--remote-debugging-port=0"]});
  const port=(await readFile(join(dir,"DevToolsActivePort"),"utf8")).split('\n')[0];
  const service=new SemanticBrowser(()=>`http://127.0.0.1:${port}`);
  t.after(async()=>{await service.close();await context.close();await rm(dir,{recursive:true,force:true});});
  const page=context.pages()[0];
  const cdp=await context.newCDPSession(page);
  const {targetInfo}=await cdp.send("Target.getTargetInfo");await cdp.detach();
  const target=targetInfo.targetId;

  await t.test("waits for a delayed enabled button and verifies its postcondition", async()=>{
    await page.setContent(`<button disabled onclick="window.clicks++;document.querySelector('output').textContent='Saved'">Save</button><output>Waiting</output><script>window.clicks=0;setTimeout(()=>document.querySelector('button').disabled=false,120)</script>`);
    const snapshot=await service.run("snapshot",{target});
    assert.equal(snapshot.observation.target,target);assert.match(snapshot.tree,/Save/);
    const result=await service.run("click",{target,locator:{role:"button",name:"Save"},after:{locator:{text:"Saved"}},timeout:2000});
    assert.equal(result.performed,true);assert.equal(result.verification.ok,true);
    assert.equal(await page.evaluate(()=>window.clicks),1);
  });
  await t.test("fills an exact label and supports visibility waits",async()=>{
    await page.setContent(`<label>Title<input oninput="document.querySelector('output').textContent='Changed'"></label><output></output>`);
    const result=await service.run("fill",{target,locator:{label:"Title"},value:"hello",after:{locator:{text:"Changed"}},timeout:1000});
    assert.equal(result.performed,true);assert.equal(await page.locator("input").inputValue(),"hello");
    assert.equal((await service.run("wait",{target,locator:{text:"Changed"},state:"visible"})).verified,true);
  });
  await t.test("cached exact targets avoid inspector setup and reject closed pages", async()=>{
    const browser = await service.browser();
    const original = browser.newBrowserCDPSession;
    browser.newBrowserCDPSession = () => { throw new Error("unexpected inspector setup"); };
    try { assert.equal(await service.page(target), service.pages.get(target)); }
    finally { browser.newBrowserCDPSession = original; }
    const extra = await context.newPage();
    const session = await context.newCDPSession(extra);
    const { targetInfo } = await session.send("Target.getTargetInfo");
    await session.detach();
    const cached = await service.page(targetInfo.targetId);
    await cached.close();
    await assert.rejects(service.page(targetInfo.targetId), /gone|exactly/);
  });
  await t.test("a waiting client does not block another client's action", async()=>{
    await page.setContent(`<button onclick="document.querySelector('output').textContent='Ready'">Start</button><output>Pending</output>`);
    const waiting=service.run("wait",{target,locator:{text:"Ready"},timeout:1500});
    const acting=service.run("click",{target,locator:{role:"button",name:"Start"},timeout:1500});
    const [waited,acted]=await Promise.all([waiting,acting]);
    assert.equal(waited.verified,true);assert.equal(acted.performed,true);
  });
  await t.test("refuses ambiguous controls and missing/substring page IDs",async()=>{
    await page.setContent(`<button onclick="window.clicks++">Same</button><button onclick="window.clicks++">Same</button><script>window.clicks=0</script>`);
    const result=await service.run("click",{target,locator:{role:"button",name:"Same"},timeout:100});
    assert.equal(result.performed,"unknown");assert.match(result.verification.error,/strict mode/);
    assert.equal(await page.evaluate(()=>window.clicks),0);
    await assert.rejects(service.run("click",{target:target.slice(0,8),locator:{text:"Same"}}),/exactly/);
  });
  await t.test("failed verification records delivered input without replaying it",async()=>{
    await page.setContent(`<button onclick="window.clicks++">Once</button><script>window.clicks=0</script>`);
    // Leave time for actionability on busy fleet hosts; the missing postcondition
    // should exhaust the deadline after dispatch, rather than the click itself.
    const result=await service.run("click",{target,locator:{text:"Once"},after:{locator:{text:"Never appears"}},timeout:1500});
    assert.equal(result.performed,true);assert.equal(result.verification.ok,false);
    assert.equal(await page.evaluate(()=>window.clicks),1);
  });
  await t.test("CLI and MCP both reach the semantic daemon with the same exact page", async()=>{
    const config=join(dir,"puppet-test.json"),sock=join(dir,"puppet.sock");
    await writeFile(config,JSON.stringify({machines:{fixture:{cdpUrl:`http://127.0.0.1:${port}`}}}));
    const env={...process.env,HOME:dir,SLAB_PUPPET_CONFIG:config,SLAB_PUPPET_SOCK:sock,SLAB_INPUT_LEASE_DIR:join(dir,"leases")};
    const daemon=spawn(process.execPath,["slab/bin/puppet.mjs","daemon"],{env,stdio:["ignore","pipe","pipe"]});
    try {
      await new Promise((resolve,reject)=>{
        const timer=setTimeout(()=>reject(new Error("fixture daemon startup timeout")),5000);
        daemon.once("error",error=>{clearTimeout(timer);reject(error);});
        daemon.stdout.on("data",chunk=>{if(String(chunk).includes("connected (")){clearTimeout(timer);resolve();}});
      });
      const cli=spawn(process.execPath,["slab/bin/puppet.mjs","snapshot","fixture",`--target=${target}`],{env,stdio:["ignore","pipe","pipe"]});
      let output="",errors="";cli.stdout.on("data",chunk=>output+=chunk);cli.stderr.on("data",chunk=>errors+=chunk);
      const [code]=await once(cli,"close");assert.equal(code,0,errors);
      assert.equal(JSON.parse(output).observation.target,target);
      const mcp=spawn(process.execPath,["slab/bin/puppet-mcp.mjs"],{env,stdio:["pipe","pipe","pipe"]});
      let reply="";mcp.stdout.on("data",chunk=>reply+=chunk);
      mcp.stdin.end(JSON.stringify({jsonrpc:"2.0",id:1,method:"tools/call",params:{name:"puppet_click",arguments:{machine:"fixture",target,locator:{text:"Once"},after:{locator:{text:"Once"}}}}})+'\n');
      await once(mcp,"close");
      const result=JSON.parse(reply).result;assert.equal(result.isError,undefined);
      const evidence=JSON.parse(result.content[0].text);assert.equal(evidence.performed,true);assert.equal(evidence.verification.ok,true);
    } finally {const closed=once(daemon,"close");daemon.kill();await closed;}
  });
  await t.test("closed page IDs cannot drift to another page",async()=>{
    const other=await context.newPage();await other.setContent(`<button>Other</button>`);
    await page.close();
    await assert.rejects(service.run("click",{target,locator:{text:"Other"}}),/gone/);
  });
});
