// The Safari driver's in-page library is plain DOM code; check its semantic
// contract (strict locators, actionability, fill, snapshot) in headless Chrome.
// Skips when no local Chrome is installed.
import test from "node:test";
import assert from "node:assert/strict";
import { existsSync } from "node:fs";
import { evalCall, pageCall, pendingCall } from "../lib/safari-page.mjs";

const CHROME = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
const FIXTURE = `<!doctype html><body>
<header><a href="/home">Home</a></header>
<main>
  <h1>Sign in</h1>
  <label for="em">Email</label><input id="em" type="email">
  <label>Password <input type="password" value="hunter2"></label>
  <button id="go" onclick="document.getElementById('done').hidden=false">Continue</button>
  <button>Twin</button><button>Twin</button>
  <button disabled>Later</button>
  <div style="display:none"><button>Ghost</button></div>
  <div data-testid="note" contenteditable="true">old</div>
  <p id="done" hidden>Welcome back</p>
  <div style="position:relative"><button id="under">Covered</button><div style="position:absolute;inset:0;background:#fff"></div></div>
</main></body>`;

test("safari page library", { skip: !existsSync(CHROME) && "no local Chrome" }, async () => {
  const { chromium } = await import("playwright-core");
  const browser = await chromium.launch({ executablePath: CHROME, headless: true });
  try {
    const page = await browser.newPage();
    await page.setContent(FIXTURE);
    const call = async c => { const r = JSON.parse(await page.evaluate(pageCall(c))); if (r.error) throw new Error(r.error); return r.ok; };

    assert.equal((await call(`P.actionable({role:"button",name:"Continue"},{})`)).ready, true);
    const twin = await call(`P.actionable({role:"button",name:"Twin"},{})`);
    assert.equal(twin.strict, true);
    assert.match((await call(`P.actionable({role:"button",name:"Later"},{})`)).reason, /disabled/);
    assert.match((await call(`P.actionable({role:"button",name:"Ghost"},{})`)).reason, /no element/);
    assert.match((await call(`P.actionable({css:"#under"},{})`)).reason, /covered/);
    await assert.rejects(call(`P.actionable({role:"button"},{})`), /exact accessible name/);
    await assert.rejects(call(`P.actionable({role:"button",name:"x",css:"b"},{})`), /exactly one locator/);

    assert.equal((await call(`P.fill({label:"Email"},"me@fuser.studio")`)).filled, true);
    assert.equal(await page.inputValue("#em"), "me@fuser.studio");
    assert.equal((await call(`P.fill({testId:"note"},"new text")`)).filled, true);
    assert.equal(await page.textContent("[data-testid=note]"), "new text");
    assert.equal((await call(`P.fill({label:"Password"},"pw")`)).filled, true);

    assert.equal(await call(`P.satisfied({text:"Welcome back"},"hidden")`), true);
    await page.click("#go");
    assert.equal(await call(`P.satisfied({text:"Welcome back"},"visible")`), true);

    const snap = await call(`P.snapshot(24000)`);
    assert.match(snap.tree, /- banner:\n  - link "Home"/);
    assert.match(snap.tree, /heading "Sign in" \[level=1\]/);
    assert.match(snap.tree, /textbox "Email": "me@fuser.studio"/);
    assert.match(snap.tree, /textbox "Password": "••••"/);
    assert.match(snap.tree, /button "Later" \[disabled\]/);
    assert.doesNotMatch(snap.tree, /Ghost|pw"/);

    assert.deepEqual(JSON.parse(await page.evaluate(evalCall("document.title || 40+2"))), { value: 42 });
    assert.deepEqual(JSON.parse(await page.evaluate(evalCall("undefined"))), { undef: true });
    const pending = JSON.parse(await page.evaluate(evalCall("new Promise(r => setTimeout(() => r('later'), 20))")));
    assert.ok(pending.pending);
    await page.waitForTimeout(60);
    assert.deepEqual(JSON.parse(await page.evaluate(pendingCall(pending.pending))), { value: "later" });
    assert.match(JSON.parse(await page.evaluate(evalCall("nope.nope"))).error, /nope/);
  } finally { await browser.close(); }
});
