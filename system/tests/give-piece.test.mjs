import test from "node:test";
import assert from "node:assert/strict";
import * as piece from "../public/aesthetic.computer/disks/give.mjs";
const tick = () => new Promise(resolve => setImmediate(resolve));

function fixture(t, query = {}, reply = { url: "https://pay.aesthetic.computer/test" }) {
  const labels = [], requests = [], jumps = [];
  class Button {
    box = {};
    act(e, { push }) {
      const b = this.box;
      if (!this.disabled && e.is("test:push") && e.x >= b.x && e.x < b.x + b.w && e.y >= b.y && e.y < b.y + b.h) push();
    }
  }
  const drawing = { box: () => drawing, write: (label, position) => { labels.push({label, position}); return drawing; } };
  const api = { query, params: [], ui: {Button}, screen: {width:195,height:422}, hud: {labelBack(){}},
    wipe(){}, ink: () => drawing, needsPaint(){}, jump: url => jumps.push(url), net:{} };
  t.mock.method(globalThis, "fetch", async (url, options) => {
    if (url.startsWith("/api/gives?")) return {ok:true,json:async()=>({activeSubscribers:5})};
    requests.push(JSON.parse(options.body));
    return {ok:true,json:async()=>reply};
  });
  const render = () => { labels.length=0; piece.paint(api); };
  const click = label => {
    render(); const found=labels.find(item=>item.label===label); assert.ok(found, `Label: ${label}`);
    piece.act({...api,event:{x:found.position.x,y:found.position.y+1,is:name=>name==="test:push"}});
  };
  piece.boot(api); t.after(()=>piece.leave());
  return {api,labels,requests,jumps,render,click};
}

test("Give shows live count and sends one checkout with chosen amount, frequency and homepage attribution", async t => {
  const f=fixture(t,{source:"homepage"}); await tick(); f.render();
  assert.ok(f.labels.some(x=>x.label==="5"));
  f.click("16"); f.click("Once"); f.click("Give $16"); f.click("Opening..."); await tick();
  assert.deepEqual(f.requests,[{amount:1600,currency:"usd",recurring:false,source:"homepage",surface:"piece"}]);
  assert.deepEqual(f.jumps,["https://pay.aesthetic.computer/test"]);
});

test("Give keeps custom amounts within currency limits and resets presets on currency change", async t => {
  const f=fixture(t,{amount:"1",source:"private-unreviewed-value"}); await tick();
  f.click("-"); f.click("Give $1 / month"); await tick();
  assert.equal(f.requests[0].amount,100);
  assert.equal(f.requests[0].source,"give-piece");
  f.click("USD"); f.click("100"); f.click("+"); f.click("Give 101 kr / month"); await tick();
  assert.equal(f.requests[1].currency,"dkk"); assert.equal(f.requests[1].amount,10100);
});

test("Give rejects an unsafe checkout destination and offers retry", async t => {
  const f=fixture(t,{}, {url:"https://pay.aesthetic.computer.evil.test/"}); await tick();
  f.click("Give $8 / month"); await tick(); f.render();
  assert.equal(f.jumps.length,0);
  assert.ok(f.labels.some(x=>x.label.includes("Couldn't open checkout")));
  f.click("Give $8 / month"); await tick(); assert.equal(f.requests.length,2);
});

test("Leaving Give during checkout prevents late navigation", async t => {
  const f=fixture(t); await tick(); let complete;
  t.mock.method(globalThis,"fetch",()=>new Promise(resolve=>{complete=resolve}));
  f.click("Give $8 / month"); piece.leave();
  complete({ok:true,json:async()=>({url:"https://pay.aesthetic.computer/test"})}); await tick();
  assert.deepEqual(f.jumps,[]);
});
