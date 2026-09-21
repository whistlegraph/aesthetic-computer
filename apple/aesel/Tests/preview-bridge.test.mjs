import test from 'node:test';
import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import vm from 'node:vm';

// Execute the shipped WKUserScript with a ready runtime; URL-only tests miss
// the dropped-source handoff which previously restored the corner HUD.
const swift=readFileSync(new URL('../Sources/AeselPieceView.swift',import.meta.url),'utf8');
const script=swift.match(/addUserScript\(WKUserScript\(source: """\n([\s\S]*?)\n        """, injectionTime/)[1];
test('native source injection carries view flags without forwarding auth parameters',()=>{
  const sent=[],ticks=[];
  const window={preloaded:true,__aeselSource:'export function paint({wipe}) { wipe("orange"); }',acSEND:message=>sent.push(message),addEventListener(){},webkit:{messageHandlers:{previewFailure:{postMessage(){}}}}};
  vm.runInNewContext(script,{window,location:{search:'?noauth=true&nolabel=true&preview&code=private&state=private'},URLSearchParams,setInterval:fn=>(ticks.push(fn),1),clearInterval(){},setTimeout(){}});
  ticks[0]();ticks[0]();
  assert.equal(sent.length,1);
  assert.equal(sent[0].type,'dropped:piece');
  const flags=new URLSearchParams(sent[0].content.search);
  for(const key of ['nogap','nolabel','autoreload'])assert.equal(flags.get(key),'true');
  assert(flags.has('preview'));
  for(const key of ['code','state','noauth'])assert(!flags.has(key));
  window.__aeselSource+='\n// revised';window.__aeselRender();
  assert.equal(sent.length,2);assert.equal(sent[1].content.source,window.__aeselSource);
});
