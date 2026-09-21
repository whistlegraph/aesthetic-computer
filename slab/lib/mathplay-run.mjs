import assert from 'node:assert/strict';

// Read only Puppet's rendered accessibility evidence. The solver has no game
// state, answer key, DOM evaluation, or keyboard shortcut to bypass clicking.
export async function playMathplay(call,browser){
  const read=r=>JSON.parse(r.content.find(c=>c.type==='text').text);
  const rounds=[],initial=performance.now();
  let evidence=read(await call('puppet_snapshot',browser));
  const initialObservationMs=performance.now()-initial,started=performance.now();
  for(let round=1;round<=30;round++){
    const t=performance.now();
    assert.ok(evidence.tree.includes(`Round ${round} of 30`),'Fresh round must be observed');
    const line=evidence.tree.split('\n').find(s=>s.includes('[level=2]'));
    const match=line?.match(/(\d+) ([+−×]) (\d+) = \?/);assert.ok(match,'Visible arithmetic problem required');
    const a=Number(match[1]),b=Number(match[3]);
    const answer=String(match[2]==='+'?a+b:match[2]==='−'?a-b:a*b);
    assert.ok(evidence.tree.includes(`button "${answer}"`),'Answer must be a visible button');
    const clickStart=performance.now();
    evidence=read(await call('puppet_click',{...browser,locator:{role:'button',name:answer},
      after:{locator:{text:`Correct · Round ${round}`}}}));
    assert.equal(evidence.performed,true);assert.equal(evidence.verification.ok,true);
    assert.ok(evidence.tree.includes(`Score: ${round} / ${round}`),'Exactly one correct answer per transition');
    rounds.push({round,problem:match[0],answer,clickMs:+(performance.now()-clickStart).toFixed(2),loopMs:+(performance.now()-t).toFixed(2)});
  }
  assert.ok(evidence.tree.includes('Complete'));
  const ms=rounds.map(r=>r.clickMs).sort((a,b)=>a-b);
  return {complete:true,correct:30,initialObservationMs:+initialObservationMs.toFixed(2),elapsedMs:+(performance.now()-started).toFixed(2),
    medianClickMs:ms[15],p95ClickMs:ms[28],maxClickMs:ms[29],under100ms:ms.filter(n=>n<100).length,rounds,
    scope:'Local HTTP/MCP, rendered-question parsing, deterministic arithmetic, click, verification and next observation. Excludes language-model inference.'};
}
