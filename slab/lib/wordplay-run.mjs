import assert from 'node:assert/strict';

// The solver reads only rendered accessibility evidence. No game source,
// answer key, DOM evaluation, or direct game-state mutation is available here.
export async function playWordplay(call, browser) {
  const read = result => JSON.parse(result.content.find(c => c.type === 'text').text);
  const rounds = [];
  let evidence = read(await call('puppet_snapshot', browser));
  for (let round = 1; round <= 8; round++) {
    const start = performance.now();
    const clueLine = evidence.tree.split('\n').find(line => line.includes('[level=2]'));
    assert.ok(clueLine, 'The visible clue must be observed');
    const startChoice = performance.now();
    const choice = read(await call('puppet_choose', { ...browser,
      goal: `Solve this word-game clue and choose its answer button: ${clueLine.trim()}` }));
    const decisionMs = Math.round(performance.now() - startChoice);
    if (choice.action !== 'target') {
      rounds.push({ round, clue: clueLine.trim(), decisionMs, fallback: choice.reason || choice.action });
      return { complete: false, rounds };
    }
    const startClick = performance.now();
    const last = round === 8;
    evidence = read(await call('puppet_click', { ...browser, locator: choice.candidate.locator,
      after: { locator: { role: 'button', name: last ? 'Play again' : 'Next word' } } }));
    assert.equal(evidence.performed, true); assert.equal(evidence.verification.ok, true);
    const clickMs = Math.round(performance.now() - startClick);
    const correct = /Correct ·/.test(evidence.tree);
    assert.ok(correct || /The answer is /.test(evidence.tree), 'Game must report the result');
    rounds.push({ round, clue: clueLine.trim(), answer: choice.candidate.label, correct,
      decisionMs, clickMs, totalMs: Math.round(performance.now() - start), probability: choice.probability });
    if (!last) {
      evidence = read(await call('puppet_click', { ...browser, locator: { role: 'button', name: 'Next word' },
        after: { locator: { text: `Round ${round + 1} of 8` } } }));
      assert.equal(evidence.performed, true); assert.equal(evidence.verification.ok, true);
    }
  }
  return { complete: true, correct: rounds.filter(r => r.correct).length, rounds };
}
