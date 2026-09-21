import test from 'node:test';
import assert from 'node:assert/strict';
import { chromium } from 'playwright-core';
import { choosePageTarget } from '../lib/puppet-choose.mjs';

test('Jev selects from fresh browser controls without sending input', async t => {
  const browser = await chromium.launch({ channel: 'chrome', headless: true });
  t.after(() => browser.close());
  const page = await browser.newPage();
  const reply = { answers: { next: { choice: 'target_0', probabilities: { target_0: .98 } } } };
  const goal = 'Choose the answer';
  await t.test('only named visible enabled unambiguous controls are candidates', async () => {
    await page.setContent('<button onclick="document.querySelector(\'output\').textContent=\'clicked\'">brief</button><button hidden>hidden</button><button disabled>disabled</button><button>duplicate</button><button>duplicate</button><input value="private value"><output>untouched</output>');
    const result = await choosePageTarget(page, 'private-page-id', { goal }, { evaluate: async request => {
      assert.deepEqual(request.state.targets, [{ id: 'target_0', label: 'brief', role: 'button' }]);
      assert.doesNotMatch(JSON.stringify(request), /private|onclick|output|locator/);
      return reply;
    } });
    assert.deepEqual(result.candidate.locator, { role: 'button', name: 'brief' });
    assert.equal(result.candidate.node, undefined);
    assert.equal(result.performed, false);
    assert.equal(await page.locator('output').textContent(), 'untouched');
  });
  await t.test('replaced controls with identical labels are rejected', async () => {
    await page.setContent('<button>brief</button>');
    const result = await choosePageTarget(page, 'page', { goal }, { evaluate: async () => {
      await page.setContent('<button>brief</button>'); return reply;
    } });
    assert.equal(result.reason, 'control_changed'); assert.equal(result.performed, false);
  });
  await t.test('disabled controls after inference are rejected', async () => {
    await page.setContent('<button>brief</button>');
    const result = await choosePageTarget(page, 'page', { goal }, { evaluate: async () => {
      await page.locator('button').evaluate(b => b.disabled = true); return reply;
    } });
    assert.equal(result.reason, 'control_changed');
  });
  await t.test('unknown prior input, dense pages, and invalid goals never call Jev', async () => {
    const options = { evaluate: () => assert.fail('must not call') };
    assert.equal((await choosePageTarget(page, 'page', { goal, previousOutcome: 'unknown' }, options)).reason, 'verify_previous_action');
    await page.setContent(Array.from({ length: 41 }, (_, i) => `<button>Word ${i}</button>`).join(''));
    assert.equal((await choosePageTarget(page, 'page', { goal }, options)).reason, 'too_many_controls');
    await assert.rejects(choosePageTarget(page, 'page', { goal: '' }, options), /goal/);
  });
  await t.test('unavailable decisions fall back without input', async () => {
    await page.setContent('<button>brief</button>');
    const result = await choosePageTarget(page, 'page', { goal }, { evaluate: async () => { throw new Error('unavailable'); } });
    assert.equal(result.reason, 'decision_unavailable'); assert.equal(result.performed, false);
  });
});
