import test from 'node:test';
import assert from 'node:assert/strict';
import { createCommentCounts } from '../system/public/aesthetic.computer/lib/chat-comments.mjs';

test('counts are batched, keyed by message identity, and omit deleted/invalid messages', async () => {
  const messages = Array.from({ length: 105 }, (_, i) => ({ id: i.toString(16).padStart(24, '0') }));
  const asked = [], changed = [];
  const counts = createCommentCounts('clock', values => changed.push(new Map(values)), async url => {
    const ids = new URL(url, 'https://example.test').searchParams.get('chatCounts').split(','); asked.push(ids);
    return { ok: true, json: async () => ({ counts: { [ids[0]]: 2 } }) };
  });
  await counts.refresh([...messages, { id: 'b'.repeat(24), deleted: true }, { id: 'not-a-record' }]);
  assert.deepEqual(asked.map(batch => batch.length), [100, 5]);
  assert.equal(counts.counts.get(messages[0].id), 2);
  assert.equal(counts.counts.get(messages[1].id), 0);
  assert.equal(counts.counts.size, 105);
  await counts.refresh(messages);
  assert.equal(asked.length, 2, 'idle frames do not refetch');
  counts.dispose(); await counts.refresh([]);
  assert.equal(asked.length, 2);
});

test('leaving the room prevents an outstanding response from updating the next piece', async () => {
  let resolve, changed = false;
  const counts = createCommentCounts('clock', () => changed = true, () => new Promise(done => resolve = done));
  const reading = counts.refresh([{ id: 'a'.repeat(24) }]);
  counts.dispose();
  resolve({ ok: true, json: async () => ({ counts: { ['a'.repeat(24)]: 7 } }) });
  await reading;
  assert.equal(changed, false); assert.equal(counts.counts.size, 0);
});
