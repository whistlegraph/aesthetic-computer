import assert from 'node:assert/strict';
import test from 'node:test';
import {markdown, tableRows, splitTables, planRows, renderFrame, cleanText, cellTone, hostInk, color} from '../src/render.mjs';
import {beginTurnActivity, recordToolActivity, finishTurnActivity, toolDetail, planFrom} from '../src/turn-activity.mjs';

const TABLE = '| Site | Visits |\n|---|---|\n| aesthetic.computer | 60 |\n| laklok.com | 27 |';

test('a Markdown table is laid out in columns, numbers set right', () => {
  const rows = tableRows(TABLE, 60, false);
  assert.deepEqual(rows, ['Site                Visits', '──────────────────────────', 'aesthetic.computer      60', 'laklok.com              27']);
  assert.deepEqual(splitTables(`Here.\n${TABLE}\nThat's all.`).map(p => p.table), [false, true, false]);
  assert.equal(splitTables('a | b | c\nno rule here').length, 1, 'pipes without a rule row are prose');
});

test('table cells take their tone from their value', () => {
  assert.equal(cellTone('Visits', {header: true}), 'highlight bold');
  assert.equal(cellTone('93', {max: 93}), 'highlight bold', 'the largest number in a column');
  assert.equal(cellTone('2', {max: 93}), 'soft', 'a small share of the column');
  assert.equal(cellTone('40', {max: 93}), 'text');
  for (const empty of ['0', '0%', '—', '-', '']) assert.equal(cellTone(empty, {max: 93}), 'muted', `${empty || 'blank'} reads as absence`);
  assert.equal(cellTone('aesthetic.computer', {first: true}), `host${hostInk('aesthetic.computer')} bold`);
  assert.equal(hostInk('www.nopaint.art'), hostInk('nopaint.art'), 'a site keeps its colour across subdomains');
  assert.notEqual(hostInk('nopaint.art'), hostInk('notepaint.art'), 'the two paints are two colours');
  const mains = ['aesthetic.computer', 'nopaint.art', 'sotce.net', 'laklok.com', 'notepaint.art', 'whistlegraph.org', 'notepat.com', 'oskiewar.com', 'mime.ac', 'prompt.ac'];
  assert.equal(new Set(mains.map(hostInk)).size, mains.length, 'the ten main sites take ten different colours');
  const rows = tableRows(TABLE, 60, true);
  assert.ok(rows[2].includes(color.highlight + color.bold + '    60'), 'the column max is painted in the highlight');
  const totals = tableRows(TABLE + '\n| Total | 87 |', 60, true);
  assert.ok(totals[2].includes(color.highlight + color.bold + '    60'), 'a total row does not take the highlight from the real max');
  assert.ok(totals[4].includes(color.bold + '    87'), 'the total is bold');
  assert.ok(!totals[4].includes(color.highlight), 'but not highlighted');
  assert.doesNotMatch(cleanText(rows[2]), /\x1b/);
});

test('a table too wide for the page becomes records', () => {
  const rows = tableRows('| Name | Note | Owner | Where |\n|---|---|---|---|\n| one | a long note | someone | somewhere far |', 20, false);
  assert.equal(rows[0], 'Name   one');
  assert.match(rows[1], /^Note   a long note/);
  assert.equal(rows.length, 4);
});

test('a reply renders its table inside the transcript', () => {
  const state = {profile:{name:'pro'}, account:'@jeffrey', input:'', entries:[{kind:'assistant', text:`Last 8 hours:\n\n${TABLE}`}]};
  const page = cleanText(renderFrame(state, 80, 20, false));
  assert.match(page, /Site\s+Visits/);
  assert.match(page, /aesthetic\.computer\s+60/);
  assert.doesNotMatch(page, /\|---/);
});

test('quotes, numbered lists, italics and handles keep their meaning as tones', () => {
  const {text, spans} = markdown('### Who\n2. read by @dirt and _twice_\n> said so\nsnake_case 2*3*4 mail@x.com');
  assert.equal(text, 'Who\n2. read by @dirt and twice\n│ said so\nsnake_case 2*3*4 mail@x.com');
  assert.deepEqual(spans.map(s => s.tone), ['bold', 'muted', 'text', 'italic', 'soft italic']);
  assert.equal(spans.find(s => s.handle).handle, '@dirt');
});

test('the plan is pinned, folded around the active step', () => {
  const plan = planFrom([{content:'a', status:'completed'}, {content:'b', activeForm:'doing b', status:'in_progress'}, {content:'c', status:'pending'}]);
  assert.deepEqual(plan.map(s => s.status), ['done', 'active', 'pending']);
  assert.equal(plan[1].text, 'doing b');
  assert.deepEqual(planFrom([{step:'x', status:'inProgress'}], null, 5), [{key:'x', text:'x', status:'active', since:5}]);
  // A step keeps its moment until its status moves.
  const first = planFrom([{content:'a', status:'in_progress'}, {content:'b', status:'pending'}], null, 1);
  const next = planFrom([{content:'a', status:'completed'}, {content:'b', status:'pending'}], first, 9);
  assert.deepEqual(next.map(s => s.since), [9, 1]);
  assert.deepEqual(planRows(plan, 40, 3, false, plan[1].since + 1000), ['plan · 1/3', '› doing b  now', '○ c']);
  // Faded ages: when a step started or finished; a waiting step has none.
  assert.deepEqual(planRows(plan, 40, 4, false, plan[0].since + 125000), ['plan · 1/3', '✓ a  2m ago', '› doing b  2m ago', '○ c']);
  const state = {profile:{name:'pro'}, account:'@jeffrey', input:'', entries:[], plan};
  assert.match(cleanText(renderFrame(state, 60, 16, false)), /plan · 1\/3/);
});

test('pro names the work, and a finished turn leaves a counted trail', () => {
  assert.equal(toolDetail({type:'dynamicToolCall', tool:'mcp__claude_ai_Gmail__search_threads'}), 'Gmail · search_threads');
  assert.equal(toolDetail({type:'commandExecution', command:'ls', input:{description:'List the shelf'}}), 'List the shelf');
  assert.equal(toolDetail({type:'commandExecution', command:"/bin/zsh -lc 'git status'"}), "$ git status");
  const state = {spend:{}};
  beginTurnActivity(state, 't', 'th', 0);
  for (const [id, tool] of [[1, 'mcp__analytics__a'], [2, 'mcp__analytics__b'], [3, 'Read · /x/y.md'], [4, 'TodoWrite']]) recordToolActivity(state, {id, type:'dynamicToolCall', tool}, false, {detail:true, now:0});
  recordToolActivity(state, {id:5, type:'commandExecution', command:'ls'}, false, {now:0});
  assert.deepEqual(state.turnActivity.feed.map(r => r.text).slice(0, 3), ['analytics · a', 'analytics · b', 'reading y.md']);
  assert.equal(finishTurnActivity(state, 'completed', 29000).text, 'analytics ×2 · 1 file read · 1 command · 29s');
});
