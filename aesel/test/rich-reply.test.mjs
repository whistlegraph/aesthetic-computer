import assert from 'node:assert/strict';
import test from 'node:test';
import {markdown, tableRows, splitTables, planRows, renderFrame, cleanText} from '../src/render.mjs';
import {beginTurnActivity, recordToolActivity, finishTurnActivity, toolDetail, planFrom} from '../src/turn-activity.mjs';

const TABLE = '| Site | Visits |\n|---|---|\n| aesthetic.computer | 60 |\n| laklok.com | 27 |';

test('a Markdown table is laid out in columns, numbers set right', () => {
  const rows = tableRows(TABLE, 60, false);
  assert.deepEqual(rows, ['Site                Visits', '──────────────────────────', 'aesthetic.computer      60', 'laklok.com              27']);
  assert.deepEqual(splitTables(`Here.\n${TABLE}\nThat's all.`).map(p => p.table), [false, true, false]);
  assert.equal(splitTables('a | b | c\nno rule here').length, 1, 'pipes without a rule row are prose');
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
  assert.deepEqual(planFrom([{step:'x', status:'inProgress'}]), [{text:'x', status:'active'}]);
  assert.deepEqual(planRows(plan, 40, 3, false), ['plan · 1/3', '› doing b', '○ c']);
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
