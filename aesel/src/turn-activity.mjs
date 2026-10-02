// The live tool feed is disposable. Only the turn's measured totals stay on the page.
const TOOL_TYPES = new Set(['commandExecution', 'fileChange', 'mcpToolCall', 'dynamicToolCall']);
export const FEED_LIMIT = 8;
const number = value => Number.isFinite(value) && value >= 0 ? value : 0;
const size = value => value >= 1e6 ? `${(value / 1e6).toFixed(1)}M` : value >= 1e3 ? `${(value / 1e3).toFixed(1)}k` : `${Math.round(value)}`;

export function toolLabel(item) {
  if (item.type === 'fileChange') return 'editing files';
  if (item.type === 'commandExecution') {
    const actions = item.commandActions || [];
    if (actions.length && actions.every(action => action.type === 'read')) return 'reading files';
    if (actions.some(action => action.type === 'search')) return 'searching files';
    if (actions.length && actions.every(action => action.type === 'listFiles')) return 'listing files';
    return 'running a command';
  }
  // Tool names, never arguments or executable source.
  const name = String(item.tool || 'tool').split(' · ')[0].split('__').at(-1).replace(/^functions\./, '');
  const labels = { exec: 'using tools', exec_command: 'running a command', Bash: 'running a command',
    Read: 'reading files', read_file: 'reading files', ac_symbol: 'reading source', ac_outline: 'reading source',
    Grep: 'searching files', Glob: 'finding files', apply_patch: 'editing files', Write: 'writing files', Edit: 'editing files',
    ac_api: 'checking the API', ac_frame: 'looking at the preview', ac_preview: 'checking the preview',
    aesel_settings: 'checking Aesel', wait: 'waiting for a tool', write_stdin: 'checking a process' };
  return labels[name] || name.replace(/[^a-zA-Z0-9_-]/g, ' ').replace(/[_-]+/g, ' ').trim().slice(0, 60) || 'using a tool';
}

export function beginTurnActivity(state, id, thread, now = Date.now()) {
  if (state.spend?.thread !== thread) state.spend = { thread, tokens: 0, usd: 0, billed: false };
  state.turnActivity = { id, startedAt: now, tools: new Set(), feed: [], tokens: 0, reasoning: 0, metered: false, usd: 0 };
}

export function recordToolActivity(state, item, completed = false) {
  const turn = state.turnActivity;
  if (!turn || !item?.id || !TOOL_TYPES.has(item.type)) return;
  turn.tools.add(item.id);
  let row = turn.feed.find(row => row.id === item.id);
  // A duplicate start must not bring a completed tool back to life.
  if (row?.status !== 'running' && !completed && row) return;
  if (!row) { row = { id: item.id, text: toolLabel(item), status: 'running' }; turn.feed.push(row); }
  if (completed) row.status = item.status === 'failed' || item.error || (item.exitCode != null && item.exitCode !== 0) ? 'failed' : 'done';
  if (turn.feed.length > FEED_LIMIT) turn.feed.splice(0, turn.feed.length - FEED_LIMIT);
}

export function measuredUsage(usage = {}) {
  const input = number(usage.input_tokens ?? usage.inputTokens ?? usage.prompt_tokens);
  const output = number(usage.output_tokens ?? usage.outputTokens ?? usage.completion_tokens);
  // Anthropic's cache buckets are separate from input. Codex/OpenAI cached
  // input and reasoning are subsets; adding them again would inflate the bill.
  const cache = number(usage.cache_read_input_tokens ?? usage.cacheReadInputTokens)
    + number(usage.cache_creation_input_tokens ?? usage.cacheCreationInputTokens);
  const total = usage.total_tokens ?? usage.totalTokens;
  return {
    tokens: total == null ? input + output + cache : number(total),
    reasoning: number(usage.reasoning_output_tokens ?? usage.reasoningOutputTokens ?? usage.output_tokens_details?.reasoning_tokens ?? usage.completion_tokens_details?.reasoning_tokens),
    usd: number(usage.cost ?? usage.costUSD),
    metered: [total, usage.input_tokens, usage.inputTokens, usage.prompt_tokens, usage.output_tokens, usage.outputTokens, usage.completion_tokens].some(Number.isFinite),
  };
}

function addUsage(state, usage) {
  const turn = state.turnActivity;
  if (!turn) return;
  turn.tokens += usage.tokens; turn.reasoning += usage.reasoning; turn.usd += usage.usd;
  turn.metered ||= usage.metered;
  if (turn.receipt) turn.receipt.text = turnUsageText(turn);
}

export function recordTurnUsage(state, usage) {
  const measured = measuredUsage(usage);
  state.spend.tokens += measured.tokens; state.spend.usd += measured.usd;
  state.spend.billed ||= Number.isFinite(usage.cost ?? usage.costUSD);
  addUsage(state, measured);
}

export function recordCodexUsage(state, params) {
  if (params.threadId !== state.spend.thread || !params.tokenUsage?.total) return;
  const total = measuredUsage(params.tokenUsage.total);
  const before = state.spend.codexUsage;
  state.spend.codexUsage = total;
  state.spend.tokens = total.tokens;
  if (!state.turnActivity || params.turnId !== state.turnActivity.id) return;
  // On resume the first reading includes earlier turns. Only `last` belongs
  // to this response; subsequent cumulative updates can be differenced.
  const delta = before ? { ...total, tokens: Math.max(0, total.tokens - before.tokens), reasoning: Math.max(0, total.reasoning - before.reasoning) }
    : measuredUsage(params.tokenUsage.last || {});
  addUsage(state, delta);
}

export function turnUsageText(turn) {
  const seconds = Math.max(0, Math.round(((turn.finishedAt ?? Date.now()) - turn.startedAt) / 1000));
  const elapsed = seconds >= 60 ? `${Math.floor(seconds / 60)}m ${seconds % 60}s` : `${seconds}s`;
  return [turn.status === 'interrupted' ? 'stopped' : turn.status === 'failed' ? 'failed' : '',
    `${turn.tools.size} tool${turn.tools.size === 1 ? '' : 's'}`, elapsed,
    turn.metered ? `${size(turn.tokens)} tokens` : '',
    turn.reasoning > 0 ? `${size(turn.reasoning)} reasoning tokens` : '',
    turn.usd > 0 ? `$${turn.usd < .01 ? turn.usd.toFixed(4) : turn.usd.toFixed(3)}` : '',
  ].filter(Boolean).join(' · ');
}

export function finishTurnActivity(state, status, now = Date.now()) {
  const turn = state.turnActivity;
  if (!turn) return null;
  turn.finishedAt = now; turn.status = status;
  turn.receipt = { id: `usage-${turn.id}`, kind: 'usage', text: turnUsageText(turn), at: now };
  turn.feed = [];
  return turn.receipt;
}
