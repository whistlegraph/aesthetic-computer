// The live tool feed is disposable. What stays on the page is the turn's trail
// (what kinds of work it did, counted) and its measured totals.
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
  state.turnActivity = { id, startedAt: now, tools: new Set(), kinds: new Map(), feed: [], tokens: 0, reasoning: 0, metered: false, usd: 0 };
}

const base = path => String(path || '').split('/').filter(Boolean).at(-1) || String(path || '');
const oneLine = value => String(value || '').replace(/\s+/g, ' ').trim();

// Pro is someone's own work, so the feed names it: the command, the file, the
// MCP server and tool. Piece mode keeps toolLabel's plain verbs.
export function toolDetail(item) {
  const input = item.input || item.arguments || {};
  if (item.type === 'fileChange') {
    const files = (item.changes || []).map(change => base(change.path)).filter(Boolean);
    return files.length ? `editing ${files.slice(0, 3).join(', ')}${files.length > 3 ? ` +${files.length - 3}` : ''}` : 'editing files';
  }
  if (item.type === 'commandExecution') {
    if (typeof input.description === 'string' && input.description.trim()) return oneLine(input.description);
    const command = oneLine(Array.isArray(item.command) ? item.command.join(' ') : item.command);
    const wrapped = /^(?:\/bin\/)?(?:ba|z)?sh -l?c (['"]?)(.*)\1$/.exec(command);
    return `$ ${wrapped ? wrapped[2] : command}`;
  }
  if (item.type === 'mcpToolCall' && item.server) return `${item.server} · ${item.tool}`;
  const [name, detail] = String(item.tool || 'tool').split(' · ');
  const mcp = /^mcp__(.+?)__(.+)$/.exec(name);
  if (mcp) return `${mcp[1].replace(/^claude_ai_/, '')} · ${mcp[2]}`;
  const says = { Read: 'reading', Grep: 'searching', Glob: 'finding', WebFetch: 'fetching', WebSearch: 'searching the web for', Agent: 'agent:', Task: 'agent:', Skill: 'skill' }[name];
  const what = name === 'Read' ? base(detail) : name === 'WebFetch' ? (detail || '').replace(/^https?:\/\/([^/]+).*/, '$1') : detail;
  return oneLine(says ? `${says} ${what || ''}` : what ? `${name} · ${what}` : name);
}

// One word per kind of work, for the trail a finished turn leaves behind.
function toolKind(item) {
  if (item.type === 'fileChange') return ['edit', 'edit', 'edits'];
  if (item.type === 'commandExecution') return ['command', 'command', 'commands'];
  if (item.type === 'mcpToolCall' && item.server) return [item.server, item.server, item.server];
  const name = String(item.tool || '').split(' · ')[0];
  const mcp = /^mcp__(.+?)__/.exec(name);
  if (mcp) { const server = mcp[1].replace(/^claude_ai_/, ''); return [server, server, server]; }
  if (name === 'Read') return ['read', 'file read', 'files read'];
  if (name === 'Grep' || name === 'Glob') return ['search', 'search', 'searches'];
  if (name === 'Agent' || name === 'Task') return ['agent', 'agent', 'agents'];
  return [name || 'tool', name || 'tool', name || 'tool'];
}

export function toolTrail(kinds) {
  return [...kinds.entries()].sort((a, b) => b[1].count - a[1].count)
    .map(([, kind]) => kind.one === kind.many ? (kind.count === 1 ? kind.one : `${kind.one} ×${kind.count}`) : `${kind.count} ${kind.count === 1 ? kind.one : kind.many}`)
    .join(' · ');
}

// The model's own checklist — Claude's TodoWrite, Codex's turn plan — as one
// shape: what the step is, and whether it is waiting, under way or done.
export function planFrom(steps) {
  if (!Array.isArray(steps)) return null;
  const plan = steps.map(step => {
    const status = String(step?.status || 'pending');
    return {
      text: oneLine(status === 'in_progress' || status === 'inProgress' ? step.activeForm || step.content || step.step : step.content || step.step),
      status: status === 'completed' ? 'done' : status === 'in_progress' || status === 'inProgress' ? 'active' : 'pending',
    };
  }).filter(step => step.text);
  return plan.length ? plan : null;
}

export const isPlanTool = item => item?.type === 'dynamicToolCall' && String(item.tool || '').split(' · ')[0] === 'TodoWrite';

export function recordToolActivity(state, item, completed = false, {detail = false, now = Date.now()} = {}) {
  const turn = state.turnActivity;
  if (!turn || !item?.id || !TOOL_TYPES.has(item.type) || isPlanTool(item)) return;
  if (!turn.tools.has(item.id)) {
    const [key, one, many] = toolKind(item);
    const kind = turn.kinds.get(key) || { one, many, count: 0 };
    kind.count += 1; turn.kinds.set(key, kind);
  }
  turn.tools.add(item.id);
  let row = turn.feed.find(row => row.id === item.id);
  // A duplicate start must not bring a completed tool back to life.
  if (row?.status !== 'running' && !completed && row) return;
  if (!row) { row = { id: item.id, text: detail ? toolDetail(item) : toolLabel(item), status: 'running', startedAt: now }; turn.feed.push(row); }
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
    turn.kinds?.size ? toolTrail(turn.kinds) : `${turn.tools.size} tool${turn.tools.size === 1 ? '' : 's'}`, elapsed,
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
