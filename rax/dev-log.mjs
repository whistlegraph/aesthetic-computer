import { open, stat } from 'node:fs/promises';
import { watch } from 'node:fs';
import { homedir } from 'node:os';
import { basename } from 'node:path';

// The live strip shows what the agent editing the game is doing. It reads
// that agent's own transcript (Claude Code or Codex JSONL) and keeps only its
// short notes and a one-word summary of each tool call. Rooms may be public,
// so every line is redacted before it leaves the server.

const MAX = 96;
const home = homedir();

export function redact(text) {
  return String(text)
    .replaceAll(home, '~')
    .replace(/\/(?:Users|home)\/[^/\s'"]+/g, '~')
    .replace(/\b(?:sk|pk|rk|ghp|gho|xox[abp])[-_][A-Za-z0-9_-]{8,}/g, '[key]')
    .replace(/\bBearer\s+\S+/gi, 'Bearer [key]')
    .replace(/\b[A-Za-z0-9_+/=-]{32,}\b/g, '[key]')
    .replace(/\b(token|secret|password|passwd|apikey|api_key)(\s*[=:]\s*)\S+/gi, '$1$2[key]')
    .replace(/[^\x20-\x7e]+/g, ' ')
    .replace(/\s+/g, ' ')
    .trim();
}

const clip = (text) => {
  const clean = redact(text);
  return clean.length > MAX ? clean.slice(0, MAX - 3).trimEnd() + '...' : clean;
};

// The first sentence carries the news; the rest is detail for the terminal.
const sentence = (text) => {
  const first = String(text).split(/\n\s*\n/)[0].replace(/[*`_#>]/g, '');
  const match = first.match(/^.*?[.!?](?=\s|$)/s);
  return clip(match ? match[0] : first);
};

const fileOf = (path) => (path ? basename(String(path)) : '');

// A few lines of what a step actually does: code being written, or the
// command being run. Redacted and clipped like everything else.
function detail(name, input = {}) {
  const lines = (text, mark, n = 3) =>
    String(text || '')
      .split('\n')
      .map((l) => l.trim())
      .filter(Boolean)
      .slice(0, n)
      .map((l) => clip(`${mark}${l}`));
  if (name === 'Edit' || name === 'MultiEdit')
    return [...lines(input.old_string, '- ', 1), ...lines(input.new_string, '+ ', 3)];
  if (name === 'Write') return lines(input.content, '+ ', 3);
  if (name === 'Bash') return lines(input.command, '$ ', 2);
  return [];
}

function codexDetail(source = '') {
  const added = String(source)
    .split('\n')
    .filter((l) => l.startsWith('+') && !l.startsWith('+++') && l.slice(1).trim())
    .slice(0, 3)
    .map((l) => clip(`+ ${l.slice(1).trim()}`));
  if (added.length) return added;
  const cmd = String(source).match(/"?cmd"?\s*:\s*"((?:[^"\\]|\\.)*)"/);
  return cmd ? [clip(`$ ${cmd[1].replace(/\\n.*/s, '')}`)] : [];
}

function claudeTool(name, input = {}) {
  if (['Edit', 'Write', 'NotebookEdit', 'MultiEdit'].includes(name))
    return `EDIT ${fileOf(input.file_path || input.notebook_path)}`;
  if (name === 'Read') return `READ ${fileOf(input.file_path)}`;
  if (name === 'Bash')
    return input.description
      ? `RUN ${input.description}`
      : `RUN ${String(input.command || '').trim().split(/\s+/)[0] || ''}`;
  if (name === 'Grep' || name === 'Glob') return 'SEARCH';
  return '';
}

function codexTool(source = '') {
  const files = [
    ...String(source).matchAll(/\*\*\* (?:Update|Add|Delete) File: (\S+)/g),
  ].map((m) => fileOf(m[1]));
  if (files.length) return `EDIT ${[...new Set(files)].slice(0, 3).join(' ')}`;
  const cmd = String(source).match(/"?cmd"?\s*:\s*"((?:[^"\\]|\\.)*)"/);
  if (cmd) {
    const word = cmd[1].replace(/\\n.*/s, '').trim().split(/\s+/);
    const tool = word[0] === 'cd' ? word[3] || word[0] : word[0];
    return `RUN ${fileOf(tool) || 'shell'}`;
  }
  if (/view_image|screencapture/.test(source)) return 'LOOK';
  return '';
}

// One transcript line in, zero or more strip entries out.
export function devEntries(line) {
  let row;
  try {
    row = JSON.parse(line);
  } catch {
    return [];
  }
  const out = [];
  const push = (kind, text, more = []) => {
    const clean = kind === 'say' ? sentence(text) : clip(text);
    if (clean) out.push({ kind, text: clean, detail: more });
  };
  // Claude Code transcript. A tool result marks the running step done.
  if (row.type === 'user' && Array.isArray(row.message?.content)) {
    if (row.message.content.some((b) => b.type === 'tool_result')) out.push({ kind: 'done', text: '' });
    return out;
  }
  if (row.type === 'assistant' && Array.isArray(row.message?.content)) {
    for (const block of row.message.content) {
      if (block.type === 'text') push('say', block.text);
      if (block.type === 'tool_use')
        push('do', claudeTool(block.name, block.input), detail(block.name, block.input));
    }
    return out;
  }
  // Codex rollout.
  const p = row.payload;
  if (row.type === 'response_item' && p) {
    if (p.type === 'message' && p.role === 'assistant')
      for (const c of p.content || [])
        if (typeof c?.text === 'string') push('say', c.text);
    if (p.type === 'function_call' || p.type === 'custom_tool_call')
      push('do', codexTool(p.arguments || p.input), codexDetail(p.arguments || p.input));
    if (p.type === 'function_call_output' || p.type === 'custom_tool_call_output')
      out.push({ kind: 'done', text: '' });
  }
  return out;
}

// Tail a growing JSONL file from its current end. Returns stop().
export function tailDevLog(path, onEntry, { interval = 500 } = {}) {
  let offset = null,
    rest = '',
    busy = false,
    stopped = false;
  const read = async () => {
    if (busy || stopped) return;
    busy = true;
    try {
      const { size } = await stat(path);
      if (offset === null || size < offset) offset = size;
      if (size > offset) {
        const handle = await open(path, 'r');
        try {
          const length = Math.min(size - offset, 1 << 20);
          const buffer = Buffer.alloc(length);
          await handle.read(buffer, 0, length, offset);
          offset += length;
          const lines = (rest + buffer.toString('utf8')).split('\n');
          rest = lines.pop();
          for (const line of lines)
            for (const entry of devEntries(line)) onEntry(entry);
        } finally {
          await handle.close();
        }
      }
    } catch {
      /* The transcript may not exist yet. */
    } finally {
      busy = false;
    }
  };
  let watcher;
  try {
    watcher = watch(path, () => void read());
  } catch {}
  const timer = setInterval(read, interval);
  void read();
  return () => {
    stopped = true;
    clearInterval(timer);
    watcher?.close();
  };
}
