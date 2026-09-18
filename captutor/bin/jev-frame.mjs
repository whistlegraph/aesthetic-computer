#!/usr/bin/env node
// Read one explicitly targeted browser page, then select a control. Never clicks.
import { parseArgs } from 'node:util';
import { randomUUID } from 'node:crypto';
import { Session } from '../lib/cdp.mjs';
import { chooseObservedTarget, candidatesFromFrame } from '../../slab/lib/jev-computer-use.mjs';
const { values } = parseArgs({ options: { cdp: { type:'string', default:'http://127.0.0.1:9222' },
  target: { type:'string' }, goal: { type:'string' } } });
if (!values.target || !values.goal) throw new Error('Usage: jev-frame.mjs --target PAGE_ID --goal "Choose Start match" [--cdp URL]. Sends the goal and up to 40 visible control labels to Jev.');
const pages = await (await fetch(new URL('/json/list', values.cdp), { signal: AbortSignal.timeout(3000) })).json();
const page = pages.find(p => p.type === 'page' && p.id === values.target);
if (!page?.webSocketDebuggerUrl) throw new Error('Exact browser target not found.');
const session = new Session(page.webSocketDebuggerUrl);
try {
  const frame = await session.frame();
  const result = await chooseObservedTarget({ goal: values.goal,
    observation: { id: randomUUID(), capturedAt: frame.capturedAt, target: page.id }, candidates: candidatesFromFrame(frame) });
  console.log(JSON.stringify(result, null, 2));
} finally { await session.close(); }
