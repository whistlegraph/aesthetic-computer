// A headless Aesel builder client. Deploy beside the relay or connect over WSS.
import { createRequire } from 'node:module';
import { resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { ACSession } from '../aesel/src/ac-session.mjs';
import { runPersonalTurn } from '../aesel/src/personal-relay.mjs';
import { DEFAULT_CLAUDE_MODEL } from '../aesel/src/provider-defaults.mjs';
import { partialGeometry, codeExcerpt } from './build-progress.mjs';
import { OBJECT_INSTRUCTIONS, parseGeometry } from './objects.mjs';
const { WebSocket } = createRequire(
  new URL('../lith/package.json', import.meta.url),
)('ws');
// KidLisp edits: rewrite a wall's 2D program as asked, keeping it KidLisp.
const KIDLISP_INSTRUCTIONS = `You edit KidLisp programs for aesthetic.computer. KidLisp is a tiny Lisp for
generative 2D art that redraws every frame: wipe, ink, line, box, circle, point, plot, spin, zoom,
scroll, blur, contrast, repeat, def, timers like 1s or 2s... and random choices with ?, colors by name,
rainbow, and fade:a-b-c gradients. Width and height are w and h. Keep the program's character and change
what is asked. Return ONLY the new KidLisp source, no markdown, no commentary, under 1200 characters.`;

export async function generateKidlisp(job, signal, { session = new ACSession(), model = DEFAULT_CLAUDE_MODEL, onProgress = () => {} } = {}) {
  let source = '';
  const result = await runPersonalTurn({
    token: () => session.token(),
    model: `anthropic/${model}`,
    instructions: KIDLISP_INSTRUCTIONS,
    tools: [],
    signal,
    content: JSON.stringify({ request: job.prompt, source: job.source }),
    onEvent: ({ method, params }) => {
      if (method !== 'item/agentMessage/delta') return;
      source += params.delta || '';
      onProgress({ phase: 'generating', code: source.slice(-900), geometry: null });
    },
  });
  const text = String(result.text || source).replace(/^```\w*\s*/, '').replace(/\s*```\s*$/, '').trim();
  if (!text) throw Object.assign(Error('Empty KidLisp'), { buildReason: 'Aesel returned no KidLisp.' });
  return text.slice(0, 4000);
}

export async function generateObject(
  job,
  signal,
  {
    session = new ACSession(),
    model = DEFAULT_CLAUDE_MODEL,
    onProgress = () => {},
  } = {},
) {
  let correction = '';
  // Keep correcting until it admits; size is fitted, so only syntax can fail.
  const ATTEMPTS = 4;
  for (let attempt = 0; attempt < ATTEMPTS; attempt++) {
    let source = '',
      lastPreview = 0;
    onProgress({
      phase: attempt ? 'correcting' : 'generating',
      code: '',
      geometry: null,
    });
    const result = await runPersonalTurn({
      token: () => session.token(),
      model: `anthropic/${model}`,
      instructions: OBJECT_INSTRUCTIONS,
      tools: [],
      signal,
      content: JSON.stringify({
        request: job.prompt,
        previous: job.previous,
        correction,
      }),
      onEvent: ({ method, params }) => {
        if (method !== 'item/agentMessage/delta') return;
        source += params.delta || '';
        if (source.length > 12000)
          throw Object.assign(
            Error('Object source is too large; request fewer parts.'),
            { buildReason: 'Object source is too large; request fewer parts.' },
          );
        if (Date.now() - lastPreview < 150) return;
        lastPreview = Date.now();
        onProgress({
          phase: 'generating',
          code: codeExcerpt(source),
          geometry: partialGeometry(source),
        });
      },
    });
    try {
      onProgress({
        phase: 'validating',
        code: codeExcerpt(result.text),
        geometry: partialGeometry(result.text),
      });
      const shape = parseGeometry(result.text);
      // Asked to make a sound (sing, hum, a bell...) but no tone came back:
      // send it back once to add one.
      const wantsSound = /\b(sing|sings|singing|hum|hums|humming|bell|chime|gong|drum|horn|whistle|music|musical|buzz|buzzing|ring|ringing|song|melody|beep|speaker)\b/i.test(job.prompt || '');
      if (wantsSound && !/\(tone\s/.test(shape.source || '') && attempt < ATTEMPTS - 1) {
        correction = 'The request asks for sound but the object has no (tone ...). Add at least one tone, using beat and pulse for rhythm, and keep the shape. Previous output: ' + result.text.slice(0, 8000);
        continue;
      }
      return shape;
    } catch (error) {
      if (attempt === ATTEMPTS - 1)
        throw Object.assign(error, {
          buildReason: `Object rejected: ${error.message}`,
        });
      correction = `Your previous output was rejected: ${error.message}. Fix it using only the supported dialect and bounds. Previous output: ${result.text.slice(0, 8000)}`;
    }
  }
}
export function connectBuilder({
  url,
  token,
  generate = generateObject,
  onState = () => {},
}) {
  const address = new URL(url);
  if (
    address.protocol !== 'wss:' &&
    !(
      address.protocol === 'ws:' &&
      ['127.0.0.1', 'localhost', '[::1]'].includes(address.hostname)
    )
  )
    throw Error('Builders require WSS or loopback');
  if (!token || token.length < 32)
    throw Error('DOOL_BUILDER_TOKEN must be at least 32 characters');
  let socket,
    retry,
    stopped = false;
  const jobs = new Map();
  // Results finished while the server was away, sent on reconnect.
  const pending = [];
  const deliver = (message) => {
    if (socket?.readyState === WebSocket.OPEN) socket.send(JSON.stringify(message));
    else pending.push(message);
  };
  function connect() {
    socket = new WebSocket(address, {
      headers: { authorization: `Bearer ${token}` },
    });
    const current = socket;
    current.on('open', () => {
      onState('connected');
      const list = [...jobs.entries()].map(([id, c]) => ({ id, meta: c.meta, phase: c.phase, code: c.code }));
      if (list.length) current.send(JSON.stringify({ type: 'resume', jobs: list }));
      for (const message of pending.splice(0)) current.send(JSON.stringify(message));
    });
    current.on('error', () => {});
    current.on('message', async (raw) => {
      let job;
      try {
        job = JSON.parse(raw);
      } catch {
        return;
      }
      if (job.type === 'cancel') {
        jobs.get(job.id)?.abort();
        jobs.delete(job.id);
        return;
      }
      if (job.type !== 'build' || jobs.has(job.id) || jobs.size >= 3) return;
      const controller = new AbortController();
      // Kept with the job: what to hand back if the server restarts.
      controller.meta = job.meta || null;
      controller.phase = 'generating';
      controller.code = '';
      jobs.set(job.id, controller);
      const timeout = setTimeout(() => {
        controller.timedOut = true;
        controller.abort();
      }, 170000);
      try {
        let lastProgress = 0;
        const onProgress = (progress) => {
          controller.phase = progress.phase;
          controller.code = progress.code || controller.code;
          if (controller.signal.aborted || socket.readyState !== WebSocket.OPEN) return;
          if (
            Date.now() - lastProgress < 150 &&
            progress.phase === 'generating'
          )
            return;
          lastProgress = Date.now();
          socket.send(JSON.stringify({ type: 'progress', id: job.id, ...progress }));
        };
        if (job.kind === 'kidlisp') {
          const source = await generateKidlisp(job, controller.signal, { onProgress });
          if (!controller.signal.aborted) deliver({ type: 'built', id: job.id, source });
        } else {
          const geometry = await generate(job, controller.signal, { onProgress });
          if (!controller.signal.aborted) deliver({ type: 'built', id: job.id, geometry });
        }
      } catch (error) {
        console.log(`Aesel job failed: ${error.status || ''} ${error.message}`.trim());
        if (!controller.signal.aborted || controller.timedOut)
          deliver(
            ({
              type: 'build-error',
              id: job.id,
              error: controller.signal.aborted
                ? 'Aesel timed out. Try a simpler object.'
                : error.buildReason ||
                  (error.status === 401 || error.status === 403
                    ? 'Aesel needs its relay login refreshed.'
                    : 'Aesel relay failed. Try again.'),
            }),
          );
      } finally {
        clearTimeout(timeout);
        jobs.delete(job.id);
      }
    });
    current.on('close', () => {
      // Keep building while the server restarts; results wait in pending.
      onState('disconnected');
      if (!stopped) retry = setTimeout(connect, 2000);
    });
  }
  connect();
  return {
    close() {
      stopped = true;
      clearTimeout(retry);
      for (const c of jobs.values()) c.abort();
      socket.close();
    },
  };
}
if (
  process.argv[1] &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url)
) {
  const { readFileSync } = await import('node:fs');
  const { homedir } = await import('node:os');
  const client = connectBuilder({
    url: process.env.DOOL_SERVER_URL || 'ws://127.0.0.1:8788/builder',
    token:
      process.env.DOOL_BUILDER_TOKEN ||
      readFileSync(`${process.env.DOOL_STATE_DIR || `${homedir()}/.local/share/dool-studio`}/builder-token`, 'utf8').trim(),
    onState: (state) => console.log(`Aesel builder ${state}`),
  });
  for (const signal of ['SIGINT', 'SIGTERM'])
    process.once(signal, () => {
      client.close();
      process.exit(0);
    });
}
