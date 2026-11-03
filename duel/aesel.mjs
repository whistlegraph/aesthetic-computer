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
      return parseGeometry(result.text);
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
  function connect() {
    socket = new WebSocket(address, {
      headers: { authorization: `Bearer ${token}` },
    });
    const current = socket;
    current.on('open', () => onState('connected'));
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
      if (job.type !== 'build' || jobs.has(job.id) || jobs.size >= 2) return;
      const controller = new AbortController();
      jobs.set(job.id, controller);
      const timeout = setTimeout(() => controller.abort(), 170000);
      try {
        let lastProgress = 0;
        const onProgress = (progress) => {
          if (
            controller.signal.aborted ||
            current.readyState !== WebSocket.OPEN
          )
            return;
          if (
            Date.now() - lastProgress < 150 &&
            progress.phase === 'generating'
          )
            return;
          lastProgress = Date.now();
          current.send(
            JSON.stringify({ type: 'progress', id: job.id, ...progress }),
          );
        };
        const geometry = await generate(job, controller.signal, { onProgress });
        if (!controller.signal.aborted && current.readyState === WebSocket.OPEN)
          current.send(JSON.stringify({ type: 'built', id: job.id, geometry }));
      } catch (error) {
        if (current.readyState === WebSocket.OPEN)
          current.send(
            JSON.stringify({
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
      for (const controller of jobs.values()) controller.abort();
      jobs.clear();
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
  const client = connectBuilder({
    url: process.env.DOOL_SERVER_URL,
    token: process.env.DOOL_BUILDER_TOKEN,
    onState: (state) => console.log(`Aesel builder ${state}`),
  });
  for (const signal of ['SIGINT', 'SIGTERM'])
    process.once(signal, () => {
      client.close();
      process.exit(0);
    });
}
